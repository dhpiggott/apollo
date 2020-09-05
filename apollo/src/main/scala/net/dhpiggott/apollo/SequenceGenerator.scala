package net.dhpiggott.apollo

import javax.sound.midi._

object SequenceGenerator {

  final case class NoteAttributes(
      octave: Octave,
      duration: Note.Duration,
      volume: Int
  )

  object NoteAttributes {
    val defaults: NoteAttributes = NoteAttributes(
      octave = Octave(4),
      duration = Note.Duration(4),
      volume = 100
    )
  }

  final case class PartState(
      currentVoice: Voice,
      instrumentStates: Map[Voice, InstrumentState]
  ) {
    def currentVoiceInstrumentState = instrumentStates(currentVoice)
  }

  final case class InstrumentState(
      offset: Long,
      noteAttributes: NoteAttributes
  )

  // TODO: DRY
  def generateSequence(part: Part, channel: Int): Sequence = {
    val pulsesPerQuarterNote = 30
    val sequence = new Sequence(Sequence.PPQ, pulsesPerQuarterNote)
    val track = sequence.createTrack()
    def generateMidiEvents(
        elements: Seq[ScoreElement],
        partState: PartState = PartState(
          currentVoice = Voice(0),
          instrumentStates = Map(
            Voice(0) -> InstrumentState(
              offset = 0L,
              NoteAttributes.defaults
            )
          )
        )
    ): (Seq[MidiEvent], PartState) =
      elements.foldLeft(
        Seq.empty[MidiEvent] -> partState
      ) {
        case ((events, partState), scoreElement) =>
          scoreElement match {
            case Attribute.Volume(_, value) =>
              events -> partState.copy(
                instrumentStates = partState.instrumentStates.updated(
                  partState.currentVoice,
                  partState.currentVoiceInstrumentState.copy(
                    noteAttributes =
                      partState.currentVoiceInstrumentState.noteAttributes.copy(
                        volume = value
                      )
                  )
                )
              )

            case voice @ Voice(value) if value == 0 =>
              events -> partState.copy(
                currentVoice = voice,
                instrumentStates = Map(
                  voice -> partState.instrumentStates
                    .filter {
                      case (Voice(value), _) => value != 0
                    }
                    .values
                    .toSeq
                    .sortBy(_.offset)
                    .lastOption
                    .getOrElse(
                      partState.currentVoiceInstrumentState
                    )
                )
              )

            case voice: Voice =>
              events -> partState.copy(
                currentVoice = voice,
                instrumentStates = partState.instrumentStates
                  .updated(
                    voice,
                    partState.instrumentStates
                      .get(voice)
                      .getOrElse(
                        partState.instrumentStates(Voice(0))
                      )
                  )
              )

            // FIXME: Check we carry octave/duration/volume changes in the same
            // way that Alda does, per
            // https://github.com/alda-lang/alda/blob/master/doc/chords.md
            case Chord(elements) =>
              def resetOffset(chordPartState: PartState): PartState =
                chordPartState.copy(
                  instrumentStates = chordPartState.instrumentStates.updated(
                    chordPartState.currentVoice,
                    chordPartState.currentVoiceInstrumentState.copy(
                      offset = partState.currentVoiceInstrumentState.offset
                    )
                  )
                )

              val (chordEvents, updatedChordPartState, shortestNoteDuration) =
                elements.foldLeft(
                  (
                    Seq.empty[MidiEvent],
                    partState,
                    partState.currentVoiceInstrumentState.noteAttributes.duration
                  )
                ) {
                  case (
                      (chordEvents, chordPartState, shortestNoteDuration),
                      chordElement
                      ) =>
                    val (chordElementEvents, updatedChordPartState) =
                      generateMidiEvents(Seq(chordElement), chordPartState)
                    val chordElementDuration = chordElement match {
                      case Note(_, Some(duration))  => Some(duration)
                      case Rest(Some(noteDuration)) => Some(noteDuration)
                      case _                        => None
                    }
                    val updatedShortestNoteDuration =
                      chordElementDuration match {
                        case None =>
                          shortestNoteDuration

                        case Some(noteDuration) =>
                          Note.Duration(
                            math.min(
                              noteDuration.value,
                              shortestNoteDuration.value
                            )
                          )
                      }
                    (
                      chordEvents ++ chordElementEvents,
                      resetOffset(updatedChordPartState),
                      updatedShortestNoteDuration
                    )
                }
              events ++ chordEvents ->
                updatedChordPartState.copy(
                  instrumentStates =
                    updatedChordPartState.instrumentStates.updated(
                      updatedChordPartState.currentVoice,
                      updatedChordPartState.currentVoiceInstrumentState.copy(
                        offset =
                          updatedChordPartState.currentVoiceInstrumentState.offset +
                            pulses(
                              shortestNoteDuration,
                              pulsesPerQuarterNote * 4
                            )
                      )
                    )
                )

            case octave: Octave =>
              events -> partState.copy(
                instrumentStates = partState.instrumentStates.updated(
                  partState.currentVoice,
                  partState.currentVoiceInstrumentState.copy(
                    noteAttributes =
                      partState.currentVoiceInstrumentState.noteAttributes.copy(
                        octave = octave
                      )
                  )
                )
              )

            case OctaveIncrement =>
              events -> partState.copy(
                instrumentStates = partState.instrumentStates.updated(
                  partState.currentVoice,
                  partState.currentVoiceInstrumentState.copy(
                    noteAttributes =
                      partState.currentVoiceInstrumentState.noteAttributes.copy(
                        octave = Octave(
                          partState.currentVoiceInstrumentState.noteAttributes.octave.value + 1
                        )
                      )
                  )
                )
              )

            case OctaveDecrement =>
              events -> partState.copy(
                instrumentStates = partState.instrumentStates.updated(
                  partState.currentVoice,
                  partState.currentVoiceInstrumentState.copy(
                    noteAttributes =
                      partState.currentVoiceInstrumentState.noteAttributes.copy(
                        octave = Octave(
                          partState.currentVoiceInstrumentState.noteAttributes.octave.value - 1
                        )
                      )
                  )
                )
              )

            case note: Note =>
              events ++ midiEvents(
                partState.currentVoiceInstrumentState.offset,
                Seq(note),
                channel,
                pulsesPerQuarterNote,
                partState.currentVoiceInstrumentState.noteAttributes
              ) ->
                partState.copy(
                  instrumentStates = partState.instrumentStates.updated(
                    partState.currentVoice,
                    partState.currentVoiceInstrumentState.copy(
                      offset = partState.currentVoiceInstrumentState.offset +
                        pulses(
                          note.duration
                            .getOrElse(
                              partState.currentVoiceInstrumentState.noteAttributes.duration
                            ),
                          pulsesPerQuarterNote * 4
                        ),
                      noteAttributes =
                        partState.currentVoiceInstrumentState.noteAttributes
                          .copy(
                            duration = note.duration
                              .getOrElse(
                                partState.currentVoiceInstrumentState.noteAttributes.duration
                              )
                          )
                    )
                  )
                )

            case Rest(noteLength) =>
              events ->
                partState.copy(
                  instrumentStates = partState.instrumentStates.updated(
                    partState.currentVoice,
                    partState.currentVoiceInstrumentState.copy(
                      offset = partState.currentVoiceInstrumentState.offset +
                        pulses(
                          noteLength
                            .getOrElse(
                              partState.currentVoiceInstrumentState.noteAttributes.duration
                            ),
                          pulsesPerQuarterNote * 4
                        ),
                      noteAttributes =
                        partState.currentVoiceInstrumentState.noteAttributes
                          .copy(
                            duration = noteLength
                              .getOrElse(
                                partState.currentVoiceInstrumentState.noteAttributes.duration
                              )
                          )
                    )
                  )
                )

            case Barline =>
              events -> partState
          }
      }
    val (events, _) = generateMidiEvents(part.elements)
    events.foreach(track.add)
    sequence
  }

  private[this] def midiEvents(
      offset: Long,
      notes: Seq[Note],
      channel: Int,
      pulsesPerQuarterNote: Int,
      noteAttributes: NoteAttributes
  ): Seq[MidiEvent] =
    (for {
      note <- notes
      toneNumber = (noteAttributes.octave.value + 1) * 12 + note.pitch.chroma
      noteOn = new MidiEvent(
        new ShortMessage(
          ShortMessage.NOTE_ON,
          channel,
          toneNumber,
          ((noteAttributes.volume / 100d) * 127).toInt
        ),
        offset
      )
      noteOff = new MidiEvent(
        new ShortMessage(
          ShortMessage.NOTE_OFF,
          channel,
          toneNumber,
          ((noteAttributes.volume / 100d) * 127).toInt
        ),
        offset + pulses(
          note.duration
            .getOrElse(noteAttributes.duration),
          pulsesPerQuarterNote * 4
        )
      )
    } yield Seq(noteOn, noteOff)).flatten

  private[this] def pulses(
      noteDuration: Note.Duration,
      pulsesPerNote: Int
  ): Long =
    (noteDuration.value * pulsesPerNote).toLong

}
