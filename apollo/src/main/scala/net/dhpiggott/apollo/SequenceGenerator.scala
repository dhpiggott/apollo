package net.dhpiggott.apollo

import javax.sound.midi._

object SequenceGenerator {

  final case class NoteAttributes(
      octave: Octave,
      duration: Note.Duration,
      volume: Int
  ) {
    def updated(scoreElement: ScoreElement): NoteAttributes =
      scoreElement match {
        case _: Voice =>
          this

        case octave: Octave =>
          copy(octave = octave)

        case OctaveIncrement =>
          copy(octave = Octave(octave.value + 1))

        case OctaveDecrement =>
          copy(octave = Octave(octave.value - 1))

        case Chord(notes) =>
          val longestNote = notes
            .sortBy(_.duration.getOrElse(duration).value)
            .headOption
          longestNote match {
            case None              => this
            case Some(longestNote) => updated(longestNote)
          }

        case note: Note =>
          copy(
            duration = note.duration.getOrElse(duration)
          )

        case Rest(noteDuration) =>
          copy(duration = noteDuration.getOrElse(duration))

        case Barline =>
          this
      }
  }

  object NoteAttributes {
    val defaults: NoteAttributes = NoteAttributes(
      octave = Octave(4),
      duration = Note.Duration(4),
      volume = 127
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
    val (events, _) =
      part.elements.foldLeft(
        Seq.empty[MidiEvent] -> PartState(
          currentVoice = Voice(0),
          instrumentStates = Map(
            Voice(0) -> InstrumentState(
              offset = 0L,
              NoteAttributes.defaults
            )
          )
        )
      ) {
        case ((events, partState), scoreElement) =>
          scoreElement match {
            case voice @ Voice(value) if value == 0 =>
              events -> partState.copy(
                currentVoice = voice,
                instrumentStates = Map(
                  voice -> partState.instrumentStates.values.toSeq
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
                instrumentStates = partState.instrumentStates.updated(
                  voice,
                  partState.instrumentStates
                    .get(voice)
                    .getOrElse(
                      partState.instrumentStates(Voice(0))
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

            // FIXME: Allow octave changes and rests, and carry duration changes
            // within chords as per
            // https://github.com/alda-lang/alda/blob/master/doc/chords.md
            case Chord(notes) =>
              val shortestNote = notes
                .sortBy(
                  _.duration
                    .getOrElse(
                      partState.currentVoiceInstrumentState.noteAttributes.duration
                    )
                    .value
                )
                .headOption
              events ++ midiEvents(
                partState.currentVoiceInstrumentState.offset,
                notes,
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
                          shortestNote
                            .flatMap(_.duration)
                            .getOrElse(
                              partState.currentVoiceInstrumentState.noteAttributes.duration
                            ),
                          pulsesPerQuarterNote * 4
                        ),
                      noteAttributes =
                        partState.currentVoiceInstrumentState.noteAttributes
                          .copy(
                            duration = shortestNote
                              .flatMap(_.duration)
                              .getOrElse(
                                partState.currentVoiceInstrumentState.noteAttributes.duration
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
          noteAttributes.volume
        ),
        offset
      )
      noteOff = new MidiEvent(
        new ShortMessage(
          ShortMessage.NOTE_OFF,
          channel,
          toneNumber,
          noteAttributes.volume
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
