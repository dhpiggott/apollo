package net.dhpiggott.apollo

import javax.sound.midi._

object SequenceGenerator {

  final case class PartState(
      currentVoice: Voice,
      instrumentStates: Map[Voice, InstrumentState],
      markers: Map[Marker, Long]
  ) {

    def currentVoiceInstrumentState: InstrumentState =
      instrumentStates(currentVoice)

    def modifyCurrentVoiceInstrumentState(
        f: InstrumentState => InstrumentState
    ): PartState =
      copy(
        instrumentStates =
        instrumentStates.updated(
          currentVoice,
          f(currentVoiceInstrumentState)
        )
      )

  }

  final case class InstrumentState(
      offset: Long,
      octave: Octave,
      duration: Note.Duration,
      quantization: Int,
      tempo: Int,
      transposition: Int,
      volume: Int
  )

  object InstrumentState {
    // TODO: Initialize sequence using these values
    val defaults: InstrumentState = InstrumentState(
      offset = 0,
      octave = Octave(4),
      duration = Note.Duration(4),
      quantization = 90,
      tempo = 120,
      transposition = 0,
      volume = 100
    )
  }

  def generateSequence(part: Part, channel: Int): Sequence = {
    val pulsesPerQuarterNote = 128
    val sequence = new Sequence(Sequence.PPQ, pulsesPerQuarterNote)
    val track = sequence.createTrack()
    track.add(
      new MidiEvent(
        new ShortMessage(
          ShortMessage.PROGRAM_CHANGE,
          channel,
          Instruments.nonPercusssionInstruments(part.instrument),
          0
        ),
        0
      )
    )
    def generateMidiEvents(
        elements: Seq[ScoreElement],
        partState: PartState = PartState(
          currentVoice = Voice(0),
          instrumentStates = Map(
            Voice(0) -> InstrumentState.defaults
          ),
          markers = Map.empty
        )
    ): (Seq[MidiEvent], PartState) =
      elements.foldLeft(
        Seq.empty[MidiEvent] -> partState
      ) {
        case ((events, partState), scoreElement) =>
          scoreElement match {
            case Attribute.Duration(_, value) =>
              events -> partState.modifyCurrentVoiceInstrumentState(
                instrumentState =>
                  instrumentState.copy(
                    duration = Note.Duration(
                      value match {
                        case Attribute.Duration.Beats(value) =>
                          value / 4

                        case Attribute.Duration.NoteLength(denominator) =>
                          1d / denominator

                        case Attribute.Duration.Milliseconds(value) =>
                          val bpm =
                            instrumentState.tempo
                          val bps = bpm / 60d
                          val seconds = value / 1000d
                          val beats = seconds * bps
                          beats / 4
                      }
                    )
                  )
              )

            case Attribute.Octave(_, value) =>
              events -> partState.modifyCurrentVoiceInstrumentState(
                instrumentState =>
                  instrumentState.copy(
                    octave = value match {
                      case Attribute.Octave.AbsoluteValue(value) =>
                        Octave(value)

                      case Attribute.Octave.Increment =>
                        Octave(instrumentState.octave.value + 1)

                      case Attribute.Octave.Decrement =>
                        Octave(instrumentState.octave.value - 1)
                    }
                  )
              )

            case Attribute.Panning(_, value) =>
              (events :+ new MidiEvent(
                new ShortMessage(
                  ShortMessage.CONTROL_CHANGE,
                  channel,
                  10,
                  ((value / 100d) * 127).toInt
                ),
                partState.currentVoiceInstrumentState.offset
              )) -> partState

            case Attribute.Quantization(_, value) =>
              events -> partState.modifyCurrentVoiceInstrumentState(
                _.copy(
                  quantization = value
                )
              )

            case Attribute.Tempo(_, beatsPerMinute) =>
              val microsecondsPerQuartnerNote =
                BigInt(60000000 / beatsPerMinute).toByteArray
              assert(microsecondsPerQuartnerNote.size <= 3)
              (events :+ new MidiEvent(
                new MetaMessage(
                  0x51,
                  microsecondsPerQuartnerNote,
                  microsecondsPerQuartnerNote.size
                ),
                partState.currentVoiceInstrumentState.offset
              )) -> partState.modifyCurrentVoiceInstrumentState(
                _.copy(
                  tempo = beatsPerMinute
                )
              )

            case Attribute.TrackVolume(_, value) =>
              (events :+ new MidiEvent(
                new ShortMessage(
                  ShortMessage.CONTROL_CHANGE,
                  channel,
                  7,
                  ((value / 100d) * 127).toInt
                ),
                partState.currentVoiceInstrumentState.offset
              )) -> partState

            case Attribute.Transposition(_, value) =>
              events -> partState.modifyCurrentVoiceInstrumentState(
                _.copy(
                  transposition = value
                )
              )

            case Attribute.Volume(_, value) =>
              events -> partState.modifyCurrentVoiceInstrumentState(
                _.copy(
                  volume = value
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

            // TODO: Check we carry octave/duration/volume changes in the same
            // way that Alda does, per
            // https://github.com/alda-lang/alda/blob/master/doc/chords.md
            case Chord(elements) =>
              def resetOffset(chordPartState: PartState): PartState =
                chordPartState.modifyCurrentVoiceInstrumentState(
                  instrumentState =>
                    instrumentState.copy(
                      offset = partState.currentVoiceInstrumentState.offset
                    )
                )

              val (chordEvents, updatedChordPartState, shortestNoteDuration) =
                elements.foldLeft(
                  (
                    Seq.empty[MidiEvent],
                    partState,
                    partState.currentVoiceInstrumentState.duration
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
              events ++ chordEvents -> updatedChordPartState
                .modifyCurrentVoiceInstrumentState(instrumentState =>
                  instrumentState.copy(
                    offset = instrumentState.offset + pulses(
                      shortestNoteDuration,
                      pulsesPerQuarterNote * 4
                    )
                  )
                )

            case octave: Octave =>
              events -> partState.modifyCurrentVoiceInstrumentState(
                _.copy(
                  octave = octave
                )
              )

            case OctaveIncrement =>
              events -> partState.modifyCurrentVoiceInstrumentState(
                instrumentState =>
                  instrumentState.copy(
                    octave = Octave(instrumentState.octave.value + 1)
                  )
              )

            case OctaveDecrement =>
              events -> partState.modifyCurrentVoiceInstrumentState(
                instrumentState =>
                  instrumentState.copy(
                    octave = Octave(instrumentState.octave.value - 1)
                  )
              )

            case note: Note =>
              events ++ midiEvents(
                partState.currentVoiceInstrumentState.offset,
                Seq(note),
                channel,
                pulsesPerQuarterNote,
                partState.currentVoiceInstrumentState
              ) ->
                partState.modifyCurrentVoiceInstrumentState(instrumentState =>
                  instrumentState.copy(
                    offset = instrumentState.offset +
                      pulses(
                        note.duration
                          .getOrElse(
                            instrumentState.duration
                          ),
                        pulsesPerQuarterNote * 4
                      ),
                    duration = note.duration
                      .getOrElse(
                        instrumentState.duration
                      )
                  )
                )

            case Rest(noteLength) =>
              events ->
                partState.modifyCurrentVoiceInstrumentState(instrumentState =>
                  instrumentState.copy(
                    offset = instrumentState.offset +
                      pulses(
                        noteLength
                          .getOrElse(
                            instrumentState.duration
                          ),
                        pulsesPerQuarterNote * 4
                      ),
                    duration = noteLength
                      .getOrElse(
                        instrumentState.duration
                      )
                  )
                )

            case Barline =>
              events -> partState

            case marker: Marker =>
              events -> partState.copy(
                markers = partState.markers.updated(
                  marker,
                  partState.currentVoiceInstrumentState.offset
                )
              )

            case MarkerReference(marker) =>
              // TODO: Better error messages
              val offset = partState.markers(marker)
              events -> partState.modifyCurrentVoiceInstrumentState(
                _.copy(
                  offset = offset
                )
              )
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
      instrumentState: InstrumentState
  ): Seq[MidiEvent] =
    (for {
      note <- notes
      toneNumber = (instrumentState.octave.value + 1) * 12 + note.pitch.chroma
      noteOn = new MidiEvent(
        new ShortMessage(
          ShortMessage.NOTE_ON,
          channel,
          toneNumber,
          ((instrumentState.volume / 100d) * 127).toInt
        ),
        offset
      )
      noteOff = new MidiEvent(
        new ShortMessage(
          ShortMessage.NOTE_OFF,
          channel,
          toneNumber,
          ((instrumentState.volume / 100d) * 127).toInt
        ),
        offset +
          ((instrumentState.quantization / 100d) *
            pulses(
              note.duration
                .getOrElse(instrumentState.duration),
              pulsesPerQuarterNote * 4
            )).toLong
      )
    } yield Seq(noteOn, noteOff)).flatten

  private[this] def pulses(
      noteDuration: Note.Duration,
      pulsesPerNote: Int
  ): Long =
    (noteDuration.value * pulsesPerNote).toLong

}
