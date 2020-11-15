package net.dhpiggott.apollo

import javax.sound.midi._

object MidiSequenceGenerator {

  final case class ScoreState(
      markers: Map[Marker, Long],
      instruments: Seq[InstrumentState]
  ) {

    // TODO: Check for consistency with Alda spec and impl
    // (see
    // https://github.com/alda-lang/alda-core/blob/master/src/alda/lisp/score/part.clj
    // and
    // https://github.com/alda-lang/alda/blob/master/doc/instance-and-group-assignment.md)
    def updateActiveInstruments(
        sequence: Sequence,
        instrumentCall: InstrumentCall
    ): ScoreState =
      instrumentCall match {
        case InstrumentCall.InstrumentInstance(
            stockInstrumentNameOrNickname,
            maybeNickname
            ) =>
          val previouslyNamedInstrumentOrGroup = maybeNickname match {
            case Some(nickname) =>
              if (instruments.exists(_.nickname.contains(nickname))) {
                throw new IllegalArgumentException(
                  s"""The alias "$nickname" has already been assigned to another instrument."""
                )
              } else if (instruments.exists(_.groupAlias.contains(nickname))) {
                throw new IllegalArgumentException(
                  s"""The alias "$nickname" has already been assigned to another group."""
                )
              } else {
                Seq.empty
              }

            case None =>
              instruments.filter(instrumentState =>
                instrumentState.nickname
                  .contains(stockInstrumentNameOrNickname) || instrumentState.groupAlias
                  .contains(stockInstrumentNameOrNickname)
              )
          }

          if (previouslyNamedInstrumentOrGroup.nonEmpty) {
            withActiveInstruments(previouslyNamedInstrumentOrGroup)
          } else {
            instruments.find(
              _.midiInstrument.names.contains(stockInstrumentNameOrNickname)
            ) match {
              case Some(instrumentState) =>
                if (maybeNickname.isDefined || instrumentState.nickname.isDefined) {
                  throw new IllegalArgumentException(
                    s"""Ambiguous instrument reference "$stockInstrumentNameOrNickname": can't use both unnamed and named instances of the same instrument in a score."""
                  )
                } else {
                  // We already have an instance, and neither the reference nor it have names
                  withActiveInstruments(Seq(instrumentState))
                }

              case None =>
                // stockInstrumentNameOrNickname is expected to be a stock instrument
                MidiInstruments.midiInstruments
                  .find(
                    _.names.contains(stockInstrumentNameOrNickname)
                  )
                  .map { midiInstrument =>
                    val (midiTrack, midiChannel) =
                      nextMidiTrackAndChannel(midiInstrument)
                    // Create a new instance
                    withActiveInstruments(
                      Seq(
                        InstrumentState.make(
                          sequence,
                          midiInstrument,
                          midiTrack,
                          midiChannel,
                          nickname = maybeNickname,
                          groupAlias = None
                        )
                      )
                    )
                  }
                  .getOrElse(
                    throw new IllegalArgumentException(
                      s"""Unrecognized instrument: "$stockInstrumentNameOrNickname"."""
                    )
                  )
            }
          }

        case InstrumentCall.InstrumentGroup(
            stockInstrumentNamesOrNicknames,
            maybeGroupAlias
            ) =>
          // Same named instance, e.g. foo/foo or same stock instrument e.g. piano/piano
          if (stockInstrumentNamesOrNicknames.size > stockInstrumentNamesOrNicknames.distinct.size) {
            throw new IllegalArgumentException(
              s"""Invalid instrument grouping: ${stockInstrumentNamesOrNicknames
                .mkString("/")}."""
            )
          } else {
            maybeGroupAlias.foreach(groupAlias =>
              if (instruments.exists(_.nickname.contains(groupAlias))) {
                throw new IllegalArgumentException(
                  s"""The alias "$groupAlias" has already been assigned to another instrument."""
                )
              } else if (instruments
                           .exists(_.groupAlias.contains(groupAlias))) {
                throw new IllegalArgumentException(
                  s"""The alias "$groupAlias" has already been assigned to another group."""
                )
              }
            )
            val namedInstances = for {
              stockInstrumentNamesOrNickname <- stockInstrumentNamesOrNicknames
              namedInstance <- instruments.find(
                _.nickname.contains(stockInstrumentNamesOrNickname)
              )
            } yield namedInstance.copy(groupAlias = maybeGroupAlias)
            if (namedInstances.size == stockInstrumentNamesOrNicknames.size) {
              withActiveInstruments(namedInstances)
            } else {
              val stockInstruments = for {
                stockInstrumentNamesOrNickname <- stockInstrumentNamesOrNicknames
                stockInstrument <- MidiInstruments.midiInstruments.find(
                  _.names.contains(stockInstrumentNamesOrNickname)
                )
              } yield stockInstrument
              if (stockInstruments.size == stockInstrumentNamesOrNicknames.size) {
                stockInstruments.foldLeft(this) {
                  (updatedScoreState, midiInstrument) =>
                    val (midiTrack, midiChannel) =
                      nextMidiTrackAndChannel(midiInstrument)
                    updatedScoreState.withActiveInstruments(
                      updatedScoreState.instruments.filter(_.isActive) :+
                        InstrumentState
                          .make(
                            sequence,
                            midiInstrument,
                            midiTrack,
                            midiChannel,
                            nickname = None,
                            groupAlias = maybeGroupAlias
                          )
                    )
                }
              } else {
                throw new IllegalArgumentException(
                  s"""Invalid instrument grouping "${stockInstrumentNamesOrNicknames
                    .mkString("/")}": can't use both stock instruments and named instances in a group.""""
                )
              }
            }
          }

        case InstrumentCall.InstrumentGroupMember(
            groupAlias,
            stockInstrumentNameOrNickname
            ) =>
          instruments
            .find(instrumentState =>
              instrumentState.groupAlias.contains(groupAlias) &&
                (instrumentState.midiInstrument.names
                  .contains(stockInstrumentNameOrNickname) ||
                  instrumentState.nickname
                    .contains(stockInstrumentNameOrNickname))
            )
            .map(instrumentState => withActiveInstruments(Seq(instrumentState)))
            .getOrElse(
              throw new IllegalArgumentException(
                s"""Unrecognized instrument: "$stockInstrumentNameOrNickname"."""
              )
            )
      }

    def withActiveInstruments(
        activeInstruments: Seq[InstrumentState]
    ): ScoreState = {
      val normalisedInstruments = instruments.map(_.copy(isActive = false))
      val normalisedActiveInstruments =
        activeInstruments.map(_.copy(isActive = false))
      copy(
        instruments = normalisedInstruments
          .filterNot(normalisedActiveInstruments.contains) ++ activeInstruments
          .map(_.copy(isActive = true))
      )
    }

    def nextMidiTrackAndChannel(
        midiInstrument: MidiInstruments.MidiInstrument
    ): (Int, Int) = midiInstrument match {
      case MidiInstruments.MidiPercussionInstrument =>
        // Channel 10 is percussion (9 here because the JVM counts from 0)
        (0, 9)

      case _: MidiInstruments.MidiNonPercussionInstrument =>
        def nextNonPercussionMidiTrackAndChannel(
            candidateMidiTrack: Int = 0,
            candidateMidiChannel: Int = 0
        ): (Int, Int) = {
          // Because channel 10 is percussion (9 here because the JVM counts from 0)
          val isAvailable = candidateMidiChannel != 9 && instruments
            .filter(_.midiTrack == candidateMidiTrack)
            .filter(_.midiChannel == candidateMidiChannel)
            .isEmpty
          if (isAvailable) {
            (candidateMidiTrack, candidateMidiChannel)
            // A track supports 16 channels (15 here because the JVM counts from 0)
          } else if (candidateMidiChannel < 15) {
            nextNonPercussionMidiTrackAndChannel(
              candidateMidiTrack,
              candidateMidiChannel + 1
            )
          } else {
            nextNonPercussionMidiTrackAndChannel(candidateMidiTrack + 1, 0)
          }
        }
        nextNonPercussionMidiTrackAndChannel()
    }

    def currentInstrumentStates: Seq[InstrumentState] =
      instruments.filter(_.isActive)

    def currentInstrumentVoiceStates: Seq[VoiceState] =
      currentInstrumentStates.map(_.currentVoiceState)

    def foreachInstrument(f: InstrumentState => Unit): Unit =
      currentInstrumentStates.foreach(f)

    def modifyCurrentInstrumentStates(
        f: InstrumentState => InstrumentState
    ): ScoreState =
      copy(
        instruments =
          instruments.filter(_.isActive).map(f) ++
            instruments.filterNot(_.isActive)
      )

    def modifyCurrentInstrumentVoiceStates(
        f: VoiceState => VoiceState
    ): ScoreState =
      modifyCurrentInstrumentStates(_.modifyCurrentVoiceState(f))

  }

  final case class InstrumentState(
      midiInstrument: MidiInstruments.MidiInstrument,
      midiTrack: Int,
      midiChannel: Int,
      nickname: Option[String],
      groupAlias: Option[String],
      isActive: Boolean,
      quantization: Int,
      tempo: Int,
      transposition: Int,
      volume: Int,
      currentVoice: Voice,
      voiceStates: Map[Voice, VoiceState]
  ) {

    def currentVoiceState: VoiceState =
      voiceStates(currentVoice)

    def modifyCurrentVoiceState(
        f: VoiceState => VoiceState
    ): InstrumentState =
      copy(
        voiceStates = voiceStates.updated(
          currentVoice,
          f(currentVoiceState)
        )
      )

  }

  object InstrumentState {
    def make(
        sequence: Sequence,
        midiInstrument: MidiInstruments.MidiInstrument,
        midiTrack: Int,
        midiChannel: Int,
        nickname: Option[String],
        groupAlias: Option[String]
    ): InstrumentState = {
      val track = sequence
        .getTracks()
        .lift(midiTrack)
        .getOrElse(
          sequence.createTrack()
        )
      midiInstrument match {
        case MidiInstruments.MidiPercussionInstrument =>
          track.add(
            new MidiEvent(
              new ShortMessage(
                ShortMessage.PROGRAM_CHANGE,
                midiChannel,
                9,
                0
              ),
              0
            )
          )
          ()

        case MidiInstruments.MidiNonPercussionInstrument(
            _,
            _,
            midiProgramNumber
            ) =>
          track.add(
            new MidiEvent(
              new ShortMessage(
                ShortMessage.PROGRAM_CHANGE,
                midiChannel,
                midiProgramNumber,
                0
              ),
              0
            )
          )
          ()
      }
      InstrumentState(
        midiInstrument = midiInstrument,
        midiTrack = midiTrack,
        midiChannel = midiChannel,
        nickname = nickname,
        groupAlias = groupAlias,
        isActive = true,
        quantization = 90,
        tempo = 120,
        transposition = 0,
        volume = 100,
        currentVoice = Voice(0),
        voiceStates = Map(
          Voice(0) -> VoiceState.default
        )
      )
    }
  }

  final case class VoiceState(
      offset: Long,
      octave: Octave,
      duration: Note.Duration
  )

  object VoiceState {
    val default: VoiceState = VoiceState(
      offset = 0,
      octave = Octave(4),
      duration = Note.Duration(0.25)
    )
  }

  def generateSequence(score: Score): Sequence = {
    val pulsesPerQuarterNote = 128
    val sequence = new Sequence(Sequence.PPQ, pulsesPerQuarterNote)
    def generateSequence(
        scoreState: ScoreState,
        elements: Seq[ScoreElement]
    ): ScoreState = {
      def pulses(noteDuration: Note.Duration): Long =
        (noteDuration.value * pulsesPerQuarterNote * 4).toLong
      elements.foldLeft(scoreState)((scoreState, element) =>
        element match {
          case instrumentCall: InstrumentCall =>
            scoreState.updateActiveInstruments(sequence, instrumentCall)

          case Attribute.Duration(_, value) =>
            scoreState.modifyCurrentInstrumentStates(instrumentState =>
              instrumentState.modifyCurrentVoiceState(
                _.copy(
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
            )

          case Attribute.Octave(_, value) =>
            scoreState.modifyCurrentInstrumentVoiceStates(voiceState =>
              voiceState.copy(
                octave = value match {
                  case Attribute.Octave.AbsoluteValue(value) =>
                    Octave(value)

                  case Attribute.Octave.Increment =>
                    Octave(
                      voiceState.octave.value + 1
                    )

                  case Attribute.Octave.Decrement =>
                    Octave(
                      voiceState.octave.value - 1
                    )
                }
              )
            )

          case Attribute.Panning(_, value) =>
            scoreState.foreachInstrument {
              instrumentState =>
                val track = sequence.getTracks()(instrumentState.midiTrack)
                track.add(
                  new MidiEvent(
                    new ShortMessage(
                      ShortMessage.CONTROL_CHANGE,
                      instrumentState.midiChannel,
                      10,
                      ((value / 100d) * 127).toInt
                    ),
                    instrumentState.currentVoiceState.offset
                  )
                )
                ()
            }
            scoreState

          case Attribute.Quantization(_, value) =>
            scoreState.modifyCurrentInstrumentStates(
              _.copy(
                quantization = value
              )
            )

          case Attribute.Tempo(_, beatsPerMinute) =>
            val microsecondsPerQuartnerNote =
              BigInt(60000000 / beatsPerMinute).toByteArray
            assert(microsecondsPerQuartnerNote.size <= 3)
            scoreState.foreachInstrument { instrumentState =>
              val track = sequence.getTracks()(instrumentState.midiTrack)
              track.add(
                new MidiEvent(
                  new MetaMessage(
                    0x51,
                    microsecondsPerQuartnerNote,
                    microsecondsPerQuartnerNote.size
                  ),
                  instrumentState.currentVoiceState.offset
                )
              )
              ()
            }
            scoreState.modifyCurrentInstrumentStates(
              _.copy(
                tempo = beatsPerMinute
              )
            )

          case Attribute.TrackVolume(_, value) =>
            scoreState.foreachInstrument {
              instrumentState =>
                val track = sequence.getTracks()(instrumentState.midiTrack)
                track.add(
                  new MidiEvent(
                    new ShortMessage(
                      ShortMessage.CONTROL_CHANGE,
                      instrumentState.midiChannel,
                      7,
                      ((value / 100d) * 127).toInt
                    ),
                    instrumentState.currentVoiceState.offset
                  )
                )
                ()
            }
            scoreState

          case Attribute.Transposition(_, value) =>
            scoreState.modifyCurrentInstrumentStates(
              _.copy(
                transposition = value
              )
            )

          case Attribute.Volume(_, value) =>
            scoreState.modifyCurrentInstrumentStates(
              _.copy(
                volume = value
              )
            )

          case Repeat(sequence, repetitions) =>
            Seq
              .fill(repetitions)(Seq(sequence))
              .foldLeft(scoreState)(generateSequence)

          case net.dhpiggott.apollo.Sequence(elements) =>
            generateSequence(scoreState, elements)

          case voice @ Voice(value) if value == 0 =>
            scoreState.modifyCurrentInstrumentStates(instrumentState =>
              instrumentState.copy(
                currentVoice = voice,
                voiceStates = Map(
                  voice -> instrumentState.voiceStates
                    .filter {
                      case (Voice(value), _) => value != 0
                    }
                    .values
                    .toSeq
                    .sortBy(_.offset)
                    .lastOption
                    .getOrElse(
                      instrumentState.currentVoiceState
                    )
                )
              )
            )

          case voice: Voice =>
            scoreState.modifyCurrentInstrumentStates(instrumentState =>
              instrumentState.copy(
                currentVoice = voice,
                voiceStates = instrumentState.voiceStates
                  .updated(
                    voice,
                    instrumentState.voiceStates
                      .get(voice)
                      .getOrElse(
                        instrumentState
                          .voiceStates(Voice(0))
                      )
                  )
              )
            )

          // TODO: Check we carry octave/duration/volume changes in the same
          // way that Alda does, per
          // https://github.com/alda-lang/alda/blob/master/doc/chords.md
          case Chord(elements) =>
            scoreState.currentInstrumentStates.foldLeft(scoreState) {
              case (scoreState, instrumentState) =>
                val voiceState = instrumentState.currentVoiceState
                val (updatedScoreState, shortestNoteDuration) =
                  elements.foldLeft((scoreState, voiceState.duration)) {
                    case (
                        (scoreState, shortestNoteDuration),
                        chordElement
                        ) =>
                      val chordElementDuration = chordElement match {
                        case Note(_, Some(duration))  => Some(duration)
                        case Rest(Some(noteDuration)) => Some(noteDuration)
                        case _                        => None
                      }
                      (
                        generateSequence(scoreState, Seq(chordElement))
                          .modifyCurrentInstrumentVoiceStates(
                            _.copy(
                              offset = voiceState.offset
                            )
                          ),
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
                      )
                  }
                updatedScoreState.modifyCurrentInstrumentVoiceStates(
                  voiceState =>
                    voiceState.copy(
                      offset =
                        voiceState.offset + pulses(shortestNoteDuration)
                    )
                )
            }

          case octave: Octave =>
            scoreState.modifyCurrentInstrumentVoiceStates(
              _.copy(
                octave = octave
              )
            )

          case OctaveIncrement =>
            scoreState.modifyCurrentInstrumentVoiceStates(voiceState =>
              voiceState.copy(
                octave = Octave(
                  voiceState.octave.value + 1
                )
              )
            )

          case OctaveDecrement =>
            scoreState.modifyCurrentInstrumentVoiceStates(voiceState =>
              voiceState.copy(
                octave = Octave(
                  voiceState.octave.value - 1
                )
              )
            )

          case note: Note =>
            scoreState.foreachInstrument {
              instrumentState =>
                val toneNumber =
                  (instrumentState.currentVoiceState.octave.value + 1) * 12 + note.pitch.chroma
                val track = sequence.getTracks()(instrumentState.midiTrack)
                track.add(
                  new MidiEvent(
                    new ShortMessage(
                      ShortMessage.NOTE_ON,
                      instrumentState.midiChannel,
                      toneNumber,
                      ((instrumentState.volume / 100d) * 127).toInt
                    ),
                    instrumentState.currentVoiceState.offset
                  )
                )
                track.add(
                  new MidiEvent(
                    new ShortMessage(
                      ShortMessage.NOTE_OFF,
                      instrumentState.midiChannel,
                      toneNumber,
                      ((instrumentState.volume / 100d) * 127).toInt
                    ),
                    instrumentState.currentVoiceState.offset +
                      ((instrumentState.quantization / 100d) *
                        pulses(
                          note.duration
                            .getOrElse(
                              instrumentState.currentVoiceState.duration
                            )
                        )).toLong
                  )
                )
                ()
            }
            scoreState.modifyCurrentInstrumentVoiceStates(voiceState =>
              voiceState.copy(
                offset = voiceState.offset +
                  pulses(
                    note.duration
                      .getOrElse(
                        voiceState.duration
                      )
                  ),
                duration = note.duration
                  .getOrElse(
                    voiceState.duration
                  )
              )
            )

          case Rest(noteLength) =>
            scoreState.modifyCurrentInstrumentVoiceStates(voiceState =>
              voiceState.copy(
                offset = voiceState.offset +
                  pulses(
                    noteLength
                      .getOrElse(
                        voiceState.duration
                      )
                  ),
                duration = noteLength
                  .getOrElse(
                    voiceState.duration
                  )
              )
            )

          case Barline =>
            scoreState

          case marker: Marker =>
            /*
             * In Alda markers can only be set if all current instruments
             * have the same offset.
             *
             * If that is not the case - as in this example -
             *
             * piano/guitar "pg":
             *
             * pg.guitar:
             *   c
             *
             * pg:
             *   %marker
             *
             * - then we must raise this ambiguity error:
             *
             * 'Can't place marker "marker" - offset unclear.'
             */
            val offsets =
              scoreState.currentInstrumentVoiceStates.map(_.offset)
            val offset = offsets.reduce((o1, o2) =>
              if (o1 != o2)
                throw new IllegalArgumentException(
                  s"""Can't place marker "${marker.name}" - offset unclear."""
                )
              else o1
            )
            scoreState.copy(
              markers = scoreState.markers.updated(
                marker,
                offset
              )
            )

          case MarkerReference(marker) =>
            val offset = scoreState.markers
              .get(marker)
              .getOrElse(
                throw new IllegalArgumentException(
                  s"""Can't set current marker to "${marker.name}"; marker does not exist."""
                )
              )
            scoreState.modifyCurrentInstrumentVoiceStates(
              _.copy(
                offset = offset
              )
            )
        }
      )
    }
    generateSequence(
      scoreState = ScoreState(
        markers = Map.empty,
        instruments = Seq.empty
      ),
      score.elements
    )
    sequence
  }

}
