package net.dhpiggott.apollo

import javax.sound.midi._

object SequenceGenerator {

  final case class ScoreState(
      markers: Map[Marker, Long],
      instruments: Seq[InstrumentState]
  ) {

    // TODO: Check for consistency with Alda spec and impl
    // (see
    // https://github.com/alda-lang/alda-core/blob/master/src/alda/lisp/score/part.clj
    // and
    // https://github.com/alda-lang/alda/blob/master/doc/instance-and-group-assignment.md)
    //
    // TODO: DRY!
    def updateCurrentInstrument(
        instrumentCall: InstrumentCall
    ): ScoreState =
      instrumentCall match {
        case InstrumentCall
              .InstrumentInstance(stockInstrumentNameOrNickname, None) =>
          instruments.find(_.nickname.contains(stockInstrumentNameOrNickname)) match {
            // stockInstrumentNameOrNickname refers to a previously named instrument
            case Some(instrumentState) =>
              // Refer to that instrument
              copy(
                instruments =
                  instruments
                    .filterNot(_ == instrumentState)
                    .map(_.copy(isActive = false)) :+ instrumentState
                    .copy(isActive = true)
              )

            case None =>
              instruments.filter(
                _.groupAlias.contains(stockInstrumentNameOrNickname)
              ) match {
                // stockInstrumentNameOrNickname refers to a previously named group
                case instrumentStates if instrumentStates.nonEmpty =>
                  // Refer to that group
                  copy(
                    instruments = instruments
                      .filterNot(
                        instrumentStates.contains
                      )
                      .map(_.copy(isActive = false)) ++ instrumentStates.map(
                      _.copy(isActive = true)
                    )
                  )

                case _ =>
                  MidiInstruments.midiInstruments.find(
                    _.names.contains(stockInstrumentNameOrNickname)
                  ) match {
                    // stockInstrumentNameOrNickname is a stock instrument
                    case Some(midiInstrument) =>
                      instruments.find(
                        _.midiInstrument.names
                          .contains(stockInstrumentNameOrNickname)
                      ) match {
                        // We don't have an instance in the score yet
                        case None =>
                          // Create a new instance
                          copy(
                            instruments = instruments.map(
                              _.copy(isActive = false)
                            ) :+ InstrumentState
                              .make(
                                midiInstrument = midiInstrument,
                                midiChannel = nextNonPercussionMidiChannel,
                                nickname = None,
                                groupAlias = None
                              )
                          )

                        // We already have a named instance in the score
                        case Some(instrumentState)
                            if instrumentState.nickname.isDefined =>
                          throw new IllegalArgumentException(
                            s"""Ambiguous instrument reference "$stockInstrumentNameOrNickname": can't use both unnamed and named instances of the same instrument in a score."""
                          )

                        // We already have an instance, and it doesn't have a name
                        case Some(instrumentState)
                            if instrumentState.nickname.isEmpty =>
                          copy(
                            instruments =
                              instruments
                                .filterNot(_ == instrumentState)
                                .map(_.copy(isActive = false)) :+ instrumentState
                                .copy(isActive = true)
                          )
                      }

                    case None =>
                      throw new IllegalArgumentException(
                        s"""Unrecognized instrument: "$stockInstrumentNameOrNickname"."""
                      )
                  }
              }
          }

        case InstrumentCall
              .InstrumentInstance(stockInstrumentName, Some(nickname)) =>
          MidiInstruments.midiInstruments.find(
            _.names.contains(stockInstrumentName)
          ) match {
            // stockInstrumentName is expected to be a stock instrument
            case None =>
              throw new IllegalArgumentException(
                s"""Unrecognized instrument: "$stockInstrumentName"."""
              )

            case Some(midiInstrument) =>
              instruments.find(_.nickname.contains(nickname)) match {
                case Some(_) =>
                  throw new IllegalArgumentException(
                    s"""The alias "$nickname" has already been assigned to another instrument."""
                  )

                case None =>
                  instruments.find(_.groupAlias.contains(nickname)) match {
                    case Some(_) =>
                      throw new IllegalArgumentException(
                        s"""The alias "$nickname" has already been assigned to another group."""
                      )

                    case None =>
                      instruments.find(
                        _.midiInstrument.names.contains(stockInstrumentName)
                      ) match {
                        case Some(_) =>
                          throw new IllegalArgumentException(
                            s"""Ambiguous instrument reference "$stockInstrumentName": can't use both unnamed and named instances of the same instrument in a score."""
                          )

                        case None =>
                          // Create a new instance
                          copy(
                            instruments = instruments.map(
                              _.copy(isActive = false)
                            ) :+ InstrumentState
                              .make(
                                midiInstrument = midiInstrument,
                                midiChannel = nextNonPercussionMidiChannel,
                                nickname = Some(nickname),
                                groupAlias = None
                              )
                          )
                      }
                  }
              }
          }

        case InstrumentCall
              .InstrumentGroup(stockInstrumentNamesOrNicknames, None) =>
          // Same named instance, e.g. foo/foo or same stock instrument e.g. piano/piano
          if (stockInstrumentNamesOrNicknames.size > stockInstrumentNamesOrNicknames.distinct.size) {
            throw new IllegalArgumentException(
              s"""Invalid instrument grouping: ${stockInstrumentNamesOrNicknames
                .mkString("/")}."""
            )
          } else {
            val namedInstances = stockInstrumentNamesOrNicknames
              .flatMap(stockInstrumentNamesOrNickname =>
                instruments
                  .find(_.nickname.contains(stockInstrumentNamesOrNickname))
              )
            if (namedInstances.size == stockInstrumentNamesOrNicknames.size) {
              copy(
                instruments = instruments
                  .filterNot(
                    namedInstances.contains
                  )
                  .map(_.copy(isActive = false)) ++ namedInstances.map(
                  _.copy(isActive = true)
                )
              )
            } else {
              val stockInstruments = stockInstrumentNamesOrNicknames
                .flatMap(stockInstrumentNamesOrNickname =>
                  MidiInstruments.midiInstruments
                    .find(_.names.contains(stockInstrumentNamesOrNickname))
                )
              if (stockInstruments.size == stockInstrumentNamesOrNicknames.size) {
                val (newActiveInstruments, _) =
                  stockInstruments.foldLeft(
                    (Set.empty[InstrumentState], nextNonPercussionMidiChannel)
                  ) {
                    case (
                        (
                          newActiveInstruments,
                          updatedNextNonPercussionMidiChannel
                        ),
                        stockInstrument
                        ) =>
                      newActiveInstruments + instruments
                        .find(instrument =>
                          instrument.midiInstrument == stockInstrument && instrument.nickname.isEmpty
                        )
                        .getOrElse(
                          InstrumentState
                            .make(
                              midiInstrument = stockInstrument,
                              midiChannel = updatedNextNonPercussionMidiChannel,
                              nickname = None,
                              groupAlias = None
                            )
                        ) -> (updatedNextNonPercussionMidiChannel + 1)
                  }
                copy(
                  instruments = instruments
                    .filterNot(
                      newActiveInstruments.contains
                    )
                    .map(_.copy(isActive = false)) ++ newActiveInstruments.map(
                    _.copy(isActive = true)
                  )
                )
              } else {
                throw new IllegalArgumentException(
                  s"""Invalid instrument grouping "${stockInstrumentNamesOrNicknames
                    .mkString("/")}": can't use both stock instruments and named instances in a group.""""
                )
              }
            }
          }

        case InstrumentCall.InstrumentGroup(
            stockInstrumentNamesOrNicknames,
            Some(groupAlias)
            ) =>
          // Same named instance, e.g. foo/foo or same stock instrument e.g. piano/piano
          if (stockInstrumentNamesOrNicknames.size > stockInstrumentNamesOrNicknames.distinct.size) {
            throw new IllegalArgumentException(
              s"""Invalid instrument grouping: ${stockInstrumentNamesOrNicknames
                .mkString("/")}."""
            )
          } else {
            instruments.find(_.nickname.contains(groupAlias)) match {
              case Some(_) =>
                throw new IllegalArgumentException(
                  s"""The alias "$groupAlias" has already been assigned to another instrument."""
                )

              case None =>
                instruments.find(_.groupAlias.contains(groupAlias)) match {
                  case Some(_) =>
                    throw new IllegalArgumentException(
                      s"""The alias "$groupAlias" has already been assigned to another group."""
                    )

                  case None =>
                    val namedInstances = stockInstrumentNamesOrNicknames
                      .flatMap(stockInstrumentNamesOrNickname =>
                        instruments.find(
                          _.nickname
                            .contains(stockInstrumentNamesOrNickname)
                        )
                      )
                      .map(_.copy(groupAlias = Some(groupAlias)))
                    if (namedInstances.size == stockInstrumentNamesOrNicknames.size) {
                      copy(
                        instruments = instruments
                          .filterNot(
                            namedInstances.contains
                          )
                          .map(_.copy(isActive = false)) ++ namedInstances.map(
                          _.copy(isActive = true)
                        )
                      )
                    } else {
                      val stockInstruments = stockInstrumentNamesOrNicknames
                        .flatMap(stockInstrumentNamesOrNickname =>
                          MidiInstruments.midiInstruments.find(
                            _.names.contains(stockInstrumentNamesOrNickname)
                          )
                        )
                      if (stockInstruments.size == stockInstrumentNamesOrNicknames.size) {
                        val (newActiveInstruments, _) =
                          stockInstruments.foldLeft(
                            (
                              Set.empty[InstrumentState],
                              nextNonPercussionMidiChannel
                            )
                          ) {
                            case (
                                (
                                  newActiveInstruments,
                                  updatedNextNonPercussionMidiChannel
                                ),
                                stockInstrument
                                ) =>
                              newActiveInstruments + InstrumentState
                                .make(
                                  midiInstrument = stockInstrument,
                                  midiChannel =
                                    updatedNextNonPercussionMidiChannel,
                                  nickname = None,
                                  groupAlias = Some(groupAlias)
                                ) -> (updatedNextNonPercussionMidiChannel + 1)
                          }
                        copy(
                          instruments = instruments
                            .filterNot(
                              newActiveInstruments.contains
                            )
                            .map(_.copy(isActive = false)) ++ newActiveInstruments
                            .map(_.copy(isActive = true))
                        )
                      } else {
                        throw new IllegalArgumentException(
                          s"""Invalid instrument grouping "${stockInstrumentNamesOrNicknames
                            .mkString("/")}": can't use both stock instruments and named instances in a group.""""
                        )
                      }
                    }
                }
            }
          }

        case InstrumentCall.InstrumentGroupMember(
            groupAlias,
            stockInstrumentNameOrNickname
            ) =>
          instruments.filter(_.groupAlias.contains(groupAlias)) match {
            case instrumentStates if instrumentStates.isEmpty =>
              throw new IllegalArgumentException(
                s"""Unrecognized instrument: "$stockInstrumentNameOrNickname"."""
              )

            case instrumentStates =>
              instrumentStates.find(instrumentState =>
                instrumentState.midiInstrument.names
                  .contains(stockInstrumentNameOrNickname) ||
                  instrumentState.nickname
                    .contains(stockInstrumentNameOrNickname)
              ) match {
                case None =>
                  throw new IllegalArgumentException(
                    s"""Unrecognized instrument: "$stockInstrumentNameOrNickname"."""
                  )

                case Some(instrumentState) =>
                  copy(
                    instruments =
                      instruments
                        .filterNot(_ == instrumentState)
                        .map(_.copy(isActive = false)) :+ instrumentState
                        .copy(isActive = true)
                  )
              }
          }
      }

    // TODO: Create new track when we run out of channels
    def nextNonPercussionMidiChannel: Int =
      instruments
        .map(_.midiChannel)
        .maxOption
        .map(_ + 1)
        // Because channel 10 is percussion (9 here because the JVM counts from 0)
        .filter(_ != 9)
        .getOrElse(0)

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
          instruments.filter(_.isActive).map(f) ++ instruments.filterNot(
            _.isActive
          )
      )

    def modifyCurrentInstrumentVoiceStates(
        f: VoiceState => VoiceState
    ): ScoreState =
      modifyCurrentInstrumentStates(_.modifyCurrentVoiceState(f))

  }

  final case class InstrumentState(
      midiInstrument: MidiInstruments.MidiInstrument,
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
    // TODO: Initialize sequence using these values
    def make(
        midiInstrument: MidiInstruments.MidiInstrument,
        midiChannel: Int,
        nickname: Option[String],
        groupAlias: Option[String]
    ): InstrumentState = InstrumentState(
      midiInstrument = midiInstrument,
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
    val track = sequence.createTrack()
    def generateSequence(
        scoreState: ScoreState,
        elements: Seq[ScoreElement]
    ): ScoreState = {
      def pulses(noteDuration: Note.Duration): Long =
        (noteDuration.value * pulsesPerQuarterNote * 4).toLong
      elements.foldLeft(scoreState)((scoreState, element) =>
        element match {
          case instrumentCall: InstrumentCall =>
            val updatedScoreState =
              scoreState.updateCurrentInstrument(instrumentCall)
            updatedScoreState.foreachInstrument(instrumentState =>
              instrumentState.midiInstrument match {
                case MidiInstruments.MidiPercussionInstrument =>
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
                        instrumentState.midiChannel,
                        midiProgramNumber,
                        0
                      ),
                      0
                    )
                  )
                  ()
              }
            )
            updatedScoreState

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
            scoreState.foreachInstrument { instrumentState =>
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
            scoreState.foreachInstrument { instrumentState =>
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
