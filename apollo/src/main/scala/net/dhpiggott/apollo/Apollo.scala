package net.dhpiggott.apollo

import javax.sound.midi._

import zio._
import zio.console._

object Apollo extends App {

  override def run(args: List[String]): URIO[ZEnv, ExitCode] =
    program.provideCustomLayer(synthesizer ++ sequencer).exitCode

  private[this] val program
      : RIO[Console with Has[Synthesizer] with Has[Sequencer], Unit] = for {
    sequence <- UIO(new Sequence(Sequence.PPQ, 30))
    track <- UIO(sequence.createTrack())
    part = Part(sequence.getResolution(), 0, 0, Seq.empty)
    notes = Seq(
      Note(Tone(Pitch.C, 4), Duration.Crotchet),
      Note(Tone(Pitch.D, 4), Duration.Crotchet),
      Note(Tone(Pitch.E, 4), Duration.Crotchet),
      Note(Tone(Pitch.F, 4), Duration.Crotchet),
      Rest(Duration.Crotchet),
      Chord(
        Seq(
          Note(Tone(Pitch.C, 4), Duration.Quaver),
          Note(Tone(Pitch.E, 4), Duration.Crotchet),
          Note(Tone(Pitch.G, 4), Duration.Crotchet)
        )
      ),
      Note(Tone(Pitch.A, 4), Duration.Crotchet),
      Note(Tone(Pitch.B, 4), Duration.Crotchet),
      Note(Tone(Pitch.C, 5), Duration.Crotchet)
    )
    _ = part.append(notes ++ notes.reverse).events.foreach(track.add)
    _ <- playSequence(
      sequence,
      instruments = Map(0 -> nonPercusssionInstruments("square-wave"))
    )
  } yield ()

  private[this] def playSequence(
      sequence: Sequence,
      instruments: Map[Int, Int]
  ): RIO[Console with Has[Synthesizer] with Has[Sequencer], Unit] =
    for {
      synthesizer <- RIO.service[Synthesizer]
      sequencer <- RIO.service[Sequencer]
      _ <- Task.foreach(instruments.toSeq) {
        case (channel, program) =>
          Task(synthesizer.getChannels()(channel).programChange(program))
      }
      _ <- Task(
        sequencer.getTransmitter().setReceiver(synthesizer.getReceiver())
      )
      _ <- Task(sequencer.setSequence(sequence))
      _ <- Task(sequencer.start())
      _ <- Task.effectAsync[Unit](callback =>
        sequencer.addMetaEventListener(metaEvent =>
          if (metaEvent.getType() == 47) callback(UIO.unit)
        )
      )
    } yield ()

  private[this] def synthesizer: TaskLayer[Has[Synthesizer]] =
    Managed
      .make(
        for {
          synthesizer <- Task(MidiSystem.getSynthesizer())
          _ <- Task(synthesizer.open())
        } yield synthesizer
      )(synthesizer => Task(synthesizer.close).orDie)
      .toLayer

  private[this] def sequencer: TaskLayer[Has[Sequencer]] =
    Managed
      .make(
        for {
          sequencer <- Task(MidiSystem.getSequencer())
          _ <- Task(sequencer.open())
        } yield sequencer
      )(sequencer => Task(sequencer.close).orDie)
      .toLayer

  final case class Part(
      pulsesPerQuarterNote: Int,
      channel: Int,
      offset: Long,
      events: Seq[MidiEvent]
  ) {

    def append(events: Seq[Event]): Part =
      events.foldLeft(this)(_ append _)

    def append(event: Event): Part =
      event match {
        case chord: Chord => appendChord(chord)
        case note: Note   => appendNote(note)
        case rest: Rest   => appendRest(rest)
      }

    private[this] def appendChord(
        chord: Chord
    ): Part = 
      copy(
        pulsesPerQuarterNote,
        channel,
        offset + chord.notes.sortBy(_.duration.denominator).last.duration.ticks(pulsesPerQuarterNote),
        events ++ midiEvents(chord.notes)
      )

    private[this] def appendNote(note: Note): Part =
      copy(
        pulsesPerQuarterNote,
        channel,
        offset + note.duration.ticks(pulsesPerQuarterNote),
        events ++ midiEvents(Seq(note))
      )

    private[this] def appendRest(rest: Rest): Part =
      copy(
        pulsesPerQuarterNote,
        channel,
        offset + rest.duration.ticks(pulsesPerQuarterNote),
        events
      )

    private[this] def midiEvents(notes: Seq[Note]): Seq[MidiEvent] =
      (for {
        note <- notes
        toneNumber = (note.tone.octave + 1) * 12 + note.tone.pitch.chroma
        noteOn = new MidiEvent(
          new ShortMessage(
            ShortMessage.NOTE_ON,
            channel,
            toneNumber,
            note.velocity
          ),
          offset
        )
        noteOff = new MidiEvent(
          new ShortMessage(
            ShortMessage.NOTE_OFF,
            channel,
            toneNumber,
            note.velocity
          ),
          offset + note.duration.ticks(pulsesPerQuarterNote)
        )
      } yield Seq(noteOn, noteOff)).flatten

  }

  sealed abstract class Event
  final case class Chord(notes: Seq[Note]) extends Event
  final case class Note(tone: Tone, duration: Duration, velocity: Int = 127)
      extends Event
  final case class Rest(duration: Duration) extends Event

  final case class Tone(pitch: Pitch, octave: Int)
  final case class Duration(denominator: BigDecimal) {
    def ticks(pulsesPerQuarterNote: Int): Int =
      ((pulsesPerQuarterNote * 4) / denominator).toIntExact
  }

  object Duration {
    val Large = Duration(0.125)
    val Long = Duration(0.25)
    val Breve = Duration(0.5)
    val Semibreve = Duration(1)
    val Minim = Duration(2)
    val Crotchet = Duration(4)
    val Quaver = Duration(8)
    val Semiquaver = Duration(16)
    val Demisemiquaver = Duration(32)
    val Hemidemisemiquaver = Duration(64)
  }

  final case class Pitch(chroma: Int) {
    def + = Pitch(chroma + 1)
    def - = Pitch(chroma - 1)
  }

  object Pitch {
    val C = Pitch(0)
    val D = Pitch(2)
    val E = Pitch(4)
    val F = Pitch(5)
    val G = Pitch(7)
    val A = Pitch(9)
    val B = Pitch(11)
  }

  val nonPercusssionInstruments: Map[String, Int] =
    Seq(
      // 1-8: piano
      Seq("midi-acoustic-grand-piano", "midi-piano", "piano"),
      Seq("midi-bright-acoustic-piano"),
      Seq("midi-electric-grand-piano"),
      Seq("midi-honky-tonk-piano"),
      Seq("midi-electric-piano-1"),
      Seq("midi-electric-piano-2"),
      Seq("midi-harpsichord", "harpsichord"),
      Seq("midi-clavi", "midi-clavinet", "clavinet"),
      // 9-16: chromatic percussion
      Seq("midi-celesta", "celesta", "celeste", "midi-celeste"),
      Seq("midi-glockenspiel", "glockenspiel"),
      Seq("midi-music-box", "music-box"),
      Seq("midi-vibraphone", "vibraphone", "vibes", "midi-vibes"),
      Seq("midi-marimba", "marimba"),
      Seq("midi-xylophone", "xylophone"),
      Seq("midi-tubular-bells", "tubular-bells"),
      Seq("midi-dulcimer", "dulcimer"),
      // 17-24: organ
      Seq("midi-drawbar-organ"),
      Seq("midi-percussive-organ"),
      Seq("midi-rock-organ"),
      Seq("midi-church-organ", "organ"),
      Seq("midi-reed-organ"),
      Seq("midi-accordion", "accordion"),
      Seq("midi-harmonica", "harmonica"),
      Seq("midi-tango-accordion"),
      // 25-32: guitar
      Seq(
        "midi-acoustic-guitar-nylon",
        "midi-acoustic-guitar",
        "acoustic-guitar",
        "guitar"
      ),
      Seq("midi-acoustic-guitar-steel"),
      Seq("midi-electric-guitar-jazz"),
      Seq("midi-electric-guitar-clean", "electric-guitar-clean"),
      Seq("midi-electric-guitar-palm-muted"),
      Seq("midi-electric-guitar-overdrive", "electric-guitar-overdrive"),
      Seq("midi-electric-guitar-distorted", "electric-guitar-distorted"),
      Seq("midi-electric-guitar-harmonics", "electric-guitar-harmonics"),
      // 33-40: bass
      Seq("midi-acoustic-bass", "acoustic-bass", "upright-bass"),
      Seq("midi-electric-bass-finger", "electric-bass-finger", "electric-bass"),
      Seq("midi-electric-bass-pick", "electric-bass-pick"),
      Seq("midi-fretless-bass", "fretless-bass"),
      Seq("midi-bass-slap"),
      Seq("midi-bass-pop"),
      Seq("midi-synth-bass-1"),
      Seq("midi-synth-bass-2"),
      // 41-48: strings
      Seq("midi-violin", "violin"),
      Seq("midi-viola", "viola"),
      Seq("midi-cello", "cello"),
      Seq(
        "midi-contrabass",
        "string-bass",
        "arco-bass",
        "double-bass",
        "contrabass",
        "midi-string-bass",
        "midi-arco-bass",
        "midi-double-bass"
      ),
      Seq("midi-tremolo-strings"),
      Seq("midi-pizzicato-strings"),
      Seq("midi-orchestral-harp", "harp", "orchestral-harp", "midi-harp"),
      // no idea why this is in strings, but ok! ¯\_(ツ)_/¯
      Seq("midi-timpani", "timpani"),
      // 49-56: ensemble
      Seq("midi-string-ensemble-1"),
      Seq("midi-string-ensemble-2"),
      Seq("midi-synth-strings-1"),
      Seq("midi-synth-strings-2"),
      Seq("midi-choir-aahs"),
      Seq("midi-voice-oohs"),
      Seq("midi-synth-voice"),
      Seq("midi-orchestra-hit"),
      // 57-64: brass
      Seq("midi-trumpet", "trumpet"),
      Seq("midi-trombone", "trombone"),
      Seq("midi-tuba", "tuba"),
      Seq("midi-muted-trumpet"),
      Seq("midi-french-horn", "french-horn"),
      Seq("midi-brass-section"),
      Seq("midi-synth-brass-1"),
      Seq("midi-synth-brass-2"),
      // 65-72: reed
      Seq(
        "midi-soprano-saxophone",
        "midi-soprano-sax",
        "soprano-saxophone",
        "soprano-sax"
      ),
      Seq("midi-alto-saxophone", "midi-alto-sax", "alto-saxophone", "alto-sax"),
      Seq(
        "midi-tenor-saxophone",
        "midi-tenor-sax",
        "tenor-saxophone",
        "tenor-sax"
      ),
      Seq(
        "midi-baritone-saxophone",
        "midi-baritone-sax",
        "midi-bari-sax",
        "baritone-saxophone",
        "baritone-sax",
        "bari-sax"
      ),
      Seq("midi-oboe", "oboe"),
      Seq("midi-english-horn", "english-horn"),
      Seq("midi-bassoon", "bassoon"),
      Seq("midi-clarinet", "clarinet"),
      // 73-80: pipe
      Seq("midi-piccolo", "piccolo"),
      Seq("midi-flute", "flute"),
      Seq("midi-recorder", "recorder"),
      Seq("midi-pan-flute", "pan-flute"),
      Seq("midi-bottle", "bottle"),
      Seq("midi-shakuhachi", "shakuhachi"),
      Seq("midi-whistle", "whistle"),
      Seq("midi-ocarina", "ocarina"),
      // 81-88: synth lead
      Seq(
        "midi-square-lead",
        "square",
        "square-wave",
        "square-lead",
        "midi-square",
        "midi-square-wave"
      ),
      Seq(
        "midi-saw-wave",
        "sawtooth",
        "saw-wave",
        "saw-lead",
        "midi-sawtooth",
        "midi-saw-lead"
      ),
      Seq("midi-calliope-lead", "calliope-lead", "calliope", "midi-calliope"),
      Seq(
        "midi-chiffer-lead",
        "chiffer-lead",
        "chiffer",
        "chiff",
        "midi-chiffer",
        "midi-chiff"
      ),
      Seq("midi-charang", "charang"),
      Seq("midi-solo-vox"),
      Seq("midi-fifths", "midi-sawtooth-fifths"),
      Seq("midi-bass-and-lead", "midi-bass+lead"),
      // 89-96: synth pad
      Seq("midi-synth-pad-new-age", "midi-pad-new-age", "midi-new-age-pad"),
      Seq("midi-synth-pad-warm", "midi-pad-warm", "midi-warm-pad"),
      Seq(
        "midi-synth-pad-polysynth",
        "midi-pad-polysynth",
        "midi-polysynth-pad"
      ),
      Seq("midi-synth-pad-choir", "midi-pad-choir", "midi-choir-pad"),
      Seq(
        "midi-synth-pad-bowed",
        "midi-pad-bowed",
        "midi-bowed-pad",
        "midi-pad-bowed-glass",
        "midi-bowed-glass-pad"
      ),
      Seq(
        "midi-synth-pad-metallic",
        "midi-pad-metallic",
        "midi-metallic-pad",
        "midi-pad-metal",
        "midi-metal-pad"
      ),
      Seq("midi-synth-pad-halo", "midi-pad-halo", "midi-halo-pad"),
      Seq("midi-synth-pad-sweep", "midi-pad-sweep", "midi-sweep-pad"),
      // 97-104: synth effects
      Seq("midi-fx-rain", "midi-fx-ice-rain", "midi-rain", "midi-ice-rain"),
      Seq("midi-fx-soundtrack", "midi-soundtrack"),
      Seq("midi-fx-crystal", "midi-crystal"),
      Seq("midi-fx-atmosphere", "midi-atmosphere"),
      Seq("midi-fx-brightness", "midi-brightness"),
      Seq("midi-fx-goblins", "midi-fx-goblin", "midi-goblins", "midi-goblin"),
      Seq(
        "midi-fx-echoes",
        "midi-fx-echo-drops",
        "midi-echoes",
        "midi-echo-drops"
      ),
      Seq("midi-fx-sci-fi", "midi-sci-fi"),
      // 105-112: "ethnic" (sigh)
      Seq("midi-sitar", "sitar"),
      Seq("midi-banjo", "banjo"),
      Seq("midi-shamisen", "shamisen"),
      Seq("midi-koto", "koto"),
      Seq("midi-kalimba", "kalimba"),
      Seq("midi-bagpipes", "bagpipes"),
      Seq("midi-fiddle"),
      Seq(
        "midi-shehnai",
        "shehnai",
        "shahnai",
        "shenai",
        "shanai",
        "midi-shahnai",
        "midi-shenai",
        "midi-shanai"
      ),
      // 113-120: percussive
      Seq("midi-tinkle-bell", "midi-tinker-bell"),
      Seq("midi-agogo"),
      Seq("midi-steel-drums", "midi-steel-drum", "steel-drums", "steel-drum"),
      Seq("midi-woodblock"),
      Seq("midi-taiko-drum"),
      Seq("midi-melodic-tom"),
      Seq("midi-synth-drum"),
      Seq("midi-reverse-cymbal"),
      // 121-128: sound effects
      Seq("midi-guitar-fret-noise"),
      Seq("midi-breath-noise"),
      Seq("midi-seashore"),
      Seq("midi-bird-tweet"),
      Seq("midi-telephone-ring"),
      Seq("midi-helicopter"),
      Seq("midi-applause"),
      Seq("midi-gunshot", "midi-gun-shot")
    ).zipWithIndex.flatMap {
      case (canonicalName +: aliases, patch) =>
        Map(canonicalName -> patch) ++ aliases.map(name => name -> patch)
    }.toMap

  val percusssionInstruments: Set[String] =
    Set("midi-percussion", "percussion")

}
