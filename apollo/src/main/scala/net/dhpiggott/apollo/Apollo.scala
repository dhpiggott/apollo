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
    notes = Seq(
      Note(Tone.C4, Duration.Crotchet),
      Note(Tone.D4, Duration.Crotchet),
      Note(Tone.E4, Duration.Crotchet),
      Note(Tone.F4, Duration.Crotchet),
      Note(Tone.G4, Duration.Crotchet),
      Note(Tone.A4, Duration.Crotchet),
      Note(Tone.B4, Duration.Crotchet),
      Note(Tone.C5, Duration.Crotchet)
    )
    _ = (notes ++ notes.reverse).foreach(note =>
      addToTrack(
        sequence,
        track,
        note
      )
    )
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

  final case class Note(tone: Tone, duration: Duration)
  final case class Rest(duration: Duration)

  def addToTrack(
      sequence: Sequence,
      track: Track,
      note: Note,
      channel: Int = 0,
      velocity: Int = 127
  ): Unit = {
    val toneNumber =
      (note.tone.octave + 1) * 12 + note.tone.pitch.chroma
    val pulsesPerQuarterNote = sequence.getResolution()
    val ticks =
      ((pulsesPerQuarterNote * 4) / note.duration.denominator).toIntExact
    val _ = track.add(
      new MidiEvent(
        new ShortMessage(
          ShortMessage.NOTE_ON,
          channel,
          toneNumber,
          velocity
        ),
        track.ticks()
      )
    )
    val _ = track.add(
      new MidiEvent(
        new ShortMessage(
          ShortMessage.NOTE_OFF,
          channel,
          toneNumber,
          velocity
        ),
        track.ticks() + ticks
      )
    )
  }

  final case class Tone(pitch: Pitch, octave: Int)

  object Tone {
    val A0 = Tone(Pitch.A, 0)
    val `A+0` = Tone(Pitch.`A+`, 0)
    val `B-0` = `A+0`
    val B0 = Tone(Pitch.B, 0)

    val C1 = Tone(Pitch.C, 1)
    val `C+1` = Tone(Pitch.`C+`, 1)
    val `D-1` = `C+1`
    val D1 = Tone(Pitch.D, 1)
    val `D+1` = Tone(Pitch.`D+`, 1)
    val `E-1` = `D+1`
    val E1 = Tone(Pitch.E, 1)
    val F1 = Tone(Pitch.F, 1)
    val `F+1` = Tone(Pitch.`F+`, 1)
    val `G-1` = `F+1`
    val G1 = Tone(Pitch.G, 1)
    val `G+1` = Tone(Pitch.`G+`, 1)
    val `A-1` = `G+1`
    val A1 = Tone(Pitch.A, 1)
    val `A+1` = Tone(Pitch.`A+`, 1)
    val `B-1` = `A+1`
    val B1 = Tone(Pitch.B, 1)

    val C2 = Tone(Pitch.C, 2)
    val `C+2` = Tone(Pitch.`C+`, 2)
    val `D-2` = `C+2`
    val D2 = Tone(Pitch.D, 2)
    val `D+2` = Tone(Pitch.`D+`, 2)
    val `E-2` = `D+2`
    val E2 = Tone(Pitch.E, 2)
    val F2 = Tone(Pitch.F, 2)
    val `F+2` = Tone(Pitch.`F+`, 2)
    val `G-2` = `F+2`
    val G2 = Tone(Pitch.G, 2)
    val `G+2` = Tone(Pitch.`G+`, 2)
    val `A-2` = `G+2`
    val A2 = Tone(Pitch.A, 2)
    val `A+2` = Tone(Pitch.`A+`, 2)
    val `B-2` = `A+2`
    val B2 = Tone(Pitch.B, 2)

    val C3 = Tone(Pitch.C, 3)
    val `C+3` = Tone(Pitch.`C+`, 3)
    val `D-3` = `C+3`
    val D3 = Tone(Pitch.D, 3)
    val `D+3` = Tone(Pitch.`D+`, 3)
    val `E-3` = `D+3`
    val E3 = Tone(Pitch.E, 3)
    val F3 = Tone(Pitch.F, 3)
    val `F+3` = Tone(Pitch.`F+`, 3)
    val `G-3` = `F+3`
    val G3 = Tone(Pitch.G, 3)
    val `G+3` = Tone(Pitch.`G+`, 3)
    val `A-3` = `G+3`
    val A3 = Tone(Pitch.A, 3)
    val `A+3` = Tone(Pitch.`A+`, 3)
    val `B-3` = `A+3`
    val B3 = Tone(Pitch.B, 3)

    val C4 = Tone(Pitch.C, 4)
    val `C+4` = Tone(Pitch.`C+`, 4)
    val `D-4` = `C+4`
    val D4 = Tone(Pitch.D, 4)
    val `D+4` = Tone(Pitch.`D+`, 4)
    val `E-4` = `D+4`
    val E4 = Tone(Pitch.E, 4)
    val F4 = Tone(Pitch.F, 4)
    val `F+4` = Tone(Pitch.`F+`, 4)
    val `G-4` = `F+4`
    val G4 = Tone(Pitch.G, 4)
    val `G+4` = Tone(Pitch.`G+`, 4)
    val `A-4` = `G+4`
    val A4 = Tone(Pitch.A, 4)
    val `A+4` = Tone(Pitch.`A+`, 4)
    val `B-4` = `A+4`
    val B4 = Tone(Pitch.B, 4)

    val C5 = Tone(Pitch.C, 5)
    val `C+5` = Tone(Pitch.`C+`, 5)
    val `D-5` = `C+5`
    val D5 = Tone(Pitch.D, 5)
    val `D+5` = Tone(Pitch.`D+`, 5)
    val `E-5` = `D+5`
    val E5 = Tone(Pitch.E, 5)
    val F5 = Tone(Pitch.F, 5)
    val `F+5` = Tone(Pitch.`F+`, 5)
    val `G-5` = `F+5`
    val G5 = Tone(Pitch.G, 5)
    val `G+5` = Tone(Pitch.`G+`, 5)
    val `A-5` = `G+5`
    val A5 = Tone(Pitch.A, 5)
    val `A+5` = Tone(Pitch.`A+`, 5)
    val `B-5` = `A+5`
    val B5 = Tone(Pitch.B, 5)

    val C6 = Tone(Pitch.C, 6)
    val `C+6` = Tone(Pitch.`C+`, 6)
    val `D-6` = `C+6`
    val D6 = Tone(Pitch.D, 6)
    val `D+6` = Tone(Pitch.`D+`, 6)
    val `E-6` = `D+6`
    val E6 = Tone(Pitch.E, 6)
    val F6 = Tone(Pitch.F, 6)
    val `F+6` = Tone(Pitch.`F+`, 6)
    val `G-6` = `F+6`
    val G6 = Tone(Pitch.G, 6)
    val `G+6` = Tone(Pitch.`G+`, 6)
    val `A-6` = `G+6`
    val A6 = Tone(Pitch.A, 6)
    val `A+6` = Tone(Pitch.`A+`, 6)
    val `B-6` = `A+6`
    val B6 = Tone(Pitch.B, 6)

    val C7 = Tone(Pitch.C, 7)
    val `C+7` = Tone(Pitch.`C+`, 7)
    val `D-7` = `C+7`
    val D7 = Tone(Pitch.D, 7)
    val `D+7` = Tone(Pitch.`D+`, 7)
    val `E-7` = `D+7`
    val E7 = Tone(Pitch.E, 7)
    val F7 = Tone(Pitch.F, 7)
    val `F+7` = Tone(Pitch.`F+`, 7)
    val `G-7` = `F+7`
    val G7 = Tone(Pitch.G, 7)
    val `G+7` = Tone(Pitch.`G+`, 7)
    val `A-7` = `G+7`
    val A7 = Tone(Pitch.A, 7)
    val `A+7` = Tone(Pitch.`A+`, 7)
    val `B-7` = `A+7`
    val B7 = Tone(Pitch.B, 7)

    val C8 = Tone(Pitch.C, 8)
  }

  final case class Duration(denominator: BigDecimal)

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

  final case class Pitch(chroma: Int)

  object Pitch {
    val C = Pitch(0)
    val `C+` = Pitch(1)
    val `D-` = `C+`
    val D = Pitch(2)
    val `D+` = Pitch(3)
    val `E-` = `D+`
    val E = Pitch(4)
    val F = Pitch(5)
    val `F+` = Pitch(6)
    val `G-` = `F+`
    val G = Pitch(7)
    val `G+` = Pitch(8)
    val `A-` = `G+`
    val A = Pitch(9)
    val `A+` = Pitch(10)
    val `B-` = `A+`
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
