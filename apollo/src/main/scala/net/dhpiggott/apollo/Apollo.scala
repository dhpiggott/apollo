package net.dhpiggott.apollo

import javax.sound.midi._

import zio._
import zio.console._

// TODO: Review
// https://github.com/alda-lang/alda-core/tree/master/src/alda/lisp,
// https://github.com/alda-lang/alda-core/blob/master/src/alda/lisp/score.clj,
// https://github.com/alda-lang/alda-core/blob/master/src/alda/lisp/score/part.clj
// and
// https://github.com/alda-lang/alda-server-clj/blob/master/src/alda/worker.clj,
// https://github.com/alda-lang/alda-server-clj/blob/master/src/alda/server.clj
// and
// https://github.com/alda-lang/alda-sound-engine-clj/blob/master/src/alda/sound.clj,
// https://github.com/alda-lang/alda-sound-engine-clj/blob/master/src/alda/sound/midi.clj
object Apollo extends App {

  override def run(args: List[String]): URIO[ZEnv, ExitCode] =
    program.provideCustomLayer(synthesizer ++ sequencer).exitCode

  private[this] val program
      : RIO[Console with Has[Synthesizer] with Has[Sequencer], Unit] = for {
    part <- ScoreParser.parseScorePart(
      """oboe:
           (volume 50) V0: o4 c4. d8 r2 | (volume 75) e8 f r2. | (volume 100) c8/e4/g4 r8 a4 b > c | c2.~4 |
           V1: (quantization 50) (tempo 240) < c8 d e f (transposition 2) f (transposition 0) a b > c |
           V2: (quantization 50) (tempo 240) c8 < b a g f e d c |
           V0: (panning 100) (track-volume 50) o4 c1~1/e1~1/g1~1 |
      """
    )
    _ <- putStrLn(
      s"${part.instrument}: ${part.elements.map(_.show).mkString(" ")}"
    )
    _ <- playSequence(
      SequenceGenerator.generateSequence(part, channel = 0)
    )
  } yield ()

  private[this] def playSequence(
      sequence: Sequence
  ): RIO[Console with Has[Synthesizer] with Has[Sequencer], Unit] =
    for {
      synthesizer <- RIO.service[Synthesizer]
      sequencer <- RIO.service[Sequencer]
      _ <- Task(
        sequencer.getTransmitter().setReceiver(synthesizer.getReceiver())
      )
      _ <- Task(sequencer.setSequence(sequence))
      _ <- Task(sequencer.start())
      _ <- Task.effectAsync[Unit](callback =>
        sequencer.addMetaEventListener(metaEvent =>
          if (metaEvent.getType() == 0x2F) callback(UIO.unit)
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
}
