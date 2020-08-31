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
      """piano:
           V0: o4 c4. d8 r2 | e8 f r2. | c8/e4/g4 r8 a4 b > c | c2.~4 |
           V1: < c8 d e f g a b > c |
           V2: c8 < b a g f e d c |
           V0: o4 c1~1/e1~1/g1~1 |
      """
    )
    _ <- putStrLn(part.elements.toString())
    _ <- playSequence(
      SequenceGenerator.generateSequence(part, channel = 0),
      instruments =
        Map(0 -> Instruments.nonPercusssionInstruments(part.instrument))
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
}
