package net.dhpiggott.apollo

import java.nio.file.Files
import java.nio.file.Path
import javax.sound.midi._

import fastparse._
import zio._
import zio.console._

// TODO: Review
// https://github.com/alda-lang/alda-server-clj/blob/master/src/alda/worker.clj,
// https://github.com/alda-lang/alda-server-clj/blob/master/src/alda/server.clj
// and
// https://github.com/alda-lang/alda-sound-engine-clj/blob/master/src/alda/sound.clj,
// https://github.com/alda-lang/alda-sound-engine-clj/blob/master/src/alda/sound/midi.clj
object Apollo extends App {

  override def run(args: List[String]): URIO[ZEnv, ExitCode] =
    program(args).provideCustomLayer(midiSynthesizer ++ midiSequencer).exitCode

  private[this] def program(
      args: List[String]
  ): RIO[Console with Has[Synthesizer] with Has[Sequencer], Unit] =
    for {
      // TODO: Proper CLI
      rawScore <- Task(Files.readString(Path.of(args(0))))
      parsedScore <- parse(rawScore, Score.parse(_)) match {
        case failure: Parsed.Failure  => Task.fail(failure.get)
        case Parsed.Success(value, _) => UIO(value)
      }
      _ <- putStrLn(parsedScore.show)
      _ <- playMidiSequence(MidiSequenceGenerator.generateSequence(parsedScore))
    } yield ()

  private[this] def playMidiSequence(
      sequence: Sequence
  ): RIO[Has[Synthesizer] with Has[Sequencer], Unit] =
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

  private[this] def midiSynthesizer: TaskLayer[Has[Synthesizer]] =
    Managed
      .make(
        for {
          synthesizer <- Task(MidiSystem.getSynthesizer())
          _ <- Task(synthesizer.open())
        } yield synthesizer
      )(synthesizer => Task(synthesizer.close).orDie)
      .toLayer

  private[this] def midiSequencer: TaskLayer[Has[Sequencer]] =
    Managed
      .make(
        for {
          sequencer <- Task(MidiSystem.getSequencer())
          _ <- Task(sequencer.open())
        } yield sequencer
      )(sequencer => Task(sequencer.close).orDie)
      .toLayer
}
