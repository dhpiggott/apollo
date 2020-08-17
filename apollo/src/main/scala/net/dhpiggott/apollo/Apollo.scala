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
    part = Part(
      sequence.getResolution(),
      currentOctave = 4,
      currentNoteDuration = Duration(4),
      currentNoteVolume = 127,
      channel = 0,
      offset = 0,
      events = Seq.empty
    )
    notes = Seq(
      Note(Pitch.C),
      Note(Pitch.D),
      Rest(Duration(2)),
      Barline,
      Note(Pitch.E, 4, Duration(4)),
      Note(Pitch.F),
      Rest(Duration(2)),
      Barline,
      Chord(
        Seq(
          Note(Pitch.C, 4, Duration(8)),
          Note(Pitch.E, 4, Duration(4)),
          Note(Pitch.G, 4, Duration(4))
        )
      ),
      Rest(Duration(8)),
      Note(Pitch.A, 4, Duration(4)),
      // FIXME: Shouldn't need to specify this here just to have it
      // get reset when reversed...
      Note(Pitch.B, 4),
      Note(Pitch.C, 5),
      Barline
    )
    _ = part.append(notes ++ notes.reverse).events.foreach(track.add)
    _ <- playSequence(
      sequence,
      instruments =
        Map(0 -> Instruments.nonPercusssionInstruments("square-wave"))
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
