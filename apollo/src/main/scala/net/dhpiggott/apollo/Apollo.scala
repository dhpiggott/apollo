package net.dhpiggott.apollo

import javax.sound.midi._

import zio._
import zio.console._

object Apollo extends App {

  override def run(args: List[String]): URIO[ZEnv, ExitCode] =
    program.provideCustomLayer(synthesizer ++ sequencer).exitCode

  private[this] val program
      : RIO[Console with Has[Synthesizer] with Has[Sequencer], Unit] = {
    val part = Part(
      instrument = "square-wave",
      defaultNoteAttributes = NoteAttributes(
        octave = 4,
        length = Note.Length(4),
        volume = 127
      ),
      // TODO: Build parser - see
      // https://github.com/alda-lang/alda-core/tree/master/src/alda/parser
      elements = Seq(
        Note(Pitch.C),
        Note(Pitch.D),
        Rest(Note.Length(2)),
        Barline,
        Note(Pitch.E, 4, Note.Length(4)),
        Note(Pitch.F),
        Rest(Note.Length(2)),
        Barline,
        Chord(
          Seq(
            Note(Pitch.C, 4, Note.Length(8)),
            Note(Pitch.E, 4, Note.Length(4)),
            Note(Pitch.G, 4, Note.Length(4))
          )
        ),
        Rest(Note.Length(8)),
        Note(Pitch.A, 4, Note.Length(4)),
        Note(Pitch.B),
        Note(Pitch.C, 5),
        Barline
      )
    )
    playSequence(
      SequenceGenerator.generateSequence(
        part.copy(
          elements = part.elements ++ part.elements.reverse
        ),
        channel = 0
      ),
      instruments =
        Map(0 -> Instruments.nonPercusssionInstruments(part.instrument))
    )
  }

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
