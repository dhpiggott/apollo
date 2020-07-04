package net.dhpiggott.apollo

import java.util.concurrent.TimeUnit
import javax.sound.midi._

import zio._
import zio.clock._
import zio.duration._

class Channel(channel: MidiChannel) {

  val bpm = 80
  val sq =
    60L * 1000 / bpm / 4 // 16th note (semiquaver) duration in milliseconds

  def A(octave: Int = 3, n: Int = 1) = note((octave * 12) + 21, sq * n)
  def B(octave: Int = 3, n: Int = 1) = note((octave * 12) + 23, sq * n)
  def C(octave: Int = 3, n: Int = 1) = note((octave * 12) + 24, sq * n)
  def D(octave: Int = 3, n: Int = 1) = note((octave * 12) + 26, sq * n)
  def E(octave: Int = 3, n: Int = 1) = note((octave * 12) + 28, sq * n)
  def F(octave: Int = 3, n: Int = 1) = note((octave * 12) + 29, sq * n)
  def G(octave: Int = 3, n: Int = 1) = note((octave * 12) + 31, sq * n)

  private[this] def note(
      key: Int,
      gateTime: Long,
      velocity: Int = 127
  ): RIO[Clock, Unit] = {
    val press = Task(channel.noteOn(key, velocity))
    val wait = ZIO.sleep(Duration(gateTime, TimeUnit.MILLISECONDS))
    val release = Task(channel.noteOff(key))
    press *> wait *> release
  }
}

object Channel {

  def make(channel: Int, program: Int): RLayer[Has[Synthesizer], Has[Channel]] =
    ZManaged
      .make(
        for {
          synthesizer <- RIO.service[Synthesizer]
          channel <- Task(synthesizer.getChannels()(channel))
          _ <- Task(channel.programChange(program))
        } yield channel
      )(_ => UIO.unit)
      .map(new Channel(_))
      .toLayer

}

object Synthesizer {

  val make: TaskLayer[Has[Synthesizer]] =
    ZManaged
      .make(
        for {
          synthesizer <- Task(MidiSystem.getSynthesizer())
          _ <- Task(synthesizer.open())
        } yield synthesizer
      )(synthesizer => Task(synthesizer.close).orDie)
      .toLayer

}
