package net.dhpiggott.apollo

import zio._
import zio.clock._

object Apollo extends App {

  override def run(
      args: List[String]
  ): URIO[ZEnv, ExitCode] =
    program.provideCustomLayer(channelLayer).exitCode

  private[this] val program: RIO[Clock with Has[Channel], Unit] = for {
    channel <- ZIO.service[Channel]
    _ <- RIO
      .collectAll(
        List(
          channel.C(3, 4) <&> channel.E(3, 4) <&> channel.G(3, 4),
          channel.C(4, 4) <&> channel.E(4, 4) <&> channel.G(4, 4),
          channel.C(3, 4) <&> channel.E(3, 4) <&> channel.G(3, 4)
        )
      )
      .unit
  } yield ()

  private[this] val channelLayer: TaskLayer[Has[Channel]] =
    Synthesizer.make >>> Channel.make(0, 1)

}
