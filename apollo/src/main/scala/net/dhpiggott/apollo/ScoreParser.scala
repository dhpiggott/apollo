package net.dhpiggott.apollo

import fastparse._, MultiLineWhitespace._
import zio._

object ScoreParser {

  def parseScorePart(scorePart: String): Task[Part] = {
    def partWithEnd[_: P]: P[Part] = P(part ~ End)
    parse(scorePart, partWithEnd(_)) match {
      case failure: Parsed.Failure  => Task.fail(failure.get)
      case Parsed.Success(value, _) => UIO(value)
    }
  }

  private[this] def part[_: P]: P[Part] =
    P(name ~/ ":" ~/ scoreElement.rep(1)).map((Part.apply _).tupled)

  private[this] def scoreElement[_: P]: P[ScoreElement] =
    durationAttribute | octaveAttribute | panningAttribute |
      quantizationAttribute | tempoAttribute | trackVolumeAttribute |
      transpositionAttribute | volumeAttribute |
      repeat | sequence | voice | chord | octave | octaveIncrement | octaveDecrement | note |
      rest | barline | marker | markerReference

  private[this] def durationAttribute[_: P]: P[Attribute.Duration] =
    attribute(
      "set-duration",
      double.map(Attribute.Duration.Beats) |
        P("(" ~ "note-length" ~/ int ~/ ")")
          .map(Attribute.Duration.NoteLength) |
        P("(" ~ "ms" ~/ int ~/ ")").map(Attribute.Duration.Milliseconds)
    ).map(
      (Attribute.Duration.apply _).tupled
    )

  private[this] def octaveAttribute[_: P]: P[Attribute.Octave] =
    attribute(
      "octave",
      int.map(Attribute.Octave.AbsoluteValue) |
        P(":up").map(_ => Attribute.Octave.Increment) |
        P(":down").map(_ => Attribute.Octave.Decrement)
    ).map(
      (Attribute.Octave.apply _).tupled
    )

  private[this] def panningAttribute[_: P]: P[Attribute.Panning] =
    attribute(
      "panning" | "pan",
      int
    ).map(
      (Attribute.Panning.apply _).tupled
    )

  private[this] def quantizationAttribute[_: P]: P[Attribute.Quantization] =
    attribute(
      "quantization" | "quant" | "quantize",
      int
    ).map(
      (Attribute.Quantization.apply _).tupled
    )

  private[this] def tempoAttribute[_: P]: P[Attribute.Tempo] =
    attribute(
      "tempo",
      int
    ).map(
      (Attribute.Tempo.apply _).tupled
    )

  private[this] def trackVolumeAttribute[_: P]: P[Attribute.TrackVolume] =
    attribute(
      "track-volume" | "track-vol",
      int
    ).map(
      (Attribute.TrackVolume.apply _).tupled
    )

  private[this] def transpositionAttribute[_: P]: P[Attribute.Transposition] =
    attribute(
      "transposition" | "transpose",
      int
    ).map(
      (Attribute.Transposition.apply _).tupled
    )

  private[this] def volumeAttribute[_: P]: P[Attribute.Volume] =
    attribute(
      "volume" | "vol",
      int
    ).map(
      (Attribute.Volume.apply _).tupled
    )

  private[this] def attribute[V, _: P](
      name: => P[Unit],
      value: => P[V]
  ): P[(Boolean, V)] =
    P(
      "(" ~ name ~/ "!".!.? ~/ value ~/ ")"
    ).map {
      case (maybeGlobal, value) =>
        (maybeGlobal.isDefined, value)
    }

  private[this] def repeat[_: P]: P[Repeat] =
    P(sequence ~ P("*") ~/ int).map(
      (Repeat.apply _).tupled
    )

  private[this] def sequence[_: P]: P[Sequence] =
    P("[" ~ scoreElement.rep ~ "]").map(Sequence)

  private[this] def voice[_: P]: P[Voice] =
    P("V" ~ int ~/ ":").map(Voice)

  private[this] def chord[_: P]: P[Chord] =
    (octaveIncrement | octaveDecrement | note | rest)
      .repX(min = 2, sep = "/")
      .map(Chord)

  private[this] def octave[_: P]: P[Octave] =
    P("o" ~ int).map(Octave)

  private[this] def octaveIncrement[_: P]: P[OctaveIncrement.type] =
    P(">").map(_ => OctaveIncrement)

  private[this] def octaveDecrement[_: P]: P[OctaveDecrement.type] =
    P("<").map(_ => OctaveDecrement)

  private[this] def note[_: P]: P[Note] =
    P(
      CharIn("a-g").!.map {
        case "a" => Pitch.A
        case "b" => Pitch.B
        case "c" => Pitch.C
        case "d" => Pitch.D
        case "e" => Pitch.E
        case "f" => Pitch.F
        case "g" => Pitch.G
      } ~~
        (P("+").map(_ => 1) | P("-").map(_ => -1)).repX ~~
        noteDuration.?
    ).map {
      case (pitch, accidentals, maybeDuration) =>
        Note(
          pitch.copy(chroma = pitch.chroma + accidentals.sum),
          duration = maybeDuration
        )
    }

  private[this] def rest[_: P]: P[Rest] =
    P("r" ~~ noteDuration.?).map(maybeDuration =>
      Rest(noteDuration = maybeDuration)
    )

  private[this] def barline[_: P]: P[Barline.type] =
    P("|").map(_ => Barline)

  private[this] def marker[_: P]: P[Marker] =
    ("%" ~~/ name).map(Marker)

  private[this] def markerReference[_: P]: P[MarkerReference] =
    ("@" ~~/ name).map(MarkerReference.compose(Marker))

  private[this] def noteDuration[_: P]: P[Note.Duration] =
    P(int ~~ ".".!.repX)
      .map {
        case (denominator, dots) =>
          Note.Duration(
            (1d / denominator) + dots.zipWithIndex.map {
              case (_, index) =>
                1d / (math.pow(2d, (index + 1).toDouble) * denominator)
            }.sum
          )
      }
      .repX(min = 1, sep = "~")
      .map(noteDurations => Note.Duration(noteDurations.map(_.value).sum))

  private[this] def name[_: P]: P[String] =
    P(
      CharIn("a-zA-Z").repX(exactly = 2).! ~~/
        CharIn("a-zA-Z0-9_\\-+'()").repX.!
    ).map {
      case (prefix, suffix) => prefix + suffix
    }

  private[this] def int[_: P]: P[Int] =
    CharIn("0-9").repX(1).!.map(_.toInt)

  private[this] def double[_: P]: P[Double] =
    CharIn("0-9.").repX(1).!.map(_.toDouble)

}
