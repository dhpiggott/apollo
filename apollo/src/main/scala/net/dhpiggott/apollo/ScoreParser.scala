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
    P(name ~/ ":" ~/ scoreElement.rep(1)).map(Part.tupled)

  private[this] def scoreElement[_: P]: P[ScoreElement] =
    attribute | voice | chord | octave | octaveIncrement | octaveDecrement | note | rest | barline | marker | markerReference

  private[this] def attribute[_: P]: P[ScoreElement with Attribute] =
    durationAttribute | octaveAttribute | panningAttribute | quantizationAttribute | tempoAttribute | trackVolumeAttribute | transpositionAttribute | volumeAttribute

  private[this] def durationAttribute[_: P]: P[Attribute.Duration] =
    P("(" ~ "set-duration" ~/ "!".!.? ~/ durationValueAttribute ~/ ")").map {
      case (maybeGlobal, value) =>
        Attribute.Duration(global = maybeGlobal.isDefined, value)
    }

  private[this] def durationValueAttribute[_: P]: P[Attribute.Duration.Value] =
    durationBeatsAttribute | durationNoteLengthAttribute | durationMillisecondsAttribute

  private[this] def durationBeatsAttribute[_: P]: P[Attribute.Duration.Beats] =
    double.map(Attribute.Duration.Beats)

  private[this] def durationNoteLengthAttribute[_: P]
      : P[Attribute.Duration.NoteLength] =
    P("(" ~ "note-length" ~/ int ~/ ")").map(Attribute.Duration.NoteLength)

  private[this] def durationMillisecondsAttribute[_: P]
      : P[Attribute.Duration.Milliseconds] =
    P("(" ~ "ms" ~/ int ~/ ")").map(Attribute.Duration.Milliseconds)

  private[this] def octaveAttribute[_: P]: P[Attribute.Octave] =
    P("(" ~ "octave" ~/ "!".!.? ~/ octaveValueAttribute ~/ ")").map {
      case (maybeGlobal, value) =>
        Attribute.Octave(global = maybeGlobal.isDefined, value)
    }

  private[this] def octaveValueAttribute[_: P]: P[Attribute.Octave.Value] =
    octaveAbsoluteValueAttribute | octaveIncrementAttribute | octaveDecrementAttribute

  private[this] def octaveAbsoluteValueAttribute[_: P]
      : P[Attribute.Octave.AbsoluteValue] =
    int.map(Attribute.Octave.AbsoluteValue)

  private[this] def octaveIncrementAttribute[_: P]
      : P[Attribute.Octave.Increment.type] =
    P(":up").map(_ => Attribute.Octave.Increment)

  private[this] def octaveDecrementAttribute[_: P]
      : P[Attribute.Octave.Decrement.type] =
    P(":down").map(_ => Attribute.Octave.Decrement)

  private[this] def panningAttribute[_: P]: P[Attribute.Panning] =
    P("(" ~ ("panning" | "pan") ~/ "!".!.? ~/ int ~/ ")").map {
      case (maybeGlobal, value) =>
        Attribute.Panning(global = maybeGlobal.isDefined, value)
    }

  private[this] def quantizationAttribute[_: P]: P[Attribute.Quantization] =
    P("(" ~ ("quantization" | "quant" | "quantize") ~/ "!".!.? ~/ int ~/ ")")
      .map {
        case (maybeGlobal, value) =>
          Attribute.Quantization(global = maybeGlobal.isDefined, value)
      }

  private[this] def tempoAttribute[_: P]: P[Attribute.Tempo] =
    P("(" ~ "tempo" ~/ "!".!.? ~/ int ~/ ")").map {
      case (maybeGlobal, beatsPerMinute) =>
        Attribute.Tempo(global = maybeGlobal.isDefined, beatsPerMinute)
    }

  private[this] def trackVolumeAttribute[_: P]: P[Attribute.TrackVolume] =
    P("(" ~ ("track-volume" | "track-vol") ~/ "!".!.? ~/ int ~/ ")").map {
      case (maybeGlobal, value) =>
        Attribute.TrackVolume(global = maybeGlobal.isDefined, value)
    }

  private[this] def transpositionAttribute[_: P]: P[Attribute.Transposition] =
    P("(" ~ ("transposition" | "transpose") ~/ "!".!.? ~/ int ~/ ")").map {
      case (maybeGlobal, value) =>
        Attribute.Transposition(global = maybeGlobal.isDefined, value)
    }

  private[this] def volumeAttribute[_: P]: P[Attribute.Volume] =
    P("(" ~ ("volume" | "vol") ~/ "!".!.? ~/ int ~/ ")").map {
      case (maybeGlobal, value) =>
        Attribute.Volume(global = maybeGlobal.isDefined, value)
    }

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
    P(pitch ~~ noteDuration.?).map {
      case (pitch, maybeDuration) =>
        Note(pitch, duration = maybeDuration)
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

  private[this] def pitch[_: P]: P[Pitch] =
    P(CharIn("a-g").!.map {
      case "a" => Pitch.A
      case "b" => Pitch.B
      case "c" => Pitch.C
      case "d" => Pitch.D
      case "e" => Pitch.E
      case "f" => Pitch.F
      case "g" => Pitch.G
    } ~~ (P("+").map(_ => 1) | P("-").map(_ => -1)).repX).map {
      case (pitch, accidentals) =>
        pitch.copy(chroma = pitch.chroma + accidentals.sum)
    }

  private[this] def noteDuration[_: P]: P[Note.Duration] =
    P(int ~~ ".".!.repX).map {
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
