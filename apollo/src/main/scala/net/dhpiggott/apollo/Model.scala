package net.dhpiggott.apollo

import fastparse._, MultiLineWhitespace._

final case class Part(instrument: String, elements: Seq[ScoreElement])
object Part {
  def parse[_: P]: P[Part] =
    P(Primitives.name ~/ ":" ~/ ScoreElement.parse.rep(1))
      .map((Part.apply _).tupled)
}

final case class Pitch(chroma: Int) {
  def + = Pitch(chroma + 1)
  def - = Pitch(chroma - 1)
}
object Pitch {
  val C = Pitch(0)
  val D = Pitch(2)
  val E = Pitch(4)
  val F = Pitch(5)
  val G = Pitch(7)
  val A = Pitch(9)
  val B = Pitch(11)
  // TODO: Preserve original form
  def show(pitch: Pitch): String = pitch.chroma match {
    case 0  => "c"
    case 1  => "c#"
    case 2  => "d"
    case 3  => "d#"
    case 4  => "e"
    case 5  => "f"
    case 6  => "f#"
    case 7  => "g"
    case 8  => "g#"
    case 9  => "a"
    case 10 => "a#"
    case 11 => "b"
  }
}

sealed abstract class ScoreElement {
  def show: String
}
object ScoreElement {
  def parse[_: P]: P[ScoreElement] =
    Attribute.Duration.parse | Attribute.Octave.parse | Attribute.Panning.parse |
      Attribute.Quantization.parse | Attribute.Tempo.parse | Attribute.TrackVolume.parse |
      Attribute.Transposition.parse | Attribute.Volume.parse |
      Repeat.parse | Sequence.parse | Voice.parse | Chord.parse |
      Octave.parse | OctaveIncrement.parse | OctaveDecrement.parse |
      Note.parse | Rest.parse | Barline.parse | Marker.parse | MarkerReference.parse
}

sealed trait Attribute {
  def global: Boolean
}
object Attribute {

  private[this] def parser[V, _: P](
      name: => P[Unit],
      value: => P[V]
  ): P[(Boolean, V)] =
    P(
      "(" ~ name ~/ "!".!.? ~/ value ~/ ")"
    ).map {
      case (maybeGlobal, value) =>
        (maybeGlobal.isDefined, value)
    }

  final case class Duration(override val global: Boolean, value: Duration.Value)
      extends ScoreElement
      with Attribute {
    def show: String = s"(set-duration${if (global) "!" else ""} ${value.show})"
  }
  object Duration {
    sealed abstract class Value {
      def show: String
    }
    final case class Beats(value: Double) extends Value {
      override def show: String = s"$value"
    }
    final case class NoteLength(value: Int) extends Value {
      override def show: String = s"$value"
    }
    final case class Milliseconds(value: Int) extends Value {
      override def show: String = s"$value"
    }
    def parse[_: P]: P[Attribute.Duration] =
      parser(
        "set-duration",
        Primitives.double.map(Attribute.Duration.Beats) |
          P("(" ~ "note-length" ~/ Primitives.int ~/ ")")
            .map(Attribute.Duration.NoteLength) |
          P("(" ~ "ms" ~/ Primitives.int ~/ ")")
            .map(Attribute.Duration.Milliseconds)
      ).map(
          (Attribute.Duration.apply _).tupled
        )
  }

  // TODO: https://github.com/alda-lang/alda/blob/master/doc/attributes.md#key-signature

  final case class Octave(override val global: Boolean, value: Octave.Value)
      extends ScoreElement
      with Attribute {
    def show: String = s"(octave${if (global) "!" else ""} ${value.show})"
  }
  object Octave {
    sealed abstract class Value {
      def show: String
    }
    final case class AbsoluteValue(value: Int) extends Value {
      override def show: String = s"$value"
    }
    case object Increment extends Value {
      override def show: String = ":up"
    }
    case object Decrement extends Value {
      override def show: String = ":down"
    }
    def parse[_: P]: P[Attribute.Octave] =
      parser(
        "octave",
        Primitives.int.map(Attribute.Octave.AbsoluteValue) |
          P(":up").map(_ => Attribute.Octave.Increment) |
          P(":down").map(_ => Attribute.Octave.Decrement)
      ).map(
          (Attribute.Octave.apply _).tupled
        )
  }

  final case class Panning(override val global: Boolean, value: Int)
      extends ScoreElement
      with Attribute {
    def show: String = s"(panning${if (global) "!" else ""} $value)"
  }
  object Panning {
    def parse[_: P]: P[Attribute.Panning] =
      parser(
        "panning" | "pan",
        Primitives.int
      ).map(
          (Attribute.Panning.apply _).tupled
        )
  }

  final case class Quantization(override val global: Boolean, value: Int)
      extends ScoreElement
      with Attribute {
    def show: String = s"(quantization${if (global) "!" else ""} $value)"
  }
  object Quantization {
    def parse[_: P]: P[Attribute.Quantization] =
      parser(
        "quantization" | "quant" | "quantize",
        Primitives.int
      ).map(
          (Attribute.Quantization.apply _).tupled
        )
  }

  final case class Tempo(override val global: Boolean, beatsPerMinute: Int)
      extends ScoreElement
      with Attribute {
    def show: String = s"(tempo${if (global) "!" else ""} $beatsPerMinute)"
  }
  object Tempo {
    def parse[_: P]: P[Attribute.Tempo] =
      parser(
        "tempo",
        Primitives.int
      ).map(
        (Attribute.Tempo.apply _).tupled
      )
  }

  final case class TrackVolume(override val global: Boolean, value: Int)
      extends ScoreElement
      with Attribute {
    def show: String = s"(track-volume${if (global) "!" else ""} $value)"
  }
  object TrackVolume {
    def parse[_: P]: P[Attribute.TrackVolume] =
      parser(
        "track-volume" | "track-vol",
        Primitives.int
      ).map(
        (Attribute.TrackVolume.apply _).tupled
      )
  }

  final case class Transposition(override val global: Boolean, value: Int)
      extends ScoreElement
      with Attribute {
    def show: String = s"(transposition${if (global) "!" else ""} $value)"
  }
  object Transposition {
    def parse[_: P]: P[Attribute.Transposition] =
      parser(
        "transposition" | "transpose",
        Primitives.int
      ).map(
        (Attribute.Transposition.apply _).tupled
      )
  }

  final case class Volume(override val global: Boolean, value: Int)
      extends ScoreElement
      with Attribute {
    def show: String = s"(volume${if (global) "!" else ""} $value)"
  }
  object Volume {
    def parse[_: P]: P[Attribute.Volume] =
      parser(
        "volume" | "vol",
        Primitives.int
      ).map(
        (Attribute.Volume.apply _).tupled
      )
  }
}

// TODO: https://github.com/alda-lang/alda/blob/master/doc/repeats.md#repeats
final case class Repeat(sequence: Sequence, repetitions: Int)
    extends ScoreElement {
  def show: String = s"${sequence.show}*$repetitions"
}
object Repeat {
  def parse[_: P]: P[Repeat] =
    P(Sequence.parse ~ P("*") ~/ Primitives.int).map(
      (Repeat.apply _).tupled
    )
}

final case class Sequence(elements: Seq[ScoreElement]) extends ScoreElement {
  def show: String = s"[${elements.map(_.show).mkString(" ")}]"
}
object Sequence {
  def parse[_: P]: P[Sequence] =
    P("[" ~ ScoreElement.parse.rep ~ "]").map(Sequence(_))
}

final case class Voice(value: Int) extends ScoreElement {
  def show: String = s"V$value:"
}
object Voice {
  def parse[_: P]: P[Voice] =
    P("V" ~ Primitives.int ~/ ":").map(Voice(_))
}

final case class Chord(elements: Seq[ScoreElement with ChordElement])
    extends ScoreElement {
  def show: String = elements.map(_.show).mkString("/")
}
object Chord {
  def parse[_: P]: P[Chord] =
    (OctaveIncrement.parse | OctaveDecrement.parse | Note.parse | Rest.parse)
      .repX(min = 2, sep = "/")
      .map(Chord(_))
}

final case class Octave(value: Int) extends ScoreElement {
  def show: String = s"o$value"
}
object Octave {
  def parse[_: P]: P[Octave] =
    P("o" ~ Primitives.int).map(Octave(_))
}

sealed trait ChordElement

case object OctaveIncrement extends ScoreElement with ChordElement {
  def show: String = ">"
  def parse[_: P]: P[OctaveIncrement.type] =
    P(">").map(_ => OctaveIncrement)
}

case object OctaveDecrement extends ScoreElement with ChordElement {
  def show: String = "<"
  def parse[_: P]: P[OctaveDecrement.type] =
    P("<").map(_ => OctaveDecrement)
}

final case class Note(pitch: Pitch, duration: Option[Note.Duration])
    extends ScoreElement
    with ChordElement {
  def show: String =
    // TODO: Preserve original duration form
    s"${Pitch.show(pitch)}${duration.fold("")(_.value.toString)}"
}
object Note {
  final case class Duration(value: Double)
  object Duration {
    def parse[_: P]: P[Duration] =
      P(Primitives.int ~~ ".".!.repX)
        .map {
          case (denominator, dots) =>
            Duration(
              (1d / denominator) + dots.zipWithIndex.map {
                case (_, index) =>
                  1d / (math.pow(2d, (index + 1).toDouble) * denominator)
              }.sum
            )
        }
        .repX(min = 1, sep = "~")
        .map(noteDurations => Duration(noteDurations.map(_.value).sum))
  }
  def parse[_: P]: P[Note] =
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
        Duration.parse.?
    ).map {
      case (pitch, accidentals, maybeDuration) =>
        Note(
          pitch.copy(chroma = pitch.chroma + accidentals.sum),
          duration = maybeDuration
        )
    }
}

final case class Rest(noteDuration: Option[Note.Duration])
    extends ScoreElement
    with ChordElement {
  // TODO: Preserve original duration form
  def show: String = s"r${noteDuration.fold("")(_.value.toString)}"
}
object Rest {
  def parse[_: P]: P[Rest] =
    P("r" ~~ Note.Duration.parse.?).map(maybeDuration =>
      Rest(noteDuration = maybeDuration)
    )
}

case object Barline extends ScoreElement {
  def show: String = "|"
  def parse[_: P]: P[Barline.type] =
    P("|").map(_ => Barline)
}

final case class Marker(name: String) extends ScoreElement {
  def show: String = s"@$name"
}
object Marker {
  def parse[_: P]: P[Marker] =
    ("%" ~~/ Primitives.name).map(Marker(_))
}

final case class MarkerReference(marker: Marker) extends ScoreElement {
  def show: String = s"%${marker.name}"
}
object MarkerReference {
  def parse[_: P]: P[MarkerReference] =
    ("@" ~~/ Primitives.name).map((MarkerReference.apply _).compose(Marker(_)))
}

object Primitives {

  def name[_: P]: P[String] =
    P(
      CharIn("a-zA-Z").repX(exactly = 2).! ~~/
        CharIn("a-zA-Z0-9_\\-+'()").repX.!
    ).map {
      case (prefix, suffix) => prefix + suffix
    }

  def int[_: P]: P[Int] =
    CharIn("0-9").repX(1).!.map(_.toInt)

  def double[_: P]: P[Double] =
    CharIn("0-9.").repX(1).!.map(_.toDouble)

}
