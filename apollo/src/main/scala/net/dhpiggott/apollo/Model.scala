package net.dhpiggott.apollo

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

sealed trait Attribute {
  def global: Boolean
}

object Attribute {
  final case class Transposition(override val global: Boolean, value: Int)
      extends ScoreElement
      with Attribute {
    def show: String = s"transposition${if (global) "!" else ""}: $value"
  }

  final case class Volume(override val global: Boolean, value: Int)
      extends ScoreElement
      with Attribute {
    def show: String = s"volume${if (global) "!" else ""}: $value"
  }
}

final case class Voice(value: Int) extends ScoreElement {
  def show: String = s"V$value:"
}

final case class Chord(elements: Seq[ScoreElement with ChordElement])
    extends ScoreElement {
  def show: String = elements.map(_.show).mkString("/")
}

final case class Octave(value: Int) extends ScoreElement {
  def show: String = s"o$value"
}

sealed trait ChordElement

case object OctaveIncrement extends ScoreElement with ChordElement {
  def show: String = ">"
}

case object OctaveDecrement extends ScoreElement with ChordElement {
  def show: String = "<"
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
}

final case class Rest(noteDuration: Option[Note.Duration])
    extends ScoreElement
    with ChordElement {
  // TODO: Preserve original duration form
  def show: String = s"r${noteDuration.fold("")(_.value.toString)}"
}

case object Barline extends ScoreElement {
  def show: String = "|"
}

final case class Part(instrument: String, elements: Seq[ScoreElement])
