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
}

sealed abstract class ScoreElement

final case class Voice(value: Int) extends ScoreElement

final case class Octave(value: Int) extends ScoreElement

case object OctaveIncrement extends ScoreElement

case object OctaveDecrement extends ScoreElement

final case class Chord(notes: Seq[Note]) extends ScoreElement

final case class Note(pitch: Pitch, duration: Option[Note.Duration])
    extends ScoreElement

object Note {
  final case class Duration(value: Double)
}

final case class Rest(noteDuration: Option[Note.Duration]) extends ScoreElement

case object Barline extends ScoreElement

final case class Part(instrument: String, elements: Seq[ScoreElement])
