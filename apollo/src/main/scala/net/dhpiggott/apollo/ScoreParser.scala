package net.dhpiggott.apollo

import fastparse._, MultiLineWhitespace._

object ScoreParser {

  def part[_: P]: P[Part] =
    P(scoreElement.rep(1)).map(scoreElements =>
      Part(
        instrument = "piano",
        defaultNoteAttributes = NoteAttributes(4, Note.Length(4), 100),
        scoreElements
      )
    ).log

  def scoreElement[_: P]: P[ScoreElement] = 
    P(note | rest)

  def note[_: P]: P[Note] =
    P(pitch ~ noteLength.?).map {
      case (pitch, maybeLength) =>
        Note(pitch, octave = None, length = maybeLength, volume = None)
    }

  def rest[_: P]: P[Rest] =
    P("r" ~ noteLength.?).map(maybeLength => Rest(noteLength = maybeLength))

  def pitch[_: P]: P[Pitch] =
    P(CharIn("a-g").!.map {
      case "a" => Pitch.A
      case "b" => Pitch.B
      case "c" => Pitch.C
      case "d" => Pitch.D
      case "e" => Pitch.E
      case "f" => Pitch.F
      case "g" => Pitch.G
    } ~ (sharp | flat).rep).map {
      case (pitch, accidentals) =>
        pitch.copy(chroma = pitch.chroma + accidentals.sum)
    }

  def noteLength[_: P]: P[Note.Length] = number.map(Note.Length)

  def sharp[_: P]: P[Int] = P("+").map(_ => 1)

  def flat[_: P]: P[Int] = P("-").map(_ => -1)

  def number[_: P]: P[Int] = P(CharIn("0-9").rep(1).!.map(_.toInt))

  def main(args: Array[String]): Unit = {
    println(
      parse("a2 r b4", part(_))
    )
  }
}
