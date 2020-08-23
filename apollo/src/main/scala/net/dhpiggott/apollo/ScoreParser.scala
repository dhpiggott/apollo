package net.dhpiggott.apollo

import fastparse._, MultiLineWhitespace._
import zio._

object ScoreParser {

  def parseScorePart(scorePart: String): Task[Part] =
    parse(scorePart, part(_)) match {
      case failure: Parsed.Failure  => Task.fail(failure.get)
      case Parsed.Success(value, _) => UIO(value)
    }

  private[this] def part[_: P]: P[Part] =
    P(instrumentName ~ ":" ~ scoreElement.rep(1)).map {
      case (instrument, scoreElements) =>
        Part(
          instrument,
          defaultNoteAttributes =
            NoteAttributes(octave = 4, length = Note.Length(4), volume = 127),
          scoreElements
        )
    }

  private[this] def instrumentName[_: P]: P[String] =
    P(
      CharIn("a-zA-Z").rep(exactly = 2, sep = "").! ~
        CharIn("a-zA-Z0-9_\\-+'()").rep(sep = "").!
    ).map {
      case (prefix, suffix) => prefix + suffix
    }

  private[this] def scoreElement[_: P]: P[ScoreElement] =
    P(
      octave | octaveIncrement | octaveDecrement | chord | note | rest | barline
    )

  private[this] def octave[_: P]: P[Octave] =
    P("o" ~ number).map(Octave)

  private[this] def octaveIncrement[_: P]: P[OctaveIncrement.type] =
    P(">").map(_ => OctaveIncrement)

  private[this] def octaveDecrement[_: P]: P[OctaveDecrement.type] =
    P("<").map(_ => OctaveDecrement)

  private[this] def chord[_: P]: P[Chord] =
    P(note.rep(min = 2, sep = "/")).map(Chord)

  private[this] def note[_: P]: P[Note] =
    P(pitch ~ noteLength.?).map {
      case (pitch, maybeLength) =>
        Note(pitch, length = maybeLength)
    }

  private[this] def rest[_: P]: P[Rest] =
    P("r" ~ noteLength.?).map(maybeLength => Rest(noteLength = maybeLength))

  private[this] def barline[_: P]: P[Barline.type] =
    P("|").map(_ => Barline)

  private[this] def pitch[_: P]: P[Pitch] =
    P(CharIn("a-g").!.map {
      case "a" => Pitch.A
      case "b" => Pitch.B
      case "c" => Pitch.C
      case "d" => Pitch.D
      case "e" => Pitch.E
      case "f" => Pitch.F
      case "g" => Pitch.G
    } ~ (P("+").map(_ => 1) | P("-").map(_ => -1)).rep).map {
      case (pitch, accidentals) =>
        pitch.copy(chroma = pitch.chroma + accidentals.sum)
    }

  private[this] def noteLength[_: P]: P[Note.Length] = number.map(Note.Length)

  private[this] def number[_: P]: P[Int] =
    P(CharIn("0-9").rep(1).!.map(_.toInt))

}
