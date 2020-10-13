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

// TODO: CRAM notation (see https://github.com/alda-lang/alda-core/blob/master/src/alda/lisp/events/cram.clj)
// TODO: Variables (see https://github.com/alda-lang/alda-core/blob/master/src/alda/lisp/events/variable.clj)
// TODO: Voices (see https://github.com/alda-lang/alda-core/blob/master/src/alda/lisp/events/voice.clj)
// See also: https://github.com/alda-lang/alda/tree/master/doc
sealed abstract class ScoreElement

final case class Note(
    pitch: Pitch,
    octave: Option[Int],
    length: Option[Note.Length],
    volume: Option[Int]
) extends ScoreElement

object Note {

  final case class Length(reciprocal: Int)

  def apply(pitch: Pitch): Note =
    Note(pitch, octave = None, length = None, volume = None)
  def apply(pitch: Pitch, octave: Int): Note =
    Note(pitch, Some(octave), length = None, volume = None)
  def apply(pitch: Pitch, octave: Int, length: Length): Note =
    Note(pitch, Some(octave), Some(length), volume = None)
  def apply(
      pitch: Pitch,
      octave: Int,
      length: Length,
      volume: Int
  ): Note =
    Note(pitch, Some(octave), Some(length), Some(volume))
}

final case class Chord(notes: Seq[Note]) extends ScoreElement

final case class Rest(noteLength: Option[Note.Length]) extends ScoreElement
object Rest {
  def apply(noteLength: Note.Length): Rest =
    Rest(Some(noteLength))
}

case object Barline extends ScoreElement

final case class NoteAttributes(
    octave: Int,
    length: Note.Length,
    volume: Int
) {
  def updated(scoreElement: ScoreElement): NoteAttributes = scoreElement match {
    case chord: Chord =>
      val longestNote = chord.notes
        .sortBy(_.length.getOrElse(length).reciprocal)
        .headOption
      longestNote match {
        case None              => this
        case Some(longestNote) => updated(longestNote)
      }

    case note: Note =>
      copy(
        octave = note.octave.getOrElse(octave),
        length = note.length.getOrElse(length),
        volume = note.volume.getOrElse(volume)
      )

    case rest: Rest =>
      copy(length = rest.noteLength.getOrElse(length))

    case Barline =>
      this
  }
}

// TODO: Review
// https://github.com/alda-lang/alda-core/tree/master/src/alda/lisp,
// https://github.com/alda-lang/alda-core/blob/master/src/alda/lisp/score.clj,
// https://github.com/alda-lang/alda-core/blob/master/src/alda/lisp/score/part.clj
// and
// https://github.com/alda-lang/alda-server-clj/blob/master/src/alda/worker.clj,
// https://github.com/alda-lang/alda-server-clj/blob/master/src/alda/server.clj
// and
// https://github.com/alda-lang/alda-sound-engine-clj/blob/master/src/alda/sound.clj,
// https://github.com/alda-lang/alda-sound-engine-clj/blob/master/src/alda/sound/midi.clj
final case class Part(
    instrument: String,
    elements: Seq[(ScoreElement, NoteAttributes)]
)

object Part {
  def apply(
      instrument: String,
      defaultNoteAttributes: NoteAttributes,
      elements: Seq[ScoreElement]
  ): Part =
    Part(
      instrument,
      elements
        .foldLeft(Seq.empty[(ScoreElement, NoteAttributes)]) {
          case (resolvedElements, element) =>
            resolvedElements match {
              case (_ -> noteAttributes) +: _ =>
                (element -> noteAttributes.updated(element)) +: resolvedElements

              case Nil =>
                Seq(element -> defaultNoteAttributes.updated(element))
            }
        }
        .reverse
    )
}
