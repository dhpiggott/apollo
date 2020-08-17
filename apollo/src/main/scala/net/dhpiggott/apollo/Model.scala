package net.dhpiggott.apollo

import javax.sound.midi._

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
sealed abstract class Event

final case class Note(
    pitch: Pitch,
    octave: Option[Int],
    length: Option[Note.Length],
    volume: Option[Int]
) extends Event

object Note {

  final case class Length(reciprocal: Int) {
    def ticks(pulsesPerQuarterNote: Int): Int =
      ((pulsesPerQuarterNote * 4) / reciprocal)
  }

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

final case class Chord(notes: Seq[Note]) extends Event

final case class Rest(noteLength: Option[Note.Length]) extends Event
object Rest {
  def apply(noteLength: Note.Length): Rest =
    Rest(Some(noteLength))
}

case object Barline extends Event

final case class Part(
    pulsesPerQuarterNote: Int,
    currentOctave: Int,
    currentNoteLength: Note.Length,
    currentNoteVolume: Int,
    channel: Int,
    offset: Long,
    events: Seq[MidiEvent]
) {

  def append(events: Seq[Event]): Part =
    events.foldLeft(this)(_ append _)

  def append(event: Event): Part =
    event match {
      case chord: Chord => appendChord(chord)
      case note: Note   => appendNote(note)
      case rest: Rest   => appendRest(rest)
      case Barline      => this
    }

  private[this] def appendChord(
      chord: Chord
  ): Part =
    copy(
      pulsesPerQuarterNote,
      // Length of longest note
      currentOctave = chord.notes
        .sortBy(_.length.getOrElse(currentNoteLength).reciprocal)
        .head
        .octave
        .getOrElse(currentOctave),
      currentNoteLength = chord.notes
        .sortBy(_.length.getOrElse(currentNoteLength).reciprocal)
        .head
        .length
        .getOrElse(currentNoteLength),
      currentNoteVolume = chord.notes
        .sortBy(_.length.getOrElse(currentNoteLength).reciprocal)
        .head
        .volume
        .getOrElse(currentNoteVolume),
      channel,
      // Length of shortest note
      offset + chord.notes
        .sortBy(_.length.getOrElse(currentNoteLength).reciprocal)
        .last
        .length
        .getOrElse(currentNoteLength)
        .ticks(pulsesPerQuarterNote),
      events ++ midiEvents(chord.notes)
    )

  private[this] def appendNote(note: Note): Part =
    copy(
      pulsesPerQuarterNote,
      currentOctave = note.octave.getOrElse(currentOctave),
      currentNoteLength = note.length.getOrElse(currentNoteLength),
      currentNoteVolume = note.volume.getOrElse(currentNoteVolume),
      channel,
      offset + note.length
        .getOrElse(currentNoteLength)
        .ticks(pulsesPerQuarterNote),
      events ++ midiEvents(Seq(note))
    )

  private[this] def appendRest(rest: Rest): Part =
    copy(
      pulsesPerQuarterNote,
      currentOctave,
      currentNoteLength = rest.noteLength.getOrElse(currentNoteLength),
      currentNoteVolume,
      channel,
      offset + rest.noteLength
        .getOrElse(currentNoteLength)
        .ticks(pulsesPerQuarterNote),
      events
    )

  private[this] def midiEvents(notes: Seq[Note]): Seq[MidiEvent] =
    (for {
      note <- notes
      toneNumber = (note.octave.getOrElse(currentOctave) + 1) * 12 + note.pitch.chroma
      noteOn = new MidiEvent(
        new ShortMessage(
          ShortMessage.NOTE_ON,
          channel,
          toneNumber,
          note.volume.getOrElse(currentNoteVolume)
        ),
        offset
      )
      noteOff = new MidiEvent(
        new ShortMessage(
          ShortMessage.NOTE_OFF,
          channel,
          toneNumber,
          note.volume.getOrElse(currentNoteVolume)
        ),
        offset + note.length
          .getOrElse(currentNoteLength)
          .ticks(pulsesPerQuarterNote)
      )
    } yield Seq(noteOn, noteOff)).flatten

}
