package net.dhpiggott.apollo

import javax.sound.midi._

final case class Part(
    pulsesPerQuarterNote: Int,
    currentOctave: Int,
    currentNoteDuration: Duration,
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
      // Duration of longest note
      currentOctave = chord.notes
        .sortBy(_.duration.getOrElse(currentNoteDuration).reciprocal)
        .head
        .octave
        .getOrElse(currentOctave),
      currentNoteDuration = chord.notes
        .sortBy(_.duration.getOrElse(currentNoteDuration).reciprocal)
        .head
        .duration
        .getOrElse(currentNoteDuration),
      currentNoteVolume = chord.notes
        .sortBy(_.duration.getOrElse(currentNoteDuration).reciprocal)
        .head
        .volume
        .getOrElse(currentNoteVolume),
      channel,
      // Duration of shortest note
      offset + chord.notes
        .sortBy(_.duration.getOrElse(currentNoteDuration).reciprocal)
        .last
        .duration
        .getOrElse(currentNoteDuration)
        .ticks(pulsesPerQuarterNote),
      events ++ midiEvents(chord.notes)
    )

  private[this] def appendNote(note: Note): Part =
    copy(
      pulsesPerQuarterNote,
      currentOctave = note.octave.getOrElse(currentOctave),
      currentNoteDuration = note.duration.getOrElse(currentNoteDuration),
      currentNoteVolume = note.volume.getOrElse(currentNoteVolume),
      channel,
      offset + note.duration
        .getOrElse(currentNoteDuration)
        .ticks(pulsesPerQuarterNote),
      events ++ midiEvents(Seq(note))
    )

  private[this] def appendRest(rest: Rest): Part =
    copy(
      pulsesPerQuarterNote,
      currentOctave,
      currentNoteDuration = rest.duration.getOrElse(currentNoteDuration),
      currentNoteVolume,
      channel,
      offset + rest.duration
        .getOrElse(currentNoteDuration)
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
        offset + note.duration
          .getOrElse(currentNoteDuration)
          .ticks(pulsesPerQuarterNote)
      )
    } yield Seq(noteOn, noteOff)).flatten

}

// TODO: CRAM notation (see https://github.com/alda-lang/alda-core/blob/master/src/alda/lisp/events/cram.clj)
// TODO: Variables (see https://github.com/alda-lang/alda-core/blob/master/src/alda/lisp/events/variable.clj)
// TODO: Voices (see https://github.com/alda-lang/alda-core/blob/master/src/alda/lisp/events/voice.clj)
sealed abstract class Event
final case class Chord(notes: Seq[Note]) extends Event
final case class Note(
    pitch: Pitch,
    octave: Option[Int],
    duration: Option[Duration],
    volume: Option[Int]
) extends Event
object Note {
  def apply(pitch: Pitch): Note =
    Note(pitch, octave = None, duration = None, volume = None)
  def apply(pitch: Pitch, octave: Int): Note =
    Note(pitch, Some(octave), duration = None, volume = None)
  def apply(pitch: Pitch, octave: Int, duration: Duration): Note =
    Note(pitch, Some(octave), Some(duration), volume = None)
  def apply(
      pitch: Pitch,
      octave: Int,
      duration: Duration,
      volume: Int
  ): Note =
    Note(pitch, Some(octave), Some(duration), Some(volume))
}

final case class Rest(duration: Option[Duration]) extends Event
object Rest {
  def apply(duration: Duration): Rest =
    Rest(Some(duration))
}
case object Barline extends Event

final case class Duration(reciprocal: Int) {
  def ticks(pulsesPerQuarterNote: Int): Int =
    ((pulsesPerQuarterNote * 4) / reciprocal)
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
}
