package net.dhpiggott.apollo

import javax.sound.midi._

object SequenceGenerator {

  def generateSequence(part: Part, channel: Int): Sequence = {
    val pulsesPerQuarterNote = 30
    val sequence = new Sequence(Sequence.PPQ, pulsesPerQuarterNote)
    val track = sequence.createTrack()
    val (events, _) =
      part.elements.foldLeft(Seq.empty[MidiEvent] -> 0L) {
        case ((events, offset), (scoreElement, noteAttributes)) =>
          val (newEvents, offsetIncrement) = scoreElement match {
            case Octave(_) | OctaveIncrement | OctaveDecrement =>
              Seq.empty -> 0L

            case Chord(notes) =>
              val shortestNote = notes
                .sortBy(_.duration.getOrElse(noteAttributes.duration).value)
                .headOption
              midiEvents(
                offset,
                notes,
                channel,
                pulsesPerQuarterNote,
                noteAttributes
              ) -> pulses(
                shortestNote
                  .flatMap(_.duration)
                  .getOrElse(noteAttributes.duration),
                pulsesPerQuarterNote * 4
              )

            case note: Note =>
              midiEvents(
                offset,
                Seq(note),
                channel,
                pulsesPerQuarterNote,
                noteAttributes
              ) -> pulses(
                note.duration
                  .getOrElse(noteAttributes.duration),
                pulsesPerQuarterNote * 4
              )

            case Rest(noteLength) =>
              Seq.empty -> pulses(
                noteLength
                  .getOrElse(noteAttributes.duration),
                pulsesPerQuarterNote * 4
              )

            case Barline =>
              Seq.empty -> 0L
          }
          (events ++ newEvents) -> (offset + offsetIncrement)
      }
    events.foreach(track.add)
    sequence
  }

  private[this] def midiEvents(
      offset: Long,
      notes: Seq[Note],
      channel: Int,
      pulsesPerQuarterNote: Int,
      noteAttributes: NoteAttributes
  ): Seq[MidiEvent] =
    (for {
      note <- notes
      toneNumber = (noteAttributes.octave.value + 1) * 12 + note.pitch.chroma
      noteOn = new MidiEvent(
        new ShortMessage(
          ShortMessage.NOTE_ON,
          channel,
          toneNumber,
          noteAttributes.volume
        ),
        offset
      )
      noteOff = new MidiEvent(
        new ShortMessage(
          ShortMessage.NOTE_OFF,
          channel,
          toneNumber,
          noteAttributes.volume
        ),
        offset + pulses(
          note.duration
            .getOrElse(noteAttributes.duration),
          pulsesPerQuarterNote * 4
        )
      )
    } yield Seq(noteOn, noteOff)).flatten

  private[this] def pulses(
      noteDuration: Note.Duration,
      pulsesPerNote: Int
  ): Long =
    (noteDuration.value * pulsesPerNote).toLong

}
