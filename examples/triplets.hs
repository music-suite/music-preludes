
import Music.Prelude.Piano

main = do
    writeMidi "triplets.mid" score
    writeXml "triplets.xml" $ score^/4
    playMidiIO "Graphic MIDI" $ score^/10

score :: Score Note
score = legato $ accent $ repTimes 2 $ melody [c,d,e] ^*(2/3 * 0.5)
