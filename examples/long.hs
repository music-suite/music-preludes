
import System.Process (runCommand)
import Music.Prelude.Basic

-- | Bela Bartok – “Wandering” from Mikrokosmos, volume III
--   This example was adaptd from Abjad

main = do
    -- writeMidi "test.mid" score
    -- writeXml "test.xml" $ score^/4
    -- openXml score
    openLy score
    -- playMidiIO "Graphic MIDI" $ score^/10


n = 500

score :: Score Note
score = dynamics ff $ times n (legato m </> ( times 3 . staccato . modifyPitches (subtract  5)) (m^/3)) ^/4

m = asScore (scat [c,d,e]) 
