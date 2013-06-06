
import System.Process
import Music.Prelude.StringQuartet

main = do
    writeMidi "test.mid" score
    writeXml "test.xml" $ score^/4
    writeLy "test.ly" $ score
    runCommand "lilypond test.ly"
    -- playMidiIO "Graphic MIDI" $ score^/10

score :: Score Note
score = legato $ accent $ 
    (repTimes 35 $ melody [c,d,e] ^*(2/3 * 1/4)) 
        </>
    (repTimes 35 $ melody [c,d,e] ^*(4/5 * 1/4)) 
        </>
    (repTimes 35 $ melody [c,d,e] ^*(2/3 * 1/2)) 
        </>
    (repTimes 35 $ melody [c,d,e] ^*(4/5 * 1/2)) 
