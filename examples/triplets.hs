
import System.Process (runCommand)
import Music.Prelude.StringQuartet

main = do
    -- writeMidi "test.mid" score
    -- writeXml "test.xml" $ score^/4
    -- writeLy "test.ly" $ score
    openXml $ score
    -- runCommand "lilypond test.ly"
    -- playMidiIO "Graphic MIDI" $ score^/10

subject = legato $ accent $ scat [c',cs'^*2]

score :: Score Note
score =  
    (repTimes 5 $ subject ^*(2/3 * 1/4)) 
        </>
    (repTimes 5 $ subject ^*(1/4)) 
        </>
    (repTimes 5 $ subject ^*(2/3 * 1/2)) 
        </>
    (repTimes 5 $ subject ^*(1/2)) 
