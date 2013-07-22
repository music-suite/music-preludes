
import System.Process (runCommand)
import Music.Prelude.StringQuartet

main = do
    -- writeMidi "test.mid" score
    -- writeXml "test.xml" $ score^/4
    -- openXml score
    openLy score
    -- playMidiIO "Graphic MIDI" $ score^/10

subject = legato $ accent $ scat [c',cs'^*2]

score :: Score Note
score =
    (times 5 $ subject ^*(2/3 * 1/4)) 
        </>
    (times 5 $ subject ^*(1/4)) 
        </>
    (times 5 $ subject ^*(2/3 * 1/2)) 
        </>
    (times 5 $ subject ^*(1/2)) 
