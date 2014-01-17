
{-# LANGUAGE TypeFamilies #-}

import System.Process (runCommand)
import Music.Prelude.Basic
import Music.Pitch -- DEBUG
import qualified Music.Lilypond as Lilypond -- DEBUG
import qualified Music.Score as Score

main = do
    -- writeMidi "test.mid" score
    -- writeXml "test.xml" $ score^/4
    -- openXml score
    openLy score
    -- playMidiIO "Graphic MIDI" $ score^/10

subject = asScore $ legato $ accent $ scat [c,cs^*2,d,db]

score :: Score Note
-- score = 
--     (times 5 $ up' _P5   $ subject ^*(2/3 * 1/4)) 
--         </>
--     (times 5 $ up' _M3   $ subject ^*(1/4)) 
--         </>
--     (times 5 $ up' _P1   $ subject ^*(2/3 * 1/2)) 
--         </>
--     (times 5 $ down' _P4 $ subject ^*(1/2)) 
-- 

score = mempty
-- TODO
            