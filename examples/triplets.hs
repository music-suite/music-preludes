
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

subject = legato $ accent $ scat [c,cs^*2,d,db]

score :: Score Note
score = 
    (times 5 $ up _P5   $ subject ^*(2/3 * 1/4)) 
        </>
    (times 5 $ up _M3   $ subject ^*(1/4)) 
        </>
    (times 5 $ up _P1   $ subject ^*(2/3 * 1/2)) 
        </>
    (times 5 $ down _P4 $ subject ^*(1/2)) 


-- TODO

instance HasPitch Pitch where { type Pitch Pitch = Pitch ; getPitch = id; modifyPitch = id }

instance Tiable Pitch where { beginTie = id ; endTie = id }
instance HasLilypond Pitch where
    getLilypond d p = Lilypond.note (Lilypond.NotePitch (Lilypond.Pitch (pc,acc,oct+5)) Nothing) ^*(fromDurationT $ d*4)
        where
            pc  = toEnum $ fromEnum $ name p
            acc = fromIntegral $ accidental p
            oct = fromIntegral $ octaves (p .-. c)
            