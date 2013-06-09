
import System.Process (runCommand)
import Music.Prelude.Basic

main = do
    -- writeMidi "test.mid" score
    -- writeXml "test.xml" $ score^/4
    -- openXml $ score
    writeLy "test.ly" $ score
    runCommand "lilypond test.ly"
    -- playMidiIO "Graphic MIDI" $ score^/10

score :: Score Note
score = let
        up x = fmap (modifyPitch (+ x))
        down x = fmap (modifyPitch (subtract x))
        octave = 12

        a `x` b = a^*(3/4) |> b^*(1/4)
        a `l` b = (a |> b)^/2
    
        motive = (legato $ stretchTo 2 $ melody [g,a,bb,c',d',eb']) |> staccato (d' |> bb |> g)
        bar    = rest^*4

        song    = mempty
        left    = times 4 (times 4 $ removeRests $ triplet g)
        right   = removeRests $ times 2 (delay 4 motive |> rest^*3)

    in  stretch (1/4) $ song </> left </> down octave right
