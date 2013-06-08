
import System.Process (runCommand)
import Music.Prelude.StringQuartet

main = do
    writeMidi "test.mid" score
    writeXml "test.xml" $ score^/4
    writeLy "test.ly" $ score^/4
    runCommand "lilypond -f png test.ly"
    -- runCommand "open -a /Applications/Sibelius\\ 6.app test.xml"


-- infixr 7 //
-- (//) = flip repTimes

score = test 1 </> test 2

test 1  = group 5 c |> c^*3                  
test 2  = group 3 c |> c^*3                  
test 3  = group 3 c |> group 5 c |> group 3 c |> group 7 c
test 4  = group 3 c |> group 5 c |> c |> group 7 c
test 5  = c |> group 5 c |> c |> group 7 c
-- all above ok


test 8 = repTimes 5 c^/5 |> repTimes 3 c^/3 -- ok
test 9 = repTimes 4 c^/5 |> c^*(1/5+1/3)    -- not ok, needs to be bound from last quintuplet note

test 99 = group 5 c |> group 3 c |> c^*2

