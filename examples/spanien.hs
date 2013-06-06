
import Music.Prelude.Piano hiding (Note, NotePart)
import System.Process

main = do
    writeMidi "test.mid" score
    writeXml "test.xml" $ score^/4
    runCommand "open -a /Applications/Sibelius\\ 6.app test.xml"

score = makeLongNotes 10 |> makeLongNotes 10 |> makeLongNotes 10

makeLongNotes :: Duration -> Score Note
makeLongNotes dur = 
        (c |> b_ |> c)^*(dur*1.5)
        </>
        (delay 1 $ (c |> b_ |> c)^*dur)





















asScore :: Score Note -> Score Note
asScore = id

data NotePart
    = Vln | Vla | Vc
    deriving (Eq, Ord, Enum)

instance Show NotePart where
    show Vln  = "Violin"
    show Vla  = "Viola"
    show Vc   = "Cello"

type Note = (PartT NotePart (TieT
    (TremoloT (HarmonicT (SlideT
        (DynamicT (ArticulationT (TextT Integer))))))))
                                                              