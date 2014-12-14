
import Music.Prelude
import qualified Music.Score

subj :: Voice Pitch
subj = mconcat [c',ab,db',e,f,g,ab,bb,c']

type Chorale = [Voice (Maybe Pitch)]

renderVoice :: Voice (Maybe Pitch) -> Score StandardNote
renderVoice = fmap pitchToNote . removeRests . renderAlignedVoice . aligned 0 0
  where
    pitchToNote = fromPitch' . pure

renderChorale :: Chorale -> Score StandardNote
renderChorale = rcat . fmap renderVoice
