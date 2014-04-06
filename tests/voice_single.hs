let
    x = [ (1, c),
          (1, d),
          (1, f),
          (1, e) ]^.voice

    y = join $ [ (1, x), 
                 (0.5, up _P5 x), 
                 (4, up _P8 x) ]^.voice

in stretch (1/8) $ voiceToScore $ y
