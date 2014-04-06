let
    x = [ (0, c), (1, d), (2, e) ]^.track
    y = join $ [ (0, x), 
                (1.5,  up _P5 x), 
                (3.25, up _P8 x) ]^.track

in trackToScore (1/8) y
