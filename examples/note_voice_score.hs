
>>> [(1,c)^.note]^.voice

>>> [(1<->2,c)^.event]^.score

>>> [(1,c)^.note]^.voice <> [(2,d)^.note]^.voice
[(1,0)^.note,(2,2)^.note]^.voice

>>> :t \xs ys -> (^.from unsafePairs) $ zip xs ys
[Duration] -> [b] -> Voice b

>>> :t \xs ys -> (^.voice) $ zipWith (curry $ (^.note)) xs ys
[Duration] -> [b] -> Voice b


>>> renderAlignedVoice $ aligned 0 0.5 $ [(1,x)^.note | x <- [0..21]]^.voice

[(-11 <-> -10,0)^.event,(-10 <-> -9,1)^.event,(-9 <-> -8,2)^.event,(-8 <-> -7,3)^.event,(-7 <-> -6,4)^.event,(-6 <-> -5,5)^.event,(-5 <-> -4,6)^.event,(-4 <-> -3,7)^.event,(-3 <-> -2,8)^.event,(-2 <-> -1,9)^.event,(-1 <-> 0,10)^.event,(0 <-> 1,11)^.event,(1 <-> 2,12)^.event,(2 <-> 3,13)^.event,(3 <-> 4,14)^.event,(4 <-> 5,15)^.event,(5 <-> 6,16)^.event,(6 <-> 7,17)^.event,(7 <-> 8,18)^.event,(8 <-> 9,19)^.event,(9 <-> 10,20)^.event,(10 <-> 11,21)^.event]^.score

  :: (Num t, Enum t) => Score t
