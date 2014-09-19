
; Usage:
;   runhaskell examples/scheme.hs examples/test2.scm
(let (
  (cell1 (compress 16 (scat c cs e)))
  (cell2 (compress 16 (scat fs f e)))
  ; TODO variadic
  (sequence (lambda (i s1 s2) (scat s1 (up i s2)))))

  (sequence m3 (scat cell1 cell2) (scat cell2 cell1)))