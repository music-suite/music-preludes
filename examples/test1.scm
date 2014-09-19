
; Usage:
;   runhaskell examples/scheme.hs examples/test1.scm
(let ((first-note-duration  5/3) 
      (second-note-duration 2)
      (foo (lambda (x) x)))

  ;(rcat
  ;  (move 3 (times 4 (scat (stretch first-note-duration c) (stretch second-note-duration d) eb)))
  ;  (move 3 (times 4 (scat (stretch first-note-duration c) (stretch second-note-duration d) eb))))
  
  (stretch 4
  (scat
    (compress 8 (scat c d e))
    (compress 16 (scat c d e))
    (compress 8 (scat c d e f))
    (up (foo m3)
      (scat
        (compress 16 (scat c d e))
        (compress 8 (scat c d e))
        (compress 16 (scat c d e (pcat f bb))))))))