(let ((first-note-duration  5/3) 
      (second-note-duration 2))

  ( </>
    (move 3 (times 4 (|> (|> (stretch first-note-duration c) (stretch second-note-duration d)) eb)))
    (move 3 (times 4 (|> (|> (stretch first-note-duration c) (stretch second-note-duration d)) eb)))
    )
    )