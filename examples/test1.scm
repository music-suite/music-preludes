(let ((first-note-duration  11/5) 
      (second-note-duration 2/1))
  (|> 
    (|> (stretch first-note-duration c) (stretch second-note-duration d)) c)
  )