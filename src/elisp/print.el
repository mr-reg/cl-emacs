(symbol-name (read "a\\b\\c\\,\\ \\[\\/\\]\\'\\`"))"abc, [/]'`"
(princ '#:,)
(progn
  
  nil)
(let ((obj (quote (quote 'test))))
  (princ obj)
  (princ "\n")
  (prin1 obj)
  "")
(read "'")


