(symbol-name (read "a\\b\\c\\,\\ \\[\\/\\]\\'\\`"))"abc, [/]'`"
(princ '#:,)
(progn

  nil)

(let ((obj #("abc" 0 2 ("ab\"c" ab\ c))))
  (princ obj)
  (princ "\n")
  (princ "\n")
  (prin1 obj)
  "")
(read "'")
