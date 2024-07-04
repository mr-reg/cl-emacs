(symbol-name (read "a\\b\\c\\,\\ \\[\\/\\]\\'\\`"))"abc, [/]'`"
(princ '#:,)
(progn

  nil)

(let ((obj (read "`(,a\\,b (,@ nil))")))
  (princ obj)
  (princ "\n")
  (princ "\n")
  (prin1 obj)
  "")
(read "'")
