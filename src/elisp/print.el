(symbol-name (read "a\\b\\c\\,\\ \\[\\/\\]\\'\\`"))"abc, [/]'`"
(princ '#:,)
(progn
  
  nil)

(let ((obj " !\"#$%&'()*+,-./09:;<=>?@AZ[\\]^_`az{|}~"))
  (princ obj)
  (princ "\n")
  (prin1 obj)
  "")
(read "'")


