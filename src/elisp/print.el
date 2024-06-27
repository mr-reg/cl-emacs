(symbol-name (read "a\\b\\c\\,\\ \\[\\/\\]\\'\\`"))"abc, [/]'`"
(princ '#:,)
(progn
  
  nil)
(let ((obj (make-symbol (with-output-to-string
                          (cl-loop for code from 1 to 127
                                   do (write-char code))))))
  (princ obj)
  (princ "\n")
  (prin1 obj)
  "")
(read "'")


