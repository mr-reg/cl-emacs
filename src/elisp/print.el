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
(read "(12)")
(cl-loop for code from 0 to 300
         for str = " "
         do (setq str  (concat str (format "%c" code)))
         do (princ (format "%d:%s/%s " code str (multibyte-string-p str))))
(let ((s1 "\x3000")
      (s2 "\353")
      (s3 "\M-k")
      (s4 "±"))
  (setf (aref s1 0) ?\353)
  ;; (setf (aref s3 0) ?\353)
  (format "s1(%s):%s s2(%s):%s s3(%s):%s s4(%s):%s"
          (multibyte-string-p s1) s1
          (multibyte-string-p s2) s2
          (multibyte-string-p s3) s3
          (multibyte-string-p s4) s4
          ))
(char)
(length "\353")
(princ )
(princ "±")
(aref "±" 0)
