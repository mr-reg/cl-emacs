(read "?\\
")

?\805
(read "?\\s.")
(aref (read (concat "\"\\" "x002200000\"")) 0)
?\u00210
(symbol-name (read "\\805"))
(read "?a^a")
(memq char '(#\( #\) #\[ #\] #\" #\' #\` #\, #\#))
()[]"'
(aref (read (concat "\"\\\"")) 0)
(aref (read (concat "\"\\M-\\C-b\"")) 0)
(aref (read (concat "\"\\M-\\C-\\ \"")) 0)
(aref (read (concat "\"\\C-b\"")) 0)
(aref (read (concat "\"\\C-\\M-\\ \"")) 0)



'(a '?\$-a b)
(read "?\\$-e")
(progn
  (princ "abc\\\x7a\"")
  nil)
(aref "\t, \C-a" 3)

(let ((l '(
           "\\073"
           "\\02"
           "\\211"
           "\\377"
           "\\444"
           "\\666"
           "\\700"
           "\\850"
           "\\508"
           )))
  (dolist (str l)
    (let ((str2 (read (concat "\"" str "\""))))
      (message "(is (= %s (read-string-character %S)))" (aref str2 0) str))))
?\ (read "?\\\x0a") -1
(read "?\\\x0d") 13

"a\ b"
( "^A")

(symbol-name (read "a?\\\\\\ b"))
'a\ \tb
(type-of (unibyte-char-to-multibyte 100))


(ignore-errors (aref (read (concat "\"\\S-@\"")) 0))nil
(ignore-errors (aref (read (concat "\"\\S-A\"")) 0))65
(ignore-errors (aref (read (concat "\"\\S-Z\"")) 0))90
(ignore-errors (aref (read (concat "\"\\S-a\"")) 0))65
(ignore-errors (aref (read (concat "\"\\S-z\"")) 0))90
(ignore-errors (aref (read (concat "\"\\S-\\A\"")) 0))nil
(ignore-errors (aref (read (concat "\"\\S-\\B\"")) 0))66
(ignore-errors (aref (read (concat "\"\\S-\\C\"")) 0))nil
(ignore-errors (aref (read (concat "\"\\S-\\D\"")) 0))68
(ignore-errors (aref (read (concat "\"\\S-\\E\"")) 0))69
(ignore-errors (aref (read (concat "\"\\S-\\F\"")) 0))70
(ignore-errors (aref (read (concat "\"\\S-\\G\"")) 0))71
(ignore-errors (aref (read (concat "\"\\S-\\H\"")) 0))nil
(ignore-errors (aref (read (concat "\"\\S-\\I\"")) 0))73
(ignore-errors (aref (read (concat "\"\\S-\\J\"")) 0))74
(ignore-errors (aref (read (concat "\"\\S-\\K\"")) 0))75
(ignore-errors (aref (read (concat "\"\\S-\\L\"")) 0))76
(ignore-errors (aref (read (concat "\"\\S-\\M\"")) 0))nil
(ignore-errors (aref (read (concat "\"\\S-\\N\"")) 0))nil
(ignore-errors (aref (read (concat "\"\\S-\\O\"")) 0))79
(ignore-errors (aref (read (concat "\"\\S-\\P\"")) 0))80
(ignore-errors (aref (read (concat "\"\\S-\\Q\"")) 0))81
(ignore-errors (aref (read (concat "\"\\S-\\R\"")) 0))82
(ignore-errors (aref (read (concat "\"\\S-\\S\"")) 0))nil
(ignore-errors (aref (read (concat "\"\\S-\\T\"")) 0))84
(ignore-errors (aref (read (concat "\"\\S-\\U\"")) 0))nil
(ignore-errors (aref (read (concat "\"\\S-\\V\"")) 0))86
(ignore-errors (aref (read (concat "\"\\S-\\W\"")) 0))87
(ignore-errors (aref (read (concat "\"\\S-\\X\"")) 0))88
(ignore-errors (aref (read (concat "\"\\S-\\Y\"")) 0))89
(ignore-errors (aref (read (concat "\"\\S-\\Z\"")) 0))90
(ignore-errors (aref (read (concat "\"\\S-\\a\"")) 0))nil
(ignore-errors (aref (read (concat "\"\\S-\\b\"")) 0))nil
(ignore-errors (aref (read (concat "\"\\S-\\c\"")) 0))67
(ignore-errors (aref (read (concat "\"\\S-\\d\"")) 0))nil
(ignore-errors (aref (read (concat "\"\\S-\\e\"")) 0))nil
(ignore-errors (aref (read (concat "\"\\S-\\f\"")) 0))nil
(ignore-errors (aref (read (concat "\"\\S-\\g\"")) 0))71
(ignore-errors (aref (read (concat "\"\\S-\\h\"")) 0))72
(ignore-errors (aref (read (concat "\"\\S-\\i\"")) 0))73
(ignore-errors (aref (read (concat "\"\\S-\\j\"")) 0))74
(ignore-errors (aref (read (concat "\"\\S-\\k\"")) 0))75
(ignore-errors (aref (read (concat "\"\\S-\\l\"")) 0))76
(ignore-errors (aref (read (concat "\"\\S-\\m\"")) 0))77
(ignore-errors (aref (read (concat "\"\\S-\\n\"")) 0))nil
(ignore-errors (aref (read (concat "\"\\S-\\o\"")) 0))79
(ignore-errors (aref (read (concat "\"\\S-\\p\"")) 0))80
(ignore-errors (aref (read (concat "\"\\S-\\q\"")) 0))81
(ignore-errors (aref (read (concat "\"\\S-\\r\"")) 0))nil
(ignore-errors (aref (read (concat "\"\\S-\\s\"")) 0))nil
(ignore-errors (aref (read (concat "\"\\S-\\t\"")) 0))nil
(ignore-errors (aref (read (concat "\"\\S-\\u\"")) 0))nil
(ignore-errors (aref (read (concat "\"\\S-\\v\"")) 0))nil
(ignore-errors (aref (read (concat "\"\\S-\\w\"")) 0))87
(ignore-errors (aref (read (concat "\"\\S-\\x\"")) 0))nil
(ignore-errors (aref (read (concat "\"\\S-\\y\"")) 0))89
(ignore-errors (aref (read (concat "\"\\S-\\z\"")) 0))90

(read "(?\\s. 3)")
'(?\s. 3)
