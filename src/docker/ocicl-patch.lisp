(defun find-asd-files (dir)
  "Recursively find all files with the .asd extension in a directory."
  (let ((systems (list)))
    (uiop:collect-sub*directories dir
                                  t nil
                                  (lambda (d)
                                    (dolist (f (uiop:directory-files d))
                                      (when (equal "asd" (pathname-type f))
                                        (push f systems)))))
    systems))
