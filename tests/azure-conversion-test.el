;;; azure-conversion-test.el --- Azure content conversion tests -*- lexical-binding: t; -*-

(require 'ert)
(require 'cl-lib)
(require 'azure)

(ert-deftest azure-pandoc-conversion-does-not-use-a-shell ()
  "HTML, including shell metacharacters, is sent literally over stdin."
  (let ((input "<div><span style=\"font-family:&quot;Cascadia Code&quot;\">Done: Thing IRI + form href</span></div>")
        captured-text
        captured-program
        captured-args)
    (cl-letf (((symbol-function 'executable-find)
               (lambda (_program) "C:/Program Files/Pandoc/pandoc.exe"))
              ((symbol-function 'call-process-region)
               (lambda (start end program _delete destination _display &rest args)
                 (setq captured-text (buffer-substring-no-properties start end)
                       captured-program program
                       captured-args args)
                 (with-current-buffer (car destination)
                   (insert "=Done: Thing IRI + form href=\n"))
                 0)))
      (should (equal (azure--html-to-org input)
                     "=Done: Thing IRI + form href="))
      (should (equal captured-text input))
      (should (equal captured-program "C:/Program Files/Pandoc/pandoc.exe"))
      (should (equal captured-args
                     '("--from" "html" "--to" "org" "--wrap=none"))))))

(ert-deftest azure-org-conversion-cleans-pandoc-artifacts ()
  "Converted Org uses Unix newlines and no forced line-break markers."
  (should
   (equal
    (azure--clean-org-conversion
     (concat "Done: form\u00a0=href=; committed\r\n"
             "as\u00a0=file.rq=.\\\\\r\n"
             "Control:\x01 removed\tbut tab retained."))
    (concat "Done: form =href=; committed\n"
            "as =file.rq=.\n"
            "Control: removed\tbut tab retained."))))

(ert-deftest azure-pandoc-conversion-reports-stderr ()
  (cl-letf (((symbol-function 'executable-find)
             (lambda (_program) "pandoc"))
            ((symbol-function 'call-process-region)
             (lambda (_start _end _program _delete destination _display &rest _args)
               (with-temp-file (cadr destination)
                 (insert "bad input"))
               2)))
    (should-error (azure--html-to-org "<broken>")
                  :type 'error)))

(provide 'azure-conversion-test)
;;; azure-conversion-test.el ends here
