(require 'python)
(defun python-shell-parse-command ()
  "Return the string used to execute the inferior Python process."
  "/usr/local/bin/python3 -i"
  )

(provide 'python-settings)
