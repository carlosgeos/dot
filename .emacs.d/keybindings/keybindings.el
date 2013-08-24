;; \dfrac function for LaTeX mode.

(defun dfrac-perso (x y)
  "Inserts \\dfrac interactively and automatically"
  (interactive "sNumerator: \nsDenominator: ")
  (insert "\\dfrac{}{}")
  (backward-char 3)
  (insert x)
  (forward-char 2)
  (insert y)
  (forward-char 1)
  (message "Numerator is: %s, Denominator is: %s" x y)
)

(add-hook 'LaTeX-mode-hook
	  (lambda () 
	    (local-set-key (kbd "C-c d") 'dfrac-perso)
	    )
	  )

;; \frac function for LaTeX mode.

(defun frac-perso (x y)
  "Inserts \\frac interactively and automatically"
  (interactive "sNumerator: \nsDenominator: ")
  (insert "\\frac{}{}")
  (backward-char 3)
  (insert x)
  (forward-char 2)
  (insert y)
  (forward-char 1)
  (message "Numerator is: %s, Denominator is: %s" x y)
)

(add-hook 'LaTeX-mode-hook
	  (lambda () 
	    (local-set-key (kbd "C-c f") 'frac-perso)
	    )
	  )


;; \displaymath automatic

(defun display-math-perso ()
  "Inserts $$ $$ automatically"
  (interactive)
  (insert "$$  $$")
  (backward-char 3)
  (message "Math mode created!")
)

(add-hook 'LaTeX-mode-hook
	  (lambda () 
	    (local-set-key (kbd "C-c m") 'display-math-perso)
	    )
	  )

;; \math inline automatic

(defun math-inline-perso ()
  "Inserts \\( \\) automatically"
  (interactive)
  (insert "\\(  \\)")
  (backward-char 3)
  (message "Math mode (inline) created!")
)

(add-hook 'LaTeX-mode-hook
	  (lambda () 
	    (local-set-key (kbd "C-c i") 'math-inline-perso)
	    )
	  )


;; HTML mode, ptag function + keybinding.

(defun ptag-perso ()
  "Inserts opening and closing <p></p>"
  (interactive)
  (insert "<p></p>")
  (backward-char 4)
  )

(add-hook 'html-mode
	  (lambda ()
	    (local-set-key (kbd "C-x p") 'ptag-perso)
	    )
	  )

;; For Arduino mode, some abbreviations...

(defun const-int-perso (x y)
  "Inserts constants interactively and automatically"
  (interactive "sIdentifier: \nsValue: ")
  (insert "const int  = ;")
  (backward-char 4)
  (insert x)
  (forward-char 3)
  (insert y)
  (forward-char 1)
  (message "Identifier is: %s, Its value is: %s" x y)
)

(add-hook 'arduino-mode-hook
	  (lambda () 
	    (local-set-key (kbd "C-c c") 'const-int-perso)
	    )
	  )

(defun int-perso (x y)
  "Inserts variables interactively and automatically"
  (interactive "sIdentifier: \nsValue: ")
  (insert "int  = ;")
  (backward-char 4)
  (insert x)
  (forward-char 3)
  (insert y)
  (forward-char 1)
  (message "Identifier is: %s, Its value is: %s" x y)
)

(add-hook 'arduino-mode-hook
	  (lambda () 
	    (local-set-key (kbd "C-c i") 'int-perso)
	    )
	  )

(defun setup-perso ()
  "Inserts setup automatically"
  (interactive)
  (insert "void setup () {")
  (newline-and-indent)
  (newline)
  (insert "}")
  (newline)
  (newline)
  (insert "void loop () {")
  (newline-and-indent)
  (newline)
  (insert "}")
  (backward-char 23)
  (message "Setup created! You can now start working faggot")
)

(add-hook 'arduino-mode-hook
	  (lambda () 
	    (local-set-key (kbd "C-c s") 'setup-perso)
	    )
	  )

(defun pinMode-perso (str bool)
  "Inserts pinMode interactively and automatically"
  (interactive
   (list (read-string "Pin: ")
	 (y-or-n-p "INPUT or OUTPUT?"))
   )
  (insert "pinMode(,);")
  (backward-char 3)
  (insert str)
  (forward-char 1)
  (if (equal bool t)
      (insert "INPUT")
    (insert "OUTPUT"))
  (forward-char 2)
  (message "pinMode created!")
)

(add-hook 'arduino-mode-hook
	  (lambda () 
	    (local-set-key (kbd "C-c p") 'pinMode-perso)
	    )
	  )
