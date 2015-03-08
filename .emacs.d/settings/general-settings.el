;; Usa y/n en lugar de yes/no. Es más rápido.
(fset 'yes-or-no-p 'y-or-n-p)

;; Scroll one line at a time
(setq mouse-wheel-scroll-amount '(1 ((shift) . 1))) ;; 1 at a time
;(setq mouse-wheel-progressive-speed nil) ;; don't accelerate

;; No quiero barra de scroll
(scroll-bar-mode -1)

;; No quiero tool bar, sólo menu bar por el momento
(tool-bar-mode 0)

;; show the current line and column numbers in the stats bar as well
(line-number-mode 1)
(column-number-mode 1)

;; Actúa como un editor de texto normal y borrame texto cuando subraye
;; e introduzca nuevo texto.
(delete-selection-mode 1)

;; hace que el python-shell se ponga en UTF-8 en vez de US-ASCII, lo
;; que da problemas al intentar printear acentos etc.
(setenv "LC_CTYPE" "UTF-8")

;; Para que los backup de Emacs vayan a un solo directorio y no
;; estén desperdigados.
(setq backup-directory-alist '(("." . "~/.backupsEmacs")))

; set command key to be meta instead of option
(if (system-is-mac)
    (setq ns-command-modifier 'meta))


; font (as seen in describe-font)
(set-default-font
 "-*-Monaco-normal-normal-normal-*-13-*-*-*-m-0-iso10646-1")

(provide 'general-settings)

