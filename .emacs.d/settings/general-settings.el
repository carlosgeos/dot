(setq inhibit-startup-message t)

;; Usa y/n en lugar de yes/no. Es más rápido.
(fset 'yes-or-no-p 'y-or-n-p)

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

;; Electric brackets pair mode
(add-hook 'prog-mode-hook #'electric-pair-mode)

;; hace que el python-shell se ponga en UTF-8 en vez de US-ASCII, lo
;; que da problemas al intentar printear acentos etc.
(setenv "LC_CTYPE" "UTF-8")

;; ignore dups in shell
(setq comint-input-ignoredups t)

;; Backup files (~file) in one directory. These files are created
;; after saving a new version of a file. Made redundant by VCS
(setq backup-directory-alist '(("." . "~/.backupsEmacs")))

;; Autosave files also in a special folder. These files are created
;; continuously for modified buffers and are deleted when the buffer
;; is saved to a file.
(defvar autosave-dir (expand-file-name "~/.autosavesEmacs/"))
(make-directory autosave-dir t)
(setq auto-save-list-file-prefix autosave-dir)
(setq auto-save-file-name-transforms `((".*" ,autosave-dir t)))

; set command key to be meta instead of option
(if (system-is-mac)
    (setq ns-command-modifier 'meta))

; font (as seen in describe-font)
(if (system-is-mac)
	(set-default-font
	 "-*-Monaco-normal-normal-normal-*-13-*-*-*-m-0-iso10646-1"))

(provide 'general-settings)
;;; general-settings.el ends here
