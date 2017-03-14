;;; general-settings.el --- some general stuff for Emacs
;;; Commentary:


;;; Code:

;; no startup message
(setq inhibit-startup-message t)

;; y/n instead of yes/no
(fset 'yes-or-no-p 'y-or-n-p)

;; no scroll bar
(scroll-bar-mode -1)

;; use arrows to move around split windows.
(windmove-default-keybindings)

;; no tool bar
(tool-bar-mode -1)

;; yes menu bar
(menu-bar-mode 1)

;; show the current line and column numbers in the stats bar as well
(line-number-mode t)
(column-number-mode t)

;; do not allow horizontal split (on top of each other). Split side by side.
(setq split-height-threshold nil)
(setq split-width-threshold 0)

;; no dups in search history or minibuffer
(setq history-delete-duplicates t)

;; "normal" text editor behaviour
(delete-selection-mode t)

;; human readable Dired buffer
(setq dired-listing-switches "-alh")

;; electric brackets pair mode
(add-hook 'prog-mode-hook #'electric-pair-mode)

;; different indentation for c code
(setq-default c-default-style "stroustrup")

;; indent by default when newline
(global-set-key (kbd "RET") 'newline-and-indent)

;; move one paragraph forwards/backwards
(global-set-key (kbd "M-p") 'backward-paragraph)
(global-set-key (kbd "M-n") 'forward-paragraph)


;; solves some problems with python shell and accents
;; (setenv "LC_CTYPE" "UTF-8")

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

(provide 'general-settings)
;;; general-settings.el ends here
