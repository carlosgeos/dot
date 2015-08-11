;; -*- coding: utf-8 -*-
;;; package --- Summary
;;; Commentary: .emacs config file

;; -- Contents
;; Themes
;; Interface Enhancement
;; Navigation
;; Project Management
;; Error Checking
;; Programming
;; Editing
;; Misc


;;---------------------------------------------------;
;; Start package.el (basic) and install use-package-;
;;---------------------------------------------------;
(require 'package)
;;; Code:
(defvar package-list)
(setq package-list '(use-package))

;; The good repo
(add-to-list 'package-archives
             '("melpa" . "http://melpa.org/packages/")
	     '("marmalade" . "http://marmalade-repo.org/packages/"))

;; Package.el needs to be initialized!
(package-initialize)

;; Only refresh package list if package is not present.
;; Saves time if machine is configured correctly
(or (file-exists-p package-user-dir)
    (package-refresh-contents))

;; Install use-package before anything else
(dolist (package package-list)
  (unless (package-installed-p package)
    (package-install package)))

;; path where settings files are kept
(add-to-list 'load-path "~/.emacs.d/settings/")

;; define various custom functions (system is mac and system is linux)
(require 'custom-functions)
;; configure general settings
(require 'general-settings)
(require 'use-package)

;; ---------- Themes

(use-package monokai-theme
  :ensure t
  )

;; ---------- Interface enhancement

(use-package helm
  :ensure t
  :init
  (helm-mode t)
  :bind (("M-x" . helm-M-x)
	 ("C-x C-f" . helm-find-files)
	 ;; From helm maintainer config file, remap instead of simple
	 ;; binding
	 ("<remap> <list-buffers>" . helm-buffers-list))
  :config
  )

(use-package helm-projectile)

;; ---------- Navigation

(use-package expand-region
  :ensure t
  :bind ("C-<return>" . er/expand-region)
  )

;; ---------- Project management

(use-package projectile
  :ensure t
  :init
  ;; config: enable cache and other settings?
  :config
  (projectile-global-mode)
  )

;; ---------- Error Checking

;; flycheck in linux machine
;; TODO: add flycheck dependencies for all languages
;; Javascript --> Install JSHint with NPM node shit, rest is automatic
;; Python -->
(if (system-is-linux)
    (use-package flycheck
      :ensure t
      :init
      (add-hook 'prog-mode-hook #'flycheck-mode)
      )
  )


;; ---------- Programming

(use-package yasnippet
  :ensure t
  ;; Remove Yasnippet's default tab key binding
  ;; (define-key yas-minor-mode-map (kbd "<tab>") nil)
  ;; (define-key yas-minor-mode-map (kbd "TAB") nil)
  ;; Set Yasnippet's key binding to shift+tab
  :init
  (yas-global-mode 1)
  :config
  ;; (bind-key "C-;" 'yas-expand yas-minor-mode-map)
  )

;; ---------- Major Modes

(use-package web-mode
  :ensure t
  :init
  (setq web-mode-enable-css-colorization t)
  (setq web-mode-enable-auto-pairing t)

  :mode
  "\\.phtml\\'"
  "\\.php\\'"
  "\\.html\\'"
  "\\.js\\'"
  "\\.tpl\\.php\\'"
  "\\.[agj]sp\\'"
  "\\.as[cp]x\\'"
  "\\.erb\\'"
  "\\.mustache\\'"
  "\\.djhtml\\'"

  :config
  (add-hook 'web-mode-hook (lambda ()
			     "Sets the config for Web mode"
			     (yas-activate-extra-mode 'html-mode)
			     (yas-activate-extra-mode 'js-mode)
			     (setq web-mode-markup-indent-offset 3)
			     (setq web-mode-code-indent-offset 2)
			     (setq web-mode-css-indent-offset 2)))

  )

(use-package tex-site
  :ensure auctex
  :init
  (add-hook 'LaTeX-mode-hook 'turn-on-auto-fill)
  (add-hook 'LaTeX-mode-hook (lambda () (abbrev-mode +1)))
  )

(use-package markdown-mode
  :ensure t
  :mode
  "\\.markdown\\'"
  "\\.md\\'"
  )


;; ---------- Editing

(use-package multiple-cursors
  :ensure t
  )

(use-package avy 			;(ace-jump-mode is dead)
  :ensure t
  :bind (("C-;" . avy-goto-word-1)
	 ("C-c SPC" . avy-goto-char))
  ;; There are a lot more binds to set for avy!
  )


;; ---------- Misc


;; imaxima
;; (setq imaxima-fnt-size "Large")


;; Python mode ;TODO: check if working
;; Try :interpreter option from use-package

;; (defun python-shell-parse-command ()
;;   "Return the string used to execute the inferior Python process."
;;   "/usr/local/bin/python3 -i"
;;   )



;; TRAMP mode
(setq tramp-default-method "ssh")

;; (setq command-line-default-directory "/linode:Documents/")

;; Important to make it work
(setq tramp-auto-save-directory "~/.emacs.d/tramp-autosave")
;; Make C-c o the general key for switching windows
(global-set-key (kbd "C-c o") 'other-window)


;;-----------;
;;; Hooks ;;;
;;-----------;

(require 'keybindings-hooks)


;;---------------------------------------------------------------------
;; Put auto 'custom' changes in a separate file (this is stuff like
;; custom-set-faces and custom-set-variables)
(load
 (setq custom-file (expand-file-name "settings/custom.el" user-emacs-directory))
 'noerror)


;;(provide .emacs)
;;; .emacs ends here
