;;; .emacs --- emacs configuration file
;; -*- coding: utf-8 -*-
;;; Commentary:

;; -- these are the contents:
;; Themes
;; Interface Enhancement
;; Navigation
;; Project Management
;; Error Checking
;; Programming
;; Major modes
;; Editing
;; Misc

;;; Code:

;;---------------------------------------------------;
;; Start package.el (basic) and install use-package  ;
;;---------------------------------------------------;

(require 'package)
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

(use-package smart-mode-line
  :ensure t
  :config
  (add-hook 'after-init-hook 'sml/setup t)
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

(use-package helm-projectile
  :ensure t
  :config
  (helm-projectile-on)
  )

(use-package helm-gtags
  :ensure t
  :bind (("M-." . helm-gtags-dwim))
  :init
  (setq helm-gtags-auto-update t)
  :config
  (add-hook 'c-mode-hook 'helm-gtags-mode)
  (add-hook 'c++-mode-hook 'helm-gtags-mode)
  (add-hook 'asm-mode-hook 'helm-gtags-mode)
  )

(use-package magit
  :ensure t
  :bind ("C-x g" . magit-status)
  )

;; ---------- Error Checking

(use-package flycheck
  :ensure t
  :init
  (add-hook 'prog-mode-hook #'flycheck-mode)
  ;(setq-default flycheck-disabled-checkers '(c/c++-clang))
  (setq-default flycheck-gcc-warnings '("pedantic" "all" "extra" "conversion" "effc++" "strict-null-sentinel" "old-style-cast" "noexcept" "ctor-dtor-privacy" "overloaded-virtual" "sign-promo" "zero-as-null-pointer-constant" "suggest-final-types" "suggest-final-methods" "suggest-override"))
  (add-hook 'c++-mode-hook
	    (lambda ()
	      (setq flycheck-gcc-language-standard "c++14")
	      (setq flycheck-c/c++-gcc-executable "g++-6")
	      (flycheck-select-checker 'c/c++-gcc)
	      )
	    ) ;it does not interfere with c mode.
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
  ;; Creates conflict with avy mode for jumping to char but should be
  ;; ok.
  (bind-key "C-;" 'yas-expand yas-minor-mode-map)
  )

(use-package auto-complete
  :ensure t
  :init
  (ac-config-default)
  )

;; ---------- Major Modes

(when (memq window-system '(mac ns))
  (add-to-list 'load-path "/usr/local/Cellar/maxima/5.37.2/share/maxima/5.37.2/emacs"))
(use-package imaxima
  :config
  (setq imaxima-fnt-size "Large")
  )

(use-package php-mode
  :ensure t)

(use-package slim-mode
  :ensure t)

(use-package coffee-mode
  :ensure t)

(use-package sass-mode
  :ensure t
  :config
  (add-hook 'sass-mode-hook (lambda ()
			      (yas-activate-extra-mode 'css-mode)))
  )

(use-package scss-mode
  :ensure t
  :config
  (add-hook 'scss-mode-hook (lambda ()
			      (yas-activate-extra-mode 'css-mode)))
  )


(use-package yaml-mode
  :ensure t)

(use-package web-mode
  :ensure t
  :init
  (setq web-mode-enable-css-colorization t)
  (setq web-mode-enable-auto-pairing t)

  :mode
  "\\.phtml\\'"
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

(use-package ess-site
  :ensure ess
  :disabled t)

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

(use-package exec-path-from-shell
  :ensure t
  :init
  (when (memq window-system '(mac ns))
    (exec-path-from-shell-initialize)))


;; imaxima


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
;;; Hooks  ;;;
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
