;; -*- coding: utf-8 -*-
;;; package --- Summary
;;; Commentary: .emacs config file

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

;; packages
(require 'use-package)

(use-package atom-one-dark-theme
  :ensure t
  )

(use-package web-mode
  :ensure t
  :init
  (setq web-mode-enable-css-colorization t)
  (setq web-mode-enable-auto-pairing t)
  (setq web-mode-enable-current-element-highlight t)

  :mode
  "\\.phtml\\'"
  "\\.php\\'"
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


(use-package yasnippet
  :ensure t
  :init
  ;; Remove Yasnippet's default tab key binding
  ;; (define-key yas-minor-mode-map (kbd "<tab>") nil)
  ;; (define-key yas-minor-mode-map (kbd "TAB") nil)
  ;; Set Yasnippet's key binding to shift+tab
  ;; (define-key yas-minor-mode-map (kbd "C-;") 'yas-expand)

  (yas-global-mode 1)
  )



;; imaxima
(setq imaxima-fnt-size "Large")

;; flycheck in linux machine
					;TODO: add flycheck dependencies for all languages
(if (system-is-linux)
    (use-package flycheck
      :ensure t
      :init
      (add-hook 'prog-mode-hook #'flycheck-mode)
      )
  )

;; ido fix ;TODO:
(setq ido-enable-flex-matching t)
(setq ido-everywhere t)
(ido-mode 1)



;; Python mode ;TODO: check if working
;; Try :interpreter option from use-package

;; (defun python-shell-parse-command ()
;;   "Return the string used to execute the inferior Python process."
;;   "/usr/local/bin/python3 -i"
;;   )



;; TRAMP mode
(setq tramp-default-method "ssh")

;; (setq command-line-default-directory "/linode:Documents/")
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
