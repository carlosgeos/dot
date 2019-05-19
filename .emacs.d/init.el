;;; init.el --- emacs configuration file -*- lexical-binding: t -*-

;; Author: Carlos Requena López
;; Maintainer: Carlos Requena López
;; Version: latest
;; Package-Requires: ()
;; Homepage: https://github.com/carlosgeos/dot
;; Keywords: .emacs init.el


;; This file is not part of GNU Emacs

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; For a full copy of the GNU General Public License
;; see <http://www.gnu.org/licenses/>.


;;; Commentary:

;; commentary

;;; Code:

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Start package.el (basic) and install use-package ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'package)
(defvar package-list)
(setq package-list '(use-package))
(setq load-prefer-newer t)

;; The good repo
(add-to-list 'package-archives
             '("melpa" . "http://melpa.org/packages/")
             '("marmalade" . "http://marmalade-repo.org/packages/"))

(package-initialize)

;; Only refresh package list if package is not present.
;; Saves time if machine is configured correctly
(or (file-exists-p package-user-dir)
    (package-refresh-contents))

;; Install use-package before anything else
(dolist (package package-list)
  (unless (package-installed-p package)
    (package-refresh-contents)
    (package-install package)))

(require 'use-package)


;;;;;;;;;;;;;;;;;;;;;;
;; Helper functions ;;
;;;;;;;;;;;;;;;;;;;;;;

(defun system-is-mac ()
  "Return non-nil if system is mac."
  (string-equal system-type "darwin"))

(defun system-is-linux ()
  "Return non-nil if system is linux."
  (string-equal system-type "gnu/linux"))

(defun untabify-except-makefiles ()
  "Replace tabs with spaces except in makefiles."
  (unless (derived-mode-p 'makefile-mode)
    (untabify (point-min) (point-max))))

(add-hook 'before-save-hook 'untabify-except-makefiles)
(add-hook 'before-save-hook 'delete-trailing-whitespace)

(defun slick-copy (beg end)
  "Advice for 'kill-ring-save'.

It copies the current line if no region is selected.  BEG and END
are parameters of 'kill-ring-save'."
  (interactive
   (if mark-active
       (list (region-beginning) (region-end))
     (message "Copied line")
     (list (line-beginning-position) (line-beginning-position 2)))))

(advice-add 'kill-ring-save :before #'slick-copy)

(defun indent-buffer ()
  "Indent an entire buffer using the default intenting scheme."
  (interactive)
  (save-excursion
    (delete-trailing-whitespace)
    (indent-region (point-min) (point-max) nil)))


(global-set-key (kbd "C-x \\") 'indent-buffer)

;;;;;;;;;;;;;;;;;;;
;; General stuff ;;
;;;;;;;;;;;;;;;;;;;

;; no startup message
(setq inhibit-startup-message t)

;; y/n instead of yes/no
(fset 'yes-or-no-p 'y-or-n-p)

;; no scroll bar
(scroll-bar-mode -1)

;; Quick keybindings to resize windows
(global-set-key (kbd "S-<left>") 'shrink-window-horizontally)
(global-set-key (kbd "S-<right>") 'enlarge-window-horizontally)
(global-set-key (kbd "S-<down>") 'shrink-window)
(global-set-key (kbd "S-<up>") 'enlarge-window)

;; always open these buffers in said buffer. Buffer strings should
;; be regexes
(setq display-buffer-alist '(("\\*YASnippet Tables\\*" display-buffer-same-window)
                             ("\\*Help\\*" display-buffer-same-window)
                             ("\\*Process List\\*" display-buffer-same-window)
                             ("\\*Flycheck checkers\\*" display-buffer-same-window)
                             ("\\*Flycheck checker\\*" display-buffer-same-window)
                             ("\\magit-diff\\:" display-buffer-in-atom-window)
                             ;; The following is generated with the rx macro
                             ("magit:[[:space:]]\\(?:.\\|\\)*" display-buffer-same-window)
                             ("\\*Backtrace\\*" display-buffer-same-window)
                             ("\\*Metahelp\\*" display-buffer-same-window)
                             ("\\*Local variables\\*" display-buffer-same-window)
                             ;; Same window does not work for cider
                             ;; so, open in side-window (which is a
                             ;; bit better
                             ("\\*cider-error\\*" display-buffer-in-side-window)
                             ("\\*cider-doc\\*" display-buffer-in-side-window)))

;; Auto refresh buffers
(global-auto-revert-mode 1)

;; Highline cursorline mode
(global-hl-line-mode)

;; Move files to trash when deleting
(setq delete-by-moving-to-trash t)

;; Do not use shift to select stuff
(setq shift-select-mode nil)

;; no tool bar
(tool-bar-mode -1)

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

;; show matching paren
(show-paren-mode 1)

;; different indentation for c code
(setq-default c-default-style "stroustrup")

;; move one paragraph forwards/backwards
(global-set-key (kbd "M-p") 'backward-paragraph)
(global-set-key (kbd "M-n") 'forward-paragraph)

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

(add-to-list 'exec-path "~/.local/bin")

;;---------------------------------------------------------------------
;; Put auto 'custom' changes in a separate file (this is stuff like
;; custom-set-faces and custom-set-variables)
(load
 (setq custom-file (expand-file-name "custom.el" user-emacs-directory))
 'noerror)

;; TRAMP mode
(customize-set-variable 'tramp-default-method "ssh")

;; Important to make it work
(customize-set-variable 'tramp-auto-save-directory "~/.emacs.d/tramp-autosave")

;; Make C-c o another key for switching windows
(global-set-key (kbd "C-c o") 'other-window)

;; set command key to be meta instead of option
(when (system-is-mac)
  (customize-set-variable 'ns-command-modifier 'meta)
  ;; ar and ranlib should come from LLVM and not GNU to be able to
  ;; compile pdf-tools on mac (true as of Jan 2019)
  (setenv "AR" "/usr/bin/ar")
  (setenv "RANLIB" "/usr/bin/ranlib")
  (setenv "PKG_CONFIG_PATH" "/usr/local/lib/pkgconfig:/usr/local/Cellar/libffi/3.2.1/lib/pkgconfig"))

;;;;;;;;;;;;;;;;;;
;; Org settings ;;
;;;;;;;;;;;;;;;;;;

(require 'ob-clojure)
(setq org-babel-clojure-backend 'cider)

(org-babel-do-load-languages
 'org-babel-load-languages
 '((python . t)))

(use-package org-noter
  :ensure t
  :config
  ;; sizing adjustments depending on the screen used
  (when (system-is-linux)
    (setq org-noter-doc-split-fraction '(0.7 . 0.5))
    (plist-put org-format-latex-options :scale 2.0))
  (when (system-is-mac)
    (setq org-noter-doc-split-fraction '(0.55 . 0.5))
    (plist-put org-format-latex-options :scale 1.3))
  (add-hook 'org-noter-notes-mode-hook 'turn-on-auto-fill)
  (setq org-preview-latex-image-directory "~/.lxtimg/"))

(use-package edit-indirect
  :ensure t)

;;;;;;;;;;;;;;;;;;;
;; Editing modes ;;
;;;;;;;;;;;;;;;;;;;

;; Programming

(use-package clojure-mode
  :ensure t
  :hook (clojure-mode . subword-mode))

(use-package groovy-mode
  :ensure t)

(use-package web-mode
  :ensure t
  :init
  (setq web-mode-enable-css-colorization t)
  (setq web-mode-enable-auto-pairing t)

  :mode
  "\\.phtml\\'"
  "\\.jsx\\'"
  "\\.json\\'"
  "\\.vue\\'"
  "\\.html\\'"
  "\\.tpl\\.php\\'"
  "\\.[agj]sp\\'"
  "\\.as[cp]x\\'"
  "\\.erb\\'"
  "\\.mustache\\'"
  "\\.djhtml\\'"
  "\\.hbs\\'"

  :config
  (add-hook 'web-mode-hook (lambda ()
                             "Sets the config for Web mode"
                             (yas-activate-extra-mode 'html-mode)
                             (yas-activate-extra-mode 'js-mode)
                             (setq web-mode-markup-indent-offset 3)
                             (setq web-mode-code-indent-offset 2)
                             (setq web-mode-css-indent-offset 2))))

(use-package js2-mode
  :ensure t
  :mode "\\.js\\'"
  :config
  (setq js2-basic-offset 2))

(use-package typescript-mode
  :ensure t
  :init
  (add-hook 'typescript-mode-hook (lambda () (tide-setup) (tide-hl-identifier-mode +1))))

(use-package tide
  :ensure t)

(use-package dockerfile-mode
  :ensure t)

;; Markup, style, data and build tools

(use-package tex-site
  :ensure auctex
  :init
  (add-hook 'LaTeX-mode-hook 'turn-on-auto-fill)
  (add-hook 'LaTeX-mode-hook (lambda () (abbrev-mode +1))))

(use-package markdown-mode
  :ensure t
  :mode
  "\\.markdown\\'"
  "\\.md\\'"
  :config
  (setq markdown-command "pandoc"))

(use-package sphinx-doc
  :ensure t
  :hook (python-mode . sphinx-doc-mode))

(use-package slim-mode
  :ensure t)

(use-package handlebars-mode
  :ensure t)

(use-package coffee-mode
  :ensure t)

(use-package sass-mode
  :ensure t
  :config
  (add-hook 'sass-mode-hook (lambda ()
                              (yas-activate-extra-mode 'css-mode))))

(use-package scss-mode
  :ensure t
  :config
  (add-hook 'scss-mode-hook (lambda ()
                              (yas-activate-extra-mode 'css-mode))))

(use-package yaml-mode
  :ensure t)

(use-package cmake-mode
  :ensure t)

;;;;;;;;;;;;;;;;;;;;
;; Other packages ;;
;;;;;;;;;;;;;;;;;;;;

(use-package irony-eldoc
  :ensure t)

(use-package irony
  :ensure t
  :config
  (add-hook 'c++-mode-hook 'irony-mode)
  (add-hook 'c-mode-hook 'irony-mode)
  (add-hook 'irony-mode-hook #'irony-eldoc)
  (add-hook 'irony-mode-hook 'irony-cdb-autosetup-compile-options))

(use-package cider
  :ensure t
  :init
  (setq cider-repl-pop-to-buffer-on-connect nil)
  (setq cider-repl-use-pretty-printing t)
  (setq cider-repl-history-file "~/.emacs.d/nrepl-history"))

(use-package indium
  :ensure t
  :hook (js2-mode . indium-interaction-mode))

(use-package multiple-cursors
  ;; M-x calls to mc do not work well, keybindings are necessary
  ;; iedit does a similar thing. Maybe this one is better ?
  ;;
  ;; mc is always asking if a certain M-x action is to be done for all
  ;; cursors, annoying.
  :ensure t
  :bind (("C->" . mc/mark-next-like-this)
         ("C-<" . mc/mark-previous-like-this)
         ("C-c C-a" . mc/mark-all-like-this)))

(use-package avy
  :ensure t
  :bind (("C-;" . avy-goto-word-1)
         ("C-c SPC" . avy-goto-char)))   ;FIXME: this is also clojure-align...
;; There are a lot more binds to set for avy!

(use-package ace-window
  :ensure t
  :init
  (setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l))
  :bind ("M-o" . ace-window))

(use-package expand-region
  :ensure t
  :bind ("C-<return>" . er/expand-region))

(use-package paredit
  :ensure t
  :hook ((emacs-lisp-mode clojure-mode) . paredit-mode))

(use-package helm
  :ensure t
  :init
  (helm-mode t)
  :bind (("M-x" . helm-M-x)
         ("C-x C-f" . helm-find-files)
         ("C-x b" . helm-mini)
         ("M-y" . helm-show-kill-ring)
         ("C-h SPC" . helm-all-mark-rings)
         ("C-c h o" . helm-occur)
         ;; From helm maintainer config file, remap instead of simple
         ;; binding
         ("<remap> <list-buffers>" . helm-buffers-list))
  :config
  (customize-set-variable 'helm-mode-fuzzy-match t)
  ;; Show full name in helm-mini
  (customize-set-variable 'helm-buffer-max-length nil))

(use-package yasnippet
  :ensure t
  :init
  (yas-global-mode 1)
  ;; Remove Yasnippet's default tab key binding. Does not work well
  ;; with indenting and company mode.
  :bind (:map yas-minor-mode-map
              ("TAB" . nil)
              ("<tab>" . nil)
              ("C-c ;" . yas-expand)))

(use-package yasnippet-snippets
  ;; yasnippet no longer ships with the snippets, hence this is also
  ;; necessary
  :ensure t)

(use-package company-c-headers
  :ensure t)

(use-package company-irony
  :ensure t)

(use-package company
  :ensure t
  :init
  (global-company-mode)
  :config
  (setq company-idle-delay 0.1))

(add-to-list 'company-backends 'company-irony)
(add-to-list 'company-backends 'company-c-headers)

(use-package clj-refactor
  :diminish clj-refactor-mode
  :ensure t
  :config
  (cljr-add-keybindings-with-prefix "C-c j")
  :hook (clojure-mode . clj-refactor-mode))

;;; Project management stuff

(use-package projectile
  :ensure t
  :config
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
  (setq projectile-completion-system 'helm)
  (setq projectile-mode-line-function
        '(lambda () (format " Proj[%s]" (projectile-project-name))))
  (projectile-mode +1))

(use-package helm-projectile
  :ensure t
  :config
  (helm-projectile-on))

(use-package magit
  :ensure t
  :bind ("C-x g" . magit-status)
  :config
  (setq transient-default-level 5))

(use-package kubernetes
  :ensure t
  :bind ("C-x t" . kubernetes-overview))

(use-package flycheck-irony
  :ensure t)

(use-package flycheck
  :ensure t
  :config
  (setq flycheck-python-pycompile-executable "python3")
  (setq flycheck-python-flake8-executable "flake8")
  (setq flycheck-python-pylint-executable "pylint")

  ;; Chain checkers together
  ;; Python
  (flycheck-add-next-checker 'python-flake8 'python-pylint t)
  (flycheck-add-next-checker 'python-pylint 'python-pycompile t)
  ;; TS
  (flycheck-add-next-checker 'typescript-tide 'typescript-tslint t)
  ;; C/C++
  (flycheck-add-next-checker 'irony 'c/c++-cppcheck t)

  ;; irony-setup simply adds irony to flycheck-checkers
  (eval-after-load 'flycheck
    '(add-hook 'flycheck-mode-hook #'flycheck-irony-setup))

  ;; Activate flycheck in prog mode
  (add-hook 'prog-mode-hook #'flycheck-mode))

;; Maths, R, stats, etc

(when (system-is-mac)
  (add-to-list 'load-path "/usr/local/Cellar/maxima/5.42.2/share/maxima/5.42.2/emacs"))
(when (system-is-linux)
  ;; this folder is a copy of the one found in macOS and put under
  ;; this path (it works). The debian package maxima-emacs would also
  ;; work but it wants to install Emacs 24.
  (add-to-list 'load-path "/usr/local/share/imaxima/"))

(autoload 'maxima-mode "maxima" "Maxima mode" t)
(autoload 'imaxima "imaxima" "Frontend for maxima with image support" t)
(autoload 'maxima "maxima" "Maxima interaction" t)
(autoload 'imath-mode "imath" "Imath mode for math formula support" t)
(setq imaxima-use-maxima-mode-flag t)
(add-to-list 'auto-mode-alist '("\\.ma[cx]" . maxima-mode))
(setq imaxima-fnt-size "large")
(setq imaxima-pt-size 14)
(setq imaxima-scale-factor 1.6)

;;; Misc

(use-package restclient
  :ensure t
  :mode
  "\\.http$")

(use-package deft
  :ensure t
  :bind ("<f8>" . deft)
  :init
  (setq deft-extensions '("org" "txt"))
  (setq deft-directory "~/Nextcloud/deft")
  (setq deft-auto-save-interval 0))

(use-package pdf-tools
  :ensure t
  :config
  ;; macOS: to make retina display work, apply patch:
  ;; https://gist.github.com/carlosgeos/3b540d09e35e99786b93465f918ad10d
  ;; and check: https://github.com/politza/pdf-tools/issues/51
  (pdf-tools-install))

;;; Appearance

(use-package doom-themes
  :ensure t
  :config
  (load-theme 'doom-molokai t))

(use-package smart-mode-line
  :ensure t
  :config
  (add-hook 'after-init-hook 'sml/setup t))

(use-package aggressive-indent
  :ensure t
  :init
  (add-hook 'clojure-mode-hook #'aggressive-indent-mode)
  (add-hook 'emacs-lisp-mode-hook #'aggressive-indent-mode)
  (add-hook 'common-lisp-mode-hook #'aggressive-indent-mode)
  (add-hook 'scheme-mode-hook #'aggressive-indent-mode))

;;; Increase font size
(set-face-attribute 'default nil :height 140)

(provide 'init)

;;; init.el ends here
