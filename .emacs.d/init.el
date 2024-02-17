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

;; Increase startup gc-threshold
(setq gc-cons-threshold 100000000)      ;100MB

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Bootstrap straight.el                            ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name
        "straight/repos/straight.el/bootstrap.el"
        (or (bound-and-true-p straight-base-dir)
            user-emacs-directory)))
      (bootstrap-version 7))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

;; Install use-package
(straight-use-package 'use-package)

;; Configure use-package to use straight.el by default
(use-package straight
  :custom
  (straight-use-package-by-default t))

(use-package exec-path-from-shell
  :init
  (when (memq window-system '(mac ns x))
    (exec-path-from-shell-initialize)))

;; System LANG variable is probably broken, so fix it with:
(setenv "LANG" "en_US.UTF-8")
(set-language-environment "UTF-8")

;;;;;;;;;;;;;;;;;;;;;;
;; Helper functions ;;
;;;;;;;;;;;;;;;;;;;;;;

(defconst *sys/linux*
  (eq system-type 'gnu/linux)
  "Are we running on a GNU/Linux system?")

(defconst *sys/mac*
  (eq system-type 'darwin)
  "Are we running on a Mac system?")

(defun untabify-except-makefiles ()
  "Replace tabs with spaces except in makefiles."
  (unless (derived-mode-p 'makefile-mode)
    (untabify (point-min) (point-max))))

(add-hook 'before-save-hook 'untabify-except-makefiles)
(add-hook 'before-save-hook 'delete-trailing-whitespace)

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

;; start window maximised
(add-to-list 'default-frame-alist '(fullscreen . maximized))

;; no startup message
(setq inhibit-startup-message t)

;; y/n instead of yes/no
(fset 'yes-or-no-p 'y-or-n-p)

;; no scroll bar
(scroll-bar-mode -1)

;; line number mode
(global-display-line-numbers-mode)

;; Quick keybindings to resize windows
(global-set-key (kbd "S-<left>") 'shrink-window-horizontally)
(global-set-key (kbd "S-<right>") 'enlarge-window-horizontally)
(global-set-key (kbd "S-<down>") 'shrink-window)
(global-set-key (kbd "S-<up>") 'enlarge-window)

;; User function keybindings
(global-set-key [f5] 'anzu-query-replace)
(global-set-key [f6] 'version)          ;TBD
(global-set-key [f7] 'version)          ;TBD
(global-set-key [f8] 'cider-eval-buffer)


(defun switch-to-buffer-list (buffer alist)
  "Use some window and select it.

BUFFER and ALIST are passed from `display-buffer-alist`"
  (select-window (display-buffer-in-side-window buffer alist)))


;; always open these buffers in said buffer. Buffer strings should
;; be regexes
(setq display-buffer-alist '(("\\*YASnippet Tables\\*" display-buffer-same-window)
                             ("\\*Help\\*" display-buffer-same-window)
                             ("\\*Process List\\*" display-buffer-same-window)
                             ("\\*Flycheck checkers\\*" display-buffer-same-window)
                             ("\\*Flycheck checker\\*" display-buffer-same-window)
                             ("\\*xref\\*" display-buffer-same-window)
                             ("\\magit-diff\\:" display-buffer-use-some-window
                              display-buffer-pop-up-window (inhibit-same-window . t))
                             ("\\magit-stash\\:" display-buffer-same-window)
                             ("\\magit-revision\\:" display-buffer-same-window)
                             ;; The following is generated with the rx macro
                             ("magit:[[:space:]]\\(?:.\\|\\)*" display-buffer-same-window)
                             ("\\*Backtrace\\*" display-buffer-same-window)
                             ("\\*Metahelp\\*" display-buffer-same-window)
                             ("\\*Local variables\\*" display-buffer-same-window)
                             ;; Same window does not work for cider
                             ;; so, open in side-window (which is a
                             ;; bit better
                             ("\\*cider-error\\*" display-buffer-in-side-window)
                             ("\\*cider-test-report\\*" display-buffer-use-some-window
                              display-buffer-pop-up-window (inhibit-same-window . t))
                             ("\\*cider-spec-browser\\*" display-buffer-use-some-window
                              display-buffer-pop-up-window (inhibit-same-window . t))
                             ("\\*cider-result\\*" (switch-to-buffer-list))
                             ("\\*cider-doc\\*" display-buffer-in-side-window)
                             ;; Open ESS help in same window
                             ("\\*help.*\\*" display-buffer-same-window)
                             ;; Open ESS R repl in side window
                             ("\\*R:~\\*" display-buffer-use-some-window
                              display-buffer-pop-up-window (inhibit-same-window . t))
                             ;; Python interpreter window. Open by Elpy
                             ("\\*Python\\*" display-buffer-in-side-window)
                             ("\\*grep.*\\*" display-buffer-same-window)
                             ("\\*ag search.*\\*" display-buffer-same-window)
                             ("\\*ein:.*\\*" display-buffer-same-window)))

;; Set minimum warning level to prevent native-comp popping up a
;; buffer with a ton of warnings
(setq warning-minimum-level :error)

;; Auto refresh buffers
(global-auto-revert-mode 1)

;; Highlight cursorline mode
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

;; different paragraph fill for docstrings
(setq python-fill-docstring-style 'django)

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
(when *sys/mac*
  (customize-set-variable 'ns-command-modifier 'meta)
  ;; ar and ranlib should come from LLVM and not GNU to be able to
  ;; compile pdf-tools on mac (true as of Jan 2019)
  (setenv "AR" "/usr/bin/ar")
  (setenv "RANLIB" "/usr/bin/ranlib"))

;;;;;;;;;;;;;;;;;;
;; Org settings ;;
;;;;;;;;;;;;;;;;;;

;; (require 'ob-clojure)
;; (setq org-babel-clojure-backend 'cider)

;; (setq org-agenda-start-on-weekday 1)
;; (setq calendar-week-start-day 1)
;; (define-key global-map "\C-ca" 'org-agenda)
;; (setq org-log-done t)
;; (setq org-priority-faces '((?A . (:foreground "red" :weight 'bold))
;;                            (?B . (:foreground "cyan"))
;;                            (?C . (:foreground "green"))))
;; (setq org-todo-keyword-faces
;;       '(("REVIEW" . "#A875FF")
;;         ("ON_HOLD" . "#F81C07")))

;; (setq org-agenda-skip-deadline-prewarning-if-scheduled t)

;; (org-babel-do-load-languages
;;  'org-babel-load-languages
;;  '((python . t)))

;; (setq org-babel-python-command "python3")

;; (use-package org-noter
;;   :config
;;   ;; sizing adjustments depending on the screen used
;;   (when *sys/linux*
;;     (setq org-noter-doc-split-fraction '(0.7 . 0.5))
;;     (plist-put org-format-latex-options :scale 2.0))
;;   (when *sys/mac*
;;     (setq org-noter-doc-split-fraction '(0.55 . 0.5))
;;     (plist-put org-format-latex-options :scale 1.3))
;;   (add-hook 'org-noter-notes-mode-hook 'turn-on-auto-fill)
;;   (setq org-preview-latex-image-directory "~/.lxtimg/"))

;; (use-package org-tree-slide
;;   :custom
;;   (org-image-actual-width nil))

;; (use-package edit-indirect)

;;;;;;;;;;;;;;;;;;;
;; Editing modes ;;
;;;;;;;;;;;;;;;;;;;

;; Programming

(use-package clojure-mode
  :hook (clojure-mode . subword-mode)
  :config
  (define-clojure-indent
   (forcat 1)
   (defsc :defn)))

(use-package web-mode
  :init
  (setq web-mode-enable-css-colorization t)
  (setq web-mode-enable-auto-pairing t)

  :mode
  "\\.phtml\\'"
  "\\.vue\\'"
  "\\.tsx\\'"
  "\\.js\\'"
  "\\.jsx\\'"
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

(use-package typescript-mode)

(use-package dockerfile-mode)

;; Markup, style, data and build tools

(use-package tex
  :straight auctex
  :init
  (add-hook 'LaTeX-mode-hook 'turn-on-auto-fill)
  (add-hook 'LaTeX-mode-hook (lambda () (abbrev-mode 1))))

(use-package markdown-mode
  :mode
  "\\.markdown\\'"
  "\\.md\\'"
  :config
  (setq markdown-command "pandoc"))

(use-package sphinx-doc
  :hook (python-mode . sphinx-doc-mode))

(use-package sass-mode)

(use-package scss-mode)

(use-package yaml-mode)

(use-package cmake-mode)

(use-package gmpl-mode)

(use-package terraform-mode)

;;;;;;;;;;;;;;
;; LSP mode ;;
;;;;;;;;;;;;;;

(use-package lsp-mode
  :init
  (setq lsp-keymap-prefix "C-c l")
  (setq lsp-enable-indentation nil)
  (setq lsp-lens-enable nil)
  :config
  (setq lsp-headerline-breadcrumb-enable nil)
  (add-to-list 'lsp-file-watch-ignored-directories "[/\\\\]venv\\'")
  (add-to-list 'lsp-file-watch-ignored-directories "[/\\\\]resources\\'")
  :bind ("C-c l f" . lsp-find-references)
  :hook
  (clojure-mode . lsp)
  (typescript-mode . lsp)
  (web-mode . lsp)
  (yaml-mode . lsp)
  (scss-mode . lsp)
  (sass-mode . lsp))

(use-package lsp-ui
  :config
  (setq lsp-ui-doc-enable nil))

(use-package lsp-pyright
  :hook (python-mode . (lambda ()
                         (require 'lsp-pyright)
                         (lsp))))  ; or lsp-deferred

;;;;;;;;;;;;;;;;;;;;
;; Other packages ;;
;;;;;;;;;;;;;;;;;;;;

(use-package irony-eldoc)

(use-package irony
  :config
  (add-hook 'c++-mode-hook 'irony-mode)
  (add-hook 'c-mode-hook 'irony-mode)
  (add-hook 'irony-mode-hook #'irony-eldoc)
  (add-hook 'irony-mode-hook 'irony-cdb-autosetup-compile-options))

(use-package cider
  :init
  (setq cider-repl-pop-to-buffer-on-connect nil)
  (setq cider-repl-use-pretty-printing t)
  (setq cider-repl-history-file "~/.emacs.d/nrepl-history")
  (setq cider-connection-message-fn nil)
  :config
  (cider-enable-flex-completion))

(use-package ess
  ;; ESS needs aggressive scroll on the inferior interactive
  ;; REPL. It's set as a buffer-local variable
  :init (require 'ess-site)
  :hook (inferior-ess-mode . (lambda ()
                               (setq scroll-down-aggressively 1.0))))

(use-package multiple-cursors
  ;; M-x calls to mc do not work well, keybindings are necessary
  ;; iedit does a similar thing. Maybe this one is better ?
  ;;
  ;; mc is always asking if a certain M-x action is to be done for all
  ;; cursors, annoying.
  :bind (("C->" . mc/mark-next-like-this)
         ("C-<" . mc/mark-previous-like-this)
         ("C-c C-a" . mc/mark-all-like-this)))

(use-package avy
  :bind (("C-;" . avy-goto-word-1)))


(use-package expand-region
  :bind ("C-<return>" . er/expand-region))

(use-package paredit
  :hook ((emacs-lisp-mode clojure-mode) . paredit-mode))

(use-package helm
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
  (setq completions-detailed t)
  (customize-set-variable 'helm-mode-fuzzy-match t)
  ;; Show full name in helm-mini
  (customize-set-variable 'helm-buffer-max-length nil))

(use-package helm-ag
  :bind
  (("M-g a" . helm-do-grep-ag)))

(use-package anzu
  :init
  (global-anzu-mode 1))

(use-package yasnippet
  :init
  (yas-global-mode 1)
  ;; Remove Yasnippet's default tab key binding. Does not work well
  ;; with indenting and company mode.
  :bind (:map yas-minor-mode-map
              ("TAB" . nil)
              ("<tab>" . nil)
              ("C-c ;" . yas-expand)))

;; yasnippet no longer ships with the snippets, hence this is also
;; necessary
(use-package yasnippet-snippets)

(use-package company
  :init
  (global-company-mode)
  :config
  ;; Anything lower than 0.3 doesn't work (hangs often) in a big
  ;; project, when LSP provides a `completion-at-point` fn to
  ;; `company-capf`. In those cases, 'lsp-completion-enable' should be
  ;; set to 'nil' in .dir-locals
  ;;
  ;; 'company-dabbrev' (simple completion in buffer) remains fast with
  ;; a 0.15 idle delay
  (setq company-idle-delay 0.15))

(use-package clj-refactor
  :diminish clj-refactor-mode
  :config
  (cljr-add-keybindings-with-prefix "C-c j")
  :hook (clojure-mode . clj-refactor-mode))

;;; Project management stuff

(use-package projectile
  :config
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
  (setq projectile-completion-system 'helm)
  (setq projectile-mode-line-function
        '(lambda () (format " Proj[%s]" (projectile-project-name))))
  (projectile-mode 1))

(use-package helm-projectile
  :config
  (helm-projectile-on))

(use-package magit
  :bind ("C-x g" . magit-status)
  :config
  (setq transient-default-level 5))

(defun ediff-copy-both-to-C ()
  "Select both options in a Magit ediff conflict.

In a Magit ediff screen, during a conflict, this fn copies option
A and then B."
  (interactive)
  (ediff-copy-diff ediff-current-difference nil 'C nil
                   (concat
                    (ediff-get-region-contents ediff-current-difference 'A ediff-control-buffer)
                    (ediff-get-region-contents ediff-current-difference 'B ediff-control-buffer))))

(defun add-d-to-ediff-mode-map () (define-key ediff-mode-map "d" 'ediff-copy-both-to-C))

(add-hook 'ediff-keymap-setup-hook 'add-d-to-ediff-mode-map)

(use-package kubernetes
  :bind ("C-x t" . kubernetes-overview))

(use-package flycheck-irony)

(use-package flycheck
  :config
  ;; Chain checkers together
  ;; C/C++
  (flycheck-add-next-checker 'irony 'c/c++-cppcheck t)

  ;; irony-setup simply adds irony to flycheck-checkers
  (eval-after-load 'flycheck
    '(add-hook 'flycheck-mode-hook #'flycheck-irony-setup))
  (setq flycheck-python-pylint-executable "python3")

  ;; Activate flycheck in prog mode
  (add-hook 'prog-mode-hook #'flycheck-mode))

;; Maxima CAS

(autoload 'maxima-mode "maxima" "Maxima mode" t)
(autoload 'imaxima "imaxima" "Frontend for maxima with image support" t)
(autoload 'maxima "maxima" "Maxima interaction" t)
(autoload 'imath-mode "imath" "Imath mode for math formula support" t)
(add-to-list 'auto-mode-alist '("\\.ma[cx]" . maxima-mode))
(setq imaxima-scale-factor 2)
;;; Maxima mode will send lines for evaluation to the *maxima*
;;; buffer. This flag makes the *maxima* buffer use imaxima under the
;;; hood. M-x imaxima should be run first though !
(setq imaxima-use-maxima-mode-flag t)
(when *sys/mac*
  (setq imaxima-scale-factor 1.3))

;;; Misc

(use-package pdf-tools
  :config
  (pdf-tools-install)
  (setq pdf-view-use-scaling t))

(use-package hackernews)

;;; Appearance

(use-package doom-themes
  :config
  (load-theme 'doom-molokai t))

(use-package smart-mode-line
  :config
  (add-hook 'after-init-hook 'sml/setup t))

;; (use-package aggressive-indent
;;   :init
;;   (add-hook 'clojure-mode-hook #'aggressive-indent-mode)
;;   (add-hook 'emacs-lisp-mode-hook #'aggressive-indent-mode)
;;   (add-hook 'common-lisp-mode-hook #'aggressive-indent-mode)
;;   (add-hook 'scheme-mode-hook #'aggressive-indent-mode))

;;; Performance tweaks

;; Around 2MB is the sweetspot.
;; This is also an option: http://bling.github.io/blog/2016/01/18/why-are-you-changing-gc-cons-threshold/
;; Setting it to a big number like 10MB or 100MB is not a good
;; idea. When it does actually garbage collect, Emacs will freeze
;; during a second or two
(setq gc-cons-threshold 3000000)        ;around 3MB
(setq read-process-output-max (* 1024 1024 2))
(setq max-lisp-eval-depth 3200)

(provide 'init)

;;; init.el ends here
