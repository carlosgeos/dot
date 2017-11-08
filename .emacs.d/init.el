;;; .init.el --- emacs configuration file
;; -*- coding: utf-8 -*-
;;; Commentary:
;; Main entry point of dot files

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
(add-to-list 'load-path "~/.emacs.d/settings/packages/")

;; define various custom functions (system is mac and system is linux)
(require 'custom-functions)
;; configure general settings
(require 'general-settings)
(require 'use-package)
(require 'major-modes)
(require 'misc)
(require 'completion)
(require 'editing)
(require 'project-mgnt)
(require 'linting)
(require 'appearance)

;; TRAMP mode
(setq tramp-default-method "ssh")

;; Important to make it work
(setq tramp-auto-save-directory "~/.emacs.d/tramp-autosave")
;; Make C-c o the general key for switching windows
(global-set-key (kbd "C-c o") 'other-window)


(require 'keybindings-hooks)

;;---------------------------------------------------------------------
;; Put auto 'custom' changes in a separate file (this is stuff like
;; custom-set-faces and custom-set-variables)
(load
 (setq custom-file (expand-file-name "settings/custom.el" user-emacs-directory))
 'noerror)
