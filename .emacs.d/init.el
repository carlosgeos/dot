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

;; path where settings files are kept
(add-to-list 'load-path "~/.emacs.d/settings/")
(add-to-list 'load-path "~/.emacs.d/settings/packages/")

;; define various custom functions (system is mac and system is linux)
(require 'custom-functions)
;; configure general settings
(require 'general-settings)
(require 'org-settings)
(require 'use-package)
(require 'major-modes)
(require 'my-misc)
(require 'editing)
(require 'project-mgnt)
(require 'linting)
(require 'appearance)
