; -*- coding: utf-8 -*-
;;; package --- Summary
;;; Commentary:
;test

;--------------------------------------------------;
;; Start all installed packages (require + config)-;
;--------------------------------------------------;
(require 'package)
;;; Code:
(add-to-list 'package-archives
             '("melpa" . "http://melpa.org/packages/") t)
(package-initialize)			;this is the key


;; path where settings files are kept
(add-to-list 'load-path "~/.emacs.d/settings/")

;; define various custom functions (system is mac and system is linux)
(require 'custom-functions)

;; configure general settings
(require 'general-settings)

;---------------;
;;; Utilities ;;;
;---------------;

;; auctex + latex
(require 'auctex-settings)

;; yasnippet
(require 'yasnippet-settings)

;; Auto complete
(require 'auto-complete-settings)

;; imaxima
(require 'maxima-settings)

;; flycheck in linux machine
(if (system-is-linux)
	(require 'flycheck-settings))

;; ido
(require  'ido-settings)

;-----------;
;;; Modes ;;;
;-----------;

;; Python mode
(require 'python-settings)

;; LaTeX and Auctex
;TODO:


;; TRAMP mode
(require 'tramp-settings)

;-----------;
;;; Hooks ;;;
;-----------;

(require 'keybindings-hooks)


;---------------------------------------------------------------------
;; Put auto 'custom' changes in a separate file (this is stuff like
;; custom-set-faces and custom-set-variables)
(load
 (setq custom-file (expand-file-name "settings/custom.el" user-emacs-directory))
 'noerror)


;(provide .emacs)
;;; .emacs ends here
