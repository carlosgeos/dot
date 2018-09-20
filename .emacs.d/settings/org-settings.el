;;; org-settings.el --- settings for org mode -*- lexical-binding: t -*-

;;; Commentary:

;;

;;; Code:

(require 'ob-clojure)
(setq org-babel-clojure-backend 'cider)

(org-babel-do-load-languages
 'org-babel-load-languages
 '((python . t)))

(use-package org-tree-slide
  :ensure t)

(provide 'org-settings)

;;; org-settings.el ends here
