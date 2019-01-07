;;; org-settings.el --- settings for org mode -*- lexical-binding: t -*-

;;; Commentary:

;;

;;; Code:

(require 'ob-clojure)
(setq org-babel-clojure-backend 'cider)

(org-babel-do-load-languages
 'org-babel-load-languages
 '((python . t)))

(use-package org-noter
  :ensure t
  :config
  ;; make the pdf side take a bit more space than half the screen
  (when (system-is-linux)
    (setq org-noter-doc-split-fraction '(0.7 . 0.5)))
  (when (system-is-mac)
    (setq org-noter-doc-split-fraction '(0.6 . 0.5)))
  ;; make embedded latex appear larger, 1.8 works well
  (plist-put org-format-latex-options :scale 1.8)
  (add-hook 'org-noter-notes-mode-hook 'turn-on-auto-fill))

(provide 'org-settings)

;;; org-settings.el ends here
