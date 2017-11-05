;; ---------- Editing

(use-package multiple-cursors
  ;; No keybindings for the moment... using M-x
  :ensure t)

(use-package avy                        ;(ace-jump-mode is dead)
  :ensure t
  :bind (("C-;" . avy-goto-word-1)
         ("C-c SPC" . avy-goto-char)))   ;FIXME: this is also clojure-align...
;; There are a lot more binds to set for avy!

(use-package expand-region
  :ensure t
  :bind ("C-<return>" . er/expand-region))

(use-package smartparens
  :ensure t
  :init
  (add-hook 'clojure-mode-hook #'smartparens-strict-mode)
  (add-hook 'emacs-lisp-mode-hook #'smartparens-mode)
  (add-hook 'common-lisp-mode-hook #'smartparens-mode)
  (add-hook 'scheme-mode-hook #'smartparens-mode))


(provide 'editing)
