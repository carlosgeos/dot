;; ---------- Editing

(use-package multiple-cursors
  ;; No keybindings for the moment... using M-x
  :ensure t
  )

(use-package avy                        ;(ace-jump-mode is dead)
  :ensure t
  :bind (("C-;" . avy-goto-word-1)
         ("C-c SPC" . avy-goto-char))   ;FIXME: this is also clojure-align...
  ;; There are a lot more binds to set for avy!
  )

(use-package expand-region
  :ensure t
  :bind ("C-<return>" . er/expand-region)
  )

(use-package paredit
  :ensure t);TODO: configure keybindings !!!


(provide 'editing)
