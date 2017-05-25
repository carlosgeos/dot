;; ---------- Editing

(use-package multiple-cursors
  :ensure t
  )

(use-package avy                        ;(ace-jump-mode is dead)
  :ensure t
  :bind (("C-;" . avy-goto-word-1)
         ("C-c SPC" . avy-goto-char))
  ;; There are a lot more binds to set for avy!
  )

(use-package expand-region
  :ensure t
  :bind ("C-<return>" . er/expand-region)
  )

(provide 'editing)
