;; ---------- Themes

(use-package monokai-theme
  :ensure t)

(use-package smart-mode-line
  :ensure t
  :config
  (add-hook 'after-init-hook 'sml/setup t))

(use-package rainbow-delimiters
  ;; Testing, let's see if it is useful...
  :ensure t)

(provide 'appearance)
