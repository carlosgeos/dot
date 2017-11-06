;; ---------- Themes

(use-package monokai-theme
  :ensure t)

(use-package smart-mode-line
  :ensure t
  :config
  (add-hook 'after-init-hook 'sml/setup t))

;; (use-package rainbow-delimiters
;;   ;; Testing, let's see if it is useful...
;;   :ensure t)

(use-package aggresive-indent-mode
  :ensure t
  :init
  (add-hook 'clojure-mode-hook #'aggressive-indent-mode)
  (add-hook 'emacs-lisp-mode-hook #'aggressive-indent-mode)
  (add-hook 'common-lisp-mode-hook #'aggressive-indent-mode)
  (add-hook 'scheme-mode-hook #'aggressive-indent-mode))

(provide 'appearance)
