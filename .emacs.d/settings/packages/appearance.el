
;; ---------- Themes

(use-package doom-themes
  :ensure t
  :config
  (load-theme 'doom-molokai t))

(use-package smart-mode-line
  :ensure t
  :config
  (add-hook 'after-init-hook 'sml/setup t))

;; (use-package rainbow-delimiters
;;   ;; Testing, let's see if it is useful...
;;   :ensure t)

(use-package aggressive-indent
  :ensure t
  :init
  (add-hook 'clojure-mode-hook #'aggressive-indent-mode)
  (add-hook 'emacs-lisp-mode-hook #'aggressive-indent-mode)
  (add-hook 'common-lisp-mode-hook #'aggressive-indent-mode)
  (add-hook 'scheme-mode-hook #'aggressive-indent-mode))

(use-package emojify
  :ensure t
  :init
  (add-hook 'after-init-hook #'global-emojify-mode))

(when (system-is-linux)
  (set-face-attribute 'default (selected-frame) :height 140))

(provide 'appearance)
