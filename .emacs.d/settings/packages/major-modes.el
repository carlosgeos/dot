;; ---------- Major Modes

(when (memq window-system '(mac ns))
  (add-to-list 'load-path "/usr/local/Cellar/maxima/5.38.1/share/maxima/5.38.1/emacs"))
(use-package imaxima
  :config
  (setq imaxima-fnt-size "Large")
  )

(use-package slim-mode
  :ensure t)

(use-package handlebars-mode
  :ensure t)

(use-package coffee-mode
  :ensure t)

(use-package sass-mode
  :ensure t
  :config
  (add-hook 'sass-mode-hook (lambda ()
                              (yas-activate-extra-mode 'css-mode)))
  )

(use-package scss-mode
  :ensure t
  :config
  (add-hook 'scss-mode-hook (lambda ()
                              (yas-activate-extra-mode 'css-mode)))
  )


(use-package yaml-mode
  :ensure t)

(use-package web-mode
  :ensure t
  :init
  (setq web-mode-enable-css-colorization t)
  (setq web-mode-enable-auto-pairing t)

  :mode
  "\\.phtml\\'"
  "\\.html\\'"
  "\\.js\\'"
  "\\.tpl\\.php\\'"
  "\\.[agj]sp\\'"
  "\\.as[cp]x\\'"
  "\\.erb\\'"
  "\\.mustache\\'"
  "\\.djhtml\\'"
  "\\.hbs\\'"

  :config
  (add-hook 'web-mode-hook (lambda ()
                             "Sets the config for Web mode"
                             (yas-activate-extra-mode 'html-mode)
                             (yas-activate-extra-mode 'js-mode)
                             (setq web-mode-markup-indent-offset 3)
                             (setq web-mode-code-indent-offset 2)
                             (setq web-mode-css-indent-offset 2)))

  )

(use-package tex-site
  :ensure auctex
  :init
  (add-hook 'LaTeX-mode-hook 'turn-on-auto-fill)
  (add-hook 'LaTeX-mode-hook (lambda () (abbrev-mode +1)))
  )

(use-package ess-site
  :ensure ess
  :disabled t)

(use-package markdown-mode
  :ensure t
  :mode
  "\\.markdown\\'"
  "\\.md\\'"
  )

(use-package typescript-mode
  :ensure t
  )

(use-package groovy-mode
  :ensure t
  )

(use-package rjsx-mode
  :ensure t
  )

(provide 'major-modes)
