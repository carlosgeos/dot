;; ---------- Major Modes

;; Programming

(use-package clojure-mode
  ;; also check out CIDER in misc file !
  :ensure t
  :init
  (add-hook 'clojure-mode-hook #'subword-mode))

(use-package groovy-mode
  :ensure t)

;; Markup

(use-package tex-site
  :ensure auctex
  :init
  (add-hook 'LaTeX-mode-hook 'turn-on-auto-fill)
  (add-hook 'LaTeX-mode-hook (lambda () (abbrev-mode +1))))

(use-package markdown-mode
  :ensure t
  :mode
  "\\.markdown\\'"
  "\\.md\\'")

;; Web

(use-package web-mode
  :ensure t
  :init
  (setq web-mode-enable-css-colorization t)
  (setq web-mode-enable-auto-pairing t)

  :mode
  "\\.phtml\\'"
  "\\.html\\'"
  "\\.js\\'"
  "\\.jsx\\'"
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
                             (setq web-mode-css-indent-offset 2))))

(use-package typescript-mode
  :ensure t)

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
                              (yas-activate-extra-mode 'css-mode))))

(use-package scss-mode
  :ensure t
  :config
  (add-hook 'scss-mode-hook (lambda ()
                              (yas-activate-extra-mode 'css-mode))))

(use-package restclient
  :ensure t
  :mode
  "\\.http$")

(use-package yaml-mode
  :ensure t)


;; Maths, R, stats, etc

(when (system-is-mac)
  (add-to-list 'load-path "/usr/local/Cellar/maxima/5.41.0/share/maxima/5.41.0/emacs"))

(use-package imaxima
  :config
  (setq imaxima-equation-color "#ffffff")
  (setq imaxima-fnt-size "Large"))

(use-package ess-site
  :ensure ess
  :disabled t)

;; Other

(use-package deft
  :ensure t
  :bind ("<f8>" . deft)
  :init
  (setq deft-extensions '("org" "txt"))
  (setq deft-directory "~/ownCloud/deft")
  (setq deft-auto-save-interval 0))


(provide 'major-modes)
