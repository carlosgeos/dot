;; ---------- Major Modes

;; Programming

(use-package clojure-mode
  ;; also check out CIDER in misc file !
  :ensure t
  :hook (clojure-mode . subword-mode))

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
  "\\.vue\\'"
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
                             (setq web-mode-css-indent-offset 2))))

(use-package typescript-mode
  :ensure t)

(use-package rjsx-mode
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
(when (system-is-linux)
  ;; this folder is a copy of the one found in macOS and put under
  ;; this path (it works). The debian package maxima-emacs would also
  ;; work but it wants to install Emacs 24.
  (add-to-list 'load-path "/usr/local/share/imaxima/"))

(autoload 'maxima-mode "maxima" "Maxima mod" t)
(autoload 'imaxima "imaxima" "Frontend for maxima with image support" t)
(autoload 'maxima "maxima" "Maxima interaction" t)
(autoload 'imath-mode "imath" "Imath mode for math formula support" t)
(setq imaxima-use-maxima-mode-flag t)
(add-to-list 'auto-mode-alist '("\\.ma[cx]" . maxima-mode))
(setq imaxima-fnt-size "large")
(setq imaxima-pt-size 14)
(setq imaxima-scale-factor 1.6)

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


(use-package pdf-tools
  :ensure t
  :config
  ;; this command will say `cant find brew` or something like that on
  ;; macOS, just run (exec-path-from-shell-initialize) so emacs can
  ;; find it.
  (pdf-tools-install))

(provide 'major-modes)
