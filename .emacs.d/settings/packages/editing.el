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

;; some functions are ok. global paredit mode is too strict configure
;; wrapping of sexps
(use-package paredit
  :ensure t)

(use-package paxedit
  ;; using transposing and deleting functions from paxedit, not
  ;; smartparens
  :ensure t
  :init
  ;; bindings in keychords
  (key-chord-define-global "kk" 'paxedit-delete) ;nice sexp delete
  :bind
  (("M-<right>" . paxedit-transpose-forward)
   ("M-<left>" . paxedit-transpose-backward)))

(use-package smartparens
  :ensure t
  :init
  (add-hook 'clojure-mode-hook #'smartparens-mode)
  (add-hook 'emacs-lisp-mode-hook #'smartparens-mode)
  (add-hook 'common-lisp-mode-hook #'smartparens-mode)
  (add-hook 'scheme-mode-hook #'smartparens-mode)
  ;; LaTeX and HTML as well ??
  :bind
  (("M-s" . sp-splice-sexp)
   ("C-M-f" . sp-forward-sexp)
   ("C-M-b" . sp-backward-sexp)
   ("C-M-n" . sp-next-sexp)
   ("C-M-p" . sp-previous-sexp)
   ("C-M-d" . sp-down-sexp)
   ("C-M-u" . sp-up-sexp)
   ("C-<right>" . sp-forward-slurp-sexp)
   ("C-<left>" . sp-backward-slurp-sexp)
   ("C-M-<right>" . sp-forward-barf-sexp)
   ("C-M-<left>" . sp-backward-barf-sexp)))
;; TODO: config bindings to navigate through sexps. 'add-to-next-sexp'
;; and similar are quite interesting as well

(use-package helm
  :ensure t
  :init
  (helm-mode t)
  :bind (("M-x" . helm-M-x)
         ("C-x C-f" . helm-find-files)
         ("C-x b" . helm-mini)
         ("M-y" . helm-show-kill-ring)
         ;; From helm maintainer config file, remap instead of simple
         ;; binding
         ("<remap> <list-buffers>" . helm-buffers-list))
  :config
  (setq helm-mode-fuzzy-match t))

(use-package yasnippet
  :ensure t
  :init
  (yas-global-mode 1)
  ;; Remove Yasnippet's default tab key binding
  (define-key yas-keymap (kbd "TAB") nil)
  (define-key yas-keymap (kbd "<tab>") nil)
  :bind (("C-c C-m" . yas-expand)))

(use-package yasnippet-snippets
  ;; yasnippet no longer ships with the snippets, hence this is also
  ;; necessary
  :ensure t)

(use-package company
  :ensure t
  :init
  (global-company-mode)
  :config
  (setq company-idle-delay 0.1))

(use-package sphinx-doc
  :ensure t
  :hook (python-mode . sphinx-doc-mode))

(use-package clj-refactor
  :diminish clj-refactor-mode
  :ensure t
  :config
  (cljr-add-keybindings-with-prefix "C-c j")
  :hook (clojure-mode . clj-refactor-mode))

(provide 'editing)
