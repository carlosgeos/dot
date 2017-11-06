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

;; configure wrapping of sexps
(use-package paredit
  :ensure t)

(use-package smartparens
  :ensure t
  :init
  (add-hook 'clojure-mode-hook #'smartparens-strict-mode)
  (add-hook 'emacs-lisp-mode-hook #'smartparens-mode)
  (add-hook 'common-lisp-mode-hook #'smartparens-mode)
  (add-hook 'scheme-mode-hook #'smartparens-mode)
  ;; LaTeX and HTML as well ??
  :bind
  (("M-s" . sp-splice-sexp)
   ("C-<right>" . sp-forward-slurp-sexp)
   ("C-<left>" . sp-backward-slurp-sexp)
   ("C-M-<right>" . sp-forward-barf-sexp)
   ("C-M-<left>" . sp-backward-barf-sexp)))
;;TODO: config bindings to navigate through sexps. 'add-to-next-sexp'
;;and similar are quite interesting as well

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

(provide 'editing)
