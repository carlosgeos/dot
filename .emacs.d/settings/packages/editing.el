;; ---------- Editing

(use-package multiple-cursors
  ;; M-x calls to mc do not work well, keybindings are necessary
  ;; iedit does a similar thing. Maybe this one is better ?
  ;;
  ;; mc is always asking if a certain M-x action is to be done for all
  ;; cursors, annoying.
  :ensure t
  :bind (("C->" . mc/mark-next-like-this)
         ("C-<" . mc/mark-previous-like-this)
         ("C-c C-a" . mc/mark-all-like-this)))

(use-package avy
  :ensure t
  :bind (("C-;" . avy-goto-word-1)
         ("C-c SPC" . avy-goto-char)))   ;FIXME: this is also clojure-align...
;; There are a lot more binds to set for avy!

(use-package expand-region
  :ensure t
  :bind ("C-<return>" . er/expand-region))


(use-package paredit
  :ensure t
  :hook ((emacs-lisp-mode clojure-mode) . paredit-mode)
  ;; (add-hook 'emacs-lisp-mode-hook (lambda () (paredit-mode 1)))
  ;; (add-hook 'clojure-mode-hook (lambda () (par-mode 1))))
  )

(use-package helm
  :ensure t
  :init
  (helm-mode t)
  :bind (("M-x" . helm-M-x)
         ("C-x C-f" . helm-find-files)
         ("C-x b" . helm-mini)
         ("M-y" . helm-show-kill-ring)
         ("C-h SPC" . helm-all-mark-rings)
         ("C-c h o" . helm-occur)
         ;; From helm maintainer config file, remap instead of simple
         ;; binding
         ("<remap> <list-buffers>" . helm-buffers-list))
  :config
  (setq helm-mode-fuzzy-match t)
  ;; Show full name in helm-mini
  (setq helm-buffer-max-length nil))

(use-package yasnippet
  :ensure t
  :init
  (yas-global-mode 1)
  ;; Remove Yasnippet's default tab key binding. Does not work well
  ;; with indenting and company mode.
  :bind (:map yas-minor-mode-map
              ("TAB" . nil)
              ("<tab>" . nil)
              ("C-c ;" . yas-expand)))

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
