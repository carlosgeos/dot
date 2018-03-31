
;; ---------- Programming

(use-package helm
  :ensure t
  :init
  (helm-mode t)
  :bind (("M-x" . helm-M-x)
         ("C-x C-f" . helm-find-files)
         ;; From helm maintainer config file, remap instead of simple
         ;; binding
         ("<remap> <list-buffers>" . helm-buffers-list))
  :config)

(use-package yasnippet
  :ensure t
  :init
  (yas-global-mode 1)
  :config
  ;; Remove Yasnippet's default tab key binding
  ;; (define-key yas-minor-mode-map (kbd "TAB") nil)
  ;; (define-key yas-minor-mode-map (kbd "<tab>") nil)
  )

(use-package yasnippet-snippets
  ;; yasnippet no longer ships with the snippets, hence this is also
  ;; necessary
  :ensure t)

(use-package company
  :ensure t
  :init
  (global-company-mode))

(use-package sphinx-doc
  :ensure t
  :config
  :hook python-mode)

(use-package clj-refactor
  :ensure t
  :hook clojure-mode)

(provide 'completion)
