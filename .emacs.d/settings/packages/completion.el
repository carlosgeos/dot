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
  :config
  )

(use-package yasnippet
  :ensure t
  :init
  (yas-global-mode 1)
  :config
  ;; Remove Yasnippet's default tab key binding
  ;; (define-key yas-minor-mode-map (kbd "TAB") nil)
  ;; (define-key yas-minor-mode-map (kbd "<tab>") nil)
  )

(use-package company-mode
  :ensure t
  :init
  (global-company-mode)
  )

(provide 'completion)
