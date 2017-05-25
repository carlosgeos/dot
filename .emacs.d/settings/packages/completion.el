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
  ;; Remove Yasnippet's default tab key binding
  ;; (define-key yas-minor-mode-map (kbd "<tab>") nil)
  ;; (define-key yas-minor-mode-map (kbd "TAB") nil)
  ;; Set Yasnippet's key binding to shift+tab
  :init
  (yas-global-mode 1)
  :config
  ;; Creates conflict with avy mode for jumping to char but should be
  ;; ok.
  (bind-key "C-;" 'yas-expand yas-minor-mode-map)
  )

(use-package auto-complete
  :ensure t
  :init
  (ac-config-default)
  )

(provide 'completion)
