;; ---------- Project management

(use-package projectile
  :ensure t
  :init
  ;; config: enable cache and other settings?
  :config
  (projectile-global-mode)
  )

(use-package helm-projectile
  :ensure t
  :config
  (helm-projectile-on)
  )

(use-package helm-gtags
  :ensure t
  :bind (("M-." . helm-gtags-dwim))
  :init
  (setq helm-gtags-auto-update t)
  :config
  (add-hook 'c-mode-hook 'helm-gtags-mode)
  (add-hook 'c++-mode-hook 'helm-gtags-mode)
  (add-hook 'asm-mode-hook 'helm-gtags-mode)
  )

(use-package magit
  :ensure t
  :bind ("C-x g" . magit-status)
  )

(provide 'project-mgnt)
