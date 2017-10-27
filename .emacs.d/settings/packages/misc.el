;; Necessary to find PATH in macOS
(use-package exec-path-from-shell
  :ensure t
  :init
  (when (memq window-system '(mac ns))
    (exec-path-from-shell-initialize)))

(use-package cider
  :ensure t
  )

(provide 'misc)
