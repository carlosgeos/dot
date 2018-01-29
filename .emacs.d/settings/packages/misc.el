;; Necessary to find PATH in macOS
(use-package exec-path-from-shell
  :ensure t
  :init
  (when (system-is-mac)
    (exec-path-from-shell-initialize)))

(use-package key-chord
  :ensure t
  :init
  (key-chord-mode 1))
;; (add-hook 'clojure-mode-hook #'(key-chord-mode +1))
;; (add-hook 'emacs-lisp-mode-hook #'(key-chord-mode +1))
;; (add-hook 'common-lisp-mode-hook #'(key-chord-mode +1))
;; (add-hook 'scheme-mode-hook #'(key-chord-mode +1)))

;; Development environments

(use-package cider
  :ensure t
  :init
  (setq cider-repl-pop-to-buffer-on-connect nil)
  (setq cider-repl-use-pretty-printing t)
  (setq cider-history-file "~/.emacs.d/nrepl-history"))

(use-package indium
  :ensure t)

(provide 'misc)
