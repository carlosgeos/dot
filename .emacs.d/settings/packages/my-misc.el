;; Necessary to find PATH in macOS
(use-package exec-path-from-shell
  :ensure t
  :init
  (when (system-is-mac)
    (exec-path-from-shell-initialize)
    ;; ar and ranlib should come from LLVM and not GNU to be able to
    ;; compile pdf-tools (true in Nov 2018)
    (setenv "AR" "/usr/bin/ar")
    (setenv "RANLIB" "/usr/bin/ranlib")))

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
  (setq cider-repl-history-file "~/.emacs.d/nrepl-history"))

(provide 'my-misc)
