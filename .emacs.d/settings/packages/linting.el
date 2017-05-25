;; ---------- Error Checking

(use-package flycheck
  :ensure t
  :init
  (add-hook 'prog-mode-hook #'flycheck-mode)

  ;; SHOULD BE DONE ON A PER PROJECT BASIS WITH DIR LOCALS
  ;; (setq-default flycheck-disabled-checkers '(c/c++-clang))
  ;; (setq-default flycheck-gcc-warnings '("pedantic" "all" "extra" "conversion" "effc++" "strict-null-sentinel" "old-style-cast" "noexcept" "ctor-dtor-privacy" "overloaded-virtual" "sign-promo" "zero-as-null-pointer-constant" "suggest-final-types" "suggest-final-methods" "suggest-override"))
  (add-hook 'c++-mode-hook
            (lambda ()
              (setq flycheck-gcc-language-standard "c++14")
              (setq flycheck-c/c++-gcc-executable "g++-6")
              (flycheck-select-checker 'c/c++-gcc)
              )
            ) ;it does not interfere with c mode.
  )

(provide 'linting)
