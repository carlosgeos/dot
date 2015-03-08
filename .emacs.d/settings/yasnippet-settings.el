;---------------;
;;; yasnippet ;;;
;---------------;

(require 'yasnippet)
(define-key yas-minor-mode-map (kbd "<tab>") nil)
(define-key yas-minor-mode-map (kbd "TAB") nil)
(define-key yas-minor-mode-map (kbd "C-;") 'yas-expand)

(yas-global-mode 1)

(provide 'yasnippet-settings)
