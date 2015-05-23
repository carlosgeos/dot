;;; Auctex --- config file

(add-hook 'LaTeX-mode-hook 'turn-on-auto-fill)
(add-hook 'LaTeX-mode-hook (lambda () (abbrev-mode +1)))

(provide 'auctex-settings)
