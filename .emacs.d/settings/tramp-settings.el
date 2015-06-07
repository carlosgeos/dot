(setq tramp-default-method "ssh")

(setq command-line-default-directory "/linode:Documents/")
(setq tramp-auto-save-directory "~/.emacs.d/tramp-autosave")
; Make C-c o the general key for switching windows
(global-set-key (kbd "C-c o") 'other-window)

(provide 'tramp-settings)
