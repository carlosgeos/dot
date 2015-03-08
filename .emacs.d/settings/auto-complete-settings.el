;; AC config file

;the load path of the plugin and the dictionnaries is already provided
;by package-initialize
(ac-config-default)
(setq ac-source-yasnippet nil)

;; from
;; http://truongtx.me/2013/01/06/config-yasnippet-and-autocomplete-on-emacs/
; set the trigger key so that it can work together with yasnippet on
; tab key, if the word exists in yasnippet, pressing tab will cause
; yasnippet to activate, otherwise, auto-complete will
;(ac-set-trigger-key "TAB")
;(ac-set-trigger-key "<tab>")

(provide 'auto-complete-settings)
