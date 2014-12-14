;;;;;;;;;;;;;;;;;;;;;;;;;;
;; N3 Mode
;;;;;;;;;;;;;;;;;;;;;;;;;;

; https://github.com/kurtjx/n3-mode-for-emacs

(add-to-list 'load-path "~/.emacs.d/ventor/n3-mode.el")
(autoload 'n3-mode "n3-mode" "Major mode for OWL or N3 files" t)

;; Turn on font lock when in n3 mode
(add-hook 'n3-mode-hook
          'turn-on-font-lock)

(setq auto-mode-alist
      (append
       (list
        '("\\.n3" . n3-mode)
        '("\\.ttl" . n3-mode)
        '("\\.ttl\\.owl" . n3-mode)
        )
       auto-mode-alist))

(provide 'setup-n3)
