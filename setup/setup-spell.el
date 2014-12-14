;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Spell Checking
;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq flyspell-issue-welcome-flag nil)
(if (string= system-type "darwin")
    (setq ispell-program-name "/usr/local/bin/aspell")
)
(if (string= system-type "gnu/linux")
    (setq ispell-program-name "/usr/bin/aspell")
)

(add-hook 'LaTeX-mode-hook 'flyspell-mode)

(provide 'setup-spell)
