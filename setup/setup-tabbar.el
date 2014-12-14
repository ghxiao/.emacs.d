;; (require 'tabbar)

(if window-system
	(progn
	  (tabbar-mode 1)
	  (global-set-key [C-tab] 'next-buffer)
	  (global-set-key [C-S-tab] 'previous-buffer)

	  (setq tabbar-ruler-global-tabbar t) ; If you want tabbar
	  (setq tabbar-ruler-popup-menu t) ; If you want a popup menu.
	  (setq tabbar-ruler-popup-toolbar t) ; If you want a popup toolbar
	  (require 'tabbar-ruler) ) )

(provide 'setup-tabbar)
