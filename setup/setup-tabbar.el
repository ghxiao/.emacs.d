;; (require 'tabbar)

(tabbar-mode 1)
(global-set-key [C-tab] 'next-buffer)
(global-set-key [C-S-tab] 'previous-buffer)

(setq tabbar-ruler-global-tabbar t) ; If you want tabbar
; (setq tabbar-ruler-global-ruler t) ; if you want a global ruler
(setq tabbar-ruler-popup-menu t) ; If you want a popup menu.
(setq tabbar-ruler-popup-toolbar t) ; If you want a popup toolbar
; (setq tabbar-ruler-popup-scrollbar t) ; If you want to only show the
                                      ; scroll bar when your mouse is moving.
(require 'tabbar-ruler)

(provide 'setup-tabbar)
