;; Set path to dependencies
(setq setup-lisp-dir
      (expand-file-name "setup" user-emacs-directory))
(add-to-list 'load-path setup-lisp-dir)

;; Keep emacs Custom-settings in separate file
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(load custom-file)

;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Initialize Packages
;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'package)
(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/") t)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/") t)
(package-initialize)

(when (not package-archive-contents)
  (package-refresh-contents))

(defvar my-packages '(
                      ; starter-kit 
                      auctex ;latex-pretty-symbols
                                  cmake-mode nlinum autopair 
;                                  ecb color-theme
;                                  color-theme-solarized
;                                  twilight-bright-theme
;                                  color-theme-blackboard
;                                  color-theme-sanityinc-tomorrow
                                  markdown-mode
                                        ; emacs-eclim company
                                  auto-complete 
                                  ac-math
                                        ;yasnippet 
                                  tidy
                                        ; ipython 
                                  python-mode epc deferred auto-complete jedi jedi-direx ein
                                  dsvn
                                  ; helm
                                  xclip
                                  sparql-mode
                                  dired+
                                  evil
								  magit
								  tabbar
								  tabbar-ruler
                                  )
  "A list of packages to ensure are installed at launch.")

(dolist (p my-packages)
  (when (not (package-installed-p p))
    (package-install p)))


;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Basci configuration
;;;;;;;;;;;;;;;;;;;;;;;;;;

(setf inhibit-splash-screen t)

(global-set-key (kbd "C-x C-b") 'ibuffer)

(setq default-directory (concat (getenv "HOME") "/"))

(require 'cl)

(defun system-type-is-darwin ()
  (interactive)
  "Return true if system is darwin-based (Mac OS X)"
  (string-equal system-type "darwin")
)
(if (system-type-is-darwin)
    (setq confirm-kill-emacs 'y-or-n-p)
)

(setq-default cursor-type 'bar) 
(setq-default tab-width 4)

(setq ring-bell-function 'ignore)
(if (string= system-type "darwin")
;    (set-default-font "Monoca 12")
;    (set-default-font "Consolas 15")
    (set-default-font "SourceCodePro 13")
)

(show-paren-mode 1)


(menu-bar-mode 1) 
(tool-bar-mode -1)
 
(defun revert-buffer-no-confirm ()
    "Revert buffer without confirmation."
    (interactive) (revert-buffer t t))

(global-set-key (kbd "<f5>") 'revert-buffer-no-confirm) 

(global-set-key [(control x) (k)] 'kill-this-buffer)

(setenv "PATH"   (concat  "/usr/local/bin" ":" (getenv "PATH")))

(add-to-list 'exec-path "/usr/local/bin")

; enable mouse in terminal mode
(unless window-system
  (require 'mouse)
  (xterm-mouse-mode t)
  (global-set-key [mouse-4] '(lambda ()
                              (interactive)
                              (scroll-down 1)))
  (global-set-key [mouse-5] '(lambda ()
                              (interactive)
                              (scroll-up 1)))
  (defun track-mouse (e))
  (setq mouse-sel-mode t)
)

; don't forget apt-get install xclip
(xclip-mode 1)

(add-to-list 'load-path "~/.emacs.d/vendor")

; see https://gist.github.com/daniel-nelson/1023272
(require 'pbcopy)
(turn-on-pbcopy)

;; Unbind Pesky Sleep Button
(global-set-key "\C-z" nil)
(global-set-key "\C-x\C-z" nil)

; dired+
(define-key ctl-x-map   "d" 'diredp-dired-files)
(define-key ctl-x-4-map "d" 'diredp-dired-files-other-window)

; hl-line
(global-hl-line-mode)

; open file at cursor
(ffap-bindings)

(global-set-key [C-tab] 'next-buffer)
(global-set-key [C-S-tab] 'previous-buffer)

;;;;;;;;;;;;;;;;;;;;;;;;;;
;; evil mode
;;;;;;;;;;;;;;;;;;;;;;;;;;

; (require 'evil)
; (evil-mode 1)

(tabbar-mode 1)

(require 'recentf)
(recentf-mode 1)
(setq recentf-max-menu-items 25)
(global-set-key "\C-x\ \C-r" 'recentf-open-files)

;;;;;;;;;;;;;;;;;;;;;;;;;;
;; helm
;;;;;;;;;;;;;;;;;;;;;;;;;;

; (helm-mode 1)
; (global-set-key (kbd "C-c h") 'helm-mini)
; (global-set-key (kbd "C-x C-f") 'helm-find-files)

;;;;;;;;;;;;;;;;;;;;;;;;;;
;; SVN
;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'vc-svn)                                   
                                                    
(autoload 'svn-status "dsvn" "Run `svn status'." t) 
(autoload 'svn-update "dsvn" "Run `svn update'." t) 


;; (require 'tabbar)

(setq tabbar-ruler-global-tabbar t) ; If you want tabbar
; (setq tabbar-ruler-global-ruler t) ; if you want a global ruler
(setq tabbar-ruler-popup-menu t) ; If you want a popup menu.
(setq tabbar-ruler-popup-toolbar t) ; If you want a popup toolbar
; (setq tabbar-ruler-popup-scrollbar t) ; If you want to only show the
                                      ; scroll bar when your mouse is moving.
(require 'tabbar-ruler)



;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Auto Complete
;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'auto-complete-config)
(ac-config-default)


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

(require 'setup-python)

(require 'setup-tex)

;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Uniquify
;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'uniquify) 
(setq 
  uniquify-buffer-name-style 'post-forward
  uniquify-separator ":")

;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Auto complete
;;;;;;;;;;;;;;;;;;;;;;;;;;

; auto-complete walkaround for linum
(ac-flyspell-workaround)
(ac-linum-workaround)

;;;;;;;;;;;;;;;;;;;;;;;;;;
;; linum
;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'linum)
(global-linum-mode 1)
; (setq linum-format "%05d ")

;;;;;;;;;;;;;;;;;;;;;;;;;;
;; CMake
;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'cmake-mode)
(setq auto-mode-alist
      (append '(("CMakeLists\\.txt\\'" . cmake-mode)
		("\\.cmake\\'" . cmake-mode))
	      auto-mode-alist))

;;;;;;;;;;;;;;;;;;;;;;;;;;
;; markdown-mode
;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'markdown-mode)
(setq auto-mode-alist
      (append '(("\\.md$" . markdown-mode)
		("\\.markdown$'" . markdown-mode))
	      auto-mode-alist))

;;;;;;;;;;;;;;;;;;;;;;;;;;
;; autopair
;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'autopair)
(autopair-global-mode) ;; to enable in all buffers

;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Prolog
;;;;;;;;;;;;;;;;;;;;;;;;;;

(autoload 'run-prolog "prolog" "Start a Prolog sub-process." t)
(autoload 'prolog-mode "prolog" "Major mode for editing Prolog programs." t)
(autoload 'mercury-mode "prolog" "Major mode for editing Mercury programs." t)
(setq prolog-system 'swi)  ; optional, the system you are using;
                           ; see `prolog-system' below for possible values
(setq auto-mode-alist (append '(("\\.pl$" . prolog-mode)
                                ("\\.asp$" . prolog-mode)
                                ("\\.dl.?$" . prolog-mode)
                                ("\\.lp$" . prolog-mode)
                                ("\\.hex$" . prolog-mode)
                                )
                              auto-mode-alist))

;;;;;;;;;;;;;;;;;;;;;;;;;;
;; SPARQL
;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (add-to-list 'load-path "~/.emacs.d/vendor/sparql-mode")
(autoload 'sparql-mode "sparql-mode.el"
  "Major mode for editing SPARQL files" t)
(add-to-list 'auto-mode-alist '("\\.sparql$" . sparql-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;
;; octave mode
;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq inferior-octave-program "/opt/local/bin/octave")
(autoload 'octave-mode "octave-mod" nil t)
(setq auto-mode-alist
      (cons '("\\.m$" . octave-mode) auto-mode-alist))

(add-hook 'octave-mode-hook
          (lambda ()
            (abbrev-mode 1)
            (auto-fill-mode 1)
            (if (eq window-system 'x)
                (font-lock-mode 1))))

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

;;;;;;;;;;;;;;;;;;;;;;;;;;
;; DIR Tree
;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'dirtree)
(require 'tree-mode)
(require 'windata)
(autoload 'dirtree "dirtree" "Add directory to tree view")

;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Color Theme
;;;;;;;;;;;;;;;;;;;;;;;;;;

; (require 'color-theme)

;(load-theme 'zenburn t)


; (require 'twilight-bright-theme)

;; (if (string= system-type "darwin")
;;     (if (not (display-graphic-p))
;;         (color-theme-solarized-dark)
;;     )
;; )
; (require 'color-theme-solarized)
; 
; (require 'color-theme-sanityinc-tomorrow)

;; (If window-system
;;      (color-theme-sanityinc-tomorrow-eighties)
;; )

;(if (not window-system)   
;   (color-theme-solarized-dark)   
;)


; (require 'color-theme-blackboard)

; (require 'color-theme-mods)
; (color-theme-billc)
; (bc-color-theme)

;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Emacs powerline
;;;;;;;;;;;;;;;;;;;;;;;;;;

(if window-system
    (progn
      (add-to-list 'load-path "~/.emacs.d/vendor/emacs-powerline")
      (require 'powerline) 
      )
)

; (require 'viper)
; (setq viper-mode t)

; CEDET




;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ECB
;;;;;;;;;;;;;;;;;;;;;;;;;;


; (require 'ecb-autoloads)
; (setq ecb-tip-of-the-day nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Games
;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq tetris-score-file "~/.emacs.d/tetris-scores")

;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Rename and Move file
;;;;;;;;;;;;;;;;;;;;;;;;;;

;; source: http://steve.yegge.googlepages.com/my-dot-emacs-file
(defun rename-file-and-buffer (new-name)
  "Renames both current buffer and file it's visiting to NEW-NAME."
  (interactive "sNew name: ")
  (let ((name (buffer-name))
        (filename (buffer-file-name)))
    (if (not filename)
        (message "Buffer '%s' is not visiting a file!" name)
      (if (get-buffer new-name)
          (message "A buffer named '%s' already exists!" new-name)
        (progn
          (rename-file name new-name 1)
          (rename-buffer new-name)
          (set-visited-file-name new-name)
          (set-buffer-modified-p nil))))))

(defun move-buffer-file (dir)
 "Moves both current buffer and file it's visiting to DIR." (interactive "DNew directory: ")
 (let* ((name (buffer-name))
	 (filename (buffer-file-name))
	 (dir
	 (if (string-match dir "\\(?:/\\|\\\\)$")
	 (substring dir 0 -1) dir))
	 (newname (concat dir "/" name)))

 (if (not filename)
	(message "Buffer '%s' is not visiting a file!" name)
   (progn 	
     (copy-file filename newname 1)
     (delete-file filename)
     (set-visited-file-name newname)
     (set-buffer-modified-p nil)
     t)))) 
(put 'upcase-region 'disabled nil)

(setf inhibit-splash-screen t)
