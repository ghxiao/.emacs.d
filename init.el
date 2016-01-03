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
					  auctex-latexmk
                                  cmake-mode nlinum autopair 
;                                  ecb color-theme
;                                  color-theme-solarized
;                                  twilight-bright-theme
;                                  color-theme-blackboard
;                                  color-theme-sanityinc-tomorrow
                                  markdown-mode
                                  auto-complete 
                                  ac-math
                                        ;yasnippet 
                                  tidy
                                  python-mode epc deferred auto-complete jedi jedi-direx ein
                                  dsvn
                                  xclip
                                  sparql-mode
                                  dired+
                                  evil
								  magit
								  tabbar
								  tabbar-ruler
								  scala-mode2
                                  )
  "A list of packages to ensure are installed at launch.")

(dolist (p my-packages)
  (when (not (package-installed-p p))
    (package-install p)))


;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Basci configuration
;;;;;;;;;;;;;;;;;;;;;;;;;;

(delete-selection-mode 1)

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
(defalias 'yes-or-no-p 'y-or-n-p)


(setq-default cursor-type 'bar) 
(setq-default tab-width 4)

(setq ring-bell-function 'ignore)
(if (string= system-type "darwin")
	  	;	(set-default-font "Inconsolata 13")
		(set-default-font "Menlo 14")
;    (set-default-font "Monoca 12")
;    (set-default-font "Consolas 15")
;    (set-default-font "SourceCodePro 13")
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
(if window-system
	(global-hl-line-mode)
 )

; open file at cursor
(ffap-bindings)

(require 'recentf)
(recentf-mode 1)
(setq recentf-max-menu-items 25)
(setq recentf-max-saved-items 200)
(global-set-key "\C-x\ \C-r" 'recentf-open-files)

; <http://stackoverflow.com/questions/2068697/emacs-is-slow-opening-recent-files>
(setq recentf-keep '(file-remote-p file-readable-p))


;; Magit rules!
(global-set-key (kbd "C-x g") 'magit-status)

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


;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Auto Complete
;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'auto-complete-config)
(ac-config-default)

; auto-complete walkaround for linum
(ac-flyspell-workaround)
(ac-linum-workaround)

;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Uniquify
;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'uniquify) 
(setq 
  uniquify-buffer-name-style 'post-forward
  uniquify-separator ":")

;;;;;;;;;;;;;;;;;;;;;;;;;;
;; linum
;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'linum)
(global-linum-mode 1)
; (setq linum-format "%05d ")

;;;;;;;;;;;;;;;;;;;;;;;;;;
;; autopair
;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'autopair)
(autopair-global-mode) ;; to enable in all buffers



;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Emacs powerline
;;;;;;;;;;;;;;;;;;;;;;;;;;

(if window-system
    (progn
      (add-to-list 'load-path "~/.emacs.d/vendor/emacs-powerline")
      (require 'powerline) 
      )
)

; <http://stackoverflow.com/questions/4076360/error-in-dired-sorting-on-os-x>o
(when (eq system-type 'darwin)
  (require 'ls-lisp)
  (setq ls-lisp-use-insert-directory-program nil))

;; Hide DOT files with M-o
;(require 'dired-x)
;(setq-default dired-omit-files-p t) ; Buffer-local variable
										;(setq dired-omit-files (concat dired-omit-files "\\|^\\..+$"))

 ;; (defun my-ibuffer-hook ()
    
 ;;      ;; add another sorting method for ibuffer (allow the grouping of
 ;;      ;; filenames and dired buffers
    
 ;;      (ibuffer-define-sorter pathname
 ;;         (:documentation
 ;;          "Sort the buffers by their pathname."
 ;;          :description "path")
 ;;         (string-lessp (with-current-buffer (car a)
 ;;                         (or buffer-file-name
 ;;                             (if (eq major-mode 'dired-mode)
 ;;                                 (expand-file-name dired-directory))
 ;;                             ;; so that all non pathnames are at the end
 ;;                             "~"))
 ;;                       (with-current-buffer (car b)
 ;;                         (or buffer-file-name
 ;;                             (if (eq major-mode 'dired-mode)
 ;;                                 (expand-file-name dired-directory))
 ;;                             ;; so that all non pathnames are at the end
 ;;                             "~"))))
    
 ;;      ;; add key binding
    
 ;;      (define-key ibuffer-mode-map (kbd "s p") 'ibuffer-do-sort-by-pathname))
    
 ;;    (add-hook 'ibuffer-mode-hooks 'my-ibuffer-hook)

;;;;;;;;;;;;;;;;;;;;;;;;;;
;; IDO
;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'ido)
(ido-mode t)


  
(require 'setup-tabbar)
(require 'setup-spell)
(require 'setup-tex)
(require 'setup-python)
(require 'setup-n3)
(require 'setup-sparql)
(require 'setup-octave)
(require 'setup-prolog)
(require 'setup-markdown)
(require 'setup-cmake)
(require 'setup-misc)
