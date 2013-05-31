;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Initialize Packages
;;;;;;;;;;;;;;;;;;;;;;;;;;

(add-to-list 'load-path "~/.emacs.d")
(require 'package)
(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/") t)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/") t)
(package-initialize)

(when (not package-archive-contents)
  (package-refresh-contents))

(defvar my-packages '(starter-kit 
                      auctex ;latex-pretty-symbols
                                  cmake-mode nlinum autopair 
                                  ecb
                                  color-theme color-theme-solarized  twilight-bright-theme
                                  color-theme-blackboard color-theme-sanityinc-tomorrow
                                  markdown-mode
                                        ; emacs-eclim company
                                  auto-complete 
                                  ac-math
                                        ;yasnippet 
                                  tidy
                                        ; ipython 
                                  python-mode epc deferred auto-complete jedi ein
                                  dsvn
                                  helm
                                  xclip
                                  sparql-mode
                                  )
  "A list of packages to ensure are installed at launch.")

(dolist (p my-packages)
  (when (not (package-installed-p p))
    (package-install p)))


;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Basci configuration
;;;;;;;;;;;;;;;;;;;;;;;;;;

; disable line hightling from starter-kit
(remove-hook 'prog-mode-hook 'esk-turn-on-hl-line-mode)

(setq-default cursor-type 'bar) 
(setq-default tab-width 4)

(setq ring-bell-function 'ignore)
(if (string= system-type "darwin")
    (set-default-font "Monoca 12")
)

(menu-bar-mode 1) 

(defun revert-buffer-no-confirm ()
    "Revert buffer without confirmation."
    (interactive) (revert-buffer t t))

(global-set-key (kbd "<f5>") 'revert-buffer-no-confirm) 

(global-set-key [(control x) (k)] 'kill-this-buffer)

(setenv "PATH"   (concat  "/usr/texbin" ":" (getenv "PATH")))
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

;(tabbar-mode 1)
;(custom-set-variables
; '(tabbar-separator (quote (1.0))))

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


;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Python 
;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (require 'python-mode)
;; (add-to-list 'auto-mode-alist '("\\.py\\'" . python-mode))

(setq jedi:setup-keys t)
(add-hook 'python-mode-hook 'jedi:setup)

(require 'ein)
(setq ein:use-auto-complete t)
(setq ein:use-smartrep t)

;; ipdb configuration
(defun python-add-breakpoint ()
  (interactive)
  (py-newline-and-indent)
  (insert "import ipdb; ipdb.set_trace()")
  (highlight-lines-matching-regexp "^[ ]*import ipdb; ipdb.set_trace()"))


; (define-key py-mode-map (kbd "C-c C-t") 'python-add-breakpoint)            
(defun annotate-pdb ()
  (interactive)
;  (highlight-lines-matching-regexp "import pdb")
;  (highlight-lines-matching-regexp "pdb.set_trace()")
 (highlight-lines-matching-regexp "^[ ]*import ipdb; ipdb.set_trace()")
)

(add-hook 'python-mode-hook 'annotate-pdb)

(if (string= system-type "darwin")
    (setenv "PYTHONPATH" "/usr/local/lib/python2.7/site-packages")
  ; linux server
  (setenv "PYTHONPATH" "/home/xiao/usr/local/lib/python2.7/site-packages")
)

;;;;;;;;;;;;;;;;;;;;;;;;;;
;; LaTeX
;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq reftex-plug-into-AUCTeX t)
(setq TeX-auto-save t)
(setq TeX-parse-self t)
(setq-default TeX-master nil)
(setq TeX-source-specials-view-start-server t)

; LaTeX-math-mode
(add-hook 'LaTeX-mode-hook 'LaTeX-math-mode)
(setq TeX-insert-braces nil)

(add-hook 'LaTeX-mode-hook 'TeX-source-correlate-mode)
(add-hook 'LaTeX-mode-hook 'TeX-PDF-mode) 
(add-hook 'LaTeX-mode-hook 'turn-on-reftex) 

; autocomplete for latex
(require 'ac-math)

(add-to-list 'ac-modes 'latex-mode)   ; make auto-complete aware of `latex-mode`

(defun ac-latex-mode-setup ()         ; add ac-sources to default ac-sources
  (setq ac-sources
     (append '(ac-source-math-unicode ac-source-math-latex ac-source-latex-commands)
               ac-sources)))

(add-hook 'latex-mode-hook 'ac-latex-mode-setup)
(ac-flyspell-workaround)

; (require 'latex-pretty-symbols)

; walkaround for dollar pair insertion in autopair-mode
; see <http://code.google.com/p/autopair/issues/detail?id=18>
(add-hook 'TeX-mode-hook
          #'(lambda ()
              (modify-syntax-entry ?$ "\"")
	      (autopair-mode)))

(add-hook 'TeX-mode-hook '(lambda () (setq TeX-command-default "LaTeX")))
  (setq TeX-view-program-selection '((output-pdf "PDF Viewer"))) 

(setq TeX-save-query nil) ;;autosave before compiling

(if (string= system-type "darwin" )
  (setq TeX-view-program-list '(("PDF Viewer" 
                               "/Applications/Skim.app/Contents/SharedSupport/displayline -r -b %n %o %b"))) 
)


(setq LaTeX-command "latex -synctex=1")

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ecb-options-version "2.40")
 '(safe-local-variable-values (quote ((TeX-master . \.\./main)))))

;; (add-to-list 'helm-completing-read-handlers-alist
;;              '(Tex-command-master . nil) )

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

(require 'color-theme)

;(load-theme 'zenburn t)


; (require 'twilight-bright-theme)

;; (if (string= system-type "darwin")
;;     (if (not (display-graphic-p))
;;         (color-theme-solarized-dark)
;;     )
;; )
(require 'color-theme-solarized)
(require 'color-theme-sanityinc-tomorrow)

;; (if window-system
;;      (color-theme-sanityinc-tomorrow-eighties)
;; )

(if (not window-system)   
   (color-theme-solarized-dark)   
)

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(mode-line ((t (:foreground "#030303" :background "#bdbdbd" :box nil))))
 '(mode-line-inactive ((t (:foreground "#f9f9f9" :background "#666666" :box nil)))))

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


(require 'ecb-autoloads)
(setq ecb-tip-of-the-day nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Games
;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq tetris-score-file "~/.emacs.d/tetris-scores")
