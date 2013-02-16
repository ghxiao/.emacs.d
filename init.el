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

(defvar my-packages '(starter-kit auctex cmake-mode nlinum autopair 
                                        ; ecb-snapshot
                                  color-theme color-theme-solarized markdown-mode ;cedet
                                        ; emacs-eclim company
                                  auto-complete 
                                        ;yasnippet 
                                  tidy
                                        ; ipython 
                                  python-mode epc deferred auto-complete jedi ein
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
;    (set-default-font "Monoca 12")
    (set-default-font "Menlo 12")
)
(menu-bar-mode 1) 

(defun revert-buffer-no-confirm ()
    "Revert buffer without confirmation."
    (interactive) (revert-buffer t t))

(global-set-key (kbd "<f5>") 'revert-buffer-no-confirm) 

(setenv "PATH"   (concat  "/usr/texbin" ":" (getenv "PATH")))
(setenv "PATH"   (concat  "/usr/local/bin" ":" (getenv "PATH")))

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

(require 'python-mode)
(add-to-list 'auto-mode-alist '("\\.py\\'" . python-mode))

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
  (highlight-lines-matching-regexp "import pdb")
  (highlight-lines-matching-regexp "pdb.set_trace()"))

(add-hook 'python-mode-hook 'annotate-pdb)

(if (string= system-type "darwin")
    (setenv "PYTHONPATH" "/usr/local/lib/python2.7/site-packages")
  (setenv "PYTHONPATH" "/home/xiao/usr/local/lib/python2.7/site-packages")
)

;;;;;;;;;;;;;;;;;;;;;;;;;;
;; LaTeX
;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'tex-site)

(setq reftex-plug-into-AUCTeX t)
(setq TeX-auto-save t)
(setq TeX-parse-self t)
(setq-default TeX-master nil)
(setq TeX-source-specials-view-start-server t)

(add-hook 'LaTeX-mode-hook 'TeX-source-correlate-mode)
(add-hook 'LaTeX-mode-hook 'TeX-PDF-mode) 
(add-hook 'LaTeX-mode-hook 'turn-on-reftex) 

(add-hook 'TeX-mode-hook '(lambda () (setq TeX-command-default "LaTeX")))
  (setq TeX-view-program-selection '((output-pdf "PDF Viewer"))) 

(if (string= system-type "darwin" )
;; use Skim as default pdf viewer 
;; Skim's displayline is used for forward search (from .tex to .pdf) 
;; option -b highlights the current line; option -g opens Skim in the
;; background 
  (setq TeX-view-program-list '(("PDF Viewer" 
                               "/Applications/Skim.app/Contents/SharedSupport/displayline -b %n %o %b"))) 
)

(custom-set-variables
 '(safe-local-variable-values 
   (quote ((TeX-master . \.\./main) 
           (reftex-plug-into-AUCTeX . t) 
           (TeX-auto-save . t) 
           (TeX-parse-self . t) 
           (TeX-debug-bad-boxes . t) 
           (whitespace-line-column . 80) 
           (lexical-binding . t)))))


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
; (global-linum-mode 1)
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
                                )
                              auto-mode-alist))

;;;;;;;;;;;;;;;;;;;;;;;;;;
;; SPARQL
;;;;;;;;;;;;;;;;;;;;;;;;;;

(add-to-list 'load-path "~/.emacs.d/vendor/sparql-mode")
(autoload 'sparql-mode "sparql-mode.el"
  "Major mode for editing SPARQL files" t)
(add-to-list 'auto-mode-alist '("\\.sparql$" . sparql-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;
;; DIR Tree
;;;;;;;;;;;;;;;;;;;;;;;;;;

(add-to-list 'load-path "~/.emacs.d/vendor")
(require 'dirtree)
(require 'tree-mode)
(require 'windata)
(autoload 'dirtree "dirtree" "Add directory to tree view")

;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Color Theme
;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'color-theme)

(require 'color-theme-solarized)

(if (string= system-type "darwin")
    (if (not (display-graphic-p))
        (color-theme-solarized-dark)
      )
)
; (require 'color-theme-mods)
; (color-theme-billc)
; (bc-color-theme)



;; ;; predictive install location
;; (add-to-list 'load-path "~/.emacs.d/vendor/predictive/")
;; ;; dictionary locations
;; (add-to-list 'load-path "~/.emacs.d/vendor/predictive/latex/")
;; (add-to-list 'load-path "~/.emacs.d/vendor/predictive/html/")
;; ;; load predictive package
;; (require 'predictive)



; (require 'viper)
; (setq viper-mode t)

; CEDET

;; Load CEDET.
;; See cedet/common/cedet.info for configuration details.
;; IMPORTANT: For Emacs >= 23.2, you must place this *before* any
;; CEDET component (including EIEIO) gets activated by another 
;; package (Gnus, auth-source, ...).

; (semantic-load-enable-excessive-code-helpers)

;; Enable EDE (Project Management) features
; (global-ede-mode 1)

;; Enable EDE for a pre-existing C++ project
;; (ede-cpp-root-project "NAME" :file "~/myproject/Makefile")


;; Enabling Semantic (code-parsing, smart completion) features
;; Select one of the following:

;; * This enables the database and idle reparse engines
;(semantic-load-enable-minimum-features)

;; * This enables some tools useful for coding, such as summary mode,
;;   imenu support, and the semantic navigator
;(semantic-load-enable-code-helpers)

;; * This enables even more coding tools such as intellisense mode,
;;   decoration mode, and stickyfunc mode (plus regular code helpers)
; (semantic-load-enable-gaudy-code-helpers)

;; * This enables the use of Exuberant ctags if you have it installed.
;;   If you use C++ templates or boost, you should NOT enable it.
;; (semantic-load-enable-all-exuberent-ctags-support)
;;   Or, use one of these two types of support.
;;   Add support for new languages only via ctags.
;; (semantic-load-enable-primary-exuberent-ctags-support)
;;   Add support for using ctags as a backup parser.
; (semantic-load-enable-secondary-exuberent-ctags-support)

;; Enable SRecode (Template management) minor-mode.
;; (global-srecode-minor-mode 1)

;; ;; ecb 
;; (add-to-list 'load-path
;;              "~/opt/ecb-2.40")
;; (setq stack-trace-on-error t)
;; (require 'ecb)
;; (require 'ecb-autoloads)
