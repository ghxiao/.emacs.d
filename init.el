
(add-to-list 'load-path "~/.emacs.d")

(getenv "PATH")
(setenv "PATH"   (concat  "/usr/texbin" ":" (getenv "PATH")))
(setenv "PATH"   (concat  "/usr/local/bin" ":" (getenv "PATH")))

; (load-file "~/opt/cedet-1.1/common/cedet.el")

(require 'package)
(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/") t)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/") t)
(package-initialize)

(menu-bar-mode 1) 

(set-default-font "Monoca 14")

(when (not package-archive-contents)
  (package-refresh-contents))

(defvar my-packages '(starter-kit auctex cmake-mode nlinum autopair 
                                        ; ecb-snapshot
                                  color-theme markdown-mode ;cedet
                                  emacs-eclim company
                                  auto-complete yasnippet tidy
                                        ; ipython
                                  epc deferred auto-complete jedi ein
)
  "A list of packages to ensure are installed at launch.")


(dolist (p my-packages)
  (when (not (package-installed-p p))
    (package-install p)))

(setq-default cursor-type 'bar) 

(setq ring-bell-function 'ignore)

(require 'ein)
(setq ein:use-auto-complete t)
(setq ein:use-smartrep t)

(autoload 'jedi:setup "jedi" nil t)
(add-hook 'python-mode-hook 'jedi:setup)
(setq jedi:setup-keys t)

(setq flyspell-issue-welcome-flag nil)
(setq ispell-program-name "/usr/local/bin/aspell")

(add-hook 'LaTeX-mode-hook 'TeX-source-correlate-mode)

(setq reftex-plug-into-AUCTeX t)
(setq TeX-auto-save t)
(setq TeX-parse-self t)
(setq-default TeX-master nil)
(setq TeX-source-specials-view-start-server t)
;(setq TeX-view-program-list
;  '(("Skim" "/Applications/Skim.app/Contents/SharedSupport/displayline %q")))

; (require 'auto-complete)
(require 'auto-complete-config)
(ac-config-default)
;(auto-complete-mode 1) 

(require 'tex-site)
;(add-hook 'LaTeX-mode-hook (lambda ()
;  (push
;    '("latexmk" "latexmk -pdf %s" TeX-run-TeX nil t
;      :help "Run latexmk on file")
;    TeX-command-list)))
(add-hook 'TeX-mode-hook '(lambda () (setq TeX-command-default "LaTeX")))

;; use Skim as default pdf viewer 
;; Skim's displayline is used for forward search (from .tex to .pdf) 
;; option -b highlights the current line; option -g opens Skim in the
;; background 
(setq TeX-view-program-selection '((output-pdf "PDF Viewer"))) 
(setq TeX-view-program-list '(("PDF Viewer" 
                               "/Applications/Skim.app/Contents/SharedSupport/displayline -b -g %n %o %b"))) 
; (server-start); start emacs in server mode so that skim can talk to it

(add-hook 'LaTeX-mode-hook 'TeX-PDF-mode) 
(add-hook 'LaTeX-mode-hook 'turn-on-reftex)   ; with AUCTeX LaTeX mode

(require 'uniquify) 
(setq 
  uniquify-buffer-name-style 'post-forward
  uniquify-separator ":")

; auto-complete walkaround for linum
(ac-flyspell-workaround)
(ac-linum-workaround)


(require 'linum)
; (global-linum-mode 1)
; (setq linum-format "%05d ")

(require 'cmake-mode)
(setq auto-mode-alist
      (append '(("CMakeLists\\.txt\\'" . cmake-mode)
		("\\.cmake\\'" . cmake-mode))
	      auto-mode-alist))

(require 'markdown-mode)
(setq auto-mode-alist
      (append '(("\\.md$" . markdown-mode)
		("\\.markdown$'" . markdown-mode))
	      auto-mode-alist))

(require 'autopair)
(autopair-global-mode) ;; to enable in all buffers

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


(add-to-list 'load-path "~/.emacs.d/vendor/sparql-mode")
(autoload 'sparql-mode "sparql-mode.el"
  "Major mode for editing SPARQL files" t)
(add-to-list 'auto-mode-alist '("\\.sparql$" . sparql-mode))

(add-to-list 'load-path "~/.emacs.d/vendor")
(require 'dirtree)
(require 'tree-mode)
(require 'windata)
(autoload 'dirtree "dirtree" "Add directory to tree view")

(setq-default tab-width 4)
(require 'color-theme)

(add-to-list 'load-path "~/.emacs.d/vendor/emacs-color-theme-solarized")
; color-theme-solarized-[dark|light]
(require 'color-theme-solarized)
(color-theme-solarized-dark)
; (color-theme-charcoal-black)

; (require 'color-theme-mods)
; (color-theme-billc)
; (bc-color-theme)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(TeX-command-list (quote (("LaTeX" "%`%l%(mode)%' %t" TeX-run-TeX nil (latex-mode doctex-mode) :help "Run LaTeX") ("XeLaTeX" "xelatex %s" TeX-run-TeX nil t) ("TeX" "%(PDF)%(tex) %`%S%(PDFout)%(mode)%' %t" TeX-run-TeX nil (plain-tex-mode texinfo-mode ams-tex-mode) :help "Run plain TeX") ("Makeinfo" "makeinfo %t" TeX-run-compile nil (texinfo-mode) :help "Run Makeinfo with Info output") ("Makeinfo HTML" "makeinfo --html %t" TeX-run-compile nil (texinfo-mode) :help "Run Makeinfo with HTML output") ("AmSTeX" "%(PDF)amstex %`%S%(PDFout)%(mode)%' %t" TeX-run-TeX nil (ams-tex-mode) :help "Run AMSTeX") ("ConTeXt" "texexec --once --texutil %(execopts)%t" TeX-run-TeX nil (context-mode) :help "Run ConTeXt once") ("ConTeXt Full" "texexec %(execopts)%t" TeX-run-TeX nil (context-mode) :help "Run ConTeXt until completion") ("BibTeX" "bibtex %s" TeX-run-BibTeX nil t :help "Run BibTeX") ("View" "%V" TeX-run-discard-or-function t t :help "Run Viewer") ("Print" "%p" TeX-run-command t t :help "Print the file") ("Queue" "%q" TeX-run-background nil t :help "View the printer queue" :visible TeX-queue-command) ("File" "%(o?)dvips %d -o %f " TeX-run-command t t :help "Generate PostScript file") ("Index" "makeindex %s" TeX-run-command nil t :help "Create index file") ("Check" "lacheck %s" TeX-run-compile nil (latex-mode) :help "Check LaTeX file for correctness") ("Spell" "(TeX-ispell-document \"\")" TeX-run-function nil t :help "Spell-check the document") ("Clean" "TeX-clean" TeX-run-function nil t :help "Delete generated intermediate files") ("Clean All" "(TeX-clean t)" TeX-run-function nil t :help "Delete generated intermediate and output files") ("Other" "" TeX-run-command t t :help "Run an arbitrary command"))))
 '(ecb-options-version "2.40")
 '(safe-local-variable-values (quote ((TeX-master . \.\./main) (reftex-plug-into-AUCTeX . t) (TeX-auto-save . t) (TeX-parse-self . t) (TeX-debug-bad-boxes . t) (whitespace-line-column . 80) (lexical-binding . t)))))


;; predictive install location
(add-to-list 'load-path "~/.emacs.d/vendor/predictive/")
;; dictionary locations
(add-to-list 'load-path "~/.emacs.d/vendor/predictive/latex/")
(add-to-list 'load-path "~/.emacs.d/vendor/predictive/html/")
;; load predictive package
(require 'predictive)

(defun revert-buffer-no-confirm ()
    "Revert buffer without confirmation."
    (interactive) (revert-buffer t t))

(global-set-key (kbd "<f5>") 'revert-buffer-no-confirm) 

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


;;  (load-file "/Users/xiao/opt/emacs-for-python/epy-init.el")
;;  (add-to-list 'load-path "/Users/xiao/opt/emacs-for-python") ;; tell where to
;; ;; load the various files
;;  (require 'epy-setup)      ;; It will setup other loads, it is
;; ;; required!
;;  (require 'epy-python)     ;; If you want the python facilities
;; ;; [optional]
;;  (require 'epy-completion) ;; If you want the autocompletion settings
;; ;; [optional]
;;  (require 'epy-editing)    ;; For configurations related to editing
;; ;; [optional]
;;  (require 'epy-bindings)   ;; For my suggested keybindings [optional]
;;  (require 'epy-nose)       ;; For nose integration

(setenv "PYTHONPATH" "/usr/local/lib/python2.7/site-packages")
