(add-to-list 'load-path "~/.emacs.d")
(require 'package)
(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/") t)
(package-initialize)

(menu-bar-mode 1) 

(when (not package-archive-contents)
  (package-refresh-contents))

(defvar my-packages '(starter-kit auctex cmake-mode nlinum autopair 
                                  color-theme markdown-mode)
  "A list of packages to ensure are installed at launch.")

(dolist (p my-packages)
  (when (not (package-installed-p p))
    (package-install p)))

(setq-default cursor-type 'bar) 

(setq ring-bell-function 'ignore)

(getenv "PATH")
(setenv "PATH"   (concat  "/usr/texbin" ":" (getenv "PATH")))
(setenv "PATH"   (concat  "/usr/local/bin" ":" (getenv "PATH")))

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

(require 'tex-site)
(add-hook 'LaTeX-mode-hook (lambda ()
  (push
    '("latexmk" "latexmk -pdf %s" TeX-run-TeX nil t
      :help "Run latexmk on file")
    TeX-command-list)))
(add-hook 'TeX-mode-hook '(lambda () (setq TeX-command-default "latexmk")))

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

(require 'linum)
(global-linum-mode 1)
(setq linum-format "%d ")

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
(color-theme-charcoal-black)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(TeX-command-list (quote (("latexmk" "latexmk -pdf %s" TeX-run-TeX nil t :help "Run latexmk on file") ("XeLaTeX" "xelatex %s" TeX-run-TeX nil t) ("TeX" "%(PDF)%(tex) %`%S%(PDFout)%(mode)%' %t" TeX-run-TeX nil (plain-tex-mode texinfo-mode ams-tex-mode) :help "Run plain TeX") ("LaTeX" "%`%l%(mode)%' %t" TeX-run-TeX nil (latex-mode doctex-mode) :help "Run LaTeX") ("Makeinfo" "makeinfo %t" TeX-run-compile nil (texinfo-mode) :help "Run Makeinfo with Info output") ("Makeinfo HTML" "makeinfo --html %t" TeX-run-compile nil (texinfo-mode) :help "Run Makeinfo with HTML output") ("AmSTeX" "%(PDF)amstex %`%S%(PDFout)%(mode)%' %t" TeX-run-TeX nil (ams-tex-mode) :help "Run AMSTeX") ("ConTeXt" "texexec --once --texutil %(execopts)%t" TeX-run-TeX nil (context-mode) :help "Run ConTeXt once") ("ConTeXt Full" "texexec %(execopts)%t" TeX-run-TeX nil (context-mode) :help "Run ConTeXt until completion") ("BibTeX" "bibtex %s" TeX-run-BibTeX nil t :help "Run BibTeX") ("View" "%V" TeX-run-discard-or-function t t :help "Run Viewer") ("Print" "%p" TeX-run-command t t :help "Print the file") ("Queue" "%q" TeX-run-background nil t :help "View the printer queue" :visible TeX-queue-command) ("File" "%(o?)dvips %d -o %f " TeX-run-command t t :help "Generate PostScript file") ("Index" "makeindex %s" TeX-run-command nil t :help "Create index file") ("Check" "lacheck %s" TeX-run-compile nil (latex-mode) :help "Check LaTeX file for correctness") ("Spell" "(TeX-ispell-document \"\")" TeX-run-function nil t :help "Spell-check the document") ("Clean" "TeX-clean" TeX-run-function nil t :help "Delete generated intermediate files") ("Clean All" "(TeX-clean t)" TeX-run-function nil t :help "Delete generated intermediate and output files") ("Other" "" TeX-run-command t t :help "Run an arbitrary command"))))
 '(safe-local-variable-values (quote ((reftex-plug-into-AUCTeX . t) (TeX-auto-save . t) (TeX-parse-self . t) (TeX-debug-bad-boxes . t) (whitespace-line-column . 80) (lexical-binding . t)))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
