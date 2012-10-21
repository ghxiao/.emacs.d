(add-to-list 'load-path "~/.emacs.d")
(require 'package)
(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/") t)
(package-initialize)

(menu-bar-mode 1) 

(when (not package-archive-contents)
  (package-refresh-contents))

(defvar my-packages '(starter-kit auctex cmake-mode nlinum autopair color-theme markdown-mode)
  "A list of packages to ensure are installed at launch.")

(dolist (p my-packages)
  (when (not (package-installed-p p))
    (package-install p)))

(setq-default cursor-type 'bar) 


(getenv "PATH")
(setenv "PATH"   (concat  "/usr/texbin" ":" (getenv "PATH")))

(add-hook 'LaTeX-mode-hook 'TeX-source-correlate-mode)

(setq reftex-plug-into-AUCTeX t)
(setq TeX-auto-save t)
(setq TeX-parse-self t)
(setq-default TeX-master nil)
(setq TeX-source-specials-view-start-server t)
;(setq TeX-view-program-list
;  '(("Skim" "/Applications/Skim.app/Contents/SharedSupport/displayline %q")))


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
(server-start); start emacs in server mode so that skim can talk to it

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

(require 'color-theme)
(color-theme-charcoal-black)
