;;;;;;;;;;;;;;;;;;;;;;;;;;
;; LaTeX
;;;;;;;;;;;;;;;;;;;;;;;;;;

(setenv "PATH"   (concat  "/usr/texbin" ":" (getenv "PATH")))

(require 'tex-site)

(setenv "TEXINPUTS"
  		(concat (getenv "TEXINPUTS") ":" "." ":" (getenv "HOME") "/Dropbox/Optique/osloSVN/LaTeX/latex" ":"          (getenv "HOME") "/Dropbox/Optique/osloSVN/LaTeX/img"))


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

; <http://tex.stackexchange.com/questions/124246/uninformative-error-message-when-using-auctex>
(setq LaTeX-command-style '(("" "%(PDF)%(latex) -file-line-error %S%(PDFout)")))


;; RefTeX also recognizes \addbibresource. 
(setq reftex-bibliography-commands '("bibliography" "nobibliography" "addbibresource"))

;; ;; add two environments from subcaption
;; (TeX-add-style-hook
;;  "subtable"
;;  (lambda ()
;;    (LaTeX-add-environments
;;     '("subtable" "width"))))

;; (TeX-add-style-hook
;;  "subfigure"
;;  (lambda ()
;;    (LaTeX-add-environments
;;     '("subfigure" "width"))))

;;set XeTeX mode in TeX/LaTeX
(add-hook 'LaTeX-mode-hook (lambda()
                             (add-to-list 'TeX-command-list '("XeLaTeX" "%`xelatex --synctex=1%(mode)%' %t" TeX-run-TeX nil t)))
)

;; (add-to-list 'helm-completing-read-handlers-alist
;;               '(TeX-command-master . nil) )

;; (add-to-list 'helm-completing-read-handlers-alist
;;               '(LaTeX-environment . nil) )



(provide 'setup-tex)


										
