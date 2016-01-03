;;;;;;;;;;;;;;;;;;;;;;;;;;
;; LaTeX
;;;;;;;;;;;;;;;;;;;;;;;;;;

(setenv "PATH"   (concat  "/usr/texbin" ":" (getenv "PATH")))
(setenv "PATH"   (concat  "/Library/TeX/texbin" ":" (getenv "PATH")))

(require 'tex-site)

(setenv "TEXINPUTS"
  		(concat (getenv "TEXINPUTS")
				":" (getenv "HOME") "/Dropbox/Optique/osloSVN/LaTeX/latex"
				":" (getenv "HOME") "/Dropbox/Optique/osloSVN/LaTeX/img"))


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

;;set XeTeX mode in TeX/LaTeX
(add-hook 'LaTeX-mode-hook (lambda()
                             (add-to-list 'TeX-command-list '("XeLaTeX" "%`xelatex --synctex=1%(mode)%' %t" TeX-run-TeX nil t)))
)

;; auxtex-latexmk
(require 'auctex-latexmk)
(auctex-latexmk-setup)


;; (add-to-list 'helm-completing-read-handlers-alist
;;               '(TeX-command-master . nil) )

;; (add-to-list 'helm-completing-read-handlers-alist
;;               '(LaTeX-environment . nil) )


;;;----------------------------------------------------------------------------
;;; splitting and merging of LaTeX files
;;;----------------------------------------------------------------------------

(defun my-LaTeX-inputs-merge (&optional arg)
  "Replace \\input{...} commands in LaTeX files with the corresponding file
content.
With optional prefix arg, the \\input{...} commands are expanded recursively.
If an included file contains an \\endinput command, the content of the file
from that point till the end of the file is removed.
"
  (interactive "*P")
  (save-restriction
    (when (and (boundp 'transient-mark-mode)
             transient-mark-mode mark-active)
      (narrow-to-region (point) (mark))
      (deactivate-mark))
    (goto-char (point-min))
    (while (re-search-forward "^[ \t]*\\(\\\\input\\){\\([^} ]+\\)}"
                              nil 'no-error)
      (let* ((input-name (match-string 2))
             (file-name (concat (file-name-directory buffer-file-name)
                                (if (file-name-extension input-name)
                                    input-name
                                  (concat input-name ".tex")))))
        (when (and (file-readable-p file-name)
                   (file-regular-p file-name))
          (replace-match "INPUT-BEGIN" t t nil 1)
          (beginning-of-line)
          (insert-char ?% 78)
          (insert "\n%%%")
          (forward-line 1)
          (insert "\n%%%INPUT-END{" input-name "}\n")
          (insert-char ?% 78)
          (insert "\n")
          (forward-line -3)
          (insert-file-contents file-name)
          (save-excursion
            (let ((beg (point))
                  (end))
              (re-search-forward (concat "^%%%INPUT-END{" input-name "}$")
                                 nil 'no-error)
              (setq end (match-beginning 0))
              (goto-char beg)
              (when (re-search-forward "^[ \t]*\\\\endinput"
                                       end 'no-error)
                (delete-region (match-beginning 0) end))))
          (if (not arg)
              (re-search-forward (concat "^%%%INPUT-END{"
                                         input-name "}$")
                                 nil 'no-error)))))))



(provide 'setup-tex)


