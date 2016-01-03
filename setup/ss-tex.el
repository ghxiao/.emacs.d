;;; ss-tex.el --- initialization for major modes related to TeX, LaTeX, BibTex

;;;----------------------------------------------------------------------------
;;; MAJOR MODES RELATED TO TeX, LaTeX, BibTex
;;;----------------------------------------------------------------------------

;;;----------------------------------------------------------------------------
;;; Load aucted and set correct load-path and directories
;;;----------------------------------------------------------------------------

(load-library "auctex")
;;(load-library "preview-latex")

(setq load-path (remove TeX-lisp-directory load-path))
(setq TeX-lisp-directory
      (expand-file-name "auctex" (file-name-directory load-file-name)))
(add-to-list 'load-path TeX-lisp-directory)

(setq TeX-data-directory TeX-lisp-directory)
(setq TeX-auto-global (concat TeX-lisp-directory "/auto/"))


(setq TeX-shell "/bin/tcsh")

;;;----------------------------------------------------------------------------
;;; AUC TeX mode
;;;----------------------------------------------------------------------------

(eval-after-load "latex"
  '(progn
     (require 'font-latex)

     (require 'reftex)
     (diminish 'reftex-mode)

     (require 'bib-cite)
     (diminish 'bib-cite-minor-mode)

     (require 'outline)
     (setq outline-minor-mode-prefix "\C-c\C-o")
     (diminish 'outline-minor-mode)

     (require 'flyspell)
     (diminish 'flyspell-mode)

     ;; (defadvice TeX-command-filter
     ;; (before TeX-pipe-fill activate)
     ;; (when (< (length string) 80)
     ;;  (sleep-for 0.05)
     ;;  ))

     ;; (require 'xdvi-search)
     ;; (require 'txs-search)
     ;; (setq txs-path "/Applications/-Programs/Writing/TeXniscope.app")

     ;; (require 'light)
     ;; (require 'ultex-setup)

     ;; (LaTeX-install-toolbar)
     ;; (setq TeX-bar-LaTeX-buttons
     ;;       '(open-file save-buffer cut copy paste undo
     ;;                   [separator nil]
     ;;                   latex next-error view bibtex
     ;;                   LaTeX-symbols-experimental))
     ;;
     ;; (setq TeX-macro-global
     ;;       '("/usr/local/teTeX/share/texmf.local/tex/"
     ;;         "/usr/local/teTeX/share/texmf.gwtex/tex/"
     ;;         "/usr/local/teTeX/share/texmf.tetex/tex/"
     ;;         "/usr/local/teTeX/share/texmf/tex/"
     ;;         "~/Library/texmf/tex/"))

     (setq TeX-command-list
           (append TeX-command-list
                   '(("2up" "lncs2side -o 0cm 0cm -d \"0cm -6.5cm\" -s 1.25 -n \"1x2\" -f %s.pdf" TeX-run-command t t
                      :help "Run script for 2up visualization"))
                   ))

     (let ((V-value (assoc "%V" TeX-expand-list)))
       (setq TeX-expand-list
             (append
              (delete V-value TeX-expand-list)
              '(("%V"
                 (lambda ()         ; EnricoFranconi: this is the only change
                   (TeX-source-correlate-start-server-maybe)
                   (cond ((equal TeX-view-format "dvi")
                          (TeX-output-style-check (TeX-output-dvi-view-style)))
                         ((equal TeX-view-format "ps")
                          (TeX-output-style-check (TeX-output-ps-view-style)))
                         ((equal TeX-view-format "pdf")
                          (TeX-output-style-check (TeX-output-pdf-view-style)))
                         )))
                ("%O" (lambda ()
                        (expand-file-name
                         "%s.pdf"
                         (TeX-master-directory))))
                ("%(A4)" (lambda ()
                           (if TeX-a4-paper "-ta4" "")))
                ))))

     (defun TeX-output-dvi-view-style ()
       `(("^dvi$" "." ,(concat dvi-previewer-program " %d"))
         ("^pdf$" "." ,(concat pdf-previewer-program " %o" " %b"))
         ("^html?$" "." "mozilla %o")
         ))

     (defun TeX-output-ps-view-style ()
       `(("^dvi$" "." ,(concat (TeX-dvips-command) " ; "
                               ps-previewer-program " %f"))
         ("^pdf$" "." ,(concat pdf-previewer-program " %o" " %b"))
         ("^html?$" "." "mozilla %o")
         ))

     (defun TeX-output-pdf-view-style ()
       `(("^dvi$" "." ,(concat (TeX-dvipdf-command) " ; "
                               pdf-previewer-program " %s.pdf" " %b"))
         ("^pdf$" "." ,(concat pdf-previewer-program " %o" " %b"))
         ("^html?$" "." "mozilla %o")
         ))

     (easy-menu-define TeX-options-menu
       LaTeX-mode-map
       "Options menu used in TeX mode."
       (list "TeX-Prefs"
             ["A4 paper" (setq TeX-a4-paper (not TeX-a4-paper))
              :style radio
              :selected TeX-a4-paper]
             (list "View format"
                   ["Dvi" (setq TeX-view-format "dvi")
                    :style radio
                    :selected (equal TeX-view-format "dvi")]
                   ["Postscript" (setq TeX-view-format "ps")
                    :style radio
                    :selected (equal TeX-view-format "ps")]
                   ["PDF" (setq TeX-view-format "pdf")
                    :style radio
                    :selected (equal TeX-view-format "pdf")]
                   )
             ))

     (easy-menu-add TeX-options-menu LaTeX-mode-map)

     (custom-set-variables
      '(LaTeX-paragraph-commands
        '("vfill" "input" "reminder" "skipline" "overlay" "backline" "pause")
        now))

     (setq TeX-clean-default-intermediate-suffixes
           (delete "\\.bbl" TeX-clean-default-intermediate-suffixes))

     (setq TeX-clean-default-output-suffixes
           (cons "\\.bbl" TeX-clean-default-output-suffixes))

     ;;; The following does not seem to have any effect.
     ;(font-latex-add-keywords
     ; '(("cref" "{") 'reference)

     )) ; eval-after-load "latex"


;;; Defined analogously to TeX-view, using TeX-master-file instead of
;;; TeX-active-master. We bind it later to C-c C-v
(defun my-TeX-view-master ()
  "Start a viewer on master file without confirmation."
  (interactive)
  (let ((output-file (TeX-master-file (TeX-output-extension))))
    (if (file-exists-p output-file)
	(TeX-command "View" 'TeX-active-master 0)
      (message "Output file %S does not exist." output-file))))


(defvar TeX-a4-paper t
  "A4 switch used for dvips and dvipdf conversion.")

(defun TeX-dvips-command ()
  "dvips %(A4) -Ppdf -G0 %d -o %f ")

(defun TeX-dvipdf-command ()
  (if TeX-a4-paper
      "dvips %(A4) -Ppdf -G0 %d -o %f ; gs -dNOPAUSE -dBATCH -sDEVICE=pdfwrite -dAutoFilterColorImages=false -dColorImageFilter=/FlateEncode -dAutoFilterGrayImages=false -dGrayImageFilter=/FlateEncode -sOutputFile=%s.pdf -sPAPERSIZE=a4 %f "
    "dvips -Ppdf -G0 %d -o %f ; gs -dNOPAUSE -dBATCH -sDEVICE=pdfwrite -dAutoFilterColorImages=false -dColorImageFilter=/FlateEncode -dAutoFilterGrayImages=false -dGrayImageFilter=/FlateEncode -sOutputFile=%s.pdf %f "))

(defvar TeX-view-format "pdf"
  "Preferred view format: dvi, ps, pdf.")

(defvar pdf-previewer-program
  (if darwinp
      ;;"open -a texniscope"
      ;;"open -a pdfview"
      ;;"/Applications/-Programs/Writing/PDFView.app/Contents/MacOS/gotoline.sh %n"
      ;;"open -a skim"
      "/Applications/-Programs/Writing/Skim.app/Contents/SharedSupport/displayline %n"
    "acroread")
  "PDF previewer.")

(defvar ps-previewer-program
  (if darwinp "open" "gv")
  "PS previewer.")

(defvar dvi-previewer-program
  (if darwinp
      "/Applications/-Programs/Writing/Skim.app/Contents/SharedSupport/displayline %n"
    ;;"open -a skim"
    "xdvi")
  ;;
  ;; "xdvi %dS"
  ;; Note:
  ;; - %dS activates the emacs server (calling `start-server') and
  ;;   is expanded to "-editor %cS"
  ;; - Then, %cS is expanded to something like "emacsclient --no-wait +%%l %%f"
  ;;   (there is a bug, since it does generate the full path to emacsclient)
  ;; Hence, we start the server in advance and hardcode the editor through the
  ;; environment variables EDITOR, XEDITOR, VISUAL (set in .profile, .cshrc,
  ;; and in environment.plist), and omit %dS
  ;; "xdvi"
  "DVI previewer.")

;;(require 'tex-site)

(setq-default TeX-master 'dwim) ; Do what I mean regarding the master file
;; (setq TeX-region "_region_") ;default value
;; (setq LaTeX-header-end "\\\\begin *{document}"); default value
;; (setq LaTeX-trailer-start "\\\\bibliography{");
;; (setq LaTeX-trailer-start "\\\\end *{document}"); default value

(setq TeX-parse-self t) ; Enable parse on load.
(setq TeX-auto-save t) ; Enable parse on save.
(setq TeX-auto-untabify nil)  ; Leaves tabs as tabs.
(setq TeX-insert-braces nil)  ; Does not insert braces after a macro

(setq TeX-brace-indent-level 1)
;; (setq LaTeX-indent-level 4)
;; (setq LaTeX-item-indent -2)

(setq LaTeX-beamer-item-overlay-flag nil) ; see file auctex/style/beamer.el

(setq LaTeX-section-label
      '(("chapter" . "chapter:")
	("section" . "sec:")
	("subsection" . "sec:")))

;; Environment labels

(defvar LaTeX-environment-label
  '(("figure" .       "fig:")
    ("figure*" .      "fig:")
    ("table" .        "tab:")
    ("table*" .       "tab:")
    ("theorem" .      "thm:")
    ("definition" .   "def:")
    ("proposition" .  "prop:")
    ("lemma" .        "lem:")
    ("corollary" .    "cor:")
    ("claim" .        "claim:")
    ("example" .      "exa:")
    ("examples" .     "exa:")
    ("construction" . "con:")
    ("algorithm" .    "alg:")
    ("equation" .     "eqn:")
    ("eqnarray" .     "eqn:"))
  "*Default prefixes to labels for environments.")
(make-variable-buffer-local 'LaTeX-environment-label)

(defun LaTeX-label-environments (environment)
  "Function called to insert a label for ENVIRONMENT at point."
  (let (label)
    (let ((prefix
	   (cond
	    ((assoc environment LaTeX-environment-label)
	     (cdr (assoc environment LaTeX-environment-label)))
	    ((assoc environment LaTeX-section-list)
	     (cond
	      ((stringp LaTeX-section-label) LaTeX-section-label)
	      ((and (listp LaTeX-section-label)
		    (assoc environment LaTeX-section-label))
	       (cdr (assoc environment LaTeX-section-label)))
	      (t nil)))
	    (t "")))
	  )
      (setq label (completing-read (TeX-argument-prompt t nil "What label")
                                   (LaTeX-label-list) nil nil
				   prefix))
      (if (or (string= label "") (string= prefix label))
	  (setq label nil))     ; No label entered
      (if label
	  (progn
	    (insert TeX-esc "label" TeX-grop label TeX-grcl)
	    (LaTeX-add-labels label)
	    label)
	nil))))

(defun LaTeX-env-comment (environment)
  "Create comment ENVIRONMENT with surrounding comments."
  (LaTeX-insert-environment environment)
  (end-of-line 2)
  (insert "\n%%%%%%%%%%%%%%%\n")
  (beginning-of-line -1)
  (LaTeX-find-matching-begin)
  (insert "%%%%%%%%%%%%%%%\n")
  (forward-line 1))

(defun LaTeX-add-theorem-environments ()
  (LaTeX-add-environments
   '("theorem" LaTeX-env-label)
   '("definition" LaTeX-env-label)
   '("proposition" LaTeX-env-label)
   '("lemma" LaTeX-env-label)
   '("corollary" LaTeX-env-label)
   '("claim" LaTeX-env-label)
   '("example" LaTeX-env-label)
   '("examples" LaTeX-env-label)
   '("construction" LaTeX-env-label)
   '("algorithm" LaTeX-env-label)
   '("comment" LaTeX-env-comment)
   ))

;;; note: the following variable is reset by reftex
(setq LaTeX-label-function 'LaTeX-label-environments)
(add-hook 'LaTeX-mode-hook 'LaTeX-add-theorem-environments)

;; Indentation
(setq LaTeX-indent-environment-list nil)
(setq LaTeX-verbatim-regexp
      (concat "\\("
              "verbatim\\*?\\|alltt\\|boxedverbatim"
              "\\|\\(short\\|par\\|large\\)?code\\(inline\\)?"
              "\\|screen\\(\\(in\\)?line\\)?\\|syntax\\|algorithm\\|program"
              "\\)"
              ))
(setq LaTeX-begin-regexp "begin\\b\\|\\[")
(setq LaTeX-end-regexp "end\\b\\|\\]")
(setq LaTeX-indent-ignore-environment-regexp
      "comment[^}]*")
(setq LaTeX-no-indent-environment-regexp
      (concat "\\("
              "proof\\(sk\\)?"
              "\\|construction"
              "\\|\\(no\\)?\\(frame\\|slide[ct*]?\\)"
              "\\|sloppypar"
              "\\)"
              ))

(setq LaTeX-item-regexp "\\(bib\\|step\\)?item\\b")

;; Due to the use of framepop

(when nil ; use-framepop
  (eval-after-load "framepop"
    '(progn
       (framepop-wrap 'TeX-next-error "*TeX Help*")
       (framepop-wrap 'TeX-run-background "*TeX background*")
       ;;; framepop implemented directly
       ;; (framepop-wrap 'TeX-recenter-output-buffer '(TeX-active-buffer))
       ;; (defadvice TeX-recenter-output-buffer
       ;;   (after framepop-goto-end-of-buffer act)
       ;;   "Position point at the end of output buffer in framepop window"
       ;;   (framepop-eob))
       ))

  (defun my-TeX-recenter-output-buffer ()
    "Redisplay buffer of TeX job output so that most recent output can be seen."
    (interactive)
    (let ((buffer (TeX-active-buffer))
          (framepop-in-wrap t))
      (if (bufferp buffer)
          (let ((old-buffer (current-buffer)))
            (delete-windows-on buffer)
            (framepop-display-buffer buffer)
            (framepop-eob)
            (switch-to-buffer old-buffer))
        (message "No process for this document."))))
  ) ; use-framepop

;; Shows
(setq TeX-display-help 'expert) ; default is t

(defun my-TeX-recenter-output-buffer (line)
  "Sets `pop-up-frames' and calls `TeX-recenter-output-buffer' to redisplay
buffer of TeX job output so that most recent output can be seen.
The last line of the buffer is displayed on line LINE of the window, or
at bottom if LINE is nil."
  (interactive "P")
  (let ((pop-up-frames t))
    (TeX-recenter-output-buffer line)))

(defun my-TeX-next-error (reparse)
  "Sets `pop-up-frames' and calls `TeX-next-error' to find the next error in
the TeX output buffer.
With \\[universal-argument] prefix, start from the beginning of the errors."
  (interactive "P")
  (let ((pop-up-frames t))
    (TeX-next-error reparse)))

;; Key bindings in LaTeX mode for additional functions
(eval-after-load "latex"
  '(progn
     (custom-set-default 'LaTeX-label-function 'LaTeX-label-environments)
     (TeX-global-PDF-mode 1)

     ;; (setq TeX-source-specials-tex-flags "-synctex=1")
                                        ; default is "-src-specials"
     ;; (setq-default TeX-source-specials-mode t)

     (define-key LaTeX-mode-map "\C-c\C-w" 'TeX-toggle-debug-warnings)
     (define-key LaTeX-mode-map "\C-o\C-f" 'TeX-fold-mode)
     (define-key LaTeX-mode-map (kbd "C-c C-;") 'my-comment-region)

     ;; used to be functionality in xdvi-search and txs-search
     (define-key LaTeX-mode-map "\C-c\C-v" 'my-TeX-view-master)
                                        ; overrides 'TeX-view
     (define-key LaTeX-mode-map "\C-cv" 'my-TeX-view-master)
                                        ; overrides view-file
     (define-key LaTeX-mode-map "\C-cx" 'my-TeX-view-master)
     (define-key LaTeX-mode-map [M-S-mouse-1] 'TeX-view-mouse)

     ;; defined above
     (define-key TeX-mode-map "\C-c\C-l" 'my-TeX-recenter-output-buffer)
     (define-key LaTeX-mode-map "\C-c\C-l" 'my-TeX-recenter-output-buffer)
     (define-key TeX-mode-map "\C-c`" 'my-TeX-next-error)
     (define-key LaTeX-mode-map "\C-c`" 'my-TeX-next-error)

     ;; in reftex
     ;;(define-key LaTeX-mode-map "\C-cg" 'reftex-goto-label)
     (define-key LaTeX-mode-map "\C-c\M-s" 'reftex-search-document)
     ;;(define-key LaTeX-mode-map "\C-cm" 'reftex-search-macro)
     (define-key LaTeX-mode-map "\C-c%" 'reftex-query-replace-document)
     (define-key LaTeX-mode-map "\C-c\C-g" 'reftex-grep-document)

     ;; in bib-cite
     ;;(define-key LaTeX-mode-map [C-M-home] 'LaTeX-find-matching-begin)
     ;;(define-key LaTeX-mode-map [C-M-kp-home] 'LaTeX-find-matching-begin)
     ;;(define-key LaTeX-mode-map [C-M-end] 'LaTeX-find-matching-end)
     ;;(define-key LaTeX-mode-map [C-M-kp-end] 'LaTeX-find-matching-end)

     (define-key LaTeX-mode-map [C-M-prior] 'outline-previous-visible-heading)
     (define-key LaTeX-mode-map [C-M-next] 'outline-next-visible-heading)
     (define-key LaTeX-mode-map [C-M-home] 'outline-backward-same-level)
     (define-key LaTeX-mode-map [C-M-end] 'outline-forward-same-level)

     (define-key LaTeX-mode-map [C-prior] 'LaTeX-find-matching-begin)
     (define-key LaTeX-mode-map [C-next] 'LaTeX-find-matching-end)

     ;; in flyspell
     (define-key LaTeX-mode-map "\C-c\M-f" 'flyspell-mode)

     ;; defined below
     (define-key LaTeX-mode-map (kbd "C-c C-,") 'my-TeX-command-frame)
     (define-key LaTeX-mode-map (kbd "C-c C-.") 'my-LaTeX-mark-beamer-frame)
     (define-key LaTeX-mode-map (kbd "C-c C-/") 'my-LaTeX-goto-current-position)

     ;; use word-help - info-lookup does not work for current latex info file
     ;;(define-key LaTeX-mode-map [\C-tab] 'word-help-complete)
     ;;(define-key LaTeX-mode-map (kbd "C-h C-i") 'word-help)
     ))

;; Additional settings to run when entering LaTeX-mode
(defun LaTeX-mode-hook-function ()
  (outline-minor-mode 1)
  (TeX-fold-mode 1)
  ;;(ispell-hl-minor-mode)
  (abbrev-mode 1)
  (flyspell-mode 1)
  (paren-toggle-matching-quoted-paren 1)
  (paren-toggle-matching-paired-delimiter 1)
  (insert-matching-paren-mode 1) ; in utilities.el
  (setq show-trailing-whitespace t)
  ;; latex-imenu-create-index defined in utilities.el
  ;; (setq imenu-create-index-function 'latex-imenu-create-index)
  )

(add-hook 'LaTeX-mode-hook 'LaTeX-mode-hook-function)

(defun TeX-language-it-hook-function ()
  ;;(setq ispell-local-dictionary "italian")
                                        ; sets the ispell-dictionary to italian
                                        ; and we do not want that
  (flyspell-mode-off)
  )

(add-hook 'TeX-language-it-hook 'TeX-language-it-hook-function)

;;;----------------------------------------------------------------------------
;;; use of synctex to synchronize source with pdf
;;;----------------------------------------------------------------------------

(setq TeX-source-correlate-method 'synctex)
(setq TeX-source-correlate-mode t)

;;; See also ss-generic.el for (server-start) and raising emacs when activated
;;; by server

;;;----------------------------------------------------------------------------
;;; Fontification for LaTeX buffers
;;;----------------------------------------------------------------------------

(setq font-latex-fontify-sectioning 'color)
;; (font-latex-update-sectioning-faces)

; (setq font-latex-fontify-script nil)

;;;----------------------------------------------------------------------------
;;; TeX-fold mode
;;;----------------------------------------------------------------------------

(setq TeX-fold-keymap
  (let ((map (make-sparse-keymap)))
    (define-key map "\C-o\C-o" 'TeX-fold-dwim)
    (define-key map "\C-o\C-b" 'TeX-fold-buffer)
    (define-key map "\C-o\C-r" 'TeX-fold-region)
    (define-key map "\C-o\C-p" 'TeX-fold-paragraph)
    (define-key map "\C-o\C-m" 'TeX-fold-macro)
    (define-key map "\C-o\C-e" 'TeX-fold-env)
    (define-key map "\C-o\C-c" 'TeX-fold-comment)
    (define-key map "\C-ob"    'TeX-fold-clearout-buffer)
    (define-key map "\C-or"    'TeX-fold-clearout-region)
    (define-key map "\C-op"    'TeX-fold-clearout-paragraph)
    (define-key map "\C-oi"    'TeX-fold-clearout-item)
    map))

;;;----------------------------------------------------------------------------
;;; bib-cite mode
;;;----------------------------------------------------------------------------

(autoload #'turn-on-bib-cite "bib-cite")
(add-hook 'LaTeX-mode-hook 'turn-on-bib-cite)

(setq bib-substitute-string-in-display nil
      bib-cite-use-reftex-view-crossref nil
      ;; bib-novice nil
      ;; bib-use-imenu nil
      ;; bib-highlight-mouse-t nil
      )

;;;----------------------------------------------------------------------------
;;; reftex mode
;;;----------------------------------------------------------------------------

(add-hook 'LaTeX-mode-hook 'turn-on-reftex)

;;(setq reftex-keep-temporary-buffers nil)
(setq reftex-label-alist
      '(LaTeX
	("theorem" ?h "thm:" "~\\ref{%s}" t   ("Theorem" "Theor." "Th."))
	("definition" ?d "def:" "~\\ref{%s}" t   ("Definition" "Def."))
	("lemma" ?l "lem:" "~\\ref{%s}" t   ("Lemma" "Lem."))
	("proposition" ?p "prop:" "~\\ref{%s}" t   ("Proposition" "Prop."))
	("corollary" ?r "cor:" "~\\ref{%s}" t   ("Corollary" "Cor."))
	("example" ?x "example:" "~\\ref{%s}" t   ("Example" "Ex."))
	("algorithm" ?g "alg:" "~\\ref{%s}" t   ("Algorithm" "Alg."))
	)

      reftex-section-prefixes '((0 . "part:") (1 . "chap:") (t . "sec:"))

      reftex-insert-label-flags '(t t)
      reftex-derive-label-parameters
      '(5 50 t 1 "-"
          ("the" "on" "in" "off" "a" "for" "by" "of" "and" "is" "to")
          nil)

      reftex-support-index nil
      reftex-label-illegal-re
      "[][\000-\040\177-\377\\\\#$%&~^_{},.;:!?'`\"@(){}<>/]"

      reftex-bibfile-ignore-regexps '("\\(long\\|medium\\|short\\)-string")
      reftex-default-bibliography "krdb"
      reftex-sort-bibtex-matches 'author
      reftex-plug-into-AUCTeX '(nil nil t t t)
      reftex-enable-partial-scans t
      reftex-save-parse-info t
      reftex-auto-recenter-toc t)

;;; Fix for reftex resetting the `LaTeX-label-function' when calling
;;; `reftex-plug-into-AUCTeX'
(defun reftex-mode-hook-function ()
  (setq LaTeX-label-function 'LaTeX-label-environments)
  )

(add-hook 'reftex-mode-hook 'reftex-mode-hook-function)

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

(defun my-LaTeX-inputs-split ()
  "Replace files included with `my-LaTeX-inputs-merge' with a corresponding
\\input{...} command, and generate the appropriate file.
Asks for confirmation if a file to generate already exists."
  (interactive)
  (save-restriction
    (when (and (boundp 'transient-mark-mode)
               transient-mark-mode mark-active)
      (narrow-to-region (point) (mark))
      (deactivate-mark))
    (goto-char (point-min))

    (let ((kill-whole-line t)
          beg)
      (while (re-search-forward "^%+[ \t]*INPUT-BEGIN{\\([^} ]+\\)}"
                                nil 'no-error)
        (let* ((input-name (match-string 1))
               (file-name (concat (file-name-directory buffer-file-name)
                                  (if (file-name-extension input-name)
                                      input-name
                                    (concat input-name ".tex")))))
          (beginning-of-line)
          (forward-line 1)
          (setq beg (point))
          (if (not (re-search-forward (concat "^%+[ \t]*INPUT-END{"
                                              input-name "}")
                                      nil 'no-error))
              (goto-char beg)
            (beginning-of-line)
            (condition-case nil
                (progn
                  (write-region beg (point) file-name nil nil nil t)
                  (forward-line 1)
                  (delete-region beg (point))
                  (if (looking-at "%+$") (kill-line))
                  (forward-line -1)
                  (if (looking-at "^\\(%+\\([ \t]*\\)INPUT-BEGIN\\)")
                      (replace-match "\\2\\\\input" t nil nil 1))
                  (forward-line -1)
                  (if (looking-at "%+$") (kill-line)))
              (error (goto-char beg)))))))))

;;;----------------------------------------------------------------------------
;;; Selecting surrounding slide in beamer mode
;;;----------------------------------------------------------------------------

(defvar my-LaTeX-current-position)
(make-variable-buffer-local 'my-LaTeX-current-position)

(defun my-LaTeX-mark-beamer-frame (&optional number)
  "Set mark to end of current or next frame and point to begin of current or
previous frame."
  (interactive "p")
  (setq my-LaTeX-current-position (point))
  (let ((forward (and number (> number 0) number))
        (backward (and number (< number -1) (- number))))
    (skip-chars-backward "a-zA-Z \t{")
    (unless (bolp) (backward-char 1))
    (search-forward "\\end{frame}" nil 'toend forward)
    (beginning-of-line 2)
    (push-mark (point)) ; (set-mark (point))
    (goto-char my-LaTeX-current-position)
    (end-of-line)
    (search-backward "\\begin{frame}" nil 'toend backward))
  ;; (TeX-activate-region)
  (activate-mark)
  (TeX-pin-region (region-beginning) (region-end)))

(defun my-LaTeX-goto-current-position ()
  "Goto the current position before running the command
`my-LaTeX-mark-beamer-frame'."
  (interactive)
  (goto-char my-LaTeX-current-position)
  (deactivate-mark))

(defun my-TeX-command-frame (&optional number)
  "Run TeX on the current beamer frame.
If a prefix argument NUMBER is given, include also the next NUMBER frames."
  (interactive "p")
  (save-excursion
    (my-LaTeX-mark-beamer-frame number)
    (TeX-command-region)))

;;;----------------------------------------------------------------------------
;;; bibtex mode
;;;----------------------------------------------------------------------------

(if (not windows-nt)
    (setq bibtex-string-files '("string-medium.bib"))
  )

;; (setq bibtex-maintain-sorted-entries 'entry-class) ; we leave nil as default
(setq bibtex-sort-entry-class '(("String")
                                (catch-all)
                                ("TechReport")
                                ("Book")
                                ("Proceedings")
                                ("Manual")
                                ))

(setq-default bibtex-field-delimiters 'double-quotes)
(setq bibtex-comma-after-last-field t)
(setq bibtex-entry-format '(opts-or-alts
                            numerical-fields
                            realign
                            last-comma
                            delimiters
                            unify-case
                            sort-fields))
(setq bibtex-autokey-names 4)
(setq bibtex-autokey-names-stretch 1)
(setq bibtex-autokey-name-length -4)
(setq bibtex-autokey-additional-names "*")
(setq bibtex-autokey-name-case-convert-function 'capitalize)
(setq bibtex-autokey-titlewords 0)
(setq bibtex-autokey-titlewords-stretch 0)

(setq bibtex-autokey-before-presentation-function
      'my-bibtex-autokey-before-presentation-function)

(defun my-bibtex-autokey-before-presentation-function (key)
  "Compute new BibTeX key, according to criteria used for krdb.bib.
Disambiguates the key by appending unique character in alphabetical order."
  (let* (;(len (length key))
         (case-fold-search nil)
         (new-key
          (cond ((string-match "\\(\[[:upper:]\]\\)\[[:lower:]\]+\\(\[[:upper:]\]\\)\[[:lower:]\]+\\(\[[:upper:]\]\\)\[[:lower:]\]+\\(\[[:upper:]\]\\)\[[:lower:]\]+\\(\[[:upper:]\]\\)\[[:lower:]\]+\\(\[[:digit:]\]+\\)"
                               key)
                 (concat (match-string-no-properties 1 key)
                         (match-string-no-properties 2 key)
                         (match-string-no-properties 3 key)
                         (match-string-no-properties 4 key)
                         (match-string-no-properties 5 key)
                         (match-string-no-properties 6 key)))
                ((string-match "\\(\[[:upper:]\]\\)\[[:lower:]\]+\\(\[[:upper:]\]\\)\[[:lower:]\]+\\(\[[:upper:]\]\\)\[[:lower:]\]+\\(\[[:upper:]\]\\)\[[:lower:]\]+\\(\\*?\[[:digit:]\]+\\)"
                               key)
                 (concat (match-string-no-properties 1 key)
                         (match-string-no-properties 2 key)
                         (match-string-no-properties 3 key)
                         (match-string-no-properties 4 key)
                         (match-string-no-properties 5 key)))
                ((string-match "\\(\[[:upper:]\]\[[:lower:]\]\\)\[[:lower:]\]*\\(\[[:upper:]\]\\)\[[:lower:]\]+\\(\[[:upper:]\]\\)\[[:lower:]\]+\\(\[[:digit:]\]+\\)"
                               key)
                 (concat (match-string-no-properties 1 key)
                         (match-string-no-properties 2 key)
                         (match-string-no-properties 3 key)
                         (match-string-no-properties 4 key)))
                ((string-match "\\(\[[:upper:]\]\[[:lower:]\]\\)\[[:lower:]\]*\\(\[[:upper:]\]\[[:lower:]\]\\)\[[:lower:]\]*\\(\[[:digit:]\]+\\)"
                               key)
                 (concat (match-string-no-properties 1 key)
                         (match-string-no-properties 2 key)
                         (match-string-no-properties 3 key)))
                (t key)
                ))
         (final-key new-key)
         ;; (start) ; we cannot assume that entries are ordered
         (ending ?b))
    (save-excursion
      (;; while (setq start (bibtex-search-entry final-key nil start))
       ;;; Since bibtex entries are grouped by type, we cannot assume that they
       ;;; are ordered by increasing key!
       while (bibtex-search-entry final-key)
        (setq final-key (concat new-key (list ending)))
        (incf ending)))
    final-key))

(add-hook 'bibtex-mode-hook 'turn-on-auto-fill)
(add-hook 'bibtex-mode-hook 'turn-off-filladapt-mode)
(add-hook 'bibtex-mode-hook 'hs-minor-mode)
(eval-after-load "bibtex"
  '(progn
     (define-key bibtex-mode-map [(meta tab)] 'bibtex-complete-string)

     (defun bibtex-font-missing-comma (limit)
       "Used to detect missing comma at end of BibTeX field."
       ;; (when
       (re-search-forward
        (concat "^[^%]\\([ \t]*" bibtex-field-name
                "[ \t]*=[ \t]*[-0-9a-zA-Z]*\\|.*\"\\)[ \t.;:]*\n[ \t]*[^} \t]")
        (1+ limit) t)
       ;; (store-match-data (list (match-beginning 0) (match-end 0)))
       ;; t)
       )

     (setq bibtex-font-lock-keywords
           `(;; missing comma
             (bibtex-font-missing-comma (0 font-lock-warning-face prepend t))
             (;; reference type and reference label
              ,(concat
                "@\\(string\\|Article\\|\\(In\\)?Book\\|\\(In\\)?Proceedings\\|"
                "TechReport\\|InCollection\\|Unpublished\\|Misc\\|Booklet\\|"
                "MastersThesis\\|PhdThesis\\|Manual\\)"
                "{\\([a-zA-Z0-9_\\*:/.+-@&]+\\)[ \t\n]*[,=]")
              (1 font-lock-keyword-face) (4 font-lock-constant-face))
             ("@\\(Preamble\\){" 1 font-lock-keyword-face)
             ;; field names
             (,(concat
                "^[ \t]*"
                "\\(author\\|\\(book\\)?title\\|year\\|pages\\|journal\\|"
                "publisher\\|editor\\|volume\\|number\\|series\\|address\\|"
                "\\(an\\)?note\\|chapter\\|crossref\\|edition\\|institution\\|"
                "howpublished\\|key\\|month\\|organization\\|school\\|type\\)"
                "[ \t]*=")
              1 font-lock-function-name-face)
             ;; defined strings
             (,(concat "=[ \t]*\\([a-zA-Z0-9_\\*:-]*"
                       "[a-zA-Z_\\*:-][a-zA-Z0-9_\\*:-]*\\)[ \t]*")
              1 font-lock-string-face)
             ;; url
             (bibtex-font-lock-url)
             ;; cross references
             ;; ("crossref[ \t]*=[ \t]*\"\\([a-zA-Z0-9_\\*:/.+-@&]*\\)\""
             ;;   1 font-lock-constant-face)
             (bibtex-font-lock-crossref)
             ;; optional field names (treated as comments)
             (,(concat "^[ \t]*\\(OPT" bibtex-field-name "\\)[ \t]*=")
              1 'font-lock-comment-face)
             ;; alternative field names
             (,(concat "^[ \t]*\\(ALT" bibtex-field-name "\\)[ \t]*=")
              1 'font-lock-builtin-face)
             ;; cite
             ,@(mapcar (lambda (matcher)
                         `((lambda (bound)
                             (bibtex-font-lock-cite ',matcher bound))))
                       bibtex-cite-matcher-alist)
             ))
     ))

;;; end of ss-tex.el

;;; Local Variables:
;;; save-place: t
;;; End:
