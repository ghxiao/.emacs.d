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

(provide 'setup-python)
