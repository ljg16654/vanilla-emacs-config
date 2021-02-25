(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(company-quickhelp-color-background "#b0b0b0")
 '(company-quickhelp-color-foreground "#232333")
 '(custom-safe-themes
   '("dca812ad0b96313645d895a0e1a2b1ee87782b18f40093687f4ff5e30ba9d197" "ae30c27916f58eb24285b19d52e2c4ae36b862a3856bbbc5e932f5d436dc7d61" "70f5a47eb08fe7a4ccb88e2550d377ce085fedce81cf30c56e3077f95a2909f2" "5a0eee1070a4fc64268f008a4c7abfda32d912118e080e18c3c865ef864d1bea" "c3e6b52caa77cb09c049d3c973798bc64b5c43cc437d449eacf35b3e776bf85c" "c7737b9fc3471779c8e51ea0a37834d24aa80a0d6a79b215e7501227ada39855" "5034d4b3ebd327bbdc1bbf925b6bf7e4dfbe4f3f84ee4d21e154143f128c6e04" "51ec7bfa54adf5fff5d466248ea6431097f5a18224788d0bd7eb1257a4f7b773" "00445e6f15d31e9afaa23ed0d765850e9cd5e929be5e8e63b114a3346236c44c" "830877f4aab227556548dc0a28bf395d0abe0e3a0ab95455731c9ea5ab5fe4e1" "7f1d414afda803f3244c6fb4c2c64bea44dac040ed3731ec9d75275b9e831fe5" "2809bcb77ad21312897b541134981282dc455ccd7c14d74cc333b6e549b824f3" "06f0b439b62164c6f8f84fdda32b62fb50b6d00e8b01c2208e55543a6337433a" "82d2cac368ccdec2fcc7573f24c3f79654b78bf133096f9b40c20d97ec1d8016" "bb08c73af94ee74453c90422485b29e5643b73b05e8de029a6909af6a3fb3f58" "bffa9739ce0752a37d9b1eee78fc00ba159748f50dc328af4be661484848e476" "1b8d67b43ff1723960eb5e0cba512a2c7a2ad544ddb2533a90101fd1852b426e" "628278136f88aa1a151bb2d6c8a86bf2b7631fbea5f0f76cba2a0079cd910f7d" "fa2b58bb98b62c3b8cf3b6f02f058ef7827a8e497125de0254f56e373abee088" "33ea268218b70aa106ba51a85fe976bfae9cf6931b18ceaf57159c558bbcd1e6" default))
 '(fci-rule-color "#c7c7c7")
 '(helm-completion-style 'emacs)
 '(lispy-eval-display-style 'overlay)
 '(midnight-mode t)
 '(nrepl-message-colors
   '("#336c6c" "#205070" "#0f2050" "#806080" "#401440" "#6c1f1c" "#6b400c" "#23733c"))
 '(org-format-latex-options
   '(:foreground default :background default :scale 3.0 :html-foreground "Black" :html-background "Transparent" :html-scale 1.0 :matchers
		 ("begin" "$1" "$" "$$" "\\(" "\\[")))
 '(package-selected-packages
   '(window ag orderless embark evil-escape ripgrep evil rg general cpputils-cmake wallpaper define-word org-noter-pdftools org-pdftools solarized-theme prescient ivy-prescient cmake-mode anki-editor ivy-rich doom-modeline exwm racket-mode telephone-line diminish org-roam-server lispy which-key yasnippet vterm pdf-tools powerline dired color-theme-sanityinc-tomorrow spacemacs-theme anti-zenburn-theme eyebrowse flycheck lsp-ui company company-mode lsp-python-ms lsp-mode projectile org-bullets emms olivetti olivette avy counsel ivy magit org-roam cdlatex use-package command-log-mode))
 '(safe-local-variable-values
   '((lsp--override-calculate-lisp-indent\? . t)
     (flycheck-disabled-checkers quote
				 (emacs-lisp-checkdoc))
     (eval progn
	   (let
	       ((dirloc-lsp-defun-regexp
		 (concat
		  (concat "^\\s-*(" "lsp-defun" "\\s-+\\(")
		  (or
		   (bound-and-true-p lisp-mode-symbol-regexp)
		   "\\(?:\\sw\\|\\s_\\|\\\\.\\)+")
		  "\\)")))
	     (add-to-list 'imenu-generic-expression
			  (list "Functions" dirloc-lsp-defun-regexp 1)))
	   (defvar lsp--override-calculate-lisp-indent\? nil "Whether to override `lisp-indent-function' with
              the updated `calculate-lisp-indent' definition from
              Emacs 28.")
	   (defun wrap-calculate-lisp-indent
	       (func &optional parse-start)
	     "Return appropriate indentation for current line as Lisp code.
In usual case returns an integer: the column to indent to.
If the value is nil, that means don't change the indentation
because the line starts inside a string.

PARSE-START may be a buffer position to start parsing from, or a
parse state as returned by calling `parse-partial-sexp' up to the
beginning of the current line.

The value can also be a list of the form (COLUMN CONTAINING-SEXP-START).
This means that following lines at the same level of indentation
should not necessarily be indented the same as this line.
Then COLUMN is the column to indent to, and CONTAINING-SEXP-START
is the buffer position of the start of the containing expression."
	     (if
		 (not lsp--override-calculate-lisp-indent\?)
		 (funcall func parse-start)
	       (save-excursion
		 (beginning-of-line)
		 (let
		     ((indent-point
		       (point))
		      state
		      (desired-indent nil)
		      (retry t)
		      whitespace-after-open-paren calculate-lisp-indent-last-sexp containing-sexp)
		   (cond
		    ((or
		      (markerp parse-start)
		      (integerp parse-start))
		     (goto-char parse-start))
		    ((null parse-start)
		     (beginning-of-defun))
		    (t
		     (setq state parse-start)))
		   (unless state
		     (while
			 (<
			  (point)
			  indent-point)
		       (setq state
			     (parse-partial-sexp
			      (point)
			      indent-point 0))))
		   (while
		       (and retry state
			    (>
			     (elt state 0)
			     0))
		     (setq retry nil)
		     (setq calculate-lisp-indent-last-sexp
			   (elt state 2))
		     (setq containing-sexp
			   (elt state 1))
		     (goto-char
		      (1+ containing-sexp))
		     (if
			 (and calculate-lisp-indent-last-sexp
			      (> calculate-lisp-indent-last-sexp
				 (point)))
			 (let
			     ((peek
			       (parse-partial-sexp calculate-lisp-indent-last-sexp indent-point 0)))
			   (if
			       (setq retry
				     (car
				      (cdr peek)))
			       (setq state peek)))))
		   (if retry nil
		     (goto-char
		      (1+ containing-sexp))
		     (setq whitespace-after-open-paren
			   (looking-at
			    (rx whitespace)))
		     (if
			 (not calculate-lisp-indent-last-sexp)
			 (setq desired-indent
			       (current-column))
		       (parse-partial-sexp
			(point)
			calculate-lisp-indent-last-sexp 0 t)
		       (cond
			((looking-at "\\s("))
			((>
			  (save-excursion
			    (forward-line 1)
			    (point))
			  calculate-lisp-indent-last-sexp)
			 (if
			     (or
			      (=
			       (point)
			       calculate-lisp-indent-last-sexp)
			      whitespace-after-open-paren)
			     nil
			   (progn
			     (forward-sexp 1)
			     (parse-partial-sexp
			      (point)
			      calculate-lisp-indent-last-sexp 0 t)))
			 (backward-prefix-chars))
			(t
			 (goto-char calculate-lisp-indent-last-sexp)
			 (beginning-of-line)
			 (parse-partial-sexp
			  (point)
			  calculate-lisp-indent-last-sexp 0 t)
			 (backward-prefix-chars)))))
		   (let
		       ((normal-indent
			 (current-column)))
		     (cond
		      ((elt state 3)
		       nil)
		      ((and
			(integerp lisp-indent-offset)
			containing-sexp)
		       (goto-char containing-sexp)
		       (+
			(current-column)
			lisp-indent-offset))
		      (calculate-lisp-indent-last-sexp
		       (or
			(and lisp-indent-function
			     (not retry)
			     (funcall lisp-indent-function indent-point state))
			(and
			 (save-excursion
			   (goto-char indent-point)
			   (skip-chars-forward " 	")
			   (looking-at ":"))
			 (save-excursion
			   (goto-char calculate-lisp-indent-last-sexp)
			   (backward-prefix-chars)
			   (while
			       (not
				(or
				 (looking-back "^[ 	]*\\|([ 	]+"
					       (line-beginning-position))
				 (and containing-sexp
				      (>=
				       (1+ containing-sexp)
				       (point)))))
			     (forward-sexp -1)
			     (backward-prefix-chars))
			   (setq calculate-lisp-indent-last-sexp
				 (point)))
			 (> calculate-lisp-indent-last-sexp
			    (save-excursion
			      (goto-char
			       (1+ containing-sexp))
			      (parse-partial-sexp
			       (point)
			       calculate-lisp-indent-last-sexp 0 t)
			      (point)))
			 (let
			     ((parse-sexp-ignore-comments t)
			      indent)
			   (goto-char calculate-lisp-indent-last-sexp)
			   (or
			    (and
			     (looking-at ":")
			     (setq indent
				   (current-column)))
			    (and
			     (<
			      (line-beginning-position)
			      (prog2
				  (backward-sexp)
				  (point)))
			     (looking-at ":")
			     (setq indent
				   (current-column))))
			   indent))
			normal-indent))
		      (desired-indent)
		      (t normal-indent)))))))
	   (when
	       (< emacs-major-version 28)
	     (advice-add #'calculate-lisp-indent :around #'wrap-calculate-lisp-indent)))))
 '(yas-global-mode t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(fixed-pitch ((t (:family "Iosevka"))))
 )
