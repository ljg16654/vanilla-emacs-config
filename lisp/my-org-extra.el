(require 'cl-lib)
(require 'company)

(defvar inline-math-candidates '()
  "A list of strings. Stored for completion of inline math LaTeX
  fragments.")

(defun my-org-get-inline-math () 
  "Get inline math around cursor"
       (interactive)
       (let* ((context (org-element-context))
	      (type (org-element-type context)))
	 (when (memq type '(latex-environment latex-fragment))
	   (my-remove-inline-matrix-delimiter (org-element-property :value context))
	   )))

(defun my-org-get-inline-math-prefix ()
  "Prefix definition for company backend. Returns nil if the
cursor is no inside a latex fragment. Otherwise, the substring
between left delimiter and the current cursor is returned."
       ;; the prefix must be immediately before the pointer
       ;; first confirm the point is inside a latex-fragment
       ;; then search-forward for '\\(' and obtain the prefix
 (interactive)
       (let* ((context (org-element-context))
	      (type (org-element-type context)))
	 (when (eq type 'latex-fragment)
	   (let* ((inline-math (org-element-property :value context))
		  (inline-math-prefix-point-min (+ (save-excursion (re-search-backward (rx "\\("))) 2)))
	     (buffer-substring-no-properties inline-math-prefix-point-min (point))))))

(defun my-org-get-all-inline-math ()
  "Store all inline math LaTeX fragments in the `inline-math-candidates'"
  (interactive)
  (progn
    (save-excursion (progn (setq inline-math-candidates nil)
			   (goto-char (point-min))
			   (while (re-search-forward
				   (rx (not "=") "\\(")
				   (point-max) t)
			     (add-to-list 'inline-math-candidates (my-org-get-inline-math))
			     )))))

(defun my-remove-inline-matrix-delimiter (str)
  "Given the value of a LaTeX fragment org element, remove its
delimiters to make it an appropriate candidate for completion."
  (string-remove-prefix "\\("
			(string-remove-suffix "\\)"
					      str)))

(defun company-inline-math-backend (command &optional arg &rest ignored)
  "The company backend for completing inline math LaTeX
fragments.  The candidates are obtained by trasversing all LaTeX
fragments in the org buffer"
  (interactive (list 'interactive))
  (cl-case command
    (interactive (company-begin-backend 'company-inline-math-backend))
    (prefix (and (eq major-mode 'org-mode)
		 (my-org-get-inline-math-prefix)))
    (candidates
	     (cl-remove-if-not
	      (lambda (str) (string-prefix-p arg str))
	      (progn (my-org-get-all-inline-math)
		    inline-math-candidates)))))

(add-to-list 'company-backends #'company-inline-math-backend)

(defun my-select-inline-math ()
  "Complete inline math and insert."
  (interactive)
    (insert (completing-read "Select inline math: "
		 (progn (my-org-get-all-inline-math)
			inline-math-candidates)
		 nil t)))


