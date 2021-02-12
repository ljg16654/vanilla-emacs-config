(setq user-full-name "Jigang Li"
      user-mail-address "ljg16654@sjtu.edu.cn")

(server-start)

;; each use-package form also invoke straight.el to install the package
(setq straight-use-package-by-default t)

(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(straight-use-package 'use-package)

(use-package helm
  :config
  (progn
    (helm-mode 1)
    ))

(global-set-key (kbd "M-x") #'helm-M-x)
(global-set-key (kbd "C-x C-f") #'helm-find-files)
(global-set-key (kbd "s-o") #'helm-buffers-list)
(global-set-key (kbd "η") #'helm-buffers-list)
(global-set-key (kbd "s-O") #'helm-recentf)
(global-set-key (kbd "M-i") #'helm-imenu)
(global-set-key (kbd "C-h a") #'helm-apropos)
(global-set-key (kbd "μ") #'helm-filtered-bookmarks)

(use-package ace-jump-helm-line)
(eval-after-load "helm"
'(define-key helm-map (kbd "C-'") 'ace-jump-helm-line))

(use-package yasnippet
  :config
  (progn
    (setq yas-snippet-dirs
          (list (concat user-emacs-directory "snippet/")))
    (yas-global-mode)))

(use-package helm-c-yasnippet
  :after (helm yasnippet)
  :config
  (progn
    (setq helm-yas-space-match-any-greedy t)
    (global-set-key (kbd "C-c y") 'helm-yas-complete)
    ))

(use-package which-key
  ;; :init (which-key-mode)
  :config
  (setq which-key-idle-delay 0.3))

(use-package company
  :config
  (setq company-idle-delay 0)
  )

(add-hook 'after-init-hook 'global-company-mode)

(setq tab-always-indent 'complete)
(add-to-list 'completion-styles 'initials t)

(use-package dash)
(use-package f)

(set-face-attribute 'default nil :font "iosevka" :height 135)

(use-package anti-zenburn-theme
  :defer t)

(use-package solarized-theme
  :defer t
  :config
  (progn
    (setq solarized-use-variable-pitch nil)))

(use-package spacemacs-theme
  :defer t)

(use-package apropospriate-theme)

(use-package weyland-yutani-theme)

(load-theme 'modus-vivendi t)

(use-package general)

(use-package evil)
(use-package evil-escape
  :config
  (progn
    (setq-default evil-escape-key-sequence "jk")
    ))

(global-set-key (kbd "H-e") #'evil-mode)
(add-hook 'evil-mode-hook #'evil-escape-mode)

(use-package hydra)
(global-set-key (kbd "C-c h") #'hydra-pause-resume)

(defhydra landmark (global-map "C-c f")
  "landmarks"
  ("p" #'(lambda () (interactive)
           (find-file (concat user-emacs-directory "init.org")))
   "config")
  ("d" #'(lambda () (interactive)
           (dired "~/Downloads"))
   "downloads")
  ("c" #'(lambda () (interactive)
           (dired "~/Documents"))
   "documents")
  ("r" #'(lambda () (interactive)
           (dired "~/ROS"))
   "ros workspaces")
  ("y" #'(lambda () (interactive)
           (dired (concat user-emacs-directory "snippet/"))
           "snippets")))

(use-package rg
  :config
  (progn
    (rg-enable-default-bindings))
  :bind
  ("s-f" . rg-menu))

;; seems to be dependency for projectile-ripgrep
(use-package ripgrep)

(use-package ag)

(global-set-key (kbd "C-;") #'iedit-mode)

(use-package helm-swoop)
(global-set-key (kbd "C-s") #'isearch-forward)
;; enable whitespace to match arbitrary string that doesn't contain a newline
;; non-greedily
;; such behavior is, however, limited to non-regexp search
(setq search-whitespace-regexp ".*?")

(use-package emacs
  :config
  (defvar prot/window-configuration nil
    "Current window configuration.
Intended for use by `prot/window-monocle'.")

  (define-minor-mode prot/window-single-toggle
    "Toggle between multiple windows and single window.
This is the equivalent of maximising a window.  Tiling window
managers such as DWM, BSPWM refer to this state as 'monocle'."
    :lighter " [M]"
    :global nil
    (if (one-window-p)
        (when prot/window-configuration
          (set-window-configuration prot/window-configuration))
      (setq prot/window-configuration (current-window-configuration))
      (delete-other-windows)))
  :bind ("C-c s" . prot/window-single-toggle))

(setq display-buffer-alist
      '(
        ("\\*\\(Flymake\\|Package-Lint\\|vc-git :\\).*"
         (display-buffer-in-side-window)
         (window-height . 0.16)
         (side . top)
         (slot . 0)
         (window-parameters . ((no-other-window . t))))
        ("\\*Messages.*"
         (display-buffer-in-side-window)
         (window-height . 0.16)
         (side . top)
         (slot . 1)
         (window-parameters . ((no-other-window . t))))
        ("\\*\\(Backtrace\\|Warnings\\|Compile-Log\\)\\*"
         (display-buffer-in-side-window)
         (window-height . 0.16)
         (side . top)
         (slot . 2)
         (window-parameters . ((no-other-window . t))))
        ;; bottom side window
        ("\\*\\(Completions\\|Embark Collect Live\\).*"
         (display-buffer-in-side-window)
         (window-height . 0.16)
         (side . bottom)
         (slot . 0)
         (window-parameters . ((no-other-window . t))))
        ;; left side window
        ("\\*Help.*"
         (display-buffer-in-side-window)
         (window-width . 0.20)       ; See the :hook
         (side . left)
         (slot . 0)
         (window-parameters . ((no-other-window . t))))
        ;; right side window
        ("\\*Faces\\*"
         (display-buffer-in-side-window)
         (window-width . 0.25)
         (side . right)
         (slot . 0)
         (window-parameters
          . ((mode-line-format
              . (" "
                 mode-line-buffer-identification)))))
        ("\\*Custom.*"
         (display-buffer-in-side-window)
         (window-width . 0.25)
         (side . right)
         (slot . 1)
         (window-parameters . ((no-other-window . t))))
        ;; bottom buffer (NOT side window)
        ("\\*\\vc-\\(incoming\\|outgoing\\).*"
         (display-buffer-at-bottom))
        ("\\*\\(Output\\|Register Preview\\).*"
         (display-buffer-at-bottom)
         (window-parameters . ((no-other-window . t))))
        ;; ("\\*WordNet.*"
        ;;  (display-buffer-reuse-mode-window display-buffer-at-right)
        ;;  (slot . 0)
        ;;  (window-width . 0.4)
        ;;  )
        ("\\*.*\\([^E]eshell\\|shell\\|v?term\\).*"
         (display-buffer-reuse-mode-window display-buffer-at-bottom)
         (window-height . 0.2)
         ;; (mode . '(eshell-mode shell-mode))
         )))

(setq window-combination-resize t)
(setq even-window-sizes 'height-only)
(setq window-sides-vertical nil)
(setq switch-to-buffer-in-dedicated-window 'pop)
(global-set-key (kbd "s-q") #'window-toggle-side-windows)
(global-set-key (kbd "C-c 2") #'window-toggle-side-windows)
(add-hook 'help-mode-hook #'visual-line-mode)
(add-hook 'custom-mode-hook #'visual-line-mode)

;; between buffers

(global-set-key (kbd "s-i") #'ibuffer)
(global-set-key (kbd "s-<left>") #'previous-buffer)
(global-set-key (kbd "s-<right>") #'next-buffer)
(global-set-key (kbd "C-x <return> r")
                ;; originally bound to
                ;; revert-buffer-with-coding-system
                #'revert-buffer)

;; inside a tab

(setq aw-keys
      (list ?a ?s ?d ?f ?j ?k ?l))

(global-set-key (kbd "χ") #'other-window)
(global-set-key (kbd "H-s") #'delete-other-windows)

;; new tab starts with scratch buffer

(setq tab-bar-new-tab-choice "*scratch*")

(use-package tab-bar
  :init
  (setq tab-bar-close-button-show nil)
  (setq tab-bar-close-last-tab-choice 'tab-bar-mode-disable)
  (setq tab-bar-close-tab-select 'recent)
  (setq tab-bar-new-tab-choice t)
  (setq tab-bar-new-tab-to 'right)
  (setq tab-bar-position nil)
  (setq tab-bar-show nil)
  (setq tab-bar-tab-hints nil)
  (setq tab-bar-tab-name-function 'tab-bar-tab-name-all)
  :config
  (tab-bar-mode -1)
  (tab-bar-history-mode -1))

(defun prot-tab--tab-bar-tabs ()
  "Return a list of `tab-bar' tabs, minus the current one."
  (mapcar (lambda (tab)
            (alist-get 'name tab))
          (tab-bar--tabs-recent)))

(defun prot-tab-select-tab-dwim ()
    "Do-What-I-Mean function for getting to a `tab-bar' tab.
If no other tab exists, create one and switch to it.  If there is
one other tab (so two in total) switch to it without further
questions.  Else use completion to select the tab to switch to."
    (interactive)
    (let ((tabs (prot-tab--tab-bar-tabs)))
      (cond ((eq tabs nil)
             (tab-new))
            ((eq (length tabs) 1)
             (tab-next))
            (t
             (tab-bar-switch-to-tab
              (completing-read "Select tab: " tabs nil t))))))

(defun prot-tab-tab-bar-toggle ()
  "Toggle `tab-bar' presentation."
  (interactive)
  (if (bound-and-true-p tab-bar-mode)
      (progn
        (setq tab-bar-show nil)
        (tab-bar-mode -1))
    (setq tab-bar-show t)
    (tab-bar-mode 1)))

(defconst tab-leader "C-x t")

(general-create-definer tab-leader-def
  :prefix tab-leader)

;; global hyper leader def
(tab-leader-def
  "n" 'tab-bar-new-tab
  "r" 'tab-bar-rename-tab
  "k" 'tab-bar-close-tab
  "t" 'prot-tab-tab-bar-toggle
  "<tab>" 'prot-tab-select-tab-dwim)

(global-set-key (kbd "C-x t t") #'prot-tab-select-tab-dwim)

(defun prot-simple-kill-buffer-current (&optional arg)
  "Kill current buffer or abort recursion when in minibuffer.
With optional prefix ARG (\\[universal-argument]) delete the
buffer's window as well."
  (interactive "P")
  (if (minibufferp)
      (abort-recursive-edit)
    (kill-buffer (current-buffer)))
  (when (and arg
             (not (one-window-p)))
    (delete-window)))

(global-set-key (kbd "s-c") #'prot-simple-kill-buffer-current)
(global-set-key (kbd "s-C") #'(lambda ()
                                (interactive)
                                (prot-simple-kill-buffer-current 1)))

(global-set-key (kbd "C-c b r") #'rename-buffer)

(use-package avy)

(global-set-key (kbd "ν") #'avy-goto-char-2)
(global-set-key (kbd "σ") #'avy-goto-char)

(use-package magit
  :bind (("C-c g" . magit))
)

(use-package projectile)
(projectile-mode +1)
(define-key projectile-mode-map (kbd "s-p") 'projectile-command-map)
(define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)

(use-package helm-projectile
  :config
  (progn
    (helm-projectile-on)
    ))

(use-package dumb-jump
  :config
  (progn
    (add-hook 'xref-backend-functions #'dumb-jump-xref-activate)
    (setq dumb-jump-debug t)
    (setq dumb-jump-aggressive t)
    (setq dumb-jump-selector 'helm)
    ))

(use-package dired
  :straight nil
  :ensure nil
  :config
  (setq dired-recursive-copies 'always)
  (setq dired-recursive-deletes 'always)
  (setq delete-by-moving-to-trash t)
  (setq dired-listing-switches
        "-AGFhlv --group-directories-first --time-style=long-iso")
  (setq dired-dwim-target t))

(add-hook 'dired-mode
          #'(lambda ()
              (progn
                (dired-hide-details-mode +1))))

(use-package dired-subtree
  :after dired
  :config
  (progn
    (setq dired-subtree-use-backgrounds nil)
    )
  :bind
  (:map dired-mode-map
        ("<tab>" . dired-subtree-toggle)
        ("C-<tab>" . dired-subtree-cycle)
        ))

(use-package peep-dired
  :bind
  (:map dired-mode-map
   ("`" . peep-dired)
   ))

(use-package dired-filter
  :bind
    (:map dired-mode-map
    ("/" . dired-filter-mark-map)
    )
)

(require 'general)

(general-define-key
 :keymaps 'dired-mode-map
 ";" #'dired-up-directory
 )

(use-package org
  :config
  (progn
    (setq org-ellipsis " ▾"
          org-hide-emphasis-markers t
          org-imenu-depth 7
          )
    (local-unset-key (kbd "C-'"))
    (font-lock-add-keywords 'org-mode
                            '(("^ *\\([-]\\) "
                               (0 (prog1 () (compose-region (match-beginning 1) (match-end 1) "•"))))))))

(general-define-key
 :keymaps 'org-mode-map
 "M-h" #'org-metaleft
 "M-l" #'org-metaright
 "C-c e" #'org-mark-element)

(add-hook 'org-mode-hook #'auto-fill-mode)

(use-package org-bullets
  :ensure t
  :config
  (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1))))

(add-hook 'org-mode-hook #'org-indent-mode)

(setq +personal-org-roam-files+ (apply (function append)
				(mapcar
				 (lambda (directory)
					(directory-files-recursively directory org-agenda-file-regexp))
				    '("~/org-roam/"))))

(setq org-refile-targets
      '((nil :maxlevel . 5)
	(org-agenda-files :maxlevel . 5)
	(+personal-org-roam-files+ :maxlevel . 5)
	)
      ;; Without this, completers like ivy/helm are only given the first level of
      ;; each outline candidates. i.e. all the candidates under the "Tasks" heading
      ;; are just "Tasks/". This is unhelpful. We want the full path to each refile
      ;; target! e.g. FILE/Tasks/heading/subheading
      org-refile-use-outline-path 'file
      org-outline-path-complete-in-steps nil)

(setq org-todo-keywords
      '((sequence "MAYBE(m@)" "TODO(t)" "IN-PROGRESS(i@)" "STUCK(s@/@)" "|" "DONE(d@)" "CANCELLED(c@)")
	(sequence "REPORT(r)" "BUG(b/@)" "KNOWNCAUSE(k@)" "|" "FIXED(f)")
	))

(setq org-stuck-projects
      ;; identify a project with TODO keywords/tags
      ;; identify non-stuck state with TODO keywords
      ;; identify non-stuck state with tags
      ;; regexp match non-stuck projects
      '("-moyu&-MAYBE" ("TODO" "IN-PROGRESS" "BUG" "KNOWNCAUSE") nil ""))

(setq org-export-with-toc nil)

;; allow for export=>beamer by placing

;; #+LaTeX_CLASS: beamer in org files
(unless (boundp 'org-latex-classes)
  (setq org-latex-classes nil))
(add-to-list 'org-latex-classes
  ;; beamer class, for presentations
  '("beamer"
    "\\documentclass[11pt]{beamer}\n
      \\mode<{{{beamermode}}}>\n
      \\usetheme{{{{beamertheme}}}}\n
      \\usecolortheme{{{{beamercolortheme}}}}\n
      \\beamertemplateballitem\n
      \\setbeameroption{show notes}
      \\usepackage[utf8]{inputenc}\n
      \\usepackage[T1]{fontenc}\n
      \\usepackage{hyperref}\n
      \\usepackage{color}
      \\usepackage{listings}
      \\usepackage{physics}
      \\lstset{numbers=none,language=[ISO]C++,tabsize=4,
  frame=single,
  basicstyle=\\small,
  showspaces=false,showstringspaces=false,
  showtabs=false,
  keywordstyle=\\color{blue}\\bfseries,
  commentstyle=\\color{red},
  }\n
      \\usepackage{verbatim}\n
      \\institute{{{{beamerinstitute}}}}\n          
       \\subject{{{{beamersubject}}}}\n"

    ("\\section{%s}" . "\\section*{%s}")

    ("\\begin{frame}[fragile]\\frametitle{%s}"
     "\\end{frame}"
     "\\begin{frame}[fragile]\\frametitle{%s}"
     "\\end{frame}")))

  ;; letter class, for formal letters

  (add-to-list 'org-latex-classes

  '("letter"
     "\\documentclass[11pt]{letter}\n
      \\usepackage[utf8]{inputenc}\n
      \\usepackage[T1]{fontenc}\n
      \\usepackage{color}"

     ("\\section{%s}" . "\\section*{%s}")
     ("\\subsection{%s}" . "\\subsection*{%s}")
     ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
     ("\\paragraph{%s}" . "\\paragraph*{%s}")
     ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))

(use-package org-sidebar)

(defhydra org-sidebar (org-mode-map "C-c l")
  "sidebar"
  ("t" #'org-sidebar-tree-toggle "tree")
  ("s" #'org-sidebar-toggle "default sidebar")
  )

(setq org-confirm-babel-evaluate nil)
(setq org-src-window-setup 'current-window)

(org-babel-do-load-languages
 'org-babel-load-languages
 '((python . t)
 (emacs-lisp . t)
 (gnuplot . t)
 (shell . t)
 (java . t)
 (C . t)
 (clojure . t)
 (js . t)
 (ditaa . t)
 (dot . t)
 (org . t)
 (latex . t)
 (haskell . t)
 (ditaa . t)
 ))

(setq org-babel-python-command "python3")

(use-package auctex
  :defer t)

(use-package cdlatex
  :hook (org-mode . turn-on-org-cdlatex))

(use-package org-fragtog
  :after org)
(add-hook 'org-mode-hook 'org-fragtog-mode)

(defun langou/org-latex-delete-cache () (interactive)
       (delete-directory "~/.emacs.d/.local/cache/org-latex" :RECURSIVE t))

(use-package org-roam
  :commands org-roam-mode
  :init (add-hook 'after-init-hook 'org-roam-mode)
  :config
  (progn
    ;; all subdirectories of org-roam-directory are considered part of
    ;; org-roam regardless of level of nesting.
    (setq org-roam-directory "~/org-roam")
    (setq org-roam-tag-sources
          (list
           'prop
           'last-directory)))
  :bind (
         ("C-c r t" . org-roam-tag-add)
         ))

(defhydra roam (global-map "C-c r")
  "Org Roam"
  ("d" #'(lambda () (interactive)
           (dired org-roam-directory))
   "visit org-roam-directory")
  ("f" #'org-roam-find-file
   "find-file")
  ("x" #'org-roam-dailies-capture-today
   "capture today")
  ("j" #'org-roam-dailies-today
   "visit today")
  ("i" #'org-roam-insert
   "insert")
  ("c" #'org-roam-build-cache
   "build cache")
  )

(use-package org-roam-server
  :ensure t
  :config
  (setq org-roam-server-host "127.0.0.1"
        org-roam-server-port 8080
        org-roam-server-authenticate nil
        org-roam-server-export-inline-images t
        org-roam-server-serve-files nil
        org-roam-server-served-file-extensions '("pdf" "mp4" "ogv")
        org-roam-server-network-poll t
        org-roam-server-network-arrows nil
        org-roam-server-network-label-truncate t
        org-roam-server-network-label-truncate-length 60
        org-roam-server-network-label-wrap-length 20))

(setq org-roam-dailies-directory "daily/")

(setq org-roam-dailies-capture-templates
      '(("d" "default" entry
         #'org-roam-capture--get-point
         "* %?"
         :file-name "daily/%<%Y-%m-%d>"
         :head "#+title: %<%Y-%m-%d>\n\n")))

(defvar +org-capture-journal-file+ "journal.org")
(defvar +org-capture-todo-file+ "todo.org")
(defvar +org-capture-notes-file+ "notes.org")
(defvar +org-capture-just-for-fun-file+ "just-for-fun.org")

;;;; org-journal
(global-set-key (kbd "C-c j") #'(lambda ()
                                  (interactive)
                                  (find-file
                                   (concat org-directory "/journal.org"))))

(global-set-key (kbd "C-c c") #'org-capture)
(global-set-key (kbd "H-c") #'org-capture)

(setq org-capture-templates
        '(("t" "Personal todo" entry
           (file+headline "todo.org" "Inbox")
           "* TODO %?\n%i" :prepend t)

          ("n" "Personal notes" entry
           (file+headline "notes.org" "Inbox")
           "* %U %?\n%i\n%a" :prepend t)

          ("f" "Maybe it would be fun someday..." entry
           (file+headline "just-for-fun.org" "Inbox")
           "* MAYBE %U %?" :prepend t)

          ;; declare root node j
          ("j" "Journal")

          ("ja" "Journal arbitrary recording" entry
           (file+olp+datetree "journal.org")
           "* %?\n%U\n%i" :tree-type week)

          ("jc" "journal clock into something new" entry
           (file+olp+datetree "journal.org")
           "* %?" :clock-in t :clock-keep t :tree-type week)

          ("jn" "journal edit the task currently clocked in" plain
           (clock) "%?" :unnarrowed t)

          ("r" "read later" checkitem
           (file+headline "read-later.org" "Inbox")
           "[ ] %? ")

          ("b" "bug" entry
           (file+headline "bug.org" "Inbox")
           "* BUG %^{header}\n%U\n#+begin_src\n\n%i\n\n#+end_src\n%?")

          ("v" "vocabularies" entry
           (file+headline "voc.org" "inbox")
           "* %<%Y-%m-%d %H:%M:%S>\n:PROPERTIES:\n:ANKI_NOTE_TYPE: Basic\n:ANKI_DECK: langou gre\n:END:\n** Front\n%?\n** Back\n%i\n")))

(setq org-agenda-files (apply (function append)
			        (mapcar
			         (lambda (directory)
				        (directory-files-recursively directory org-agenda-file-regexp))
			            '("~/org/"))))

(add-to-list 'org-modules 'org-habit)
(global-set-key (kbd "s-a") #'org-agenda)

(add-to-list 'org-modules 'org-tempo)
(setq org-structure-template-alist
  '(("a" . "export ascii\n")
    ("c" . "center\n")
    ("C" . "comment\n")
    ("e" . "src emacs-lisp\n")
    ("cp" . "src cpp\n")
    ("py" . "src python\n")
    ("sh" . "src shell")
    ("E" . "export")
    ("h" . "export html\n")
    ("l" . "export latex\n")
    ("q" . "quote\n")
    ("s" . "src")
    ("v" . "verse\n")))

(use-package org-pdftools
  :hook (org-mode . org-pdftools-setup-link))

(use-package org-noter)

(use-package org-noter-pdftools
  :after org-noter
  :config
  (with-eval-after-load 'pdf-annot
    (add-hook 'pdf-annot-activate-handler-functions #'org-noter-pdftools-jump-to-note)))

(setq debug-on-error nil)
(setq custom-file (concat user-emacs-directory "custom.el"))
(load custom-file)

(setq browse-url-browser-function 'browse-url-firefox)

(use-package command-log-mode)

(defconst lisp--prettify-symbols-alist
      '(("lambda"  . ?λ)))

  (add-hook 'lisp-mode-hook #'(lambda () (interactive)
			       (prettify-symbols-mode +1)))


(setq python-prettify-symbols-alist
      (list
       '("lambda"  . ?λ)
       '("**2" . ?²)
       '("sum" . ?∑)
       '("sigma" . ?σ)
       '("mu" . ?μ)
       '("theta" . ?θ)
       '("_0" . ?₀)
       '("_1" . ?₁)
       '("_2" . ?₂)
       ))

(add-hook 'kill-emacs-hook #'bookmark-save)

(use-package helm-mode-manager
  :after helm)

(use-package search-web)
(use-package wordnut)
(setq search-web-engines
      '(
        ("duck" "https://duckduckgo.com/?q=%s" nil)
        ("github" "https://github.com/search?q=%s" nil)
        ("google" "http://www.google.com/search?q=%s" nil)
        ("google scholar" "https://scholar.google.co.jp/scholar?q=%s" nil)
        ("youtube" "http://www.youtube.com/results?search_type=&search_query=%s&aq=f" External)
        ("emacswiki" "http://www.google.com/cse?cx=004774160799092323420%%3A6-ff2s0o6yi&q=%s&sa=Search" nil)
        ("wikipedia en" "http://www.wikipedia.org/search-redirect.php?search=%s&language=en" nil)
        ("stackoveflow en" "http://stackoverflow.com/search?q=%s" nil)
        ))

(defhydra define (global-map "s-d")
  "define"
  ("w" wordnut-search "wordnet")
  ("i" search-web "web search")
  ("m" man "man")
  )

(defun transparency (value)
  "sets the transparency of the frame window. 0=transparent/100=opaque"
  (interactive "ntransparency value 0 - 100 opaque:")
  (set-frame-parameter (selected-frame) 'alpha value))

(defvar +frame-transparency+ '(95 95))
(add-to-list 'default-frame-alist `(alpha . ,+frame-transparency+))

(use-package olivetti
  :config
  (progn
    ;; occupies 7/10 of the window width  
    (setq-default olivetti-body-width 0.7)
    )
  :bind (("C-c f e" . olivetti-mode)))

(use-package expand-region
  :config
  (progn
    (global-set-key (kbd "C-=") #'er/expand-region)
    ))

(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(setq use-file-dialog nil)
(setq use-dialog-box t)               ; only for mouse events
;; (setq inhibit-splash-screen t)

(defun +default/save-to-king-ring-buffer-filename ()
  "Copy the current buffer's path to the kill ring."
  (interactive)
  (if-let (filename (or buffer-file-name (bound-and-true-p list-buffers-directory)))
      (message (kill-new (abbreviate-file-name filename)))
    (error "Couldn't find filename in current buffer")))

(global-set-key (kbd "C-c k f")  #'+default/save-to-king-ring-buffer-filename)

(fset 'yes-or-no-p 'y-or-n-p)

(use-package yaml-mode)

(use-package lsp-mode)

(use-package flycheck)

(use-package lsp-ui
  :after lsp-mode
  :demand flycheck
  )

(use-package lsp-python-ms
  :init (setq lsp-python-ms-auto-install-server t
              read-process-output-max 1048576)
  :hook (python-mode . (lambda ()
                         (require 'lsp-python-ms)
                         (lsp))))

(setq lsp-keymap-prefix "ρ")
(define-key lsp-ui-mode-map [remap xref-find-definitions] #'lsp-ui-peek-find-definitions)
(define-key lsp-ui-mode-map [remap xref-find-references] #'lsp-ui-peek-find-references)

(setq lsp-ui-doc-position 'bottom)
(setq lsp-ui-doc-use-childframe nil)
(setq lsp-ui-doc-delay 0)
(setq lsp-ui-sideline-show-diagnostics t)
(setq lsp-ui-sideline-show-hover nil)
(setq lsp-eldoc-render-all nil)

(setq
 mouse-wheel-scroll-amount
 '(1
   ((shift) . 1))
 mouse-wheel-progressive-speed nil)

(general-define-key
 :maps 'lsp-mode-map
 "C-c u i" #'lsp-ui-imenu
 "C-c d" #'lsp-ui-doc-focus-frame)

(use-package helm-lsp
  :after (lsp helm))

(use-package emms
  :config
  (progn
    (emms-all)
    (emms-default-players)
    (setq emms-source-file-default-directory "~/Music")
    (setq emms-player-mplayer-parameters
	    '("-slave" "-quiet" "-really-quiet" "-novideo"))))

(global-set-key (kbd "C-c m m") #'emms)
(global-set-key (kbd "C-c m p") #'emms-add-playlist)

(pdf-tools-install)
(setq pdf-view-midnight-colors
      '("#cccccc" . "#000000"))

(general-define-key
 :keymaps 'pdf-view-mode-map
 "o" #'pdf-outline
 "j" #'pdf-view-next-line-or-next-page
 "k" #'pdf-view-previous-line-or-previous-page
 "]" #'pdf-view-next-page-command
 "[" #'pdf-view-previous-page-command
 "/" #'pdf-occur)

(use-package vterm)

(use-package multi-vterm
  :after vterm)

(global-set-key (kbd "s-v v") #'multi-vterm)
(global-set-key (kbd "s-v d") #'multi-vterm-dedicated-toggle)

(defhydra multi-vterm (vterm-mode-map "s-v")
  "multi-vterm"
  ("d" #'multi-vterm-dedicated-toggle "dedicated")
  ("n" #'multi-vterm-next "next")
  ("p" #'multi-vterm-prev "prev")
  ("s" #'multi-vterm-dedicated-select "select as dedicated")
  ("r" #'multi-vterm-rename-buffer "rename")
  )

(setq multi-vterm-dedicated-window-height 15)

(use-package eshell-toggle)
(global-set-key (kbd "s-e") #'eshell-toggle)

(use-package eshell-git-prompt
  :config
  (progn
    (eshell-git-prompt-use-theme 'robbyrussell)
    ))

(global-set-key (kbd "s-e") #'eshell)

(setenv "PATH"
  (concat
   ;; manually added
   "/usr/local/cbc/bin" ";"
   "~/.local/bin" ";"
   (getenv "PATH")			; inherited from OS
   )
)

(defun mode-line-format-raw ()
  (interactive)

  (setq mode-line-format
          '("%e" mode-line-front-space mode-line-mule-info mode-line-client
            mode-line-modified mode-line-remote
            mode-line-frame-identification
            mode-line-buffer-identification " " mode-line-position
            (vc-mode vc-mode)
            "  " mode-line-modes mode-line-misc-info mode-line-end-spaces)
))

(use-package diminish)
(diminish 'ivy-mode)
(diminish 'auto-revert-mode)
(diminish 'yas-minor-mode)
(diminish 'which-key-mode "which?")
(diminish 'org-indent-mode)
(diminish 'org-roam-mode)
(diminish 'org-cdlatex-mode "cd")
(diminish 'company-mode)
(diminish 'projectile-mode)
(diminish 'helm-mode)
(diminish 'auto-fill-function "AuF")

(use-package doom-modeline
  ;; :init (doom-modeline-mode 1)
  :config
  (progn
    (setq doom-modeline-height 15)))

(use-package helm-catkin)

(use-package pamparam
  :after org)

(use-package org-drill
  :after org)

(global-set-key (kbd "H-r") #'compile)

(use-package imenu-anywhere)
(global-set-key (kbd "C-.") #'imenu-anywhere)

(use-package lispy)
(add-hook 'emacs-lisp-mode-hook (lambda () (lispy-mode 1)))
(add-hook 'racket-mode-hook (lambda () (lispy-mode 1)))

(use-package paren-face)
(add-hook 'emacs-lisp-mode-hook (lambda () (paren-face-mode 1)))

(use-package racket-mode)

(use-package cmake-mode)

(defhydra python-move-defun (python-mode-map "C-c n")
  "python mode movement"
  ("a" #'beginning-of-defun "beginning of defun")
  ("e" #'python-nav-end-of-defun "end of defun")
  ("p" #'python-nav-backward-defun "prev defun")
  ("n" #'python-nav-forward-defun "next defun")
  ("b" #'python-nav-backward-sexp "prev sexp")
  ("f" #'python-nav-forward-sexp "next sexp")
  ("k" #'python-nav-backward-block "prev block")
  ("j" #'python-nav-forward-block "next block")
  )

(desktop-save-mode nil)

(use-package anki-editor)

(defun efs/org-babel-tangle-config ()
  (when (string-equal (file-name-directory (buffer-file-name))
                      (expand-file-name user-emacs-directory))
    ;; Dynamic scoping to the rescue
    (let ((org-confirm-babel-evaluate nil))
      (org-babel-tangle))))

(add-hook 'org-mode-hook (lambda () (add-hook 'after-save-hook #'efs/org-babel-tangle-config)))
