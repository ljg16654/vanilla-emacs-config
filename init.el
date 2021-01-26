(setq user-full-name "Jigang Li"
      user-mail-address "ljg16654@sjtu.edu.cn")

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

(load-theme 'modus-vivendi t)

(use-package general)

(use-package evil)
(use-package evil-escape
  :config
  (progn
    (setq-default evil-escape-key-sequence "jk")
    (evil-escape-mode)))

(global-set-key (kbd "H-e") #'evil-mode)

(use-package rg
  :config
  (progn
    (rg-enable-default-bindings))
  :bind
  ("s-f" . rg-menu))

;; seems to be dependency for projectile-ripgrep
(use-package ripgrep)

(use-package ag)

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
  :bind ("s-s" . prot/window-single-toggle))

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
(add-hook 'help-mode-hook #'visual-line-mode)
(add-hook 'custom-mode-hook #'visual-line-mode)

;; between buffers

(global-set-key (kbd "s-i") #'ibuffer)
;; (global-set-key (kbd "s-o") #'switch-to-buffer)
(global-set-key (kbd "s-<left>") #'previous-buffer)
(global-set-key (kbd "s-<right>") #'next-buffer)
(global-set-key (kbd "C-x <return> r")
                ;; originally bound to
                ;; revert-buffer-with-coding-system
                #'revert-buffer)

;; inside a tab

(setq aw-keys
      (list ?a ?s ?d ?f ?j ?k ?l))

(global-set-key (kbd "s-j") #'other-window)
(global-set-key (kbd "s-k") #'(lambda () (interactive)
                                (other-window -1)))
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

(use-package avy
  :bind (("M-l" . avy-goto-line)))

(global-unset-key (kbd "C-'"))
(global-set-key (kbd "C-'") #'avy-goto-char-2)
(global-set-key (kbd "H-d") #'avy-goto-char-2)
(global-set-key (kbd "H-f") #'avy-goto-char)

(global-set-key (kbd "s-9") #'(lambda () (interactive) (avy-goto-char ?\()))
(global-set-key (kbd "s-(") #'check-parens)

(defun langou/goto-config ()
  "go to personal configuration of emacs"
  (interactive)
  (find-file "~/.config/emacs/init.org"))

(global-set-key (kbd "C-c f p") #'langou/goto-config)

(use-package magit
  :bind (("C-c g" . magit))
)

(use-package projectile)
(projectile-mode +1)
(define-key projectile-mode-map (kbd "s-p") 'projectile-command-map)
(define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)

(use-package dumb-jump
  :config
  (progn
    (add-hook 'xref-backend-functions #'dumb-jump-xref-activate)
    (setq dumb-jump-debug t)
    (setq dumb-jump-aggressive t)
    (setq dumb-jump-selector 'helm)
    ))

(use-package helm
  :config
  (progn
    (helm-mode 1)
    ))

(global-set-key (kbd "M-x") #'helm-M-x)
(global-set-key (kbd "C-x C-f") #'helm-find-files)
(global-set-key (kbd "s-o") #'helm-buffers-list)
(global-set-key (kbd "s-O") #'helm-recentf)
(global-set-key (kbd "M-i") #'helm-imenu)
(global-set-key (kbd "C-h a") #'helm-apropos)
(global-set-key (kbd "s-<return>") #'helm-filtered-bookmarks)

(use-package helm-projectile
   :config
   (progn
     (helm-projectile-on)
     ))

(global-set-key (kbd "H-SPC") #'helm-projectile)

(straight-use-package
 '(helm-wordnut :host github :repo "emacs-helm/helm-wordnut"))

(require 'helm-wordnut)

(defun helm-wordnet-at-point ()
  "Use `helm-wordnut--persistent-action' to define word at point.
When the region is active, define the marked phrase."
  (interactive)
  ;; the extraction of word is copied from
  ;; package define-word
  (let ((word
         (cond
          ((eq major-mode 'pdf-view-mode)
           (car (pdf-view-active-region-text)))
          ((use-region-p)
           (buffer-substring-no-properties
            (region-beginning)
            (region-end)))
          (t
           (substring-no-properties
            (thing-at-point 'word))))))
    (helm-wordnut--persistent-action word)))

(global-set-key (kbd "s-d") #'helm-wordnut)
(global-set-key (kbd "s-D") #'helm-wordnet-at-point)

(use-package helm-swoop)
(global-set-key (kbd "C-s") #'helm-swoop)

(use-package yasnippet
  :config
  (progn
    (setq yas-snippet-dirs
	   (list "~/.doom.d/snippets"))
    (yas-global-mode)))

(use-package which-key
  ;; :init (which-key-mode)
  :config
  (setq which-key-idle-delay 0.3))

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

(require 'general)

(general-define-key
 :keymaps 'dired-mode-map
 ";" #'dired-up-directory)

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

(defun langou/org-latex-delete-cache () (interactive)
       (delete-directory "~/.emacs.d/.local/cache/org-latex" :RECURSIVE t))

(use-package org-roam
  :commands org-roam-mode
  :init (add-hook 'after-init-hook 'org-roam-mode)
  :config
  (progn (setq org-roam-directory "~/org-roam")
         (setq org-roam-tag-sources
               (list
                'prop
                'last-directory)))
  :bind (("C-c r f" . org-roam-find-file)
         ("C-c r c" . org-roam-db-build-cache)
         ("C-c r i" . org-roam-insert)
         ("C-c r t" . org-roam-tag-add)
         ))

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
           "* TODO [%^{Select the urgency|A|B|C}] %?\n%i\n%a\n" :prepend t)

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
           "* %<%Y-%m-%d %H:%M:%S>\n:PROPERTIES:\n:ANKI_NOTE_TYPE: Basic\n:ANKI_DECK: langou gre\n:END:\n** Front\n%?\n** Back\n%i\n")
          ))

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

(setq debug-on-error t)
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

(global-set-key (kbd "s-m") #'bookmark-set)

(use-package define-word
  :bind
  (("C-c d" . define-word-at-point)
   ("C-c D" . define-word)))

(defun transparency (value)
  "sets the transparency of the frame window. 0=transparent/100=opaque"
  (interactive "ntransparency value 0 - 100 opaque:")
  (set-frame-parameter (selected-frame) 'alpha value))

(defvar +frame-transparency+ '(85 85))
(add-to-list 'default-frame-alist `(alpha . ,+frame-transparency+))

(use-package olivetti
  :config
  (progn
    ;; occupies 7/10 of the window width  
    (setq-default olivetti-body-width 0.7)
    )
  :bind (("C-c f e" . olivetti-mode)))


(add-hook 'org-mode-hook
          #'(lambda () (interactive)
              (olivetti-mode +1)))

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

(use-package company
  :config
  (setq company-idle-delay 0)
  )
(add-hook 'after-init-hook 'global-company-mode)
(use-package lsp-mode)
(use-package flycheck)
(use-package lsp-ui
  :demand flycheck
  :config
  (setq lsp-ui-sideline-show-diagnostics t
        lsp-ui-sideline-show-hover t))
(use-package lsp-python-ms
  :ensure t
  :init (setq lsp-python-ms-auto-install-server t
              read-process-output-max 1048576)
  :hook (python-mode . (lambda ()
                          (require 'lsp-python-ms)
                          (lsp))))

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

(use-package pdf-tools
  :config (pdf-tools-install))

(use-package vterm
  :bind (("s-v" . vterm)))

(global-set-key (kbd "s-e") #'eshell)

(setenv "PATH"
  (concat
   ;; manually added
   "/usr/local/cbc/bin" ";"
   (getenv "PATH") ; inherited from OS
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

(use-package doom-modeline
  ;; :init (doom-modeline-mode 1)
  :config
  (progn
    (setq doom-modeline-height 15)))

(global-set-key (kbd "H-r") #'compile)

(use-package lispy)
(add-hook 'emacs-lisp-mode-hook (lambda () (lispy-mode 1)))
(add-hook 'racket-mode-hook (lambda () (lispy-mode 1)))

(use-package racket-mode)

(use-package cmake-mode)

(use-package exwm
  :config
  (progn
    (setq exwm-workspace-number 3)
    (setq exwm-input-prefix-keys
          `(?\C-x
            ?\s-o ;; switch-to-buffer
            ?\s-i ;; ibuffer
            ?\s-j ;; window switch
            ?\s-c ;; kill window
            ?\s-C ;; kill buffer and window(if not single)
            ?\s-k ;; window switch
            ?\s-v ;; vterm
            ?\s-s ;; single-window-toggle
            ?\s-e ;; eshell
            ?\s-q ;; toggle side windows
            ?\s-t ;; toggle touchpad
            ?\s-d ;; helm-wordnut
            ?\C-u ;; general command
            ?\C-h ;; help
            ?\M-x
            ?\M-&
            ?\M-:
            ?\H-c ;; org-capture
            ?\H-s ;; kill other windows
            ?\C-\ ))
    (setq exwm-input-global-keys
          `(([?\s-r] . exwm-reset)
            ([?\s-w] . exwm-workspace-switch)
            ([?\s-\;] . (lambda (command)
                          (interactive (list (read-shell-command "$ ")))
                          (start-process-shell-command command nil command)))
            ,@(mapcar (lambda (i)
                        `(,(kbd (format "s-%d" i)) .
                          (lambda ()
                            (interactive)
                            (exwm-workspace-switch-create ,i))))
                      (number-sequence 0 2))))
    (exwm-input-set-simulation-keys
     '(([?\C-b] . left)
       ([?\C-f] . right)
       ([?\C-p] . up)
       ([?\C-n] . down)
       ([?\C-a] . home)
       ([?\C-e] . end)
       ([?\M-w] . [?\C-c])
       ;; ([?\M-b] . [?\C-?\<left>])
       ;; ([?\M-f] . [?\C-?\<left>])
       ))
    (setq exwm-workspace-warp-cursor t
          mouse-autoselect-window t
          focus-follows-mouse t)
    (exwm-enable)
    ))

;; After C-q, send key to the window 
(define-key exwm-mode-map [?\C-q] 'exwm-input-send-next-key)
(exwm-input-set-key (kbd "s-SPC") 'counsel-linux-app)

(defun efs/run-in-background (command)
  (let ((command-parts (split-string command "[ ]+")))
    (apply #'call-process `(,(car command-parts) nil 0 nil ,@(cdr command-parts)))))

(defun efs/exwm-init-hook ()
  ;; Make workspace 1 be the one where we land at startup
  (exwm-workspace-switch-create 1)

  ;; Start the Polybar panel
  (exwm-outer-gaps-mode)
  (efs/start-panel)

  ;; Launch apps that will run in the background
  ;; (efs/run-in-background "dunst")
  ;; (efs/run-in-background "nm-applet")
  ;; (efs/run-in-background "pasystray")
  ;; (efs/run-in-background "blueman-applet")

(defun efs/exwm-update-class ()
  (exwm-workspace-rename-buffer exwm-class-name))

(defun efs/exwm-update-title ()
  (pcase exwm-class-name
    ("Firefox" (exwm-workspace-rename-buffer (format "Firefox: %s" exwm-title)))
    )))

;; This function isn't currently used, only serves as an example how to
;; position a window
(defun efs/position-window ()
  (let* ((pos (frame-position))
	 (pos-x (car pos))
	  (pos-y (cdr pos)))
    (exwm-floating-move (- pos-x) (- pos-y))))

(defun efs/configure-window-by-class ()
  (interactive)
  (pcase exwm-class-name
    ("electron-ssr" (exwm-floating-toggle-floating))))

;; When EXWM starts up, do some extra confifuration
(add-hook 'exwm-init-hook #'efs/exwm-init-hook)

;; When window "class" updates, use it to set the buffer name
(add-hook 'exwm-update-class-hook #'efs/exwm-update-class)

;; When window title updates, use it to set the buffer name
(add-hook 'exwm-update-title-hook #'efs/exwm-update-title)

;; Configure windows as they're created
(add-hook 'exwm-manage-finish-hook #'efs/configure-window-by-class)

(straight-use-package
 '(exwm-outer-gaps :host github :repo "lucasgruss/exwm-outer-gaps")
 )

(setq exwm-outer-gaps-width [25 25 25 25])
(global-set-key (kbd "H-G") #'exwm-outer-gaps-mode)
(global-set-key (kbd "C-c 1") #'exwm-outer-gaps-mode)

(use-package desktop-environment)
(desktop-environment-mode)

(require 'exwm-randr)
(setq exwm-randr-workspace-monitor-plist '(1 "DP-1-2" 1 "DP-2" 1 "DP-1-1" 1 "DP-1"))
(exwm-randr-enable)

(defun efs/run-in-background (command)
  (let ((command-parts (split-string command "[ ]+")))
    (apply #'call-process `(,(car command-parts) nil 0 nil ,@(cdr command-parts)))))

(defun efs/update-displays ()
  (efs/run-in-background "autorandr --change --force")
  (message "Display config: %s"
	   (string-trim (shell-command-to-string "autorandr --current"))))

(add-hook 'exwm-randr-screen-change-hook #'efs/update-displays)
(efs/update-displays)

(unless (executable-find "feh")
  (display-warning 'wallpaper "External command `feh' not found!"))

;; This is an example `use-package' configuration
;; It is not tangled into wallpaper.el
(use-package wallpaper
  :ensure t
  :hook ((exwm-randr-screen-change . wallpaper-set-wallpaper)
         (after-init . wallpaper-cycle-mode))
  :custom ((wallpaper-cycle-single t)
           (wallpaper-scaling 'fill)
           (wallpaper-cycle-interval 45)
           (wallpaper-cycle-directory "~/Pictures/Wallpapers")))

;; Make sure the server is started (better to do this in your
;;  main Emacs config!)
(server-start)

(defvar efs/polybar-process nil
  "Holds the process of the running Polybar instance, if any")

(defun efs/kill-panel ()
  (interactive)
  (when efs/polybar-process
    (ignore-errors
      (kill-process efs/polybar-process)))
  (setq efs/polybar-process nil))

(defun efs/start-panel ()
  (interactive)
  (efs/kill-panel)
  (setq efs/polybar-process (start-process-shell-command "polybar" nil "polybar panel")))

(defun efs/send-polybar-hook (module-name hook-index)
  (start-process-shell-command "polybar-msg" nil (format "polybar-msg hook %s %s" module-name hook-index)))

(defun efs/polybar-exwm-workspace ()
  (pcase exwm-workspace-current-index
    (0 "")
    (1 "")
    (2 "")
    (3 "")
    (4 "")))

(defun efs/send-polybar-exwm-workspace ()
  (efs/send-polybar-hook "exwm-workspace" 1))

(defun langou/toggle-touchpad ()
  (interactive)
  (start-process-shell-command "exec" nil "exec ~/.dwm/toggleTouchpad.sh"))

(global-set-key (kbd "s-t") #'langou/toggle-touchpad)

;; Update panel indicator when workspace changes
(add-hook 'exwm-workspace-switch-hook #'efs/send-polybar-exwm-workspace)

(desktop-save-mode nil)

(use-package anki-editor)

(defun efs/org-babel-tangle-config ()
  (when (string-equal (file-name-directory (buffer-file-name))
                      (expand-file-name user-emacs-directory))
    ;; Dynamic scoping to the rescue
    (let ((org-confirm-babel-evaluate nil))
      (org-babel-tangle))))

(add-hook 'org-mode-hook (lambda () (add-hook 'after-save-hook #'efs/org-babel-tangle-config)))
