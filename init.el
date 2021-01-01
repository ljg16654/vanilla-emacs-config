(setq user-full-name "Jigang Li"
      user-mail-address "ljg16654@sjtu.edu.cn")

(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(setq use-file-dialog nil)
(setq use-dialog-box t)               ; only for mouse events
(setq inhibit-splash-screen t)

(setq debug-on-error t)
;; (toggle-frame-fullscreen) 
;;; custom
(setq custom-file (concat user-emacs-directory "custom.el"))
(load custom-file)

;;; use-package setup
(require 'package)

(setq package-enable-at-startup nil)
(add-to-list 'package-archives
	     '("melpa" . "https://melpa.org/packages/"))

(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(require 'use-package)
(setq use-package-always-ensure t)

(defun +default/save-to-king-ring-buffer-filename ()
  "Copy the current buffer's path to the kill ring."
  (interactive)
  (if-let (filename (or buffer-file-name (bound-and-true-p list-buffers-directory)))
      (message (kill-new (abbreviate-file-name filename)))
    (error "Couldn't find filename in current buffer")))

(global-set-key (kbd "C-c k f")  #'+default/save-to-king-ring-buffer-filename)

(use-package olivetti
  :bind (("C-c f e" . olivetti-mode)))

(set-face-attribute 'default nil :font "iosevka" :height 135)

(use-package anti-zenburn-theme)
(use-package spacemacs-theme
  :defer t)
(load-theme 'spacemacs-light nil)

;; between buffers
(global-set-key (kbd "s-o") #'ibuffer)
(global-set-key (kbd "H-a") #'counsel-ibuffer)
(global-set-key (kbd "s-O") #'previous-buffer)

;; inside a tab
(setq aw-keys
      (list ?a ?s ?d ?f ?j ?k ?l))
(global-set-key (kbd "s-j") #'other-window)
(global-set-key (kbd "s-k") #'(lambda () (interactive)
                                (other-window -1)))
(global-set-key (kbd "H-s") #'delete-other-windows)

;; between tabs
(global-set-key (kbd "s-n") #'tab-bar-new-tab)
(global-set-key (kbd "s-N") #'tab-bar-rename-tab)
(global-set-key (kbd "s-,") #'tab-bar-switch-to-prev-tab)
(global-set-key (kbd "s-.") #'tab-bar-switch-to-next-tab)

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

(global-set-key (kbd "s-C") #'prot-simple-kill-buffer-current)

(use-package dired
  :ensure nil
  :config
  (setq dired-recursive-copies 'always)
  (setq dired-recursive-deletes 'always)
  (setq delete-by-moving-to-trash t)
  (setq dired-listing-switches
	"-AGFhlv --group-directories-first --time-style=long-iso")
  (setq dired-dwim-target t)
  ;; Hooks' syntax is controlled by the `use-pakage-hook-name-suffix'
  ;; variable.  The "-hook" suffix is intentional
  :hook ((dired-mode-hook . dired-hide-details-mode)
	 (dired-mode-hook . hl-line-mode)))

(global-set-key (kbd "C-c b r") #'rename-buffer)
(global-set-key (kbd "H-b") #'rename-buffer)

(use-package avy
  :bind (("M-g g" . avy-goto-line)))

(defun langou/goto-config ()
  "go to personal configuration of emacs"
  (interactive)
  (find-file "~/vanilla/init.org"))

(global-set-key (kbd "C-c f p") #'langou/goto-config)
(global-set-key (kbd "H-f p") #'langou/goto-config)

(global-set-key (kbd "M-i") 'imenu)

(use-package magit
  :bind (("C-c g" . magit) ("H-g" . magit))
)

(use-package projectile)
(projectile-mode +1)
(define-key projectile-mode-map (kbd "s-p") 'projectile-command-map)
(define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)

(use-package ivy
  :config
  (setq ivy-use-virtual-buffers t
	enable-recursive-minibuffers t)
  :bind (("C-s" . swiper)
	 ("C-c C-r" . ivy-resume)
	 ("M-x" . counsel-M-x)
	 ("C-x C-f" . counsel-find-file)))

(ivy-mode 1)
(define-key minibuffer-local-map (kbd "C-r") 'counsel-minibuffer-history)

(use-package yasnippet
  :config
  (progn
    (setq yas-snippet-dirs
	   (list "~/.doom.d/snippets"))
    (yas-global-mode)))

(use-package which-key
  :init (which-key-mode)
  :config
  (setq which-key-idle-delay 0.3))

(require 'org)

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
	(sequence "REPORT(r)" "BUG(b)" "KNOWNCAUSE(k)" "|" "FIXED(f)")
	))

(setq org-stuck-projects
      ;; identify a project with TODO keywords/tags
      ;; identify non-stuck state with TODO keywords
      ;; identify non-stuck state with tags
      ;; regexp match non-stuck projects
      '("-moyu&-MAYBE" ("TODO" "IN-PROGRESS") nil ""))

(setq org-export-with-toc nil)

(setq org-confirm-babel-evaluate nil)

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
 ))

(setq org-babel-python-command "python3")

(use-package org-bullets
  :ensure t
  :config
  (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1))))

(use-package auctex
  :defer t)

(use-package cdlatex
  :hook (org-mode . turn-on-org-cdlatex))

(defun langou/org-latex-delete-cache () (interactive)
       (delete-directory "~/.emacs.d/.local/cache/org-latex" :RECURSIVE t))

(use-package org-roam
  :commands org-roam-mode
  :init (add-hook 'after-init-hook 'org-roam-mode)
  :config (setq org-roam-directory "~/org-roam")
  :bind (("C-c r f" . org-roam-find-file)
         ;("H-r f" . org-roam-find-file)
	 ("C-c r c" . org-roam-db-build-cache)
	 ; ("H-r c" . org-roam-db-build-cache)
	 ("C-c r i" . org-roam-insert)
	 ;("H-r i" . org-roam-insert)
	 )
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

	  ("v" "vocabularies" entry
	   (file+headline "voc.org" "Inbox")
	   "* %<%Y-%m-%d %H:%M:%S>\n:PROPERTIES:\n:ANKI_NOTE_TYPE: Basic\n:ANKI_DECK: langou gre\n:END:\n** Front\n%?\n** Back\n\n")))

(setq org-agenda-files (apply (function append)
			        (mapcar
			         (lambda (directory)
				        (directory-files-recursively directory org-agenda-file-regexp))
			            '("~/org/"))))

(add-to-list 'org-modules 'org-habit)
(global-set-key (kbd "s-a") #'org-agenda)

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

(global-set-key (kbd "H-<return>") #'bookmark-jump)

(use-package company
  :config
  (setq company-idle-delay 0)
  :bind
  (("TAB" . company-indent-or-complete-common)))
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
    (append emms-player-mplayer-parameters
	    (list "-novideo"))))

(global-set-key (kbd "C-c m m") #'emms)
(global-set-key (kbd "C-c m p") #'emms-add-playlist)

(use-package pdf-tools
  :config (pdf-tools-install))

(use-package vterm
  :bind (("s-v" . vterm)))

(global-set-key (kbd "s-e") #'eshell)

(use-package doom-modeline
  :init (doom-modeline-mode 1)
  :config
  (progn
    (setq doom-modeline-height 23)))

(use-package lispy)
(add-hook 'emacs-lisp-mode-hook (lambda () (lispy-mode 1)))

(use-package racket-mode)

(use-package exwm
  :config
  (progn
    (setq exwm-workspace-number 10)
    (setq exwm-input-prefix-keys
	  '(?\C-x
	    ?\s-j
	    ?\s-C
	    ?\s-k
	    ?\s-v
	    ?\C-u
	    ?\C-h
	    ?\M-x
	    ?\M-&
	    ?\M-:
	    ?\H-a
	    ?\H-b
	    ?\H-c
	    ?\H-f
	    ?\H-s
	    ?\C-\ ))
    (setq exwm-input-global-keys
	  `(([?\s-r] . exwm-reset)
	    ([?\s-w] . exwm-workspace-switch)
	    ([?\s-o] . ibuffer)
	    ([?\s-\;] . (lambda (command)
			  (interactive (list (read-shell-command "$ ")))
			  (start-process-shell-command command nil command)))
	    ,@(mapcar (lambda (i)
			`(,(kbd (format "s-%d" i)) .
			  (lambda ()
			    (interactive)
			    (exwm-workspace-switch-create ,i))))
		      (number-sequence 0 9))))
    (exwm-input-set-simulation-keys
     '(([?\C-b] . left)
       ([?\C-f] . right)
       ([?\C-p] . up)
       ([?\C-n] . down)
       ([?\C-a] . home)
       ([?\C-e] . end)
       ))
    (setq exwm-workspace-warp-cursor t
	  mouse-autoselect-window t
	  focus-follows-mouse t)
    (exwm-enable)
    ))

(display-time-mode)
(display-battery-mode)

(require 'exwm-systemtray)
(exwm-systemtray-enable)

(require 'exwm-randr)
(setq exwm-randr-workspace-monitor-plist '(9 "DP-1-2" 9 "DP-2" 9 "DP-1-1" 9 "DP-1"))
(exwm-randr-enable)

(define-key exwm-mode-map [?\C-q] 'exwm-input-send-next-key)
(exwm-input-set-key (kbd "s-SPC") 'counsel-linux-app)

(defun efs/run-in-background (command)
  (let ((command-parts (split-string command "[ ]+")))
    (apply #'call-process `(,(car command-parts) nil 0 nil ,@(cdr command-parts)))))

(defun efs/update-displays ()
  (efs/run-in-background "autorandr --change --force")
  (message "Display config: %s"
	   (string-trim (shell-command-to-string "autorandr --current"))))

(add-hook 'exwm-randr-screen-change-hook #'efs/update-displays)
(efs/update-displays)

(use-package anki-editor)
