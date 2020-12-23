(setq user-full-name "Jigang Li"
      user-mail-address "ljg16654@sjtu.edu.cn"
)

(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(setq use-file-dialog nil)
(setq use-dialog-box t)               ; only for mouse events

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

(use-package olivetti
  :bind (("C-c f e" . olivetti-mode)))

(set-face-attribute 'default nil :font "iosevka" :height 160)

(use-package anti-zenburn-theme)
(use-package spacemacs-theme
  :defer t)
(load-theme 'modus-vivendi t)

(global-set-key (kbd "s-o") #'ibuffer)
(global-set-key (kbd "s-O") #'previous-buffer)
(global-set-key (kbd "s-j") #'other-window)
(global-set-key (kbd "s-k") #'(lambda () (interactive)
				(other-window -1)))
(global-unset-key (kbd "C-x C-b"))
(global-set-key (kbd "C-x C-b") #'ibuffer)

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

(use-package avy
  :bind (("M-g g" . avy-goto-line)))

(defun langou/goto-config ()
  "go to directory of emacs config"
  (interactive)
  (find-file "~/vanilla/init.org"))

(global-set-key (kbd "C-c f p") #'langou/goto-config)

(global-set-key (kbd "M-i") 'imenu)

(use-package eyebrowse
  :config (eyebrowse-mode 1))

(use-package magit
  :bind (("C-c g" . magit)))

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

(require 'org)

(setq org-export-with-toc nil)

(setq org-confirm-babel-evaluate nil)

(use-package org-bullets
  :ensure t
  :config
  (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1))))

(use-package auctex
  :defer t)

(use-package cdlatex
  :hook (org-mode . turn-on-org-cdlatex))

(use-package org-roam
  :commands org-roam-mode
  :init (add-hook 'after-init-hook 'org-roam-mode)
  :config (setq org-roam-directory "~/org-roam")
  :bind (("C-c r f" . org-roam-find-file)
	 ("C-c r c" . org-roam-db-build-cache)))

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

(setq org-capture-templates
	'(("t" "Personal todo" entry
	   (file+headline "todo.org" "Inbox")
	      "* TODO [%^{Select the urgency|A|B|C}] %?\n%i\n%a\n" :prepend t)

	  ("n" "Personal notes" entry
	   (file+headline "notes.org" "Inbox")
	      "* %U %?\n%i\n%a" :prepend t)

	  ("f" "Maybe it would be fun someday..." entry
	   (file+headline "just-for-fun.org" "Inbox")
	   "* %U %?" :prepend t)

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
	      "[ ] %? ")))

(setq org-agenda-files (apply (function append)
			        (mapcar
			         (lambda (directory)
				        (directory-files-recursively directory org-agenda-file-regexp))
			            '("~/org/"))))

(add-to-list 'org-modules 'org-habit)
(global-set-key (kbd "s-a") #'org-agenda)

(use-package command-log-mode)

(defconst lisp--prettify-symbols-alist
    '(("lambda"  . ?λ)))

(add-hook 'lisp-mode-hook #'(lambda () (interactive)
			     (prettify-symbols-mode +1)))

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
