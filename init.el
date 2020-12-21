(setq user-full-name "Jigang Li"
      user-mail-address "ljg16654@sjtu.edu.cn")

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

(set-face-attribute 'default nil :font "ubuntu mono" :height 160)
(load-theme 'modus-operandi)

(global-set-key (kbd "s-o") #'ibuffer)
(global-set-key (kbd "s-O") #'previous-buffer)
(global-set-key (kbd "s-j") #'other-window)
(global-set-key (kbd "s-k") #'(lambda () (interactive)
				(other-window -1)))
(global-unset-key (kbd "C-x C-b"))
(global-set-key (kbd "C-x C-b") #'ibuffer)

(use-package avy
  :bind (("M-g g" . avy-goto-line)))

(defun langou/goto-config ()
  "go to directory of emacs config"
  (interactive)
  (find-file "~/vanilla/init.org"))

(global-set-key (kbd "C-c f p") #'langou/goto-config)

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
(expand-file-name +org-capture-journal-file+ org-directory)

;;;; org-journal
(global-set-key (kbd "C-c j") #'(lambda ()
				  (interactive)
				  (find-file
				   (concat org-directory "/journal.org"))))

(global-set-key (kbd "C-c c") #'org-capture)

(setq org-capture-templates
        '(("t" "Personal t" entry
           (file+headline +org-capture-todo-file "Inbox")
              "* TODO [%^{Select the urgency|A|B|C}] %?\n%i\n%a\n" :prepend t)

          ("n" "Personal notes" entry
           (file+headline +org-capture-notes-file "Inbox")
              "* %u %?\n%i\n%a" :prepend t)

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
           (file "read-later.org")
              "[ ] %? ")))

(use-package command-log-mode)

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

(use-package emms)
