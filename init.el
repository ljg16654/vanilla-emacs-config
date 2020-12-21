(setq debug-on-error t)
hi 
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

;;; font and theme
(set-face-attribute 'default nil :font "ubuntu mono" :height 120)
(load-theme 'modus-operandi)

;;; window and buffer
(global-set-key (kbd "s-j") #'next-buffer)
(global-set-key (kbd "s-k") #'previous-buffer)
(global-set-key (kbd "s-o") #'other-window)
(global-unset-key (kbd "C-x C-b"))
(global-set-key (kbd "C-x C-b") #'ibuffer)

;;; movement
(use-package avy
  :bind (("M-g g" . avy-goto-line)))

;;; project management
;;;; magit
(use-package magit
  :bind (("C-c g" . magit)))

;;; completion
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

;;; org
;;;; latex
(use-package auctex
  :defer t)

(use-package cdlatex
  :hook (org-mode . turn-on-org-cdlatex))

;;;; org-roam
(use-package org-roam
  :commands org-roam-mode
  :init (add-hook 'after-init-hook 'org-roam-mode)
  :config (setq org-roam-directory "~/org-roam")
  :bind (("C-c r f" . org-roam-find-file)
	 ("C-c r c" . org-roam-db-build-cache)))

(require 'org)

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

;;; misellaneous
(use-package command-log-mode)


