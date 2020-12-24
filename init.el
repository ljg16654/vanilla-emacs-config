(setq user-full-name "Jigang Li"
      user-mail-address "ljg16654@sjtu.edu.cn")

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

(defun +default/save-to-king-ring-buffer-filename ()
  "Copy the current buffer's path to the kill ring."
  (interactive)
  (if-let (filename (or buffer-file-name (bound-and-true-p list-buffers-directory)))
      (message (kill-new (abbreviate-file-name filename)))
    (error "Couldn't find filename in current buffer")))

(global-set-key (kbd "C-c k f")  #'+default/save-to-king-ring-buffer-filename)

(use-package olivetti
  :bind (("C-c f e" . olivetti-mode)))

(set-face-attribute 'default nil :font "iosevka" :height 130)

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
  "go to personal configuration of emacs"
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
