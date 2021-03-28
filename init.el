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

(use-package s)

(use-package flx)

(use-package general)

(use-package key-chord
  :init
  (setq key-chord-two-keys-delay 0.05)
  (setq key-chord-one-key-delay 0.2)
  :config
  (key-chord-mode t)
  (key-chord-define-global ",." "<>\C-b")
  (key-chord-define-global "pp" "()\C-b")
  (key-chord-define-global "jk" "[]\C-b")
  (key-chord-define-global "JK" "{}\C-b")
  )

;; variables that must be set before Evil is loaded:
(setq evil-respect-visual-line-mode t)
(setq evil-want-keybinding nil)

(use-package evil
  :config
  (progn
    (setq
     evil-want-C-d-scroll nil
     evil-want-C-u-delete nil
     evil-regexp-search t
     evil-auto-indent t
     evil-cross-lines t
     evil-insert-state-cursor 'hbar
     )
    (evil-mode +1)
    ))

(setq evil-want-Y-yank-to-eol t)
(setq evil-undo-system #'undo-redo)

;; major modes in which I prefer Emacs keybinding
(evil-set-initial-state 'Info-mode 'emacs)
(evil-set-initial-state 'elfeed-show-mode 'emacs)
(evil-set-initial-state 'elfeed-search-mode 'emacs)
(evil-set-initial-state 'nov-mode 'emacs)

(use-package evil-snipe
  :config
  (progn
    (setq evil-snipe-spillover-scope 'whole-buffer)
    (evil-snipe-mode +1)
    (evil-snipe-override-mode +1)))

;; '%' to jump
(use-package evil-matchit)
(global-evil-matchit-mode)

(use-package evil-exchange)
(evil-exchange-install)

;; customize evil-surround-pairs-alist
;; notice that different effects are expected from
;; cs[( and cs])
(use-package evil-surround
  :config
  (progn (global-evil-surround-mode 1)))

(use-package evil-embrace
  :after evil-surround
  :hook (LaTeX-mode . embrace-LaTeX-mode-hook)
  :hook (org-mode . embrace-org-mode-hook)
  :hook (emacs-lisp-mode . embrace-emacs-lisp-mode-hook)
  )


(use-package evil-args)
;; bind evil-args text objects
(define-key evil-inner-text-objects-map "a" 'evil-inner-arg)
(define-key evil-outer-text-objects-map "a" 'evil-outer-arg)

;; bind evil-forward/backward-args
(define-key evil-normal-state-map "L" 'evil-forward-arg)
(define-key evil-normal-state-map "H" 'evil-backward-arg)
(define-key evil-motion-state-map "L" 'evil-forward-arg)
(define-key evil-motion-state-map "H" 'evil-backward-arg)

(use-package evil-nerd-commenter
  :config (progn (evilnc-default-hotkeys)))

(global-set-key (kbd "H-x") #'helm-M-x)
(evil-escape-mode)

(evil-define-key 'normal 'prog-mode-map (kbd "SPC s") #'save-buffer)
(evil-define-key 'normal 'global (kbd "J") #'tab-bar-switch-to-next-tab)
(evil-define-key 'normal 'global (kbd "K") #'tab-bar-switch-to-prev-tab)
(evil-define-key 'normal 'global (kbd "SPC SPC") #'helm-M-x)
(evil-define-key '(insert normal) 'global (kbd "C-e") #'end-of-line)

(use-package evil-collection)
(evil-collection-init)

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
   "ros")
  ("x" #'(lambda () (interactive)
           (dired "~/code"))
   "code")
  ("o" #'(lambda () (interactive)
           (dired org-directory))
   "org")
  ("l" #'(lambda () (interactive)
           (find-file "~/org/reading-list.org")
           "reading list"))
  ("y" #'(lambda () (interactive)
           (dired (concat user-emacs-directory "snippet/"))
           "snippets")))

(use-package helm)

(setq helm-completion-style 'helm)

(helm-autoresize-mode)
(setq helm-autoresize-max-height 40)

(use-package helm-posframe
  :config
  (progn
    (setq helm-posframe-parameters
          '((left-fringe . 10)
            (right-fringe . 10)))
    ;; (helm-posframe-enable)
    ))

(use-package ace-jump-helm-line)
(eval-after-load "helm"
  '(define-key helm-map (kbd "C-'") 'ace-jump-helm-line))

(ido-mode t)
(global-set-key (kbd "M-i") #'ido-imenu-anywhere)

(use-package orderless)
(require 'orderless)

(setq completion-styles
      '(partial-completion
	flex
	initials
	substring
	orderless))

;; for file name completion, ignore case
(setq read-file-name-completion-ignore-case t)

;; set files to ignore in completion
;; completion-ignored-*

(use-package embark
  :config
  (global-set-key (kbd "s-,") #'embark-act)
  (global-set-key (kbd "H-b") #'embark-bindings))

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

(add-hook 'inferior-emacs-lisp-mode #'(lambda () (autopair-mode t)))

(use-package dash)
(use-package f)

(set-face-attribute 'default nil :family "Iosevka" :height 135)
(setq line-spacing 0.2)

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

(use-package doom-themes)

(load-theme 'modus-vivendi t)

(setq linum-format " %d  ")
(add-hook 'python-mode-hook #'linum-mode)

(use-package rg
  :config
  (progn
    (rg-enable-default-bindings))
  :bind
  ("s-f" . rg-menu))

;; seems to be dependency for projectile-ripgrep
(use-package ripgrep)

(use-package helm-rg
  :after (helm rg))

(use-package ag)

(global-set-key (kbd "C-;") #'iedit-mode)

(use-package helm-swoop)
(global-set-key (kbd "C-s") #'helm-swoop)
(global-set-key (kbd "C-_") #'helm-multi-swoop-all)
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

(use-package popper
  :init
  (setq popper-display-control nil)
  (setq popper-reference-buffers
	(list "\\*Python\\*"
              "\\*ielm\\*"))
  :config
  (defhydra 'popper-stuff
    (global-map "C-c t")
    ("t" #'popper-toggle-latest)
    ("o" #'popper-cycle)
    ("p" #'popper-toggle-type))

  (popper-mode +1))

(defun clear-popper-popup-alive ()
  "Clear popup buffers that are currently maintained by
popper.el. Useful when related rules are changed."
  (interactive)
  (progn
    (setq popper-open-popup-alist nil)
    (setq popper-buried-popup-alist nil)
    (message "Popper active alist cleared."))
  )

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
        ("\\*Python\\*"
         (display-buffer-reuse-mode-window display-buffer-at-bottom)
         (window-height . 0.4)
         (side . bottom)
         (slot . 1)
	 (window-parameters ((mode-line-format . none))
			    ))
        ("\\*ielm\\*"
         (display-buffer-reuse-mode-window display-buffer-at-bottom)
         (window-height . 0.4)
         (side . bottom)
         (slot . 2))
	("\\*Async Shell Command\\*"
	 (display-buffer-no-window))
        ;; left side window
        ("\\*Help.*"
         (display-buffer-reuse-mode-window display-buffer-at-bottom)
         (window-height . 0.35)       ; See the :hook
         (side . left)
         (slot . 0))
	("\\*pytest.*"
	 (display-buffer-in-side-window)
	 (window-width . 0.35)       ; See the :hook
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
        ("\\*.*\\([^E]eshell\\|shell\\|v?term\\).*"
         (display-buffer-reuse-mode-window display-buffer-at-bottom)
         (window-height . 0.4)
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

(setq Man-notify-method 'pushy)

(use-package eyebrowse)
(setq eyebrowse-new-workspace t)
(global-set-key (kbd "η") #'eyebrowse-prev-window-config)
(global-set-key (kbd "λ") #'eyebrowse-next-window-config)
(global-set-key (kbd "ν") #'eyebrowse-create-named-window-config)
(global-set-key (kbd "ρ") #'eyebrowse-switch-to-window-config)

;; between buffers

(global-set-key (kbd "s-o") #'switch-to-buffer)
(global-set-key (kbd "C-x C-b") #'ibuffer)
(evil-define-key 'normal 'global (kbd "SPC b i") #'ibuffer)
(global-set-key (kbd "C-M-,") #'previous-buffer)
(global-set-key (kbd "C-M-.") #'next-buffer)
(global-set-key (kbd "C-x <return> r")
		;; originally bound to
		;; revert-buffer-with-coding-system
		#'revert-buffer)
(evil-define-key 'normal 'global (kbd "SPC b r") #'revert-buffer)

;; inside a tab

(setq aw-keys
      (list ?a ?s ?d ?f ?j ?k ?l))

(global-set-key (kbd "χ") #'other-window)
(global-set-key (kbd "H-s") #'delete-other-windows)

(use-package tab-bar
  :init
  (setq tab-bar-close-button-show nil)
  (setq tab-bar-close-last-tab-choice 'tab-bar-mode-disable)
  (setq tab-bar-close-tab-select 'recent)
  (setq tab-bar-new-tab-choice "*scratch*")
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
(global-set-key (kbd "ψ") #'prot-tab-select-tab-dwim)

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

(use-package golden-ratio
  :config
  (progn
    (setq golden-ratio-exclude-modes
          (list
           "eshell-mode"
           "vterm-mode"
           "helm-mode"
           "dired-mode"))
    ))

(use-package avy)

(global-set-key (kbd "θ") #'ace-window)

(use-package magit
  :bind (("C-c g" . magit))
  )

(evil-define-key 'normal 'global (kbd "SPC g") #'magit)

(use-package git-gutter)

(use-package projectile)
(projectile-mode +1)
(define-key projectile-mode-map (kbd "s-p") 'projectile-command-map)
(define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)

(use-package helm-projectile
  ;; :after persp-projectile
  :after helm-rg
  :config
  (progn
    (helm-projectile-on)
    ))

(use-package ibuffer-projectile
  :after projectile
  :config
  (progn
    (add-hook 'ibuffer-hook
	      (lambda ()
		(ibuffer-projectile-set-filter-groups)
		(unless (eq ibuffer-sorting-mode 'alphabetic)
		  (ibuffer-do-sort-by-alphabetic))))))

(use-package org-projectile
  :after (org projectile)
  :config
  (progn
    (setq org-projectile-projects-file
          (concat org-directory "/project.org"))
    (global-set-key (kbd "C-c n p")
                    #'org-projectile-project-todo-completing-read)
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
                (dired-hide-details-mode +1)
                (dired-omit-mode +1)
                )))

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
    (define-key org-mode-map (kbd "C-,") nil)
    (setq org-ellipsis " ▾"
          org-hide-emphasis-markers t
          org-imenu-depth 7
          )
    (local-unset-key (kbd "C-'"))
    (font-lock-add-keywords 'org-mode
                            '(("^ *\\([-]\\) "
                               (0 (prog1 () (compose-region (match-beginning 1) (match-end 1) "•"))))))))

(global-set-key (kbd "C-c n l") #'org-store-link)

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
      `((nil :maxlevel . 2)
        (org-agenda-files :maxlevel . 2)
        (,(list (concat user-emacs-directory "init.org")) :maxlevel . 3)
        ;; (+personal-org-roam-files+ :maxlevel . 2)
        )
      ;; Without this, completers like ivy/helm are only given the first level of
      ;; each outline candidates. i.e. all the candidates under the "Tasks" heading
      ;; are just "Tasks/". This is unhelpful. We want the full path to each refile
      ;; target! e.g. FILE/Tasks/heading/subheading
      org-refile-use-outline-path 'file
      org-outline-path-complete-in-steps nil)

(setq org-todo-keywords
      '((sequence "MAYBE(m@)" "TODO(t)" "IN-PROGRESS(i@)" "STUCK(z@/@)" "|" "DONE(d@)" "CANCELLED(c@)")
        (sequence "REPORT(r)" "BUG(b/@)" "KNOWNCAUSE(k@)" "|" "FIXED(f)")
        (sequence "STUDY(s)" "|" "STUDIED(S@)" "ARCHIVED(a@)")
        ))

(setq org-stuck-projects
      ;; identify a project with TODO keywords/tags
      ;; identify non-stuck state with TODO keywords
      ;; identify non-stuck state with tags
      ;; regexp match non-stuck projects
      '("-moyu&-MAYBE" ("TODO" "IN-PROGRESS" "BUG" "KNOWNCAUSE") nil ""))

(setq org-export-with-toc nil)

(use-package org-sidebar)

(defhydra org-sidebar (org-mode-map "C-c l")
  "sidebar"
  ("t" #'org-sidebar-tree-toggle "tree")
  ("s" #'org-sidebar-toggle "default sidebar")
  )

(setq org-confirm-babel-evaluate nil)
(setq org-src-window-setup 'other-window)

;; display/update images in the buffer after I evaluate
(add-hook 'org-babel-after-execute-hook 'org-display-inline-images 'append)

;; additional identation relative to #begin_src token
(setq org-edit-src-content-indentation 0)
(setq org-src-tab-src-acts-natively t)
;; leading whitespace not preserved on export
(setq org-src-preserve-indentation nil)

(defun indent-org-block-automatically ()
  (when (org-in-src-block-p)
    (org-edit-special)
    (indent-region (point-min) (point-max))
    (org-edit-src-exit)))

(run-at-time 1 5 'indent-org-block-automatically)

(use-package ob-ipython)

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
   (ipython . t) 			;; provided by package ob-ipython
   ))

(setq org-babel-python-command "python3")

(unless (boundp 'org-latex-minted-langs)
  (setq org-latex-minted-langs nil))
(add-to-list 'org-latex-minted-langs '(ipython "python"))

(use-package elpy)
(use-package lispy)

(setq scimax-src-block-keymaps
      `(("ipython" . ,(let ((map (make-composed-keymap
                                  `(,elpy-mode-map ,python-mode-map ,pyvenv-mode-map)
                                  org-mode-map)))
                        ;; In org-mode I define RET so we f
                        (define-key map (kbd "<return>") 'newline)
                        (define-key map (kbd "C-c C-c") 'org-ctrl-c-ctrl-c)
                        map))
        ("python" . ,(let ((map (make-composed-keymap
                                 `(,elpy-mode-map ,python-mode-map ,pyvenv-mode-map)
                                 org-mode-map)))
                       ;; In org-mode I define RET so we f
                       (define-key map (kbd "<return>") 'newline)
                       (define-key map (kbd "C-c C-c") 'org-ctrl-c-ctrl-c)
                       map))
        ("emacs-lisp" . ,(let ((map (make-composed-keymap `(,lispy-mode-map
                                                            ,emacs-lisp-mode-map
                                                            ,outline-minor-mode-map)
                                                          org-mode-map)))
                           (define-key map (kbd "C-c C-c") 'org-ctrl-c-ctrl-c)
                           map))))

(defun scimax-add-keymap-to-src-blocks (limit)
  "Add keymaps to src-blocks defined in `scimax-src-block-keymaps'."
  (let ((case-fold-search t)
        lang)
    (while (re-search-forward org-babel-src-block-regexp limit t)
      (let ((lang (match-string 2))
            (beg (match-beginning 0))
            (end (match-end 0)))
        (if (assoc (org-no-properties lang) scimax-src-block-keymaps)
            (progn
              (add-text-properties
               beg end `(local-map ,(cdr (assoc
                                          (org-no-properties lang)
                                          scimax-src-block-keymaps))))
              (add-text-properties
               beg end `(cursor-sensor-functions
                         ((lambda (win prev-pos sym)
                            ;; This simulates a mouse click and makes a menu change
                            (org-mouse-down-mouse nil)))))))))))

(defun scimax-spoof-mode (orig-func &rest args)
  "Advice function to spoof commands in org-mode src blocks.
It is for commands that depend on the major mode. One example is
`lispy--eval'."
  (if (org-in-src-block-p)
      (let ((major-mode (intern (format "%s-mode" (first (org-babel-get-src-block-info))))))
        (apply orig-func args))
    (apply orig-func args)))

(define-minor-mode scimax-src-keymap-mode
  "Minor mode to add mode keymaps to src-blocks."
  :init-value nil
  (if scimax-src-keymap-mode
      (progn
        (add-hook 'org-font-lock-hook #'scimax-add-keymap-to-src-blocks t)
        (add-to-list 'font-lock-extra-managed-props 'local-map)
        (add-to-list 'font-lock-extra-managed-props 'cursor-sensor-functions)
        (advice-add 'lispy--eval :around 'scimax-spoof-mode)
        (cursor-sensor-mode +1))
    (remove-hook 'org-font-lock-hook #'scimax-add-keymap-to-src-blocks)
    (advice-remove 'lispy--eval 'scimax-spoof-mode)
    (cursor-sensor-mode -1))
  (font-lock-fontify-buffer))

;; (add-hook 'org-mode-hook (lambda ()
;;                             (scimax-src-keymap-mode +1)))

(use-package auctex
  :defer t)

(use-package cdlatex
  :hook (org-mode . turn-on-org-cdlatex))

(use-package org-fragtog
  :after org)
(add-hook 'org-mode-hook 'org-fragtog-mode)

(straight-use-package
 '(engrave-faces
   :host github
   :repo "tecosaur/engrave-faces"
   :branch "master"))

(with-eval-after-load 'ox-latex
  (setq org-latex-listings 'engraved))

(load-file (concat user-emacs-directory "lisp/my-org-extra.el"))

(defun langou/org-latex-delete-cache () (interactive)
       (delete-directory "~/.emacs.d/.local/cache/org-latex" :RECURSIVE t))

(use-package org-ref
  :config
  ;; list of BibTex database files used
  (setq reftex-default-bibliography
	(list "~/Zot/mylib/references.bib"
	      "~/canvassync/VR369/Assignments/ipe.bib"))
  (setq org-ref-default-bibliography
	(list "~/Zot/mylib/references.bib"
	      "~/canvassync/VR369/Assignments/ipe.bib"))
  ;; for helm completion:
  (setq bibtex-completion-bibliography
	(list "~/Zot/mylib/references.bib"
	      "~/canvassync/VR369/Assignments/ipe.bib"))
  (setq org-ref-pdf-directory
	'("~/Zot/mylib/files"))
  (setq bibtex-completion-library-path
	(list "~/Zot/mylib/files"))
  (setq org-ref-get-pdf-filename-function
	#'org-ref-get-pdf-filename-helm-bibtex)
  (setq org-ref-notes-function 'org-ref-notes-function-many-files)
  (setq bibtex-completion-pdf-open-function
	(lambda (fpath)
	  (start-process "zathura_bibtex" "*zathura open pdf*" "zathura" fpath))))

(setq bibtex-completion-pdf-field "file")

(defun my/org-ref-open-pdf-at-point ()
  "Open the pdf for bibtex key under point if it exists."
  (interactive)
  (let* ((results (org-ref-get-bibtex-key-and-file))
         (key (car results))
	 (pdf-cite:mur-artal_orb-slam2_2017file (car (bibtex-completion-find-pdf key))))
    (if (file-exists-p pdf-file)
	(org-open-file pdf-file)
      (message "No PDF found for %s" key))))

(setq org-ref-open-pdf-function 'my/org-ref-open-pdf-at-point)

(setq org-latex-pdf-process
      '("pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"
	"bibtex %b"
	"pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"
	"pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"))

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

(general-define-key
 :prefix "C-c r"
 "d" #'(lambda () (interactive)
         (dired org-roam-directory))
 "f" #'org-roam-find-file
 "y" #'org-roam-dailies-find-yesterday
 "x" #'org-roam-dailies-find-today
 "j" #'org-roam-dailies-capture-today
 "i" #'org-roam-insert)

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

(use-package org-roam-bibtex
  :after org-roam
  :hook (org-roam-mode . org-roam-bibtex-mode)
  :config
  (require 'org-ref)) ; optional: if Org Ref is not loaded anywhere else, load it here

(setq org-file-apps '((auto-mode . emacs)
                      ("\\.mm\\'" . default)
                      ("\\.x?html?\\'" . default)
                      ("\\.pdf\\'" . "zathura %s")))

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

(require 'org-projectile)
(push (org-projectile-project-todo-entry) org-capture-templates)

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
	("ex" . "example")
	("E" . "export")
	("h" . "export html\n")
	("l" . "export latex\n")
	("q" . "quote\n")
	("s" . "src")
	("v" . "verse\n")
	;; org latex stuff
	("pf" . "proof")
	("th" . "theorem")
	("le" . "lemma")
	("pr" . "proposition")))

(use-package org-pdftools
  :hook (org-mode . org-pdftools-setup-link))

(use-package org-noter)

(use-package org-noter-pdftools
  :after org-noter
  :config
  (with-eval-after-load 'pdf-annot
    (add-hook 'pdf-annot-activate-handler-functions #'org-noter-pdftools-jump-to-note)))

(defun pdf-open-with-zathura ()
  "Use zathura to open file related to current buffer"
  (interactive)
  (async-shell-command
   (concat "zathura "
	   (buffer-file-name (current-buffer)))))

(use-package zotxt
  :after org
  :config
  (org-zotxt-mode t)
  )

(use-package yequake)

(setq yequake-frames
      '(("aba" .
	 ((width . 0.75)
	  (height . 0.5)
	  (alpha . 0.90)
	  (buffer-fns .  (org-roam-dailies-find-today
			  split-window-horizontally
			  (find-file "~/org/reading-list.org")))
	  (frame-parameters . ((undecorated . t)))))))

(defun my-aba-float ()
  (progn  (select-frame (make-frame '((name . "aba")
				      (alpha . 80))))
	  (find-file "~/org/reading-list.org")
	  (split-window-horizontally)
	  (org-roam-dailies-find-today)))

(use-package lorem-ipsum)

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
       '("rho" . ?ρ)
       '("mu" . ?μ)
       '("theta" . ?θ)
       '("_0" . ?₀)
       '("_1" . ?₁)
       '("_2" . ?₂)
       ))

(add-hook 'python-mode-hook #'prettify-symbols-mode)

(add-hook 'kill-emacs-hook #'bookmark-save)

(global-set-key (kbd "H-j") #'jump-to-register)
(global-set-key (kbd "H-SPC") #'point-to-register)

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

(use-package exec-path-from-shell
  :config
  (when (memq window-system '(mac ns x))
    (exec-path-from-shell-initialize)))

(use-package rainbow-mode)

(use-package lsp-mode)

(use-package flycheck)

(use-package lsp-ui
  :after lsp-mode
  :demand flycheck
  )

(use-package lsp-python-ms
  :init (setq lsp-python-ms-auto-install-server t
              read-process-output-max 1048576)
  ;; :hook (python-mode . (lambda ()
  ;;                        (require 'lsp-python-ms)
  ;;                        (lsp)))
  )

(define-key lsp-ui-mode-map [remap xref-find-definitions] #'lsp-ui-peek-find-definitions)
(define-key lsp-ui-mode-map [remap xref-find-references] #'lsp-ui-peek-find-references)

(setq lsp-ui-doc-enable nil)
(setq lsp-enable-links nil)
(setq lsp-headerline-breadcrumb-enable nil)
(setq lsp-signature-render-documentation nil)
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

(use-package nov
  :config
  (progn
    (add-to-list 'auto-mode-alist '("\\.epub\\'" . nov-mode))
    ))

(use-package vterm)
(general-define-key
 :keymaps 'vterm-copy-mode-map
 "q" #'vterm-copy-mode)

(setq vterm-max-scrollback 10000)

(use-package multi-vterm
  :after vterm)

(global-set-key (kbd "s-v") #'multi-vterm-dedicated-toggle)

;; (global-set-key (kbd "s-v v") #'multi-vterm)
;; (defhydra multi-vterm (vterm-mode-map "s-v")
;;   "multi-vterm"
;;   ("d" #'multi-vterm-dedicated-toggle "dedicated")
;;   ("n" #'multi-vterm-next "next")
;;   ("p" #'multi-vterm-prev "prev")
;;   ("s" #'multi-vterm-dedicated-select "select as dedicated")
;;   ("r" #'multi-vterm-rename-buffer "rename")
;;   )

(setq multi-vterm-dedicated-window-height 30)

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

(set-face-attribute 'mode-line nil :box t)

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
(diminish 'evil-snipe-mode)
(diminish 'evil-escape-mode)

(use-package doom-modeline
  ;; :init (doom-modeline-mode 1)
  :config
  (progn
    (setq doom-modeline-height 15)))

(use-package nyan-mode
  ;; :config
  ;; (nyan-mode)
  ;; (nyan-start-animation)
  )

(use-package helm-catkin)

(add-to-list 'auto-mode-alist '("\\.world\\'" . xml-mode))
(add-to-list 'auto-mode-alist '("\\.launch\\'" . xml-mode))

(use-package anki-editor)

(use-package rainbow-delimiters)
(add-hook 'prog-mode-hook #'rainbow-delimiters-mode)

(global-set-key (kbd "H-r") #'compile)
(global-set-key (kbd "σ") #'compile)

(use-package imenu-anywhere)
(global-set-key (kbd "C-.") #'imenu-anywhere)

(use-package autopair)
(add-hook 'python-mode-hook #'autopair-mode)

(use-package lispy)
(use-package evil-lispy)

(use-package paren-face)
(add-hook 'emacs-lisp-mode-hook (lambda () (paren-face-mode 1)))
(add-hook 'emacs-lisp-mode-hook (lambda () (autopair-mode 1)))

(use-package highlight-parentheses)
(add-hook 'emacs-lisp-mode-hook (lambda () (highlight-parentheses-mode 1)))

(use-package racket-mode)

(use-package wolfram-mode
  :config
  (setq wolfram-path "~/.Mathematica/Applications")
  (add-to-list 'auto-mode-alist
	       '("\\.wl\\'" . wolfram-mode))
  )

(use-package cmake-mode)

(use-package elpy)
;; (elpy-enable)
(use-package jedi)

(use-package python-pytest)
(evil-define-key 'normal python-mode-map (kbd "SPC t") #'python-pytest-dispatch)

;; for font-lock and filling paragraphs inside docstring region:
(use-package python-docstring)

;; for generating docstring of a defun whenever needed
(use-package sphinx-doc)
(add-hook 'python-mode-hook #'(lambda ()
				(sphinx-doc-mode t)
				))

(defun c-mode-my-basic-settings ()
  (progn
    (linum-mode t)
    (autopair-mode t)))

(add-hook 'c-mode-hook #'c-mode-my-basic-settings)
(add-hook 'c++-mode-hook #'c-mode-my-basic-settings)

(use-package clang-format)

(use-package cider)

(setq desktop-save-mode nil)

(defun efs/org-babel-tangle-config ()
  (when (string-equal (file-name-directory (buffer-file-name))
                      (expand-file-name user-emacs-directory))
    ;; Dynamic scoping to the rescue
    (let ((org-confirm-babel-evaluate nil))
      (org-babel-tangle))))

(add-hook 'org-mode-hook (lambda () (add-hook 'after-save-hook
					      #'efs/org-babel-tangle-config)))
