;;; init.el -- custom configuration
;;;
;;; Commentary:
;;; Try to keep things simple and `helm' centric.
;;;
;;; Code:

;; User
(setq user-full-name "Johnson Denen"
      user-mail-address "jdenen@manta.com")

;; MELPA
(require 'package)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/") t)
(package-initialize)

;; johnson/package-install
(defun johnson/package-install (package)
  "Install PACKAGE if it has not already been installed."
  (unless (package-installed-p package)
    (package-install package)))

;; `use-package'
(johnson/package-install 'use-package)
(require 'use-package)

;; `helm'
(johnson/package-install 'helm)
(use-package helm
  :init
  (progn
    (helm-mode 1)
    (require 'helm-config)
    (global-unset-key (kbd "C-x c"))
    (define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action)
    (define-key helm-map (kbd "C-i") 'helm-execute-persistent-action)
    (define-key helm-map (kbd "C-z")  'helm-select-action)
    (when (executable-find "curl")
      (setq helm-google-suggest-use-curl-p t))
    (setq helm-quick-update                     t
	  helm-split-window-in-side-p           t
	  helm-buffers-fuzzy-matching           t
	  helm-move-to-line-cycle-in-source     t
	  helm-ff-search-library-in-sexp        t
	  helm-scroll-amount                    8
	  helm-ff-file-name-history-use-recentf t))
  :bind
  ("C-x m" . helm-M-x)
  ("C-c h" . helm-mini)
  ("C-x h" . helm-command-prefix))

;; `helm-swoop'
(johnson/package-install 'helm-swoop)
(use-package helm-swoop
  :bind
  ("C-S-s" . helm-swoop))

;; `projectile'
(johnson/package-install 'projectile)
(use-package projectile
  :init
  (projectile-global-mode 1))

;; `helm-projectile'
(johnson/package-install 'helm-projectile)
(use-package helm-projectile
  :init
  (helm-projectile-on))

;; `smartparens'
(johnson/package-install 'smartparens)
(use-package smartparens
  :init
  (progn
    (smartparens-global-mode 1)
    (show-smartparens-global-mode 1)
    (require 'smartparens-config)))

;; `guide-key-mode'
(johnson/package-install 'guide-key)
(use-package guide-key
  :init
  (guide-key-mode 1)
  (setq guide-key/guide-key-sequence '("C-x" "C-c"))
  (setq guide-key/idle-delay 2.0)
  (setq guide-key/recursive-key-sequence-flag t))

;; Execute the current buffer with environment variables.
(defun johnson/rspec-chrome (env)
  "Execute the current spec buffer with ENV variables."
  (interactive "sBROWSER_TYPE: ")
  (progn
    (setq rspec-use-bundler-when-possible nil)
    (setq rspec-spec-command (concat (format "BROWSER_TYPE=%s" env) " rspec"))
    (rspec-run-single-file
     (rspec-spec-file-for (buffer-file-name))
     (rspec-core-options))))

;; `rbenv'
(johnson/package-install 'rbenv)
(use-package rbenv
  :init
  (global-rbenv-mode 1))

;; `inf-ruby'
(johnson/package-install 'inf-ruby)
(use-package inf-ruby
  :init
  (add-hook 'ruby-mode-hook 'inf-ruby-minor-mode))

;; `ansi-term-pry'
(defun johnson/ansi-term-pry ()
  "Pry via `ansi-term'"
  (interactive)
  (if (get-buffer "*Pry*")
      (switch-to-buffer-other-window "*Pry*")
      (ansi-term "pry" "Pry")))
(bind-key "C-c t p" 'johnson/ansi-term-pry)

;; `key-chord'
(johnson/package-install 'key-chord)
(use-package key-chord
  :init
  (progn
    (key-chord-mode 1)
    (key-chord-define-global "jj" 'ace-jump-char-mode)
    (key-chord-define-global "jg" 'goto-line)
    (key-chord-define-global "js" 'helm-swoop)
    (key-chord-define-global "jr" 'jump-to-register)))

;; registers
(mapcar
 (lambda (r)
   (set-register (car r) (cons 'file (cdr r))))
 '((?i . "~/.emacs.d/init.el")
   (?n . "~/Code/notes.org")
   (?h . "~/Code/mantacode/manta-automated-test-suite/spec/spec_helper.rb")))

;; `yari'
(johnson/package-install 'yari)
(use-package yari
  :bind
  ("C-c y" . yari-helm))

;; `rspec-mode'
(johnson/package-install 'rspec-mode)
(use-package rspec-mode
  :init
  (setq rspec-use-rake-when-possible nil)
  (setq rspec-command-options "--format progress")
  :bind
  ("C-c , i" . johnson/rspec-chrome))

;; `magit'
(johnson/package-install 'magit)
(use-package magit
  :bind
  ("C-x g" . magit-status))

;; `company'
(johnson/package-install 'company)
(use-package company
  :init
  (global-company-mode 1)
  :bind
  ("C-c C-c" . company-complete))

;; `expand-region'
(johnson/package-install 'expand-region)
(use-package expand-region
  :bind
  ("C-=" . er/expand-region))

;; `powerline'
(johnson/package-install 'powerline)
(use-package powerline
  :config
  (powerline-default-theme))

;; `ace-jump-mode'
(johnson/package-install 'ace-jump-mode)
(use-package ace-jump-mode
  :bind
  ("C-x j" . ace-jump-char-mode))

;; `winner-mode'
(winner-mode 1)

;; `ample-theme'
(johnson/package-install 'ample-theme)
(load-theme 'ample t)

;; `ibuffer-vc'
(johnson/package-install 'ibuffer-vc)
(use-package ibuffer-vc
  :init
  (add-hook 'ibuffer-hook
	    (lambda ()
	      (ibuffer-vc-generate-filter-groups-by-vc-root)))
  :bind
  ("C-x C-b" . ibuffer))

;; visual settings
(tool-bar-mode 0)
(menu-bar-mode 0)
(scroll-bar-mode 0)
(add-to-list 'default-frame-alist '(font .  "Droid Sans Mono-10" ))
(set-face-attribute 'default t :font  "Droid Sans Mono-10" )

;; start up settings
(setq inhibit-startup-screen t)
(setq initial-major-mode 'ruby-mode)
(setq initial-buffer-choice "~/Code/notes.org")

;; misc settings
(fset 'yes-or-no-p 'y-or-n-p)
(setq backup-directory-alist '(("." . "~/tmp")))
(global-set-key (kbd "C-c C-q") 'indent-region)
(global-set-key (kbd "C-x k") 'bury-buffer)
(global-set-key (kbd "C-c n") 'new-frame)
(global-set-key (kbd "C-c k") 'delete-frame)
(global-set-key (kbd "C-x C-k") 'kill-this-buffer)
(bind-key "C-+" 'text-scale-increase)
(bind-key "C--" 'text-scale-decrease)
