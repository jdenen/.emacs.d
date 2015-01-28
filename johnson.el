
(setq user-full-name "Johnson Denen"
      user-mail-address "johnson.denen@gmail.com")

(defun johnson/package-install (package)
  "Install PACKAGE if it has not already been installed."
  (unless (package-installed-p package)
    (package-install package)))

(johnson/package-install 'use-package)
(require 'use-package)

(setq inhibit-startup-screen t)
(tool-bar-mode 0)
(menu-bar-mode 0)
(scroll-bar-mode 0)

(setq initial-major-mode 'ruby-mode)
(setq initial-scratch-message "# This is a Ruby scratch buffer. \n\n")

(johnson/package-install 'ample-theme)
(load-theme 'ample t)
(add-to-list 'default-frame-alist '(font .  "Droid Sans Mono-8"))
(set-face-attribute 'default t :font  "Droid Sans Mono-8")

(global-hl-line-mode 1)

(fset 'yes-or-no-p 'y-or-n-p)
(setq backup-directory-alist '(("." . "~/tmp")))

(johnson/package-install 'guide-key)
(use-package guide-key
  :init
  (progn
    (guide-key-mode 1)
    (setq guide-key/guide-key-sequence '("C-x" "C-c"))
    (setq guide-key/idle-delay 2.0)
    (setq guide-key/recursive-key-sequence-flag t)))

(johnson/package-install 'magit)
(use-package magit
  :bind 
  ("C-x g" . magit-status))

(johnson/package-install 'projectile)
(use-package projectile
  :init 
  (projectile-global-mode t))

(johnson/package-install 'helm)
(use-package helm
  :init
  (progn
    (helm-mode 1)
    (require 'helm-config)
    (global-unset-key (kbd "C-x c"))
    (define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action)
    (define-key helm-map (kbd "C-z")  'helm-select-action)
    (when (executable-find "curl")
      (setq helm-google-suggest-use-curl-p      t))
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
  ("C-x y"   . helm-show-kill-ring)
  ("C-x h" . helm-command-prefix))

(johnson/package-install 'helm-projectile)
(use-package helm-projectile
  :init 
  (helm-projectile-on))

(johnson/package-install 'helm-swoop)
(use-package helm-swoop
  :bind
  ("C-s" . helm-swoop))
