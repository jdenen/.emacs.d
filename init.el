;;; init.el -- custom configuration
;;;
;;; Commentary:
;;; Try to keep things simple and `helm' centric.
;;;
;;; Code:

;;; MELPA
(require 'package)
(add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/") t)
(package-initialize)

;;; Install/require PACKAGE
(defun inst-pkg (package)
  "Conditionally install the given PACKAGE."
  (if (package-installed-p package)
      (require package)
      t
    (progn
      (package-refresh-contents)
      (package-install package)
      (require package))))

;;; Packages
(inst-pkg 'helm)
(inst-pkg 'helm-ls-git)
(inst-pkg 'magit)
(inst-pkg 'company)
(inst-pkg 'expand-region)
(inst-pkg 'guide-key)
(inst-pkg 'inf-ruby)
(inst-pkg 'yari)
(inst-pkg 'smartparens)
(inst-pkg 'powerline)
(inst-pkg 'ample-theme)
(inst-pkg 'ace-jump-mode)

;;; Modes
(tool-bar-mode 0)
(menu-bar-mode 0)
(scroll-bar-mode 0)
(winner-mode 1)
(guide-key-mode 1)
(helm-mode 1)
(global-company-mode 1)
(smartparens-global-mode 1)

;;; `helm'
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
      helm-ff-file-name-history-use-recentf t)

;;; `smartparens'
(require 'smartparens-config)

;;; `guide-key-mode'
(setq guide-key/guide-key-sequence '("C-x" "C-c"))
(setq guide-key/idle-delay 2.0)
(setq guide-key/recursive-key-sequence-flag t)

;;; `ruby-mode'
(defun ri-bind()
  "Bind yari to 'ruby-mode'."
  (local-set-key (kbd "C-c y") 'yari-helm))
(add-hook 'ruby-mode-hook 'inf-ruby-minor-mode)
(add-hook 'ruby-mode-hook 'ri-bind)

;;; Keybindings
(global-set-key (kbd "C-=") 'er/expand-region)
(global-set-key (kbd "C-x m") 'helm-M-x)
(global-set-key (kbd "C-c h") 'helm-command-prefix)
(global-set-key (kbd "C-c p") 'helm-browse-project)
(global-set-key (kbd "C-x f") 'helm-for-files)
(global-set-key (kbd "C-c C-c") 'company-complete)
(global-set-key (kbd "C-x j") 'ace-jump-char-mode)
(global-set-key (kbd "C-x g") 'magit-status)

;;; Misc
(fset 'yes-or-no-p 'y-or-n-p)
(setq backup-directory-alist '(("." . "~/tmp")))
(powerline-default-theme)
(add-to-list 'default-frame-alist '(font .  "Droid Sans Mono-10" ))
(set-face-attribute 'default t :font  "Droid Sans Mono-10" )
(setq inhibit-startup-screen t)
(setq initial-major-mode 'ruby-mode)
(load-theme 'ample t)

(provide 'init)
;;; init.el ends here
