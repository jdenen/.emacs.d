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

;;; use-pkg
(defun use-pkg  (package)
  "Install and/or require PACKAGE."
  (if (package-installed-p package)
      (require package)
    t
    (progn
      (package-refresh-contents)
      (package-install package)
      (require package))))

;;; `helm'
(use-pkg 'helm)
(use-pkg 'helm-ls-git)
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
      helm-ff-file-name-history-use-recentf t)
(global-set-key (kbd "C-x m") 'helm-M-x)
(global-set-key (kbd "C-c h") 'helm-command-prefix)
(global-set-key (kbd "C-c p") 'helm-browse-project)
(global-set-key (kbd "C-x f") 'helm-for-files)

;;; `perspective'
(use-pkg 'perspective)
(persp-mode 1)

;;; `smartparens'
(use-pkg 'smartparens)
(smartparens-global-mode 1)
(require 'smartparens-config)

;;; `guide-key-mode'
(use-pkg 'guide-key)
(guide-key-mode 1)
(setq guide-key/guide-key-sequence '("C-x" "C-c"))
(setq guide-key/idle-delay 2.0)
(setq guide-key/recursive-key-sequence-flag t)

;;; `ruby-mode'
(use-pkg 'inf-ruby)
(use-pkg 'yari)
(defun ri-bind()
  "Bind yari to 'ruby-mode'."
  (local-set-key (kbd "C-c y") 'yari-helm))
(add-hook 'ruby-mode-hook 'inf-ruby-minor-mode)
(add-hook 'ruby-mode-hook 'ri-bind)

;;; `magit'
(use-pkg 'magit)
(global-set-key (kbd "C-x g") 'magit-status)

;;; `company'
(use-pkg 'company)
(global-company-mode 1)
(global-set-key (kbd "C-c C-c") 'company-complete)

;;; `expand-region'
(use-pkg 'expand-region)
(global-set-key (kbd "C-=") 'er/expand-region)

;;; `powerline'
(use-pkg 'powerline)
(powerline-default-theme)

;;; `ace-jump-mode'
(use-pkg 'ace-jump-mode)
(global-set-key (kbd "C-x j") 'ace-jump-char-mode)

;;; `winner-mode'
(winner-mode 1)

;;; `ample-theme'
(use-pkg 'ample-theme)
(load-theme 'ample t)

;;; visual
(tool-bar-mode 0)
(menu-bar-mode 0)
(scroll-bar-mode 0)
(add-to-list 'default-frame-alist '(font .  "Droid Sans Mono-10" ))
(set-face-attribute 'default t :font  "Droid Sans Mono-10" )

;;; start up
(setq inhibit-startup-screen t)
(setq initial-major-mode 'ruby-mode)
(setq initial-scratch-message "\
# This buffer is for notes you don't want to save
# and Ruby code you'll throw away. Go Cavs.")
(setq initial-buffer-choice "~/Code/notes.org")

;;; misc
(fset 'yes-or-no-p 'y-or-n-p)
(setq backup-directory-alist '(("." . "~/tmp")))

(provide 'init)
;;; init.el ends here
