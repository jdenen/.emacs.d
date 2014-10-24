;;; init.el --- custom configuration
;;;
;;; Commentary:
;;;
;;; Code:

;;;
;;; MELPA setup
;;;
(require 'package)
(add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/") t)
(package-initialize)

;;;
;;; Install packages on demand
;;;
(defun inst-pkg (package)
  "Conditionally install the given PACKAGE."
  (if (package-installed-p package)
      t
    (progn
      (package-refresh-contents)
      (package-install package))))

;;;
;;; Helm
;;;
(inst-pkg 'helm)
(require 'helm-config)
(helm-mode 1)
(global-set-key (kbd "C-c h") 'helm-command-prefix)
(global-unset-key (kbd "C-x c")) ; undo default key map because it's too close to C-x C-c
(define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action) ; rebind tab to do persistent action
(define-key helm-map (kbd "C-i") 'helm-execute-persistent-action) ; make TAB work in terminal
(define-key helm-map (kbd "C-z")  'helm-select-action) ; list actions using C-z
(when (executable-find "curl")
  (setq helm-google-suggest-use-curl-p t))
(setq helm-quick-update                     t ; do not display invisible candidates
      helm-split-window-in-side-p           t ; open helm buffer inside current window, not occupy whole other window
      helm-buffers-fuzzy-matching           t ; fuzzy matching buffer names when non--nil
      helm-move-to-line-cycle-in-source     t ; move to end or beginning of source when reaching top or bottom of source.
      helm-ff-search-library-in-sexp        t ; search for library in 'require' and 'declare-function' sexp.
      helm-scroll-amount                    8 ; scroll 8 lines other window using M-<next>/M-<prior>
      helm-ff-file-name-history-use-recentf t)

;;;
;;; Fullscreen
;;;
(inst-pkg 'fullscreen-mode)
(require 'fullscreen-mode)
(fullscreen-mode 1)

;;;
;;; Ace Jump
;;;
(inst-pkg 'ace-jump-mode)
(require 'ace-jump-mode)
(global-set-key (kbd "C-c j") 'ace-jump-char-mode)

;;;
;;; 
;;;


;;;
;;; Fuzzy
;;;
(inst-pkg 'flx-ido)
(require 'flx-ido)
(ido-mode 1)
(flx-ido-mode 1)
(ido-everywhere 1)
(setq ido-use-faces)
(global-set-key (kbd "C-x m") 'smex)

;;;
;;; Magit
;;;
(inst-pkg 'magit)
(require 'magit)
(global-set-key (kbd "C-x g") 'magit-status)

;;;
;;; Guide Key
;;;
(inst-pkg 'guide-key)
(require 'guide-key)
(guide-key-mode 1)
(setq guide-key/guide-key-sequence '("C-x" "C-c"))
(setq guide-key/idle-delay 2.0)
(setq guide-key/recursive-key-sequence-flag t)

;;;
;;; iBuffer
;;;
(inst-pkg 'ibuffer-vc)
(require 'ibuffer-vc)
(add-hook 'ibuffer-hook
	  (lambda ()
	    (ibuffer-vc-set-filter-groups-by-vc-root)))
(global-set-key (kbd "C-x C-b") 'ibuffer)
(global-set-key (kbd "C-x k") 'bury-buffer)

;;;
;;; Ruby
;;;
(inst-pkg 'inf-ruby)
(inst-pkg 'yari)
(require 'inf-ruby)
(require 'yari)
(defun ri-bind()
  "Bind yari to 'ruby-mode'."
  (local-set-key (kbd "C-c y") 'yari))
(global-set-key (kbd "C-c C-p") 'inf-ruby)  
(add-hook 'ruby-mode-hook 'ri-bind)
(add-hook 'ruby-mode-hook 'inf-ruby-minor-mode)

;;;
;;; Pry
;;;
(autoload 'inf-ruby-minor-mode "inf-ruby" "Run an inferior Ruby process" t)
(setq inf-ruby-default-implementation "pry")
;(set inf-ruby-first-prompt-pattern "^\\[[0-9]+\\] pry\\((.*)\\)> *")
;(setq inf-ruby-prompt-pattern "^\\[[0-9]+\\] pry\\((.*)\\)[>*\"'] *")

;;;
;;; Projectile
;;;
(inst-pkg 'projectile)
(inst-pkg 'helm-projectile)
(require 'projectile)
(require 'helm-projectile)
(projectile-global-mode 1)

;;;
;;; Graphene
;;;
(inst-pkg 'graphene)
(require 'graphene)

;;;
;;; General settings
;;;
(tool-bar-mode 0)
(menu-bar-mode 0)
(winner-mode 1)
(custom-set-variables '(coffee-tab-width 2))
(fset 'yes-or-no-p 'y-or-n-p)
(setq backup-directory-alist '(("." . "~/tmp")))

(global-set-key (kbd "C-=") 'er/expand-region)
(global-set-key (kbd "C-c m") 'set-mark-command)
(global-set-key (kbd "C-c n") 'new-frame)
(global-set-key (kbd "C-c k") 'delete-frame)
(global-set-key (kbd "C-c g") 'goto-line)
(global-set-key (kbd "C-c t") 'ansi-term)

;;; provide init.el
(provide 'init)
;;; init.el ends here
