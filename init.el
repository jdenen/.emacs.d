;;; init.el --- Personal configuration
;;
;;; Commentary:
;;
;;; Code:

;;---------------------------------------------------------------------------
;; Load and require custom lisps
;;---------------------------------------------------------------------------
(setq custom-dir 
      (expand-file-name "custom" user-emacs-directory))
(add-to-list 'load-path user-emacs-directory)
(add-to-list 'load-path custom-dir)

(require 'init-pkg)
(require 'init-god)
(require 'init-ruby)
(require 'init-buffer)
(require 'init-fuzzy)

;;---------------------------------------------------------------------------
;; Toggle into modes
;;---------------------------------------------------------------------------
(fullscreen-mode 1)
(projectile-global-mode 1)
(recentf-mode 1)
(winner-mode 1)

;;---------------------------------------------------------------------------
;; Toggle out of modes
;;---------------------------------------------------------------------------
(tool-bar-mode 0)
(menu-bar-mode 0)

;;---------------------------------------------------------------------------
;; Keybindings
;;---------------------------------------------------------------------------
(global-set-key (kbd "<F11>") 'fullscreen-mode)
(global-set-key (kbd "C-x g") 'magit-status)
(global-set-key (kbd "C-x C-r") 'recentf-open-files)
(global-set-key (kbd "C-x j") 'ace-jump-char-mode)
(global-set-key (kbd "C-c m") 'set-mark-command)
(global-set-key (kbd "C-c f m") 'mc/mark-all-like-this-dwim)
(global-set-key (kbd "C-c e") 'mc/edit-lines)
(global-set-key (kbd "C-c t") 'eshell)
(global-set-key (kbd "C-<backspace>") (lambda ()
                                        (interactive)
                                        (kill-line 0)
                                        (indent-according-to-mode)))

;;---------------------------------------------------------------------------
;; General settings
;;---------------------------------------------------------------------------
(custom-set-variables '(coffee-tab-width 2))
(fset 'yes-or-no-p 'y-or-n-p)

(provide 'init)
;;; init.el ends here
