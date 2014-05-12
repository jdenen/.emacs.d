;;; init-pkg.el --- Package management and setup
;;
;;; Commentary:
;;
;;; Code:

;;---------------------------------------------------------------------------
;; MELPA setup
;;---------------------------------------------------------------------------
(require 'package)
(add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/") t)
(package-initialize)

;;---------------------------------------------------------------------------
;; Install packages on demand
;;---------------------------------------------------------------------------
(defun inst-pkg (package)
  "Conditionally install the given PACKAGE."
  (if (package-installed-p package)
      t
    (progn
      (package-refresh-contents)
      (package-install package))))

;;---------------------------------------------------------------------------
;; Install external packages
;;---------------------------------------------------------------------------
(inst-pkg 'graphene)
(require 'graphene)
(inst-pkg 'fullscreen-mode)
(require 'fullscreen-mode)
(inst-pkg 'magit)
(require 'magit)
(inst-pkg 'projectile)
(require 'projectile)
(inst-pkg 'inf-ruby)
(require 'inf-ruby)
(inst-pkg 'coffee-mode)
(require 'coffee-mode)
(inst-pkg 'flx-ido)
(require 'flx-ido)
(inst-pkg 'ace-jump-mode)
(require 'ace-jump-mode)
(inst-pkg 'recentf)
(require 'recentf)
(inst-pkg 'multiple-cursors)
(require 'multiple-cursors)
(inst-pkg 'ibuffer-vc)
(require 'ibuffer)
(inst-pkg 'god-mode)
(require 'god-mode)

(provide 'init-pkg)
;;; init-pkg.el ends here
