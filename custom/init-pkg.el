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
(inst-pkg 'fullscreen-mode)
(inst-pkg 'magit)
(inst-pkg 'projectile)
(inst-pkg 'inf-ruby)
(inst-pkg 'coffee-mode)
(inst-pkg 'flx-ido)
(inst-pkg 'ace-jump-mode)
(inst-pkg 'recentf)
(inst-pkg 'multiple-cursors)
(inst-pkg 'ibuffer-vc)

(provide 'init-pkg)
;;; init-pkg.el ends here
