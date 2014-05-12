;;; init-fuzzy.el --- fuzzy search modes
;;
;;; Commentary:
;;
;;; Code:

(require 'flx-ido)

;;---------------------------------------------------------------------------
;; General settings
;;---------------------------------------------------------------------------
(ido-mode 1)
(flx-ido-mode 1)
(ido-everywhere 1)
(setq ido-use-faces nil)

;;---------------------------------------------------------------------------
;; Keybindings
;;---------------------------------------------------------------------------
(global-set-key (kbd "C-x m") 'smex)


(provide 'init-fuzzy)
;;; init-fuzzy.el ends here
