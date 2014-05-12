;;; init-buffer.el --- buffer management
;;
;;; Commentary:
;;
;;; Code:

(require 'ibuffer-vc)

;;---------------------------------------------------------------------------
;; iBuffer groups by Git project
;;---------------------------------------------------------------------------
(add-hook 'ibuffer-hook
          (lambda ()
            (ibuffer-vc-set-filter-groups-by-vc-root)))

;;---------------------------------------------------------------------------
;; Keybindings
;;---------------------------------------------------------------------------
(global-set-key (kbd "C-x C-b") 'ibuffer)
(global-set-key (kbd "C-x k") 'bury-buffer)

(provide 'init-buffer)
;;; init-buffer.el ends here
