;;; init-god.el --- god-mode
;;
;;; Commentary:
;;
;;; Code:

(require 'god-mode)

;;---------------------------------------------------------------------------
;; Toggle cursor based on god-mode
;;---------------------------------------------------------------------------
(defun cursor-mode ()
  "Set cursor on god-mode: bar if enabled, box if disabled."
  (setq cursor-type (if (or god-local-mode buffer-read-only)
                        'bar
                      'box)))

(add-hook 'god-mode-enabled-hook 'cursor-mode)
(add-hook 'god-mode-disabled-hook 'cursor-mode)

;;---------------------------------------------------------------------------
;; Remapped window manipulation in god-mode
;;---------------------------------------------------------------------------
(define-key god-local-mode-map (kbd "C-x C-1") 'delete-other-windows)
(define-key god-local-mode-map (kbd "C-x C-2") 'split-window-below)
(define-key god-local-mode-map (kbd "C-x C-3") 'split-window-right)
(define-key god-local-mode-map (kbd "C-x C-0") 'delete-window)

;;---------------------------------------------------------------------------
;; Keybindings
;;---------------------------------------------------------------------------
(global-set-key (kbd "C-SPC") 'god-mode-all)

(provide 'init-god)
;;; init-god.el ends here


