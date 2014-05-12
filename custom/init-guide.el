;;; init-guide --- guide-key
;;
;;; Commentary:
;;
;;; Code:

(require 'guide-key)
(guide-key-mode 1)

;;---------------------------------------------------------------------------
;; Show guide for these key sequences
;;---------------------------------------------------------------------------
(setq guide-key/guide-key-sequence '("C-x" "C-c"))

;;---------------------------------------------------------------------------
;; Settings
;;---------------------------------------------------------------------------
(setq guide-key/idle-delay 2.0)
(setq guide-key/recursive-key-sequence-flag t)

(provide 'init-guide)
;;; init-guide.el ends here
