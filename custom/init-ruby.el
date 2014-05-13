;;; init-ruby.el --- ruby modes
;;
;;; Commentary:
;;
;;; Code:

(require 'inf-ruby)
(require 'ruby-test-mode)
(require 'yari)

;;---------------------------------------------------------------------------
;; Add ruby-mode hooks
;;---------------------------------------------------------------------------
(add-hook 'ruby-mode-hook 'ruby-test-mode)
(add-hook 'ruby-mode-hook 'inf-ruby-minor-mode)

;;---------------------------------------------------------------------------
;; Bind yari to ruby-mode
;;---------------------------------------------------------------------------
(defun ri-bind ()
  "Hook to bind yari to 'ruby-mode'."
  (local-set-key (kbd "C-c y") 'yari))

(add-hook 'ruby-mode-hook 'ri-bind)

;;---------------------------------------------------------------------------
;; Use pry in inf-ruby
;;---------------------------------------------------------------------------
(autoload 'inf-ruby-minor-mode "inf-ruby" "Run an inferior Ruby process" t)
(setq inf-ruby-default-implementation "pry")
(setq inf-ruby-first-prompt-pattern "^\\[[0-9]+\\] pry\\((.*)\\)> *")
(setq inf-ruby-prompt-pattern "^\\[[0-9]+\\] pry\\((.*)\\)[>*\"'] *")

;;---------------------------------------------------------------------------
;; Keybindings
;;---------------------------------------------------------------------------
(global-set-key (kbd "C-c C-p") 'inf-ruby)

(provide 'init-ruby)
;;; init-ruby.el ends here
