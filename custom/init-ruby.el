;;; init-ruby.el --- Ruby modes
;;
;;; Commentary:
;;
;;; Code:

(require 'inf-ruby)

;;---------------------------------------------------------------------------
;; Use pry in inf-ruby
;;---------------------------------------------------------------------------
(autoload 'inf-ruby-minor-mode "inf-ruby" "Run an inferior Ruby process" t)
(add-hook 'ruby-mode-hook 'inf-ruby-minor-mode)
(setq inf-ruby-default-implementation "pry")
(setq inf-ruby-first-prompt-pattern "^\\[[0-9]+\\] pry\\((.*)\\)> *")
(setq inf-ruby-prompt-pattern "^\\[[0-9]+\\] pry\\((.*)\\)[>*\"'] *")

(provide 'init-ruby)
;;; init-ruby.el ends here
