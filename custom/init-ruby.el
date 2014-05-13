;;; init-ruby.el --- Ruby modes
;;
;;; Commentary:
;;
;;; Code:

(require 'inf-ruby)
(require 'ruby-test-mode)

;;---------------------------------------------------------------------------
;; Add ruby-mode hooks
;;---------------------------------------------------------------------------
(add-hook 'ruby-mode-hook 'ruby-test-mode)
(add-hook 'ruby-mode-hook 'inf-ruby-minor-mode)

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

(defun rspec-with-browser-type (b)
  "Run current spec on browser B."
  (interactive "sBROWSER_TYPE: ")
  (let ((command (format "BROWSER_TYPE=%s bundle exec rspec %s" b (ruby-test-find-file))))
        (setq default-directory (rspec-project-root))
        (compilation-start command)))

(provide 'init-ruby)
;;; init-ruby.el ends here
