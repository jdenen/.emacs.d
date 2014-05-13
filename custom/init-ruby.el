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
;; Set BROWSER_TYPE and execute current buffer
;;---------------------------------------------------------------------------
(defun rspec-with-browser-type (browser)
  "Run current spec on BROWSER."
  (interactive "sBROWSER_TYPE: ")
  (let ((command (format "BROWSER_TYPE=%s bundle exec rspec %s" browser (ruby-test-find-file))))
        (setq default-directory (rspec-project-root))
        (compilation-start command)))

;;---------------------------------------------------------------------------
;; Execute current buffer with Chrome
;;---------------------------------------------------------------------------
(defun run-desktop-spec ()
  "Use Chrome to test current spec."
  (interactive)
  (rspec-with-browser-type "chrome"))

;;---------------------------------------------------------------------------
;; Execute current buffer with iPhone
;;---------------------------------------------------------------------------
(defun run-mobile-spec ()
  "Use iPhone to test current spec."
  (interactive)
  (rspec-with-browser-type "iphone"))

;;---------------------------------------------------------------------------
;; Keybindings
;;---------------------------------------------------------------------------
(global-set-key (kbd "C-c C-p") 'inf-ruby)
(global-set-key (kbd "C-c r d") 'run-desktop-spec)
(global-set-key (kbd "C-c r m") 'run-mobile-spec)
(global-set-key (kbd "C-c r i") 'rspec-with-browser-type)

(provide 'init-ruby)
;;; init-ruby.el ends here
