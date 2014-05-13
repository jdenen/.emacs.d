;;; init-ruby.el --- Ruby modes
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
;; Bind yari to 'ruby-mode'
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
;; Set environment arguments and execute current buffer
;;---------------------------------------------------------------------------
(defun run-prompt-spec (environment_args)
  "Run current spec with ENVIRONMENT_ARGS."
  (interactive "sBROWSER_TYPE: ")
  (let ((command (format "BROWSER_TYPE=%s bundle exec rspec %s" environment_args (ruby-test-find-file))))
        (setq default-directory (rspec-project-root))
        (compilation-start command)))

;;---------------------------------------------------------------------------
;; Execute current buffer with Chrome
;;---------------------------------------------------------------------------
(defun run-desktop-spec ()
  "Use Chrome to test current spec."
  (interactive)
  (run-prompt-spec "chrome"))

;;---------------------------------------------------------------------------
;; Execute current buffer with iPhone
;;---------------------------------------------------------------------------
(defun run-mobile-spec ()
  "Use iPhone to test current spec."
  (interactive)
  (run-prompt-spec "iphone"))

;;---------------------------------------------------------------------------
;; Diff environments
;;---------------------------------------------------------------------------
(defun dtool-diff (environment)
  "Display differences between ENVIRONMENT and the next stage of the pipeline. Attach a repository to generate Git diff stats."
  (interactive "sDIFF: ")
  (let ((command (format "dtool diff %s" environment)))
    (compilation-start command)))

;;---------------------------------------------------------------------------
;; Keybindings
;;---------------------------------------------------------------------------
(global-set-key (kbd "C-c C-p") 'inf-ruby)
(global-set-key (kbd "C-c r d") 'run-desktop-spec)
(global-set-key (kbd "C-c r m") 'run-mobile-spec)
(global-set-key (kbd "C-c r i") 'run-prompt-spec)
(global-set-key (kbd "C-c d") 'dtool-diff)

(provide 'init-ruby)
;;; init-ruby.el ends here
