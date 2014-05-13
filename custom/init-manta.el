;;; init-manta.el --- manta specific commands
;;
;;; Commentary:
;;
;;; Code:

;;---------------------------------------------------------------------------
;; Set environment arguments and execute current buffer
;;---------------------------------------------------------------------------
(defun run-prompt-spec (environment_args)
  "Run current spec with ENVIRONMENT_ARGS."
  (interactive "sBROWSER_TYPE: ")
  (let ((command (format "BROWSER_TYPE=%s bundle exec rspec %s" environment_args (ruby-test-find-file))))
        (setq default-directory (rspec-project-root))
        (compilation-start command)))

(defun run-prompt-spec-bind ()
  "Hook to bind run-prompt-spec to 'ruby-mode'."
  (local-set-key (kbd "C-c r i") 'run-prompt-spec))

(add-hook 'ruby-mode-hook 'run-prompt-spec-bind)

;;---------------------------------------------------------------------------
;; Execute current buffer with Chrome
;;---------------------------------------------------------------------------
(defun run-desktop-spec ()
  "Use Chrome to test current spec."
  (interactive)
  (run-prompt-spec "chrome"))

(defun run-desktop-spec-bind ()
  "Hook to bind run-desktop-spec to 'ruby-mode'."
  (local-set-key (kbd "C-c r d") 'run-desktop-spec))

(add-hook 'ruby-mode-hook 'run-desktop-spec-bind)

;;---------------------------------------------------------------------------
;; Execute current buffer with iPhone
;;---------------------------------------------------------------------------
(defun run-mobile-spec ()
  "Use iPhone to test current spec."
  (interactive)
  (run-prompt-spec "iphone"))

(defun run-mobile-spec-bind()
  "Hook to bind run-mobile-spec to 'ruby-mode'."
  (local-set-key (kbd "C-c r m") 'run-mobile-spec))

(add-hook 'ruby-mode-hook 'run-mobile-spec-bind)

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
(global-set-key (kbd "C-c d") 'dtool-diff)

(provide 'init-manta)
;;; init-manta.el ends here
