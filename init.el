;;; init.el -- custom configuration
;;;
;;; Commentary:
;;; Try to keep things simple and `helm' centric.
;;;
;;; Code:

;;;
;;; start up and initialization
;;;
(setq user-full-name "Johnson Denen"
      user-mail-address "jdenen@manta.com")

(require 'package)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/") t)
(package-initialize)

(defun johnson/package-install (package)
  "Install PACKAGE if it has not already been installed."
  (unless (package-installed-p package)
    (package-install package)))

(johnson/package-install 'use-package)
(require 'use-package)

(setq inhibit-startup-screen t)
(setq initial-major-mode 'ruby-mode)
(setq initial-buffer-choice "~/Code/notes.org")
(fset 'yes-or-no-p 'y-or-n-p)
(setq backup-directory-alist '(("." . "~/tmp")))
(tool-bar-mode 0)
(menu-bar-mode 0)
(scroll-bar-mode 0)
(add-to-list 'default-frame-alist '(font .  "Droid Sans Mono-10"))
(set-face-attribute 'default t :font  "Droid Sans Mono-10")
(johnson/package-install 'ample-theme)
(load-theme 'ample t)

;;;
;;; emacs help
;;;
(johnson/package-install 'guide-key)
(use-package guide-key
  :init
  (progn
    (guide-key-mode 1)
    (setq guide-key/guide-key-sequence '("C-x" "C-c"))
    (setq guide-key/idle-delay 2.0)
    (setq guide-key/recursive-key-sequence-flag t)))

;;;
;;; project source control and management
;;;
(johnson/package-install 'magit)
(use-package magit
  :bind ("C-x g" . magit-status))

(johnson/package-install 'projectile)
(use-package projectile
  :init (projectile-global-mode t))

;;;
;;; helm search and navigation
;;;
(johnson/package-install 'helm)
(use-package helm
  :init
  (progn
    (helm-mode 1)
    (require 'helm-config)
    (global-unset-key (kbd "C-x c"))
    (define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action)
    (define-key helm-map (kbd "C-z")  'helm-select-action)
    (when (executable-find "curl")
      (setq helm-google-suggest-use-curl-p      t))
    (setq helm-quick-update                     t
	  helm-split-window-in-side-p           t
	  helm-buffers-fuzzy-matching           t
	  helm-move-to-line-cycle-in-source     t
	  helm-ff-search-library-in-sexp        t
	  helm-scroll-amount                    8
	  helm-ff-file-name-history-use-recentf t))
  :bind
  ("C-x m" . helm-M-x)
  ("C-c h" . helm-mini)
  ("M-y"   . helm-show-kill-ring)
  ("C-x h" . helm-command-prefix))

(johnson/package-install 'helm-swoop)
(use-package helm-swoop
  :bind ("C-s" . helm-swoop))

(johnson/package-install 'helm-projectile)
(use-package helm-projectile
  :init (helm-projectile-on))

;;;
;;; mode line styling
;;;
(johnson/package-install 'powerline)
(use-package powerline
  :init
  (progn
    (display-time-mode 1)
    (powerline-default-theme)))

(use-package diminish
  :init
  (progn
    (eval-after-load "company"     '(diminish 'company-mode))
    (eval-after-load "helm"        '(diminish 'helm-mode))
    (eval-after-load "guide-key"   '(diminish 'guide-key-mode))
    (eval-after-load "magit"       '(diminish 'magit-auto-revert-mode))
    (eval-after-load "smartparens" '(diminish 'smartparens-mode))))

;;;
;;; buffer management
;;;
(johnson/package-install 'buffer-move)
(use-package buffer-move
  :init (winner-mode 1)
  :bind
  ("<C-S-up>"    . buf-move-up)
  ("<C-S-left>"  . buf-move-left)
  ("<C-S-right>" . buf-move-right)
  ("<C-S-down>"  . buf-move-down)
  ("C-x k"       . bury-buffer)
  ("C-x C-k"     . kill-this-buffer)
  ("C-c n"       . new-frame)
  ("C-c k"       . delete-frame)
  ("C-+"         . text-scale-increase)
  ("C--"         . text-scale-decrease)
  ("C-<"         . shrink-window-horizontally)
  ("C->"         . enlarge-window-horizontally)
  ("C-,"         . shrink-window)
  ("C-."         . enlarge-window))

;;;
;;; registers
;;;
(mapcar
 (lambda (r)
   (set-register (car r) (cons 'file (cdr r))))
 '((?i . "~/.emacs.d/init.el")
   (?n . "~/Code/notes.org")
   (?h . "~/Code/mantacode/smoke/spec/spec_helper.rb")
   (?b . "~/Code/mantacode/responder/lib/responder/base.rb")))

;;;
;;; coding productivity
;;;
(johnson/package-install 'smartparens)
(use-package smartparens
  :init
  (progn
    (smartparens-global-mode 1)
    (show-smartparens-global-mode 1)
    (require 'smartparens-config)))

(johnson/package-install 'key-chord)
(use-package key-chord
  :init
  (progn
    (key-chord-mode 1)
    (key-chord-define-global "jj" 'ace-jump-char-mode)  
    (key-chord-define-global "jr" 'jump-to-register)))

(johnson/package-install 'company)
(use-package company
  :init (global-company-mode 1)
  :bind ("C-c C-c" . company-complete))

(johnson/package-install 'expand-region)
(use-package expand-region
  :bind
  ("C-=" . er/expand-region)
  ("C-c C-q" . indent-region))

(johnson/package-install 'ace-jump-mode)
(use-package ace-jump-mode
  :bind ("C-x j" . ace-jump-char-mode))

;;;
;;; ruby
;;;
(johnson/package-install 'rbenv)
(use-package rbenv
  :init (global-rbenv-mode 1))

(johnson/package-install 'yari)
(use-package yari
  :bind ("C-c y" . yari-helm))

(johnson/package-install 'rspec-mode)
(use-package rspec-mode
  :init
  (progn
    (setq rspec-use-rake-when-possible nil)
    (setq rspec-command-options "--format progress"))
  :bind
  ("C-c , i" . johnson/rspec-browser))

(johnson/package-install 'inf-ruby)
(use-package inf-ruby
  :init (add-hook 'ruby-mode-hook 'inf-ruby-minor-mode))

(defun johnson/rspec-browser (env)
  "Execute the current spec buffer with ENV variables."
  (interactive "sBROWSER_TYPE: ")
  (progn
    (setq rspec-use-bundler-when-possible nil)
    (setq rspec-spec-command (concat (format "BROWSER_TYPE=%s" env) " rspec"))
    (rspec-run-single-file
     (rspec-spec-file-for (buffer-file-name))
     (rspec-core-options))))

(defun johnson/pry-binding ()
  "Insert binding.pry."
  (interactive)
  (insert-before-markers "require 'pry'; binding.pry"))

(defun johnson/pry-binding-hook ()
  "Hook to set `johnson/pry-binding' kbd."
  (local-set-key (kbd "C-c C-p") 'johnson/pry-binding))
(add-hook 'ruby-mode-hook 'johnson/pry-binding-hook)

;;;
;;; terminals
;;;
(defun johnson/ansi-term-pry ()
  "Jump to pry session."
  (interactive)
  (if (get-buffer "*Pry*")
      (switch-to-buffer-other-window "*Pry*")
    (ansi-term "pry" "Pry")))

(defun johnson/ansi-term-bash ()
  "Jump to bash session."
  (interactive)
  (if (get-buffer "*Bash*")
      (switch-to-buffer-other-window "*Bash*")
    (ansi-term "/bin/bash" "Bash")))

(bind-key "C-c t b" 'johnson/ansi-term-bash)
(bind-key "C-c t p" 'johnson/ansi-term-pry)

;;;
;;; jekyll blogging
;;;
(defvar jekyll-directory "~/Code/jdenen/blog/"
  "Path to Jekyll blog.")
(defvar jekyll-drafts-dir "_drafts/"
  "Relative path to drafts directory.")
(defvar jekyll-posts-dir "_posts/"
  "Relative path to posts directory.")
(defvar jekyll-post-ext ".md"
  "File extension of Jekyll posts.")
(defvar jekyll-post-template 
  "---\nlayout: post\ntitle: %s\n---\n\n"
  "Default template for Jekyll posts.  %s will be replace by the post title.")
(defvar jekyll-highlight-ruby
  "{% highlight ruby %}"
  "Begin Ruby block with Jekyll markdown syntax.")
(defvar jekyll-highlight-end
  "{% endhighlight %}"
  "End Ruby block with Jekyll markdown syntax.")

(defun jekyll-make-slug (s)
  "Turn a string into a slug."
  (replace-regexp-in-string 
   " " "-" (downcase
            (replace-regexp-in-string
             "[^A-Za-z0-9 ]" "" s))))

(defun jekyll-yaml-escape (s)
  "Escape a string for YAML."
  (if (or (string-match ":" s)
          (string-match "\"" s))
      (concat "\"" (replace-regexp-in-string "\"" "\\\\\"" s) "\"") s))

(defun johnson/jekyll-ruby-snippet ()
  "Inject Ruby snippet into a blog post."
  (interactive)
  (insert (concat jekyll-highlight-ruby "\n\n" jekyll-highlight-end))
  (previous-line))

(defun johnson/blog-draft (title)
  "Create a new blog post with basic liquid config."
  (interactive "sPost Title: ")
  (let ((post-file (concat jekyll-directory jekyll-drafts-dir
			   (jekyll-make-slug title)
			   jekyll-post-ext)))
    (with-current-buffer (find-file post-file)
      (unless (re-search-forward "---" nil t)
	  (insert (format jekyll-post-template (jekyll-yaml-escape title)))))))

(defun johnson/publish-current-draft ()
  "Publish current buffer to blog."
  (interactive)
  (let ((post-file (concat jekyll-directory jekyll-posts-dir
			   (format-time-string "%Y-%m-%d-")
			   (file-name-nondirectory buffer-file-name))))
    (rename-file (buffer-file-name) post-file)
    (kill-buffer (buffer-name))
    (find-file post-file)))

(defun johnson/serve-jekyll ()
  "Serve blog with drafts."
  (interactive)
  (let ((serve-command (concat "cd " jekyll-directory "; "
			       "jekyll serve --drafts")))
    (progn
      (async-shell-command serve-command)
      (set-buffer "*Async Shell Command*")
      (rename-buffer "Jekyll"))))

(defun johnson/restart-jekyll ()
  "Restart Jekyll."
  (interactive)
  (progn
    (johnson/kill-jekyll)
    (johnson/serve-jekyll)))

(defun johnson/kill-jekyll ()
  "Stop the Jekyll server."
  (interactive)
  (if (get-buffer "Jekyll")
      (kill-buffer "Jekyll")))

(bind-key "C-c j d" 'johnson/blog-draft)
(bind-key "C-c j p" 'johnson/publish-current-draft)
(bind-key "C-c j r" 'johnson/jekyll-ruby-snippet)
(bind-key "C-c j s s" 'johnson/serve-jekyll)
(bind-key "C-c j s k" 'johnson/kill-jekyll)
(bind-key "C-c j s r" 'johnson/restart-jekyll)
