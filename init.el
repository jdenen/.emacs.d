(require 'package)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/") t)
(package-initialize)

(require 'org)
(org-babel-load-file
 (expand-file-name "johnson.org"
		   user-emacs-directory))
