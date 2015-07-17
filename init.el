(require 'package)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/") t)
(package-initialize)

(require 'org)
(org-babel-load-file
 (expand-file-name "config.org"
		   user-emacs-directory))
(put 'narrow-to-region 'disabled nil)
(put 'narrow-to-page 'disabled nil)
