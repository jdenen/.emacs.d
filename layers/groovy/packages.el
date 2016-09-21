;;; packages.el --- Groovy layer packages file for Spacemacs.
;;
;; Copyright (c) 2012-2016 Sylvain Benner & Contributors
;;
;; Author: Johnson Denen <johnson.denen@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

;;; Commentary:

;;; Code:

(defconst groovy-packages
  '(
    gradle-mode
    groovy-mode
    ))

(defun groovy/init-gradle-mode ()
  (use-package gradle-mode
    :defer t))

(defun groovy/init-groovy-mode ()
  (use-package groovy-mode
    :defer t
    :config
    (progn
      (add-hook 'groovy-mode-hook 'gradle-mode)
      (spacemacs/declare-prefix-for-mode 'groovy-mode "ms" "send")
      (spacemacs/declare-prefix-for-mode 'groovy-mode "mc" "build")
      (spacemacs/declare-prefix-for-mode 'groovy-mode "mt" "test")
      (spacemacs/set-leader-keys-for-major-mode 'groovy-mode
        "sb" 'groovy-load-file
        "si" 'run-groovy
        "sf" 'groovy-send-definition
        "sF" 'groovy-send-definition-and-go
        "sr" 'groovy-send-region
        "sR" 'groovy-send-region-and-go
        "cc" 'gradle-build
        "ct" 'gradle-execute
        "ta" 'gradle-test
        "ts" 'gradle-test
        "tt" 'gradle-single-test))))

;;; packages.el ends here
