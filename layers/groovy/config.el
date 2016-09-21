;;; config.el --- Groovy layer configuration file for Spacemacs.
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

(defun groovy/gradle-clean-build ()
  "Execute gradle clean build command."
  (interactive)
  (gradle-execute "clean build"))
