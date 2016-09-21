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

(defvar groovy-gradle-daemon nil
  "Execute gradle tasks with --daemon flag if non-nil.")

(defun groovy//gradle (task)
  "Execute TASK with gradle.

Execute with --daemon flag if `groovy-gradle-daemon' is non-nil."
  (let ((gradle-task (if groovy-gradle-daemon
                         (concat "gradle-" task "--daemon")
                       (concat "gradle-" task))))
    (call-interactively (intern gradle-task))))

(defun groovy/gradle-build ()
  "Execute gradle build task."
  (interactive)
  (groovy//gradle "build"))

(defun groovy/gradle-task ()
  "Execute a gradle task."
  (interactive)
  (groovy//gradle "task"))

(defun groovy/gradle-test ()
  "Execute gradle test task."
  (interactive)
  (groovy//gradle "test"))

(defun groovy/gradle-single-test ()
  "Execute gradle test task with a -Dtest.single= argument."
  (interactive)
  (groovy//gradle "single-test"))
