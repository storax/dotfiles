;;; config.el --- storax-org layer config file for Spacemacs.
;;
;; Copyright (c) 2012-2016 Sylvain Benner & Contributors
;;
;; Author: David Zuber <zuber.david@gmx.de>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

;;; Commentary:

;;; Code:

(defvar storax/org-organization-task-id "6a726688-4bf5-4d7d-b00c-83d766c221a3"
  "Id of the default clocking task that has to live in some file.
Use `org-id-get-create' to create a new id for some task.")
(defvar storax/org-hide-scheduled-and-waiting-next-tasks t)
(defvar storax/org-source-link-file-hist nil
  "History for files to insert links in.")

(defvar storax-org-layer-dir (file-name-directory load-file-name))

(defvar storax-org-caputre-dir
  (mapconcat
   'file-name-as-directory
   (list storax-org-layer-dir "capture")
   nil))

(defvar storax-org-template-dir
  (mapconcat
   'file-name-as-directory
   (list storax-org-layer-dir "templates")
   nil))

(defvar storax-org-rtd-template
  (concat storax-org-template-dir "rtdsinglefile.html"))

(defvar storax-org-rtd-theme-path
  (mapconcat
   'file-name-as-directory
   (list storax-org-template-dir "rtd")
   nil))

(defvar storax-org-warning-sound
  (concat storax-org-layer-dir "warning.wav"))

(defvar storax-org-lob-file
  (concat storax-org-layer-dir "lob.org"))
;;; config.el ends here
