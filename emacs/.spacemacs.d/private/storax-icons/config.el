;;; config.el --- storax-powerline layer config file for Spacemacs.
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

(defun storax/create-img (path &optional text type asc)
  "Return string with img at PATH.

TEXT is the alternative text and is determines the minimum width.  Defaults to '  '.
TYPE can be nil or an image type.  E.g. 'png, 'svg.
ASC the ascent in percent.  Defaults to 80."
  (let ((text (if text text "  "))
        (asc (if asc asc 80)))
    (propertize
     text 'display (create-image path type nil :ascent asc :mask 'heuristic))))

(defun storax/load-vendor-img (name &optional text type asc)
  "Load an image with NAME from the image directory."
  (storax/create-img (concat storax-icons/imgdir name) text type asc))

(defvar storax-icons/imgdir
  (file-name-as-directory (concat (file-name-directory load-file-name) "images")))

(defconst storax/icon-file (storax/load-vendor-img "file.svg" "  " 'svg))
(defconst storax/icon-archive (storax/load-vendor-img "archive.svg" "  " 'svg))
(defconst storax/icon-config (storax/load-vendor-img "config.svg" "  " 'svg))
(defconst storax/icon-cpp (storax/load-vendor-img "cpp.svg" "  " 'svg))
(defconst storax/icon-image (storax/load-vendor-img "image.svg" "  " 'svg))
(defconst storax/icon-xml (storax/load-vendor-img "xml.svg" "  " 'svg))
(defconst storax/icon-shell (storax/load-vendor-img "shell.svg" "  " 'svg))
(defconst storax/icon-yaml (storax/load-vendor-img "yaml.svg" "  " 'svg))
(defconst storax/icon-movie (storax/load-vendor-img "movie.svg" "  " 'svg))
(defconst storax/icon-python (storax/load-vendor-img "py.svg" "  " 'svg))
(defconst storax/icon-folder (storax/load-vendor-img "folder.svg" "  " 'svg))
(defconst storax/icon-json (storax/load-vendor-img "json.svg" "  " 'svg))
(defconst storax/icon-rst (storax/load-vendor-img "rst.svg" "  " 'svg))
(defconst storax/icon-txt (storax/load-vendor-img "txt.svg" "  " 'svg))
(defconst storax/icon-pdf (storax/load-vendor-img "pdf.svg" "  " 'svg))
(defconst storax/icon-coffee (storax/load-vendor-img "coffee.svg" "  " 'svg))
(defconst storax/icon-qss (storax/load-vendor-img "qss.svg" "  " 'svg))
(defconst storax/icon-css (storax/load-vendor-img "css.svg" "  " 'svg))
(defconst storax/icon-js (storax/load-vendor-img "js.svg" "  " 'svg))
(defconst storax/icon-el (storax/load-vendor-img "el.svg" "  " 'svg))
(defconst storax/icon-cl (storax/load-vendor-img "cl.svg" "  " 'svg))
(defconst storax/icon-ruby (storax/load-vendor-img "ruby.svg" "  " 'svg))
(defconst storax/icon-md (storax/load-vendor-img "md.svg" "  " 'svg))
(defconst storax/icon-java (storax/load-vendor-img "java.svg" "  " 'svg))
(defconst storax/icon-help (storax/load-vendor-img "help.svg" "  " 'svg))
(defconst storax/icon-mail (storax/load-vendor-img "mail.svg" "  " 'svg))
(defconst storax/icon-git (storax/load-vendor-img "git.svg" "  " 'svg))
(defconst storax/icon-git-stash (storax/load-vendor-img "git-stash.svg" "  " 'svg))
(defconst storax/icon-git-merge (storax/load-vendor-img "git-merge.svg" "  " 'svg))
(defconst storax/icon-git-commit (storax/load-vendor-img "git-commit.svg" "   " 'svg))
(defconst storax/icon-diff (storax/load-vendor-img "git-diff.svg" "  " 'svg))
(defconst storax/icon-cherry (storax/load-vendor-img "cherry.svg" "  " 'svg))
(defconst storax/icon-wheel (storax/load-vendor-img "wheel.svg" "  " 'svg))
(defconst storax/icon-log (storax/load-vendor-img "log.svg" "  " 'svg))
(defconst storax/icon-db (storax/load-vendor-img "db.svg" "  " 'svg))
(defconst storax/icon-sql (storax/load-vendor-img "sql.svg" "  " 'svg))
(defconst storax/icon-spacemacs (storax/load-vendor-img "spacemacs.svg" "  " 'svg))

;;; config.el ends here
