;;; funcs.el --- storax-helm layer functions file for Spacemacs.
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

(defun storax/helm-skip-dots (old-func &rest args)
  "Skip . and .. initially in helm-find-files.  First call OLD-FUNC with ARGS."
  (apply old-func args)
  (let ((sel (helm-get-selection)))
    (if (and (stringp sel) (string-match "/\\.$" sel))
        (helm-next-line 2)))
  (let ((sel (helm-get-selection))) ; if we reached .. move back
    (if (and (stringp sel) (string-match "/\\.\\.$" sel))
        (helm-previous-line 1))))

(defun storax/helm-ag-in-dir (dir)
  "Search with ag in the given DIR."
  (helm-do-ag nil (list dir)))

(defun storax/create-helm-ag-bindings (dir-alist)
  "Create keybindings for searching dirs with helm-ag.

The given DIR-ALIST should map keybindings to directories.
All keybindings are prepended with `storax-helm-ag-dirs-prefix'"
  (dolist (keydir dir-alist)
    (let* ((key (car keydir))
           (dir (cdr keydir))
           (keychord (concat storax-helm-ag-dirs-prefix key))
           (symname (concat "storax-helm-ag-" (file-name-base (directory-file-name dir))))
           (sym (make-symbol symname))
           (lf `(lambda ()
                  ,(concat "Search in " dir " with `helm-ag'.")
                  (interactive)
                  (storax/helm-ag-in-dir ,dir))))
      (fset sym lf)
      (spacemacs/set-leader-keys keychord sym))))

;;; funcs.el ends here
