;;; funcs.el --- storax-helm-icons layer functions file for Spacemacs.
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

(defun storax/icon-for-file (file)
  "Return a string with an icon display property for FILE."
  (let ((icon (if (file-directory-p file)
		  (cdr (assoc "/" storax/helm-file-icons))
		(cdr (assoc-string (file-name-extension file) storax/helm-file-icons)))))
    (if icon
	icon
      storax/icon-file)))

(defun storax/add-icons-to-files (oldfunc file)
  "Add icons to the candidates of OLDFUNC called with REQUIRE-MATCH."
  (let ((oldval (funcall oldfunc file)))
    (if oldval
	(cons (concat (storax/icon-for-file (cdr oldval)) " " (car oldval)) (cdr oldval)))))

;;; funcs.el ends here
