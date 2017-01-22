;;; packages.el --- storax-visual-regexp-steroids layer packages file for Spacemacs.
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

(defconst storax-visual-regexp-steroids-packages
  '(visual-regexp-steroids))

(defun storax-visual-regexp-steroids/init-visual-regexp-steroids ()
  (use-package visual-regexp-steroids
    :defer t
    :init
    (spacemacs/declare-prefix "sr" "regexp")
    (spacemacs/set-leader-keys
      "srr" 'vr/isearch-backward
      "srs" 'vr/isearch-forward
      "srq" 'vr/query-replace
      "srm" 'vr/mc-mark)))


;;; packages.el ends here
