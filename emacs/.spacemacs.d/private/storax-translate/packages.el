;;; packages.el --- storax-translate layer packages file for Spacemacs.
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

(defconst storax-translate-packages
  '(google-translate))

(defun storax-translate/post-init-google-translate ()
  (use-package google-translate
    :commands (google-translate-smooth-translate)
    :config
    (setq google-translate-default-source-language "en"
          google-translate-default-target-language "de"
          google-translate-translation-directions-alist
          '(("en" . "de") ("de" . "en")))
    (spacemacs/set-leader-keys "at" 'google-translate-smooth-translate)))

;;; packages.el ends here
