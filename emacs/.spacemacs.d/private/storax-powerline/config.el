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

(defvar storax-powerline/imgdir
  (file-name-as-directory (concat (file-name-directory load-file-name) "images")))

(defvar storax/spotify-connected nil)

(defvar storax/github-mark-data
  (storax/string-from-file (concat storax-powerline/imgdir "mark-github.svg")))
(defvar storax/bitbucket-mark-data
  (storax/string-from-file (concat storax-powerline/imgdir "mark-bitbucket.svg")))
(defvar storax/wl-decrypt-success-img
  (storax/string-from-file (concat storax-powerline/imgdir "wl-decrypt-success.svg")))
(defvar storax/wl-decrypt-fail-img
  (storax/string-from-file (concat storax-powerline/imgdir "wl-decrypt-fail.svg")))
(defvar storax/wl-verify-success-img
  (storax/string-from-file (concat storax-powerline/imgdir "wl-verify-success.svg")))
(defvar storax/wl-verify-fail-img
  (storax/string-from-file (concat storax-powerline/imgdir "wl-verify-fail.svg")))
(defvar storax/spotify-data
  (storax/string-from-file (concat storax-powerline/imgdir "spotify.svg")))
(defvar storax/mpc-right
  (storax/create-img (concat storax-powerline/imgdir "mpc-right.png") "   " 'png))
(defvar storax/mpc-left
  (storax/create-img (concat storax-powerline/imgdir "mpc-left-with-text.png") "        " 'png))

;; Save the current remote url in each buffer
(defvar storax/remoteurl "")

;;; config.el ends here
