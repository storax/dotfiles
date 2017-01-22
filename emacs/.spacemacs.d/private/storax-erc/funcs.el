;;; funcs.el --- storax-powerline layer functions file for Spacemacs.
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

(defun storax-erc/erc-gitter ()
  "Load the secrets file and use the gitter token from there to login to gitter."
  (interactive)
  (let ((erc-server-connect-function 'erc-open-tls-stream))
    (storax/load-secrets)
    (erc :server "irc.gitter.im" :port "6667" :nick "storax" :password gitter-token)))

(defun storax-erc/erc-freenode ()
  "Login to irc.freenode.net."
  (interactive)
  (erc :server "irc.freenode.net" :port "6667" :nick "notZubes"))

;;; funcs.el ends here
