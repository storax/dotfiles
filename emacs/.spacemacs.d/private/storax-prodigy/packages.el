;;; packages.el --- storax-prodigy layer packages file for Spacemacs.
;;
;; Copyright (c) 2012-2018 Sylvain Benner & Contributors
;;
;; Author: David Zuber <zuber.david@gmx.de>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

;;; Code:

(defconst storax-prodigy-packages
  '(prodigy)
  "The list of Lisp packages required by the storax-prodigy layer.")

(defun storax-prodigy/post-init-prodigy ()
  (use-package prodigy
    :config
    (progn
      (prodigy-define-tag
        :name 'flask
        :ready-message "Running on http://127\\.0\\.0\\.1:[0-9]+/")
      (prodigy-define-tag
        :name 'mongod
        :ready-message "waiting for connections on port ")
      (prodigy-define-tag
        :name 'parceljs
        :ready-message "Server running at")
      (prodigy-define-service
        :name "crib server"
        :command "crib"
        :args '("-c" "config.yaml" "server" "run")
        :cwd "~/projects/crib"
        :tags '(flask)
        :kill-process-buffer-on-stop t
        :init (lambda ()
                (pyvenv-workon "crib")))
      (prodigy-define-service
        :name "crib mongodb"
        :command "mongod"
        :args '("--config" "mongod.yaml")
        :cwd "~/projects/crib"
        :tags '(mongod)
        :kill-process-buffer-on-stop t)
      (prodigy-define-service
        :name "crib web-ui"
        :command "npm"
        :args '("run" "dev")
        :port 1234
        :cwd "~/projects/crib-web-ui"
        :tags '(parceljs)
        :kill-process-buffer-on-stop t))))


;;; packages.el ends here
