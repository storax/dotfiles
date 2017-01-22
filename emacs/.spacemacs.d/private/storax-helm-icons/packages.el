;;; packages.el --- storax-helm-icons layer packages file for Spacemacs.
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

(defconst storax-helm-icons-packages
  '(helm))

(defun storax-helm-icons/post-init-helm ()
  (use-package helm
    :defer t
    :init
    (progn
      (defvar storax/helm-file-icons
        (list
         `("py" . ,storax/icon-python)
         `("/" . ,storax/icon-folder)
         `("zip" . ,storax/icon-archive)
         `("gz" . ,storax/icon-archive)
         `("rar" . ,storax/icon-archive)
         `("xz" . ,storax/icon-archive)
         `("whl" . ,storax/icon-archive)
         `("json" . ,storax/icon-json)
         `("rst" . ,storax/icon-rst)
         `("txt" . ,storax/icon-txt)
         `("pdf" . ,storax/icon-pdf)
         `("ini" . ,storax/icon-config)
         `("cfg" . ,storax/icon-config)
         `("conf" . ,storax/icon-config)
         `("cpp" . ,storax/icon-cpp)
         `("hpp" . ,storax/icon-cpp)
         `("h" . ,storax/icon-file)
         `("mp4" . ,storax/icon-movie)
         `("mkv" . ,storax/icon-movie)
         `("avi" . ,storax/icon-movie)
         `("mov" . ,storax/icon-movie)
         `("flv" . ,storax/icon-movie)
         `("mpeg" . ,storax/icon-movie)
         `("mpg" . ,storax/icon-movie)
         `("coffee" . ,storax/icon-coffee)
         `("css" . ,storax/icon-css)
         `("qss" . ,storax/icon-qss)
         `("js" . ,storax/icon-js)
         `("jpg" . ,storax/icon-image)
         `("jpeg" . ,storax/icon-image)
         `("png" . ,storax/icon-image)
         `("gif" . ,storax/icon-image)
         `("svg" . ,storax/icon-image)
         `("tiff" . ,storax/icon-image)
         `("xml" . ,storax/icon-xml)
         `("html" . ,storax/icon-xml)
         `("htm" . ,storax/icon-xml)
         `("el" . ,storax/icon-el)
         `("rb" . ,storax/icon-ruby)
         `("md" . ,storax/icon-md)
         `("sh" . ,storax/icon-shell)
         `("db" . ,storax/icon-db)
         `("sql" . ,storax/icon-sql)
         `("yaml" . ,storax/icon-yaml)
         `("yml" . ,storax/icon-yaml))
        "Icons for helm find file."))
    :config
    (advice-add #'helm-ff-filter-candidate-one-by-one :around #'storax/add-icons-to-files)
    ;; Fix checking the text property, if the first display property is our icon
    (defun helm-confirm-and-exit-minibuffer ()
      "Maybe ask for confirmation when exiting helm.
It is similar to `minibuffer-complete-and-exit' adapted to helm.
If `minibuffer-completion-confirm' value is 'confirm,
send in minibuffer confirm message and exit on next hit.
If `minibuffer-completion-confirm' value is t,
don't exit and send message 'no match'."
      (interactive)
      (if (and (helm--updating-p)
               (null helm--reading-passwd-or-string))
          (progn (message "[Display not ready]")
                 (sit-for 0.5) (message nil))
        (let* ((empty-buffer-p (with-current-buffer helm-buffer
                                 (eq (point-min) (point-max))))
               (sel (helm-get-selection))
               (unknown (and (not empty-buffer-p)
                             (equal (get-text-property  ; use equal instead of string=
                                     0 'display
                                     (helm-get-selection nil 'withprop))
                                    "[?]"))))
          (cond ((and (or empty-buffer-p unknown)
                      (eq minibuffer-completion-confirm 'confirm))
                 (setq helm-minibuffer-confirm-state
                       'confirm)
                 (setq minibuffer-completion-confirm nil)
                 (minibuffer-message " [confirm]"))
                ((and (or empty-buffer-p
                          (unless (if minibuffer-completing-file-name
                                      (and minibuffer-completion-predicate
                                           (funcall minibuffer-completion-predicate sel))
                                    (and (stringp sel)
                                         ;; SEL may be a cons cell when helm-comp-read
                                         ;; is called directly with a collection composed
                                         ;; of (display . real) and real is a cons cell.
                                         (try-completion sel minibuffer-completion-table
                                                         minibuffer-completion-predicate)))
                            unknown))
                      (eq minibuffer-completion-confirm t))
                 (minibuffer-message " [No match]"))
                (t
                 (setq helm-minibuffer-confirm-state nil)
                 (helm-exit-minibuffer))))))))

;;; packages.el ends here
