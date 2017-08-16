;;; funcs.el --- storax-yasnippet layer functions file for Spacemacs.
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

(defvar storax-yas-curcolumn 4
  "Can be used to store the current column between
different code expansions in one snippet.")
(make-variable-buffer-local 'storax-yas-curcolumn)

(defgroup storax-yas nil
  "Yasnippet helpers."
  :group 'yasnippet)

(defcustom storax-yas-param-section-heading "Arguments:"
  "Section header for parameters."
  :options '("Arguments:" "Args:" "Parameters:")
  :type 'string
  :group 'storax-yas)

(defun storax/mapnil (function sequence)
  "Apply FUNCTION to each element of SEQUENCE and return a list
with each result.
Nil values are removed from the list."
  (remove-if-not
   'identity
   (mapcar function
           sequence)))

(defun storax/yas-makeindent (column &optional newline level)
  "Return a string with newline and COLUMN amount of whitespace.
If newline is non-nil, prepend a newline.
If LEVEL is non-nil add `python-indent' amount of whitespace per LEVEL."
  (concat (when newline "\n") (make-string (+ column (* python-indent-offset (if level level 0))) 32)))

(defun storax/yas-parse-pos-arg (param)
  "Return PARAM if it is a positional argument."
  (unless (string-match-p "[\\*=]" param)
    param))

(defun storax/yas-parse-pos-args (paramlist)
  "Return a list of positional argument names in PARAMLIST."
  (storax/mapnil 'storax/yas-parse-pos-arg paramlist))

(defun storax/yas-parse-kwarg (param)
  "Return argument . value if PARAM is a keyword argument."
  (when (string-match "\\([a-zA-Z_][a-zA-Z0-9_]*\\)[[:blank:]]*=[[:blank:]]*\\(.*\\)" param)
    (cons (match-string 1 param) (match-string 2 param))))

(defun storax/yas-parse-kwargs (paramlist)
  "Return an alist of keyword arguments from PARAMLIST."
  (storax/mapnil 'storax/yas-parse-kwarg paramlist))

(defun storax/yas-parse-poslist-param (param)
  "Return the name of the positional argument list PARAM or nil."
  (when (string-match "^\\*\\([a-zA-Z_][a-zA-Z0-9_]*\\)" param)
    (match-string 1 param)))

(defun storax/yas-parse-poslist (paramlist)
  "Return the name of the positional argument list in PARAMLIST or nil."
  (car (storax/mapnil 'storax/yas-parse-poslist-param paramlist)))

(defun storax/yas-parse-kwargdict-param (param)
  "Return the name of the keyword dictionary PARAM or nil."
  (when (string-match "\\*\\*\\([a-zA-Z_][a-zA-Z0-9_]*\\)" param)
    (match-string 1 param)))

(defun storax/yas-parse-kwargdict (paramlist)
  "Return the name of the keyword dictionary in PARAMLIST or nil."
  (car (storax/mapnil 'storax/yas-parse-kwargdict-param paramlist)))

(defun storax/yas-parse-args (paramstr)
  "Return a list with parsed PARAMSTR.

First element is a list of positional arguments.
Second one is an alist of keyword argument with default value.
Third one is the name of the argument list.
Fourth one the name of keyword dictionary."
  (let ((paramlist
         (split-string paramstr "," t "[[:blank:]]*")))
    (list
     (storax/yas-parse-pos-args paramlist)
     (storax/yas-parse-kwargs paramlist)
     (storax/yas-parse-poslist paramlist)
     (storax/yas-parse-kwargdict paramlist))))

(defun storax/yas-format-pos-args (args indent fieldindex)
  "Return formated positional ARGS indented with INDENT.
Use FIELDINDEX as a starting number for generating new fields."
  (let ((index (- fieldindex 1)))
    (mapconcat
     (lambda (x) (format "%s%s ($%d): $%d" indent x (incf index) (incf index)))
     args
     "")))

(defun storax/yas-format-kwargs (kwargs indent fieldindex)
  "Return formated KWARGS indented with INDENT.
Use FIELDINDEX as a starting number for generating new fields."
  (let ((index (- fieldindex 1)))
    (mapconcat
     (lambda (x)
       (let* ((kwarg (car x))
              (val (cdr x))
              (type (cond
                     ((string-match-p "^['\"]" val) ":str")
                     ((or (string-match-p "^True$" val) (string-match-p "^False$" val)) ":bool")
                     ((string-match-p "^[+-]*[0-9]+\\.[0-9]*$" val) ":float")
                     ((string-match-p "^[+-]*[0-9]+$" val) ":int")
                     (t ":type"))))
         (format "%s%s (Optional[${%d%s}]): $%d. Defaults to %s."
                 indent kwarg (incf index) type (incf index) val)))
     kwargs
     "")))

(defun storax/yas-format-params (paramstr)
  "Return a formated docstring for the PARAMSTR."
   (let* ((params (storax/yas-parse-args paramstr))
          (args (car params))
          (kwargs (cadr params))
          (kwargsindex (+ 1 (* 2 (length args))))
          (poslist (caddr params))
          (poslistindex (+ kwargsindex (* 2 (length kwargs))))
          (kwargdict (cadddr params))
          (kwargdictindex (+ poslistindex (if poslist 1 0)))
          (indent (storax/yas-makeindent storax-yas-curcolumn t 1))
          (indentlvl2 (storax/yas-makeindent storax-yas-curcolumn t 2)))
     (unless (equal params '(nil nil nil nil))
       (concat
        indent storax-yas-param-section-heading
        (when args
          (storax/yas-format-pos-args args indentlvl2 1))
        (when kwargs
          (storax/yas-format-kwargs kwargs indentlvl2 kwargsindex))
        (when poslist
          (format "%s*%s: $%d" indentlvl2 poslist poslistindex))
        (when kwargdict
          (format "%s**%s: $%d" indentlvl2 kwargdict kwargdictindex))
        "\n"))))

(defun storax/yas-reexpand ()
  "Expand a snipped again to jump to the new inserted anchors.
Thanks to Xaldew: http://emacs.stackexchange.com/a/19471."
  (let ((beg yas-snippet-beg)
        (end yas-snippet-end))
    (yas-expand-snippet
     (buffer-substring-no-properties beg end) beg end
     '((yas-indent-line nil) (yas-wrap-around-region nil)))
    (delete-trailing-whitespace beg (- end 1))))

;;; funcs.el ends here
