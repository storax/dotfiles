;;; funcs.el --- storax-python layer functions file for Spacemacs.
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

;; Credits to Jorgen Schaefer's elpy for the find test functions.

;;; Code:
(require 'ansi-color)
(require 'comint)
(require 'gud)
(require 'python)
(eval-when-compile (require 'cl-lib))

(cl-defstruct storax-bp bpnumber type disp enabled file line hits condition ignore)

(defvar storax-pdb-print-hist nil "History for pdb print commands.")
(defvar storax-pdb-condition-hist nil "History for pdb conditions.")

(defun storax-strip-whitespace (string)
  "Return STRING stripped of all whitespace."
  (while (string-match "^[\r\n\t ]+" string)
    (setq string (replace-match "" t t string)))
  string)

(defun storax/tox-env-list ()
  "Get a list of tox environments"
  (let* ((default-directory (projectile-project-root))
         (versions (split-string (shell-command-to-string "tox -l"))))
    (append (list "*default*" "ALL") versions)))

(defun storax/tox-read-env ()
  "Read virtual environment from user input."
  (let ((envlist (storax/tox-env-list))
        (prompt "Tox env: "))
  (if (fboundp 'helm-comp-read)
      (helm-comp-read
       prompt envlist
       :buffer "tox environments"
       :must-match t
       :history storax/tox-env-hist
       :marked-candidates t)
    (completing-read-multiple prompt envlist nil t nil storax/tox-env-hist))))

(defun storax/tox-construct-env-arg (envs)
  "Construct the -e arg out of ENVS."
  (if (member "*default*" envs)
      ""
      (concat " -e " (mapconcat 'identity envs ","))))

(defun storax/set-flycheck-error-function ()
  (setq flycheck-display-errors-function #'flycheck-display-error-messages-unless-error-list))

(defun storax/tox (envs args)
  "Test with tox.

ARGS is a string with arguments for tox."
  (interactive (list
                (storax/tox-read-env)
                (read-string "Tox arguments: " (car storax/tox-history) 'storax/tox-history)))
  (projectile-with-default-dir (projectile-project-root)
    (compilation-start (format "tox%s %s" (storax/tox-construct-env-arg envs) args) t)))

(defun storax/run-tests (cmd)
  "Execute the given CMD via compilation-start."
  (compilation-start cmd t))

(defun storax/python-test--current-test-name ()
  (let ((name (python-info-current-defun)))
    (if (and name
             (string-match "\\`\\([^.]+\\.[^.]+\\)\\." name))
        (match-string 1 name)
      name)))

(defun storax/python-test--module-name-for-file (top-level module-file)
  "Return the module name relative to TOP-LEVEL for MODULE-FILE.
For example, for a top level of /project/root/ and a module file
of /project/root/package/module.py, this would return
\"package.module\"."
  (let* ((relative-name (file-relative-name module-file top-level))
         (no-extension (replace-regexp-in-string "\\.py\\'" "" relative-name))
         (no-init (replace-regexp-in-string "/__init__\\'" "" no-extension))
         (dotted (replace-regexp-in-string "/" "." no-init)))
    (if (string-match "^\\." dotted)
        (concat "." (replace-regexp-in-string (regexp-quote "...") "." dotted))
      dotted)))

(defun storax/python-library-root ()
  "Return the root of the Python package chain of the current buffer.
That is, if you have /foo/package/module.py, it will return /foo,
so that import package.module will pick up module.py."
  (locate-dominating-file default-directory
                          (lambda (dir)
                            (not (file-exists-p
                                  (format "%s/__init__.py"
                                          dir))))))

(defun storax/python-test-at-point ()
  "Return a list specifying the test at point, if any.
This is used as the interactive
This list has four elements.
- Top level directory:
  All test files should be importable from here.
- Test file:
  The current file name.
- Test module:
  The module name, relative to the top level directory.
- Test name:
  The full name of the current test within the module, for
  example TestClass.test_method
If there is no test at point, test name is nil.
If the current buffer is not visiting a file, only the top level
directory is not nil."
  (if (not buffer-file-name)
      (progn
        (save-some-buffers)
        (list (storax/python-library-root) nil nil nil))
    (let* ((top (storax/python-library-root))
           (file buffer-file-name)
           (module (storax/python-test--module-name-for-file top file))
           (test (storax/python-test--current-test-name)))
      (if (and file (string-match "test" (or module test "")))
          (progn
            (save-buffer)
            (list top file module test))
        (save-some-buffers)
        (list top nil nil nil)))))

(defun storax/python-test-tox-runner (top file module test)
  "Test the project using tox.

This requires the tox package to be installed.
TOP is the project root.
FILE the test file.
MODULE is the module to test or nil to test all.
TEST is a single test function or nil to test all."
  (interactive (storax/python-test-at-point))
  (let (toxargs '(read-string "Tox arguments: " (car storax/tox-history) 'storax/tox-history))
  (projectile-with-default-dir (projectile-project-root)
    (storax/run-tests (format "tox %s" toxargs)))))

(defun storax/run-tox-pytest (envs toxargs pytestargs top file module test)
  "Run tox with pytest.

ENVS list of tox environments.
TOXARGS are the arguments for tox.
PYTESTARGS are the arguments for pytest.
TOP is the project root.
FILE the test file.
MODULE is the module to test or nil to test all.
TEST is a single test function or nil to test all."
  (let ((envarg (storax/tox-construct-env-arg envs)))
    (projectile-with-default-dir (projectile-project-root)
      (cond
       (test
        (storax/run-tests (concat
                           (format "tox%s %s -- py.test %s -k \"%s\" %s "
                                   envarg toxargs pytestargs test file))))
       (module
        (storax/run-tests (format "tox%s %s -- py.test %s %s" envarg toxargs pytestargs file)))
       (t
        (storax/run-tests (format "tox%s %s -- py.test %s" envarg toxargs pytestargs)))))))

(defun storax/python-test-tox-pytest-runner (top file module test)
  "Test the project using tox and pytest.

This requires the tox package to be installed and pytest as test suite in tox.
TOP is the project root.
FILE the test file.
MODULE is the module to test or nil to test all.
TEST is a single test function or nil to test all."
  (interactive (storax/python-test-at-point))
  (let ((envs (storax/tox-read-env))
        (toxargs (read-string "Tox arguments: " (car storax/tox-history) 'storax/tox-history))
	(pytestargs (read-string "py.test arguments: " (car storax/pytest-history) 'storax/pytest-history)))
  (storax/run-tox-pytest envs toxargs pytestargs top file module test)))

(defun storax/python-test-tox-pytest-runner-all (top file module test)
  "Test the project using tox and pytest.

This requires the tox package to be installed and pytest as test suite in tox.
TOP is the project root."
  (interactive (storax/python-test-at-point))
    (storax/python-test-tox-pytest-runner top nil nil nil))

(defun storax/python-test-tox-pytest-runner-module (top file module test)
  "Test the project using tox and pytest.

This requires the tox package to be installed and pytest as test suite in tox.
TOP is the project root.
FILE the test file.
MODULE is the module to test or nil to test all."
  (interactive (storax/python-test-at-point))
  (storax/python-test-tox-pytest-runner top file module nil))

(defun storax/python-test-tox-pytest-runner-module (top file module test)
  "Test the project using tox and pytest.

This requires the tox package to be installed and pytest as test suite in tox.
TOP is the project root."
  (interactive (storax/python-test-at-point))
  (storax/python-test-tox-pytest-runner top file module nil))

(defun storax/python-test-tox-pytest-runner-default (top file module test)
  "Test the project using tox and pytest.

This requires the tox package to be installed and pytest as test suite in tox.
Call `storax/python-test-tox-pytest-runner' with default values.
TOP is the project root.
FILE the test file.
MODULE is the module to test or nil to test all.
TEST is a single test function or nil to test all."
  (interactive (storax/python-test-at-point))
  (let ((toxargs (car storax/tox-history))
	(pytestargs (car storax/pytest-history)))
  (storax/run-tox-pytest toxargs pytestargs top file module test)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; PDB stuff
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar storax-pdb-output-filter-in-progress nil)
(defvar storax-pdb-output-filter-buffer nil)
(defvar storax-pdb-input-regexp "^[(<]*[Ii]?[Pp]db[>)]+ ")

(defun storax/pdb-output-filter (string)
  "Filter used in `storax/pdb-call-no-output' to grab output.
STRING is the output received to this point from the process.
This filter saves received output from the process in
`storax-pdb-output-filter-buffer' and stops receiving it after
detecting a prompt at the end of the buffer."
  (setq
   string (ansi-color-filter-apply string)
   storax-pdb-output-filter-buffer
   (concat storax-pdb-output-filter-buffer string))
  (when (string-match
         ;; XXX: It seems on OSX an extra carriage return is attached
         ;; at the end of output, this handles that too.
         (concat
          "\r?\n"
          ;; Remove initial caret from calculated regexp
          (replace-regexp-in-string
           (rx string-start ?^) ""
           storax-pdb-input-regexp)
          "$")
         storax-pdb-output-filter-buffer)
    ;; Output ends when `storax-pdb-output-filter-buffer' contains
    ;; the prompt attached at the end of it.
    (setq storax-pdb-output-filter-in-progress nil
          storax-pdb-output-filter-buffer
          (substring storax-pdb-output-filter-buffer
                     0 (match-beginning 0))))
  (if storax-pdb-output-filter-in-progress
      ""
    "(Pdb) "))

(defun storax/pdb-call-no-output (string)
  "Send STRING to PROCESS and inhibit ouput.
Return the output."
  (let ((comint-preoutput-filter-functions
         '(storax/pdb-output-filter))
        (storax-pdb-output-filter-in-progress t)
        (inhibit-quit t)
        (output-received t))
    (gud-call string)
    (while (and storax-pdb-output-filter-in-progress output-received)
      ;; `storax/pdb-output-filter' takes care of setting
      ;; `storax/pdb--output-filter-in-progress' to NIL after it
      ;; detects end of output.
      (setq output-received (accept-process-output (get-buffer-process gud-comint-buffer) 0 500)))
    (prog1
        storax-pdb-output-filter-buffer
      (setq storax-pdb-output-filter-buffer nil))))

(defun storax/pdb-parse-breakpoint (line)
  "Return a breakpoint parsed from LINE."
  (string-match
   "^\\([0-9]+\\)[ \\t]+\\(\\w+\\)[ \\t]+\\(\\w+\\)[ \\t]+\\(\\w+\\)[ \\t]+at[ \\t]+\\(.*\\):\\([0-9]+\\)"
   line)
  (make-storax-bp
   :bpnumber (string-to-number (match-string 1 line))
   :type (match-string 2 line)
   :disp (match-string 3 line)
   :enabled (string= "yes" (match-string 4 line))
   :file (match-string 5 line)
   :line (string-to-number (match-string 6 line))
   :hits 0
   :condition nil
   :ignore 0))

(defun storax/pdb-parse-condition (line)
  "Return the condition string from LINE."
  (string-match
   "^[ \t]+stop only if \\(.*\\)$" line)
  (match-string 1 line))

(defun storax/pdb-parse-ignore (line)
  "Return ignore from LINE."
  (string-match
   "^[ \t]+ignore next \\([0-9]+\\) hits?$" line)
  (string-to-number (match-string 1 line)))

(defun storax/pdb-parse-hits (line)
  "Return hits from LINE."
  (string-match
   "^[ \t]+breakpoint already hit \\([0-9]+\\) times?$" line)
  (string-to-number (match-string 1 line)))

(defun storax/pdb-get-breakpoints ()
  "Return a list of breakpoints."
  (let ((output
         (cdr (split-string
          (storax/pdb-call-no-output "break")
          "[\n\r]+")))
        breakpoints)
    (dolist (line output)
      (cond
       ((string-match "^[0-9]+" line)
        (add-to-list 'breakpoints (storax/pdb-parse-breakpoint line) t))
       ((string-match "^[ \t]+stop only if" line)
        (setf (storax-bp-condition (car (last breakpoints))) (storax/pdb-parse-condition line)))
       ((string-match "^[ \t]+ignore next" line)
        (setf (storax-bp-ignore (car (last breakpoints))) (storax/pdb-parse-ignore line)))
       ((string-match "^[ \t]+breakpoint already hit" line)
        (setf (storax-bp-hits (car (last breakpoints))) (storax/pdb-parse-hits line)))))
    breakpoints))

(defun storax/pdb-breakpoints-to-strings (breakpoints)
  "Convert the list of BREAKPOINTS to a list of strings."
  (let (strlist)
    (dolist (bp breakpoints)
      (let ((bpstr
             (format "%s: %s %s:%s"
                     (storax-bp-bpnumber bp)
                     (if (storax-bp-enabled bp)
                       "armed"
                       "off")
                     (storax-bp-file bp)
                     (storax-bp-line bp)))
            (condition (storax-bp-condition bp))
            (hits (storax-bp-hits bp))
            (ignore (storax-bp-ignore bp)))
        (when condition
          (setq bpstr (concat bpstr "\n  stop only if " condition)))
        (unless (zerop ignore)
          (setq bpstr (concat bpstr "\n  ignore next " (number-to-string ignore) " hit(s)")))
        (unless (zerop hits) 
          (setq bpstr (concat bpstr "\n  already hit " (number-to-string hits) " time(s)")))
        (add-to-list 'strlist bpstr t)))
    strlist))

(defun storax/pdb-breakpoint-from-string (bpstr breakpoints)
  "Return a breakpoint represented by BPSTR from BREAKPOINTS."
  (string-match "^\\([0-9]+\\): " bpstr)
  (let ((bpnumber (string-to-number (match-string 1 bpstr))))
    (catch 'bp
      (dolist (bp breakpoints)
        (when (equal bpnumber (storax-bp-bpnumber bp))
          (throw 'bp bp))))))

(defun storax/pdb-select-breakpoint ()
  "Return a breakpoint selected by the user."
  (let* ((breakpoints (storax/pdb-get-breakpoints))
         (breakpoint-strings (storax/pdb-breakpoints-to-strings breakpoints)))
    (if breakpoints
        (storax/pdb-breakpoint-from-string
         (completing-read "Select Breakpoint: " breakpoint-strings nil t)
         breakpoints)
      (progn (message "No breakpoints set!") nil))))

(defun storax/pdb-goto-breakpoint (breakpoint)
  "Goto the location of BREAKPOINT"
  (interactive (list (storax/pdb-select-breakpoint)))
  (when breakpoint
    (let ((cb (current-buffer)))
      (with-current-buffer
          (if (eq (current-buffer) gud-comint-buffer)
              (find-file-other-window (storax-bp-file breakpoint))
            (find-file (storax-bp-file breakpoint)))
        (goto-line (storax-bp-line breakpoint)))
      (when (eq cb gud-comint-buffer)
        (switch-to-buffer-other-window gud-comint-buffer)))))

(defun storax/pdb-clear-breakpoint (breakpoint)
  "Clear BREAKPOINT."
  (interactive (list (storax/pdb-select-breakpoint)))
  (when breakpoint
    (gud-call (format "clear %s" (storax-bp-bpnumber breakpoint)))))

(defun storax/pdb-toogle-breakpoint (breakpoint)
  "Toggle BREAKPOINT enabled status."
  (interactive (list (storax/pdb-select-breakpoint)))
  (when breakpoint
    (if (storax-bp-enabled breakpoint)
        (gud-call (format "disable %s" (storax-bp-bpnumber breakpoint)))
      (gud-call (format "enable %s" (storax-bp-bpnumber breakpoint))))))

(defun storax/pdb-ignore-breakpoint (breakpoint num)
  "Ignore BREAKPOINT NUM times."
  (interactive
   (list
    (storax/pdb-select-breakpoint)
    (read-number "Ignore N times: " 0)))
  (when breakpoint
    (gud-call (format "ignore %s %s" (storax-bp-bpnumber breakpoint) num))))

(defun storax/pdb-condition-breakpoint (breakpoint cond)
  "Execute BREAKPOINT only when COND is True."
  (interactive
   (list
    (storax/pdb-select-breakpoint)
    (read-string "Condition: " nil storax-pdb-condition-hist)))
  (when (and breakpoint (not (string= "" cond)))
    (gud-call (format "condition %s %s" (storax-bp-bpnumber breakpoint) cond))))

(defun storax/pdb-print-symbol ()
  "Print the current symbol at point."
  (interactive)
  (gud-call (format "p %s" (symbol-at-point))))

(defun storax/pdb-pprint-symbol ()
  "Pretty print the current symbol at point."
  (interactive)
  (gud-call (format "pp %s" (symbol-at-point))))

(defun storax/pdb-print-line ()
  "Print the current line without preceding whitespace."
  (interactive)
  (gud-call
   (format "p %s"
           (storax-strip-whitespace
            (buffer-substring (line-beginning-position) (line-end-position))))))

(defun storax/pdb-pprint-line ()
  "Pretty print the current line without preceding whitespace."
  (interactive)
  (gud-call
   (format "pp %s"
           (storax-strip-whitespace
            (buffer-substring (line-beginning-position) (line-end-position))))))

(defun storax/pdb-execute-line ()
  "Execute the current line without preceding whitespace."
  (interactive)
  (gud-call
   (format "!%s"
           (storax-strip-whitespace
            (buffer-substring (line-beginning-position) (line-end-position))))))

(defun storax/pdb-print-region ()
  "Print the current region without preceding whitespace."
  (interactive)
  (if (region-active-p)
      (gud-call
       (format "p %s"
               (storax-strip-whitespace
                (buffer-substring (region-beginning) (region-end)))))
    (storax/pdb-print-line)))

(defun storax/pdb-pprint-region ()
  "Pretty print the current region without preceding whitespace."
  (interactive)
  (if (region-active-p)
      (gud-call
       (format "pp %s"
               (storax-strip-whitespace
                (buffer-substring (region-beginning) (region-end)))))
    (storax/pdb-print-line)))

(defun storax/pdb-execute-region ()
  "Execute the current region without preceding whitespace."
  (interactive)
  (if (region-active-p)
      (gud-call
       (format "!%s"
               (storax-strip-whitespace
                (buffer-substring (region-beginning) (region-end)))))
    (storax/pdb-execute-line)))

(defun storax/pdb-print-prompt ()
  "Prompt user what to print."
  (interactive)
  (let ((user-input
         (read-string "Print: " nil storax-pdb-print-hist (symbol-at-point))))
  (gud-call (format "p %s" user-input))))

(defun storax/pdb-pprint-prompt ()
  "Prompt user what to pretty print."
  (interactive)
  (let ((user-input
         (read-string "Pretty Print: " nil storax-pdb-print-hist (symbol-at-point))))
    (gud-call (format "pp %s" user-input))))

(defun storax/pdb-execute-prompt ()
  "Prompt user what to execute."
  (interactive)
  (let ((user-input
         (read-string "Execute: " nil storax-pdb-print-hist (symbol-at-point))))
    (gud-call (format "!%s" user-input))))

(defun storax/gud-compilation-start (command &optional name-function marker-filter highlight-regexp)
  "Start a gud session.

Execute COMMAND.
NAME-FUNCTION can either be nil or a function which takes a mode as argument.
MARKER-FILTER is a gud marker filter, e.g. `gud-pdb-marker-filter'.
HIGHLIGHT-REGEXP is the regexp to highlight errors."
  (with-current-buffer (compilation-start command nil name-function highlight-regexp)
    (gud-mode)
    (set (make-local-variable 'gud-target-name)
         command)
    (set (make-local-variable 'gud-marker-filter) marker-filter)
    (setq gud-last-last-frame nil)
    (set-process-filter (get-buffer-process (current-buffer)) 'gud-filter)
    (set-process-sentinel (get-buffer-process (current-buffer)) 'gud-sentinel)
    (gud-set-buffer)
    (setq buffer-read-only nil)))

(defun storax/pdb-break ()
  "Set breakpoint at current line."
  (interactive)
  (gud-call "break %d%f:%l"))

(defun storax/pdb-clear-current-line ()
  "Clear breakpoint at current line."
  (interactive)
  (gud-call "clear %d%f:%l"))

(defun storax/pdb-step ()
  "Pdb step command."
  (interactive)
  (gud-call "step"))

(defun storax/pdb-next ()
  "Pdb next command."
  (interactive)
  (gud-call "next"))

(defun storax/pdb-continue ()
  "Pdb continue command."
  (interactive)
  (gud-call "continue"))

(defun storax/pdb-return ()
  "Pdb return command."
  (interactive)
  (gud-call "return"))

(defun storax/pdb-up ()
  "Pdb up command."
  (interactive)
  (gud-call "up"))

(defun storax/pdb-down ()
  "Pdb down command."
  (interactive)
  (gud-call "down"))

(defun storax/pdb-until ()
  "Pdb until command."
  (interactive)
  (gud-call "until"))

(defun storax/pdb-jump ()
  "Pdb jump command."
  (interactive)
  (gud-call "jump %l"))

(defun storax/pdb (command-line)
  "Run pdb on program COMMAND-LINE in buffer `*gud-FILE*'.
The directory containing FILE becomes the initial working directory
and source-file directory for your debugger."
  (interactive
   (list (gud-query-cmdline 'pdb)))
  (storax/gud-compilation-start command-line nil 'gud-pdb-marker-filter)
  (set (make-local-variable 'gud-minor-mode) 'pdb)
  (with-current-buffer gud-comint-buffer
    (local-set-key (kbd "C-c dR") 'storax/pdb-break)
    (local-set-key (kbd "C-c dU") 'storax/pdb-until)
    (local-set-key (kbd "C-c d SPC") 'storax/pdb-break)
    (local-set-key (kbd "C-c dbc") 'storax/pdb-condition-breakpoint)
    (local-set-key (kbd "C-c dbr") 'storax/pdb-clear-current-line)
    (local-set-key (kbd "C-c dbd") 'storax/pdb-clear-breakpoint)
    (local-set-key (kbd "C-c dbg") 'storax/pdb-goto-breakpoint)
    (local-set-key (kbd "C-c dbi") 'storax/pdb-ignore-breakpoint)
    (local-set-key (kbd "C-c dbt") 'storax/pdb-toogle-breakpoint)
    (local-set-key (kbd "C-c dc") 'storax/pdb-cont)
    (local-set-key (kbd "C-c dd") 'storax/pdb-down)
    (local-set-key (kbd "C-c de SPC") 'storax/pdb-execute-prompt)
    (local-set-key (kbd "C-c del") 'storax/pdb-execute-line)
    (local-set-key (kbd "C-c der") 'storax/pdb-execute-region)
    (local-set-key (kbd "C-c dg") 'storax/pdb-goto-breakpoint)
    (local-set-key (kbd "C-c dj") 'storax/pdb-jump)
    (local-set-key (kbd "C-c dn") 'storax/pdb-next)
    (local-set-key (kbd "C-c dp SPC") 'storax/pdb-print-prompt)
    (local-set-key (kbd "C-c dpl") 'storax/pdb-print-line)
    (local-set-key (kbd "C-c dpp SPC") 'storax/pdb-pprint-prompt)
    (local-set-key (kbd "C-c dppl") 'storax/pdb-pprint-line)
    (local-set-key (kbd "C-c dppr") 'storax/pdb-pprint-region)
    (local-set-key (kbd "C-c dpps") 'storax/pdb-pprint-symbol)
    (local-set-key (kbd "C-c dpr") 'storax/pdb-print-region)
    (local-set-key (kbd "C-c dps") 'storax/pdb-print-symbol)
    (local-set-key (kbd "C-c dr") 'storax/pdb-return)
    (local-set-key (kbd "C-c ds") 'storax/pdb-step)
    (local-set-key (kbd "C-c du") 'storax/pdb-up)
    (setq comint-prompt-regexp storax-pdb-input-regexp)
    (setq paragraph-start comint-prompt-regexp)
    (run-hooks 'pdb-mode-hook)))

;;; funcs.el ends here
