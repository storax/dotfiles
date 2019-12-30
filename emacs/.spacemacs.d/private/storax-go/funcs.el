(defun storax-go/go-setup-lsp-dap ()
  (require 'dap-go)
  (dap-register-debug-provider "go" 'storax-go/dap-go-populate-default-args))

(defun storax-go/dap-go-populate-default-args (conf)
  "Populate CONF with the default arguments."
  (setq conf (pcase (plist-get conf :mode)
               ("auto" (storax-go/dap-go-populate-auto-args conf))
               ("debug" (dap--put-if-absent conf :program "."))
               ("exec" (dap--put-if-absent conf :program (read-file-name "Select executable to debug.")))
               ("remote"
                (dap--put-if-absent conf :program (lsp-find-session-folder (lsp-session) (buffer-file-name)))
                (dap--put-if-absent conf :port (string-to-number (read-string "Enter port: " "2345")))
                (dap--put-if-absent conf :program-to-start
                                    (concat dap-go-delve-path
                                            " attach --headless --api-version=2 "
                                            (format "--listen=:%d " (plist-get conf :port))
                                            (number-to-string
                                             (dap--completing-read "Select process: "
                                                                   (list-system-processes)
                                                                   (lambda (pid)
                                                                     (-let (((&alist 'user 'comm)
                                                                             (process-attributes pid)))
                                                                       (format "%6d %-30s %s" pid comm user)))
                                                                   nil t))))
                )))

  (-> conf
      (dap--put-if-absent :dap-server-path dap-go-debug-program)
      (dap--put-if-absent :dlvToolPath dap-go-delve-path)
      (dap--put-if-absent :type "go")
      (dap--put-if-absent :name "Go Debug")))

(defun storax-go/dap-go-populate-auto-args (conf)
  "Populate auto arguments."
  (dap--put-if-absent conf :program (buffer-file-name))

  (if (string-suffix-p "_test.go" (buffer-file-name))
      (plist-put conf :mode "test")
    (plist-put conf :mode "debug")))
