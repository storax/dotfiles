(defun storax--typescript--ensure-cache (&optional limit)
  "Ensures brace cache is valid up to the character before LIMIT.
LIMIT defaults to point."
  (setq limit (or limit (point)))
  (when (< typescript--cache-end limit)

    (c-save-buffer-state
        (open-items
         orig-match-start
         orig-match-end
         orig-depth
         parse
         prev-parse-point
         name
         case-fold-search
         filtered-class-styles
         new-item
         goal-point
         end-prop)

      ;; Figure out which class styles we need to look for
      (setq filtered-class-styles
            (loop for style in typescript--class-styles
                  if (memq (plist-get style :framework)
                           typescript-enabled-frameworks)
                  collect style))

      (save-excursion
        (save-restriction
          (widen)

          ;; Find last known good position
          (goto-char typescript--cache-end)
          (unless (bobp)
            (setq open-items (get-text-property
                              (1- (point)) 'typescript--pstate))

            (unless open-items
              (goto-char (previous-single-property-change
                          (point) 'typescript--pstate nil (point-min)))

              (unless (bobp)
                (setq open-items (get-text-property (1- (point))
                                                    'typescript--pstate))
                (assert open-items))))

          (unless open-items
            ;; Make a placeholder for the top-level definition
            (setq open-items (list typescript--initial-pitem)))

          (setq parse (syntax-ppss))
          (setq prev-parse-point (point))

          (typescript--clear-stale-cache)

          (narrow-to-region (point-min) limit)

          (loop while (and typescript--quick-match-re-func (re-search-forward typescript--quick-match-re-func nil t))
                for orig-match-start = (goto-char (match-beginning 0))
                for orig-match-end = (match-end 0)
                do (typescript--ensure-cache--update-parse)
                for orig-depth = (nth 0 parse)

                ;; Each of these conditions should return non-nil if
                ;; we should add a new item and leave point at the end
                ;; of the new item's header (h-end in the
                ;; typescript--pitem diagram). This point is the one
                ;; after the last character we need to unambiguously
                ;; detect this construct. If one of these evaluates to
                ;; nil, the location of the point is ignored.
                if (cond
                    ;; In comment or string
                    ((nth 8 parse) nil)

                    ;; Regular function declaration
                    ((and (looking-at "\\_<function\\_>")
                          (setq name (typescript--forward-function-decl)))

                     (when (eq name t)
                       (setq name (typescript--guess-function-name orig-match-end))
                       (if name
                           (when typescript--guess-function-name-start
                             (setq orig-match-start
                                   typescript--guess-function-name-start))

                         (setq name t)))

                     (assert (eq (char-after) ?{))
                     (forward-char)
                     (make-typescript--pitem
                      :paren-depth orig-depth
                      :h-begin orig-match-start
                      :type 'function
                      :name (if (eq name t)
                                name
                              (typescript--split-name name))))

                    ;; "Prototype function" declaration
                    ((looking-at typescript--plain-method-re)
                     (goto-char (match-beginning 3))
                     (when (save-match-data
                             (typescript--forward-function-decl))
                       (forward-char)
                       (make-typescript--pitem
                        :paren-depth orig-depth
                        :h-begin orig-match-start
                        :type 'function
                        :name (nconc (typescript--split-name
                                      (match-string-no-properties 1))
                                     (list (match-string-no-properties 2))))))

                    ;; Class definition
                    ((loop with syntactic-context =
                           (typescript--syntactic-context-from-pstate open-items)
                           for class-style in filtered-class-styles
                           if (and (memq syntactic-context
                                         (plist-get class-style :contexts))
                                   (looking-at (plist-get class-style
                                                          :class-decl)))
                           do (goto-char (match-end 0))
                           and return
                           (make-typescript--pitem
                            :paren-depth orig-depth
                            :h-begin orig-match-start
                            :type class-style
                            :name (typescript--split-name
                                   (match-string-no-properties 1))))))

                do (typescript--ensure-cache--update-parse)
                and do (push it open-items)
                and do (put-text-property
                        (1- (point)) (point) 'typescript--pstate open-items)
                else do (goto-char orig-match-end))

          (goto-char limit)
          (typescript--ensure-cache--update-parse)
          (setq typescript--cache-end limit)
          (setq typescript--last-parse-pos limit)
          (setq typescript--state-at-last-parse-pos open-items))))))


(defun storax/typescript--ensure-cache (old-func &rest args)
  "Fix issues for vue files."
  (apply #'storax--typescript--ensure-cache args))


(defun storax--typescript--class-decl-matcher (limit)
  "Font lock function used by `typescript-mode'.
This performs fontification according to `typescript--class-styles'."
  (loop initially (typescript--ensure-cache limit)
        while (and typescript--quick-match-re (re-search-forward typescript--quick-match-re limit t))
        for orig-end = (match-end 0)
        do (goto-char (match-beginning 0))
        if (loop for style in typescript--class-styles
                 for decl-re = (plist-get style :class-decl)
                 if (and (memq (plist-get style :framework)
                               typescript-enabled-frameworks)
                         (memq (typescript-syntactic-context)
                               (plist-get style :contexts))
                         decl-re
                         (looking-at decl-re))
                 do (goto-char (match-end 0))
                 and return t)
        return t
        else do (goto-char orig-end)))


(defun storax/typescript--class-decl-matcher (old-func &rest args)
  "Fix issues for vue files."
  (apply #'storax--typescript--class-decl-matcher args))


(defun storax/use-tslint-from-node-modules ()
  (let* ((root (locate-dominating-file
                (or (buffer-file-name) default-directory)
                "node_modules"))
         (tslint (and root
                      (expand-file-name "node_modules/tslint/bin/tslint"
                                        root))))
    (when (and tslint (file-executable-p tslint))
      (setq-local flycheck-typescript-tslint-executable tslint))))
