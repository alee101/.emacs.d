;;; custom.el --- custom functions and macros

;; General
(defun comment-or-uncomment-region-or-line ()
  "Comments or uncomments the region or the current line if there's no active region."
  (interactive)
  (let (beg end)
    (if (region-active-p)
        (setq beg (region-beginning) end (region-end))
      (setq beg (line-beginning-position) end (line-end-position)))
    (comment-or-uncomment-region beg end)))

(defun delete-file-and-buffer ()
  " Kill the current buffer and delete the file it is visiting."
  (interactive)
  (let ((file-name buffer-file-name))
    (when file-name
      (progn
        (delete-file file-name)
        (message "Deleted file %s." file-name)
        (kill-buffer)))))

(defun endless/fill-or-unfill ()
  "Like `fill-paragraph', but unfill if used twice."
  (interactive)
  (let ((fill-column
         (if (eq last-command 'endless/fill-or-unfill)
             (progn (setq this-command nil)
                    (point-max))
           fill-column)))
    (call-interactively #'fill-paragraph)))

(global-set-key (kbd "M-;") 'comment-or-uncomment-region-or-line)
(global-set-key (kbd "C-x K") 'delete-file-and-buffer)
(global-set-key [remap fill-paragraph] #'endless/fill-or-unfill)


;; Blend JS stuff
(defun blend/backend-relative-file-name (file-name)
  ;; (file-relative-name file-name "/Users/albertlee/blend/lending/backend/")
  (file-relative-name file-name "/Users/albertlee/blend/running-lending/lending/backend/")
  )

(defun blend/frontend-relative-file-name (file-name)
  (file-relative-name file-name "/Users/albertlee/blend/lending/frontend/lender-react/")
  ;; (file-relative-name file-name "/Users/albertlee/blend/running-lending/lending/backend/")
  )

(defun blend/open-backend-term ()
  (if (get-buffer "*backend-test-term*")
      (switch-to-buffer "*backend-test-term*")
    (ansi-term "/bin/bash" "backend-test-term")))

(defun blend/run-cur-test ()
  (interactive)
  (let ((relative-file-name (blend/backend-relative-file-name buffer-file-name)))
    (progn
      (blend/open-backend-term)
      (process-send-string
       "*backend-test-term*"
       (s-lex-format "cd ~/blend/lending/backend && npm run testFast -- --files='${relative-file-name}'")))))

(defun blend/run-cur-frontend-test ()
  (interactive)
  (let ((relative-file-name (blend/frontend-relative-file-name buffer-file-name)))
    (progn
      (blend/open-backend-term)
      (process-send-string
       "*backend-test-term*"
       (s-lex-format "cd ~/blend/lending/frontend/lender-react/ && sh ./bin/test/jest.sh \"$@\" \"${relative-file-name}\"")))))

(defun blend/lint-cur-file ()
  (interactive)
  (let ((relative-file-name (blend/backend-relative-file-name buffer-file-name))
        (tslint-config-file (if (string-match-p "test/" buffer-file-name) "tslint.test.js" "tslint.js")))
    (progn
      (blend/open-backend-term)
      (process-send-string
       "*backend-test-term*"
       (s-lex-format "cd ~/blend/lending/backend && node --max_old_space_size=8192 ./node_modules/tslint/bin/tslint --fix --project tsconfig.json --config ${tslint-config-file} ${relative-file-name}")))))

(defun blend/find-backend-file-by-name (file-name-sans-ext)
  (let ((js-file-name (format "%s.js" file-name-sans-ext))
        (ts-file-name (format "%s.ts" file-name-sans-ext)))
    ;; prefer creating TS file if neither exists
    (find-file
     (if (file-exists-p js-file-name) js-file-name ts-file-name))))

(defun blend/toggle-dist ()
  (interactive)
  (cl-destructuring-bind
      (from-dir to-dir)
      (if (string-prefix-p "dist/" (blend/backend-relative-file-name buffer-file-name))
          '("backend/dist/" "backend/")
        '("backend/" "backend/dist/"))
    (blend/find-backend-file-by-name
     (replace-regexp-in-string from-dir to-dir (file-name-sans-extension buffer-file-name)))))

(defun blend/toggle-test-backend (file-name)
  (let ((base-file-name (file-name-sans-extension file-name)))
    (blend/find-backend-file-by-name
     (if (string-prefix-p "test/" (blend/backend-relative-file-name base-file-name))
         (replace-regexp-in-string "test/" "lib/" (replace-regexp-in-string "Tests" "" base-file-name))
       (replace-regexp-in-string "lib/" "test/" (format "%sTests" base-file-name))))))

(defun blend/toggle-test-frontend-angular (file-name)
  (find-file
   (if (string-suffix-p "spec.js" file-name)
       (replace-regexp-in-string ".spec.js$" ".js" file-name)
     (replace-regexp-in-string ".js" ".spec.js" file-name))))

(defun blend/toggle-test-frontend-react (file-parts)
  (find-file
   (if (member "__tests__" file-parts)
       (concat
        (s-join "/" (butlast file-parts 2))
        "/"
        (replace-regexp-in-string ".spec.ts" ".ts" (car (last file-parts))))
     (concat
      (s-join "/" (butlast file-parts 1))
      "/__tests__/"
      (replace-regexp-in-string ".ts" ".spec.ts" (car (last file-parts)))))))

(defun toggle-test-go (file-name)
  (find-file
   (if (string-suffix-p "_test.go" file-name)
       (replace-regexp-in-string "_test.go$" ".go" file-name)
     (replace-regexp-in-string ".go$" "_test.go" file-name))))

;; bound in init-core.el to override projectile-toggle-between-implementation-and-test
(defun toggle-test ()
  (interactive)
  (let ((file-parts (split-string buffer-file-name "/")))
    (cond ((string-suffix-p ".go" buffer-file-name) (toggle-test-go buffer-file-name))
          ;; assuming js otherwise for now
          ((member "borrower-react" file-parts) (blend/toggle-test-frontend-react file-parts))
          ((member "lender-react" file-parts) (blend/toggle-test-frontend-react file-parts))
          ((member "frontend" file-parts) (blend/toggle-test-frontend-angular buffer-file-name))
          ((member "backend" file-parts) (blend/toggle-test-backend buffer-file-name)))))

(global-set-key (kbd "C-c j l") 'blend/lint-cur-file)
(global-set-key (kbd "C-c j t") 'blend/run-cur-test)
(global-set-key (kbd "C-c j d") 'blend/toggle-dist)


;; More JS stuff
(defun rm-console-logs ()
  "Remove console.log statements in a file."
  (interactive)
  (save-excursion
    (beginning-of-buffer)
    (while (re-search-forward "console.log(.*);")
      (beginning-of-line)
      (kill-whole-line))))

(defun toggle-mocha-test-block ()
  "Toggle .only setting on nearest outer mocha describe/it block. Toggles first block if in top-level block."
  (interactive)
  (end-of-line)
  (let ((mocha-patterns "it(\\|it\.only(\\|describe(\\|describe\.only")
        (toggle (lambda ()
                  (cond
                   ((looking-at "it.only(") (replace-match "it("))
                   ((looking-at "it(") (replace-match "it.only("))
                   ((looking-at "describe.only(") (replace-match "describe("))
                   ((looking-at "describe(") (replace-match "describe.only("))))))
    (if (re-search-backward mocha-patterns nil t)
        (funcall toggle)
      (progn (re-search-forward mocha-patterns)
             (beginning-of-line)
             (funcall toggle)))))

;; Macro for converting _.partial(fn, ctx, ...) to cb => fn(ctx, ..., cb)
(fset 'albert-js-departial
      [?\C-a ?\C-s ?_ ?. ?p ?a ?r ?t ?i ?a ?l ?\( return ?\C-  ?\C-s ?, ?  return ?\C-w ?\C-b ?\C-\M-f ?\C-b ?, ?  ?c ?b ?\C-r ?p ?a ?r ?t ?i ?a ?l return ?\M-f ?\C-y backspace backspace ?\C-x ?\C-x return ?\C-_ C-backspace backspace backspace ?c ?b ?  ?= ?> ?  ?\C-e])

(fset 'albert-js-asyncify
      (lambda (&optional arg)
        "Macro for converting (x, cb) => { ... cb(); } to async.asyncify(x => { })"
        (interactive "p")
        (kmacro-exec-ring-item (quote ([19 99 98 return backspace backspace backspace backspace 4 18 40 return 97 115 121 110 99 46 97 115 121 110 99 105 102 121 19 123 return 2 134217734 41 18 99 98 40 41 59 return 1 11 backspace] 0 "%d")) arg)))

(fset 'albert-js-fn6
      (lambda (&optional arg)
        "Macro for converting function(...) {} to ES6 arrow function (...) => {}"
        (interactive "p")
        (kmacro-exec-ring-item (quote ([19 102 117 110 99 116 105 111 110 return C-backspace 19 123 return 2 61 62 32 5] 0 "%d")) arg)))

(fset 'albert-js-oneline
      (lambda
        (&optional arg)
        "Macro for converting cb => { \n ... \n } to cb => ..."
        (interactive "p")
        (kmacro-exec-ring-item (quote ([19 61 62 32 123 return 134217843 14 1 134217760 backspace backspace 5 backspace 14 1 134217760 backspace backspace 5] 0 "%d")) arg)))

;; Macro performing inverse of albert-js-oneline
(fset 'albert-js-multiline
      [?\C-a ?\C-s ?= ?> return ?\C-f ?\{ return return ?\C-f ?\C-k ?\C-p ?\C-y tab backspace ?\; ?\C-n ?,])

(fset 'albert-js-require-to-import
   [?\M-d ?i ?m ?p ?o ?r ?t ?  ?* ?  ?a ?s ?\C-s ?= return backspace ?f ?r ?o ?m ?\C-f ?\M-d ?\C-f ?\M-s ?\C-e ?\C-n ?\C-a])


(global-set-key (kbd "C-c j r") 'rm-console-logs)
(global-set-key (kbd "C-c j o") 'toggle-mocha-test-block)

(global-set-key (kbd "C-c j a") 'albert-js-asyncify)
(global-set-key (kbd "C-c j p") 'albert-js-departial)
(global-set-key (kbd "C-c j 6") 'albert-js-fn6)
(global-set-key (kbd "C-c j m") 'albert-js-multiline)
(global-set-key (kbd "C-c j 1") 'albert-js-oneline)
(global-set-key (kbd "C-c j i") 'albert-js-require-to-import)
