(require 'go-mode)

(define-prefix-command 'go-check-mode-keymap)
(define-key go-check-mode-keymap (kbd "c") 'go-check-current)
(define-key go-check-mode-keymap (kbd "a") 'go-check-all)
(define-key go-check-mode-keymap (kbd "t") 'go-check-toggle-test-and-target)
(define-key go-check-mode-keymap (kbd "s") 'go-check-single)
(define-key go-check-mode-keymap (kbd "e") 'go-check-ad-hoc)

(define-prefix-command 'go-checkable-mode-keymap)
(define-key go-checkable-mode-keymap (kbd "c") 'go-check-current)
(define-key go-checkable-mode-keymap (kbd "a") 'go-check-all)
(define-key go-checkable-mode-keymap (kbd "t") 'go-check-toggle-test-and-target)

(defgroup go-check-mode nil
  "Go check minor mode." :group 'go-mode)

(defcustom go-check-key-command-prefix  (kbd "C-c ,")
  "The prefix for all Go check related key commands"
  :type 'string)

(defcustom go-check-compilation-buffer-name "*gocheck-compilation*"
  "The buffer name in which test results should be shown"
  :type 'string)

;;;###autoload
(define-minor-mode go-check-mode
  "Minor mode for Go files using the gocheck testing framework"
  :lighter "" :keymap `((,go-check-key-command-prefix . go-check-mode-keymap)))

(define-minor-mode go-checkable-mode
  "Minor mode for Go testable files"
  :lighter "" :keymap `((,go-check-key-command-prefix . go-checkable-mode-keymap)))

(defun go-check-tests-in-region (region-start region-end)
  "Return all test functions found between `region-start'
   and `region-end'"
  (let ((tests-in-region nil))
    (save-excursion
      (goto-char region-start)
      (while (search-forward-regexp "func .* \\(Test.*\\)(.*)" region-end t)
        (setq tests-in-region (cons (match-string 1) tests-in-region)))
      (identity tests-in-region))))

(defun go-check-run-tests-in-region (region-start region-end)
  "Run all tests found between `region-start' and `region-end'"
  (let ((tests-in-region (go-check-tests-in-region region-start region-end)))
    (if (= 0 (length tests-in-region)) (message "No tests found in region/file")
      (go-check-run-tests-for-regexps tests-in-region))))

(defun go-check-f-flag-for (files)
  (let ((files (if (listp files)
                   files
                 (list files))))
    (concat "-gocheck.f " "'" (mapconcat 'identity files "|") "'")))

(defun go-check-run-tests-for-regexps (query)
  "Use -gocheck.f to run specs matching `query'. If
   query is a list, concatenate them with a | to produce
   a string of 'or' clauses"
  (go-check-compile (buffer-file-name) (go-check-f-flag-for query)))

(defun go-check-toggle-test-and-target ()
  (interactive)
  (let ((other-file (if (go-check-buffer-is-test-p)
                          (go-check-target-file-for (buffer-name))
                        (go-check-test-file-for (buffer-name)))))
    (go-check-jump-to-file other-file)))

(defun go-check-current ()
  (interactive)
  (go-check-run-tests-in-region (point-min) (point-max)))

(defun go-check-single ()
  "Run the test nearest to point. If the mark is active,
   run all tests found in region"
  (interactive)
  (if mark-active
      (go-check-run-tests-in-region (region-beginning) (region-end))
    (save-excursion
    (go-beginning-of-defun)
    (if (looking-at "func .* \\(Test.*\\)(.*)")
        (go-check-run-tests-for-regexps (match-string 1))
      (message "No Test* found around point")))))

(defun go-check-ad-hoc (query)
  (interactive "sgocheck regexp: ")
  (go-check-run-tests-for-regexps query))

(defun go-check-all ()
  "Run all tests in the current directory"
  (interactive)
  (go-check-compile (file-name-directory (buffer-file-name))))

(defun go-check-target-file-for (a-file-name)
  "Return the .go file corresponding to a-file-name's target file."
  (replace-regexp-in-string "_test\\.go$" ".go" a-file-name))

(defun go-check-test-file-for (a-file-name)
  "Return the _test.go file corresponding to a-file-name's test file."
  (replace-regexp-in-string "\\.go$" "_test.go" a-file-name))

(defun go-check-jump-to-file (a-file-name)
  "Jump to buffer visiting a-file-name. This will create a buffer for
   a-file-name if none exists - otherwise, it will jump to the existing buffer."
  (let* ((buf (get-file-buffer a-file-name))
         (window (if buf (get-buffer-window buf))))
    (if (and (window-live-p window) (not (eq window (selected-frame))))
        (select-window window)
      (if buf
          (progn
            (switch-to-buffer buf)
            (set-buffer buf))
        (find-file a-file-name)))))

(defun go-check-buffer-is-test-p ()
  "Check to see if the buffer is a go test buffer"
  (and (buffer-name) (string-match "_test\\.go$" (buffer-name))))

(defun go-check-runner ()
  "Returns the command with which to run the specs"
  "go test")

(defun go-check-runner-with-opts (&optional opts)
  "Create the command line string command to be
   run, adding opts to the call for go-check-runner"
  (let ((opts (if (listp opts)
                 opts
               (list opts))))
    (concat (go-check-runner) " " (mapconcat 'identity opts " "))))

(defun go-check-compile (a-file-or-dir &optional opts)
  "Runs a compile for the specified file or directory with the specified opts"
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "r") `(lambda () (interactive)
                                (go-check-run-from-directory (file-name-directory ,a-file-or-dir)
                                                             (go-check-compile ,a-file-or-dir (quote ,opts)))))
    (global-set-key go-check-key-command-prefix map))

  (go-check-run-from-directory (file-name-directory a-file-or-dir)
                               (let ((compilation-scroll-output t))
                                  (compile (go-check-runner-with-opts opts) 'go-check-compilation-mode))))

(defmacro go-check-run-from-directory (directory body-form)
  "Peform body-form from within the specified directory"
  `(let ((default-directory ,directory))
     ,body-form))

(defvar go-check-compilation-mode-font-lock-keywords
  '((compilation--ensure-parse)
    ("^OK: [0-9]+ passed"
     (0 compilation-info-face))
    ("^OOPS: \\([0-9]+ passed\\), [0-9]+ FAILED"
     (1 compilation-info-face))
    ("^OOPS: [0-9]+ passed, \\([0-9]+ FAILED\\)"
     (1 compilation-error-face))
    ("Compilation exited \\(abnormally\\) with"
     (1 compilation-error-face))
    ("Compilation \\(finished\\) at"
     (1 compilation-info-face))))

(define-derived-mode go-check-compilation-mode compilation-mode "Gocheck Compilation"
  "Compilation mode for Go check output."
  (set (make-local-variable 'compilation-error-regexp-alist)
       (cons 'go-check compilation-error-regexp-alist))
  (set (make-local-variable 'compilation-error-regexp-alist-alist)
       (cons '(go-check "go-check +\\([0-9A-Za-z@_./\:-]+\\.rb\\):\\([0-9]+\\)" 1 2)
             compilation-error-regexp-alist-alist))
  (setq font-lock-defaults '(go-check-compilation-mode-font-lock-keywords t)))

;; Make sure that *_test.go buffers are given the gocheck minor mode by default
;; This will also give .go files the ability to jump to its test file
;;;###autoload
(add-hook 'go-mode-hook (lambda ()
                            (if (go-check-buffer-is-test-p)
                                (go-check-mode)
                              (go-checkable-mode))))

(provide 'go-check-mode)
