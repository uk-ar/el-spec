;; Usage:
;;
;;   emacs -Q -l tests/run-test.el           # interactive mode
;;   emacs -batch -Q -l tests/run-test.el    # batch mode


;; Utils
(defun el-spec-test-join-path (path &rest rest)
  "Join a list of PATHS with appropriate separator (such as /).

\(fn &rest paths)"
  (if rest
      (concat (file-name-as-directory path) (apply 'el-spec-test-join-path rest))
    path))

(defvar el-spec-test-dir (file-name-directory load-file-name))
(defvar el-spec-root-dir (concat el-spec-test-dir ".."))


;; Setup `load-path'
(mapc (lambda (p) (add-to-list 'load-path p))
      (list el-spec-test-dir
            el-spec-root-dir))


;; Use ERT from github when this Emacs does not have it
(unless (locate-library "ert")
  (add-to-list
   'load-path
   (el-spec-test-join-path el-spec-root-dir "lib" "ert" "lisp" "emacs-lisp")))


;; load all files that match -test.el except emacs temp filezzz
(dolist (file (directory-files el-spec-test-dir
                               t
                               "^[^\.#].*-test.el"))
  (load file))

;; Run tests
(defun el-spec:run-tests ()
  (if noninteractive
      (ert-run-tests-batch-and-exit)
    (ert t)))
