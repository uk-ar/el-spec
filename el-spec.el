;;; el-spec.el --- ruby's rspec like syntax test frame work

;;-------------------------------------------------------------------
;;
;; Copyright (C) 2012 Yuuki Arisawa
;;
;; This file is NOT part of Emacs.
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2 of
;; the License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be
;; useful, but WITHOUT ANY WARRANTY; without even the implied
;; warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
;; PURPOSE.  See the GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public
;; License along with this program; if not, write to the Free
;; Software Foundation, Inc., 59 Temple Place, Suite 330, Boston,
;; MA 02111-1307 USA
;;
;;-------------------------------------------------------------------

;; Author: Yuuki Arisawa <yuuki.ari@gmail.com>
;; URL: https://github.com/uk-ar/el-spec
;; Created: 4 July 2012
;; Version: 0.1
;; Keywords: test

;;; Commentary:

;; ########   Compatibility   ########################################
;;
;; Works with Emacs-23.2.1, 23.1.1

;; ########   Quick start   ########################################
;;
;; Add to your ~/.emacs
;;
;; (require 'el-spec)
;;
;; and write some test, for example
;;
;; (dont-compile
;;   (when (fboundp 'describe)
;;     (describe "description"
;;       (before
;;         (message "before common"))
;;       (after
;;         (message "after common\n"))
;;       (context "when 1"
;;         (before
;;           (message "before 1"))
;;         (after
;;           (message "after 1"))
;;         (it "test 1"
;;           (message "test 1")))
;;       (context "when 2"
;;         (before
;;           (message "before 2"))
;;         (after
;;           (message "after 2"))
;;         (it "test 2"
;;           (message "test 2")))
;;       )))
;;
;; output is like this.
;;
;; before common
;; before 1
;; test 1
;; after 1
;; after common
;;
;; before common
;; before 2
;; test 2
;; after 2
;; after common
;;
;;; History:

;; Revision 0.1 2012/07/05 00:55:38
;; * First release
;;
;;; Bug:
;; (shared-examples ("examples for quote" :vars ((quote)))

(require 'ert)
(require 'cl)

(defmacro el-spec:around (&rest body)
  (push
   `(lambda (el-spec:example)
      ,@body)
   el-spec:full-context)
  nil)

(defmacro el-spec:before (&rest body)
  `(el-spec:around
    ,@body
    (funcall el-spec:example)
    ))

(defmacro el-spec:after (&rest body)
  ;; don't use around macro because of `el-spec:example' variable binding
  ;; use `el-spec:example' for test ability
  ;; (let ((example-sym (make-symbol "example")))
  ;;   (push
  ;;    `(lambda (,example-sym)
  ;;       (funcall ,example-sym)
  ;;       ,@body)
  ;;    el-spec:full-context))
  ;; nil)
  `(el-spec:around
    (funcall el-spec:example)
    ,@body
    ))

(defun el-spec:prepare-arglist (arglist)
  (declare (indent 1))
  (let ((arglist arglist))
    (typecase arglist
      (string (list arglist))
      (null (list nil))
      (cons
       (typecase (car-safe arglist)
         (string arglist)
         (null arglist)
         (keyword (push nil arglist))
         (t
          (error "%S is not string or list or nil" arglist))))
      (t
       (error "%S is not string or list or nil" arglist)))
    ))

(defun el-spec:compose (f g)
  `(lambda () (funcall (function ,g) (function ,f))))

(defmacro el-spec:it (arglist &rest body)
  (declare (indent 1))
  (destructuring-bind (&optional desc &key vars) (el-spec:prepare-arglist arglist)
    (lexical-let ((el-spec:full-context el-spec:full-context)
                  (el-spec:descriptions el-spec:descriptions))
      (push
       `(lambda () ,@body)
       el-spec:full-context)
      (when vars
        (push (format "%S" vars) el-spec:descriptions)
        (push el-spec:separator el-spec:descriptions)
        )
      (push (or desc (list (format "%S" body))) el-spec:descriptions)
      (let ((test-symbol (intern
                          (apply 'concat (reverse el-spec:descriptions)))))
        (when (ert-test-boundp test-symbol)
          (warn "test function \"%s\" already exist" test-symbol))
        `(el-spec:let ,vars
           (lexical-let ,(mapcar (lambda (var)
                                   `(,var ,var)) el-spec:vars)
             (ert-deftest ,test-symbol ()
               (funcall ,(reduce #'el-spec:compose
                                 el-spec:full-context))
               )
             )
           )
        ))))

(defconst el-spec:separator "\n")

(defmacro el-spec:context (arglist &rest body)
  (declare (indent 1))
  ;; typecase
  (destructuring-bind (desc &key vars) (el-spec:prepare-arglist arglist)
    (when (null desc)
      (error "%S is not has string description" arglist))
    `(let ((el-spec:full-context
            (if (boundp 'el-spec:full-context) el-spec:full-context nil))
           (el-spec:descriptions
            (if (boundp 'el-spec:descriptions) el-spec:descriptions nil))
           (el-spec:vars
            (if (boundp 'el-spec:descriptions) el-spec:vars nil)))
       (push ,desc el-spec:descriptions)
       (push el-spec:separator el-spec:descriptions)
       ;; fix?
       (el-spec:let ,vars
         ,@body
         )
       )
    ))

(defmacro describe (arglist &rest body)
  (declare (indent 1))
  ;; for failed test
  (makunbound 'el-spec:full-context)
  (makunbound 'el-spec:descriptions)
  (makunbound 'el-spec:vars)
  `(let ((el-spec:full-context nil)
         (el-spec:descriptions nil)
         (el-spec:vars nil))
     ;; macrolet
     (letf (((symbol-function 'around) (symbol-function 'el-spec:around))
            ((symbol-function 'after) (symbol-function 'el-spec:after))
            ((symbol-function 'before) (symbol-function 'el-spec:before))
            ((symbol-function 'it) (symbol-function 'el-spec:it))
            ((symbol-function 'context) (symbol-function 'el-spec:context))
            ((symbol-function 'shared-context)
             (symbol-function 'el-spec:shared-context))
            ((symbol-function 'include-context)
             (symbol-function 'el-spec:include-context))
            ((symbol-function 'shared-examples)
             (symbol-function 'el-spec:shared-examples))
            ((symbol-function 'include-examples)
             (symbol-function 'el-spec:include-examples))
            )
       (el-spec:context ,arglist
         ,@body
         ))
     )
  )

(put 'it 'lisp-indent-function 1)
(put 'context 'lisp-indent-function 1)
(put 'before 'lisp-indent-function 0)
(put 'around 'lisp-indent-function 0)
(put 'after 'lisp-indent-function 0)
(put 'shared-context 'lisp-indent-function 1)
(put 'shared-examples 'lisp-indent-function 1)

(defmacro el-spec:let (varlist &rest body)
  (declare (indent 1))
  (mapcar (lambda (element)
            (add-to-list
             'el-spec:vars (if (consp element) (car element) element)))
          varlist)
  `(let ,varlist
     ,@body
     )
  )

;; copy from el-expectations
(defun el-sepc:current-form-is-describe ()
  (save-excursion
    (beginning-of-defun)
    (looking-at "(describe\\|(.+(fboundp 'describe)\\|(dont-compile\n.*describe")))

(substitute-key-definition 'expectations-eval-defun 'eval-defun emacs-lisp-mode-map)
(substitute-key-definition 'expectations-eval-defun 'eval-defun lisp-interaction-mode-map)

(defvar el-spec:selection 'all
  ;; all or context
  )

(defun el-spec:toggle-selection ()
  (interactive)
  (cond
   ((eq el-spec:selection 'all)
    (setq el-spec:selection 'context)
    (message "selection:context")
    )
   ((eq el-spec:selection 'context)
    (setq el-spec:selection 'all)
    (message "selection:all"))
   (t (warn "el-spec:selection is invalid"))))

(defadvice ert (around el-spec:ert-advice activate)
  (if (and (fboundp 'popwin:popup-buffer-tail)
           (not (interactive-p)))
      (let ((special-display-function 'popwin:popup-buffer-tail))
        ad-do-it
        (popwin:popup-buffer-tail "*ert*"))
    ad-do-it))

(defadvice eval-defun (around el-spec:eval-defun-advice activate)
  (if (not (and (interactive-p)
                (el-sepc:current-form-is-describe)))
      ad-do-it
    (ert-delete-all-tests)
    ad-do-it
    (cond
     ((eq el-spec:selection 'all)
      (ert t))
     ((eq el-spec:selection 'context)
      (el-spec:execute-context))
     (t (warn "el-spec:selection is invalid")))
    ))

(defun el-spec:eval-and-execute-all ()
  (interactive)
  (let ((el-spec:selection 'all))
    (call-interactively 'eval-defun)
    ))

(defun el-spec:eval-and-execute-context ()
  (interactive)
  (let ((el-spec:selection 'context))
    (call-interactively 'eval-defun)
    ))

(defun el-spec:execute-context ()
  (save-excursion
    (if (not (save-excursion
               (backward-sexp)
               (looking-at "(context\\|(describe")))
        (el-spec:execute-context-1)
      (backward-sexp)
      (down-list)
      (el-spec:execute-context-1)
      )))

(defun el-spec:backward-up-list ()
  (condition-case err
      (progn
        (if (or (nth 3 (syntax-ppss));string
                (nth 4 (syntax-ppss)));comment
            (goto-char (nth 8 (syntax-ppss))))
        (backward-up-list))
    (scan-error
     (message "top level")
     ;;top level
     )))

(defun el-spec:execute-context-1 ()
  (save-excursion
    (let ((start-point
           (save-excursion (beginning-of-defun) (point)))
          (test-name nil)
          ;; for backward-up-list
          ;;(parse-sexp-ignore-comments t)
          )
      (while (not (eq (point) start-point))
        (el-spec:backward-up-list)
        (when
            (looking-at "(context\\|(describe")
          (save-excursion
            (forward-word)
            (forward-word)
            (setq test-name
                  (concat
                   (car (split-string-and-unquote (thing-at-point 'string) "\""))
                   "\n" test-name))
            )
          )
        )
      (ert test-name)
      )))


(defmacro el-spec:shared-context (arglist &rest body)
  (declare (indent 1))
  `(let ((el-spec:full-context nil)
         (el-spec:descriptions nil)
         (el-spec:vars nil))
     (el-spec:context ,arglist
       ,@body
       ;; (message "des:%S" (car (last el-spec:descriptions)))
       (set (intern (format "el-spec:context-%s"
                            (car (last el-spec:descriptions))))
            (list el-spec:full-context
                  el-spec:descriptions
                  el-spec:vars))
       )))

(defmacro el-spec:include-context (desc)
  ;; macro for set el-spec:full-context
  (let ((context (intern (format "el-spec:context-%s" desc))))
    `(progn
          (setq el-spec:full-context (append (car ,context)
                                        el-spec:full-context))
     ;; (setq el-spec:descriptions (append (nth 1 (symbol-value context))
     ;;                                    el-spec:descriptions))
          (setq el-spec:vars (append (nth 2 ,context)
                                     el-spec:vars))
       )
     ))

(defmacro el-spec:shared-examples (arglist &rest body)
  (declare (indent 1))
  (destructuring-bind (desc &key vars) (el-spec:prepare-arglist arglist)
    (when (null desc)
      (error "%S is not has string description" arglist))
    `(setq ,(intern (format "el-spec:examples-%s" desc))
           (lambda ()
             ;; (let ((el-spec:full-context nil)
             ;;      (el-spec:descriptions nil)
             ;;      (el-spec:vars nil))
             (el-spec:context ,arglist
               ,@body
               )));; )
    ))

(defmacro el-spec:include-examples (arglist)
  (cond
   ((stringp arglist)
    (setq arglist (list arglist)))
   ((not (consp arglist))
    (error "%S is not string or list" arglist)
    ))
  (destructuring-bind (desc &key vars) arglist
    (let ((context
              (intern (format "el-spec:examples-%s" desc))))
      `(el-spec:let ,vars
         (funcall ,context)
         )
      )))

;; (setq cmd "=")を忘れたとき

(defun el-spec:eval-buffer ()
  (interactive)
  (ert-delete-all-tests)
  (eval-buffer)
  (ert t)
  )

;;print test name
(defun ert-insert-test-name-button (test-name)
  "Insert a button that links to TEST-NAME."
  (insert-text-button (format "%s" test-name)
                      :type 'ert--test-name-button
                      'ert-test-name test-name))

(provide 'el-spec)
;;; el-spec.el ends here
