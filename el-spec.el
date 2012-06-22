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

(defun el-spec:compose (f g)
  `(lambda () (funcall (function ,g) (function ,f))))

(defmacro el-spec:it (arglist &rest body)
  (declare (indent 1))
  (cond
   ((stringp arglist)
    (setq arglist (list arglist)))
   ((null arglist)
    (setq arglist (list (format "%S" body))))
   ((not (stringp (car arglist)))
    (push (format "%S" body) arglist))
   ((not (consp arglist))
    (error "%S is not string or list or nil" arglist)
    ))
  (destructuring-bind (&optional desc &key vars) arglist
    (lexical-let ((el-spec:full-context el-spec:full-context)
                  (el-spec:descriptions el-spec:descriptions))
      (push
       `(lambda () ,@body)
       el-spec:full-context)
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
  (cond
   ((stringp arglist)
    (setq arglist (list arglist)))
   ((not (consp arglist))
    (error "%S is not string or list" arglist)
    ))
  (destructuring-bind (desc &key vars) arglist
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
            ;; ((symbol-function 'context) (symbol-function 'el-spec:context))
            ((symbol-function 'after) (symbol-function 'el-spec:after))
            ((symbol-function 'before) (symbol-function 'el-spec:before))
            ((symbol-function 'it) (symbol-function 'el-spec:it))
            ((symbol-function 'context) (symbol-function 'el-spec:context))
            )
       (el-spec:context ,arglist
         ,@body
         ))
     )
  )

;; (put 'around 'lisp-indent-function 0)
;; (put 'after 'lisp-indent-function 0)
(put 'it 'lisp-indent-function 1)
;; (put 'before 'lisp-indent-function 0)
(put 'context 'lisp-indent-function 1)

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

;;; useage
;;
;; (describe "a"
;;   (before
;;    (message "a0"))
;;   (context "b"
;;     (before
;;      (message "a1"))
;;     (it "ex1"
;;         (message "ex1")
;;         (should nil)))
;;   (context "c"
;;     (before
;;      (message "a2"))
;;     (it "ex2"
;;         (message "ex2"))
;;     ))

;; copy from el-expectations
(defun el-sepc:current-form-is-describe ()
  (save-excursion
    (beginning-of-defun)
    (looking-at "(describe\\|(.+(fboundp 'describe)\\|(dont-compile\n.*describe")))

(substitute-key-definition 'expectations-eval-defun 'eval-defun emacs-lisp-mode-map)
(substitute-key-definition 'expectations-eval-defun 'eval-defun lisp-interaction-mode-map)

(defadvice eval-defun (around el-spec:eval-defun activate)
  (if (not (and (interactive-p)
                (el-sepc:current-form-is-describe)))
      ad-do-it
    (ert-delete-all-tests)
    ad-do-it
    (ert t)
    ))

(defmacro shared-context (arglist &rest body)
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

(defmacro include-context (desc)
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

(defmacro shared-examples (arglist &rest body)
  (declare (indent 1))
  (cond
   ((stringp arglist)
    (setq arglist (list arglist)))
   ((not (consp arglist))
    (error "%S is not string or list" arglist)
    ))
  (destructuring-bind (desc &key vars) arglist
    `(setq ,(intern (format "el-spec:examples-%s" desc))
           (lambda ()
             ;; (let ((el-spec:full-context nil)
             ;;      (el-spec:descriptions nil)
             ;;      (el-spec:vars nil))
             (el-spec:context ,arglist
               ,@body
               )));; )
    ))

(defmacro include-examples (desc)
  (let ((context
            (intern (format "el-spec:examples-%s" desc))))
    `(funcall ,context)
    ))

;; (setq cmd "=")を忘れたとき

(defun my-ert ()
  (interactive)
  (ert-delete-all-tests)
  (eval-buffer)
  (ert t)
  )

(provide 'el-spec)
;;; el-spec.el ends here
