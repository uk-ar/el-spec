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

(defmacro around (&rest body)
  (push
   `(lambda (el-spec:example)
      ,@body)
   el-spec:full-context)
  nil)

(defmacro before (&rest body)
  `(around
    ,@body
    )
  )
    (funcall el-spec:example)
(defmacro after (&rest body)
  `(around
    (funcall el-spec:example)
    ,@body
    )
  )

(defun el-spec:compose (f g)
  `(lambda () (funcall (function ,g) (function ,f))))

(defmacro it (desc &rest body)
  (unless (stringp desc)
    (error "%S is not string" desc))
  (lexical-let ((el-spec:full-context el-spec:full-context)
                (el-spec:descriptions el-spec:descriptions))
    (push
     `(lambda ()
        ,@body)
     el-spec:full-context)
    (push desc el-spec:descriptions)
    `(ert-deftest ,(intern (apply 'concat (reverse el-spec:descriptions))) ()
       (funcall ,(reduce #'el-spec:compose
                         el-spec:full-context)))
    ))

(defmacro context (desc &rest body)
  (declare (indent 1))
  (unless (stringp desc)
    (error "%S is not string" desc))
  `(let ((el-spec:full-context
          (if (boundp 'el-spec:full-context) el-spec:full-context nil))
         (el-spec:descriptions
          (if (boundp 'el-spec:descriptions) el-spec:descriptions nil)))
     (push ,desc el-spec:descriptions)
     (push " " el-spec:descriptions)
     ,@body
     ))

(defmacro describe (desc &rest body)
  (declare (indent 1))
  (unless (stringp desc)
    (error "%S is not string" desc))
  `(let ((el-spec:full-context nil)
         (el-spec:descriptions nil))
     (push ,desc el-spec:descriptions)
     (push " " el-spec:descriptions)
     ,@body
     ))

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

(setq el-spec:full-context nil)
(setq el-spec:descriptions nil)
;;; self-test
(ert-deftest describe-initial-value ()
  (should (eq el-spec:full-context nil))
  (should (equal el-spec:descriptions nil))
  (describe "a"
    (should (eq el-spec:full-context nil))
    (should (equal el-spec:descriptions '(" " "a")))
    )
  )
(ert-deftest describe-before ()
  (describe "a"
    (should (eq el-spec:full-context nil))
    (should (equal el-spec:descriptions '(" " "a")))
    (before "b")
    (should (equal el-spec:full-context
                   '((lambda (el-spec:example)
                       "b"
                       (funcall el-spec:example)))))
    (should (equal el-spec:descriptions '(" " "a")))
    )
  )

(ert-deftest describe-after ()
  (describe "a"
    (should (eq el-spec:full-context nil))
    (should (equal el-spec:descriptions '(" " "a")))
    (after "b")
    (should (equal el-spec:full-context
                   '((lambda (el-spec:example)
                       (funcall el-spec:example) "b"))))
    (should (equal el-spec:descriptions '(" " "a")))
    )
  )

