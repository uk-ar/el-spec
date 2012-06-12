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
   `(lambda (example)
      ,@body)
   li)
  nil)

(defmacro before (&rest body)
  `(around
    ,@body
    (funcall example)
    )
  )
(defmacro after (&rest body)
  `(around
    (funcall example)
    ,@body
    )
  )

(defun compose (f g)
  `(lambda () (funcall (function ,g) (function ,f))))

(defmacro it (&rest body)
  (lexical-let ((li li))
    (push
     `(lambda ()
        ,@body)
     li)
    (funcall (reduce #'compose li))
    ))

(defmacro context (desc &rest body)
  (declare (indent 1))
  `(lexical-let ((li li))
     ,@body
     )
  )

(setq li nil)

(context "a"
  (before
   (message "a0"))
  (context "b"
    (before
     (message "a1"))
    (it
     (message "ex1")))
li

;; li
(setq li nil)
(around
 (message "a0")
 (funcall example)
 (message "b0")
 )
(around
 (message "a1")
 (funcall example)
 (message "b1")
 )
(before
 (message "a2")
 )
(before
 (message "a3")
 )
(it
 (message "ex")
 )
(it
 (message "ex2")
 )