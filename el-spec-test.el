;;; el-spec-test.el --- test of el-spec.el

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

 ;;; self-test
(ert-deftest el-spec:test-describe-initial-value ()
  (should (not (boundp 'el-spec:full-context)))
  (should (not (boundp 'el-spec:descriptions)))
  (describe "describe"
    (should (eq el-spec:full-context nil))
    (should (equal el-spec:descriptions '("\n" "describe")))
    )
  (should (not (boundp 'el-spec:full-context)))
  (should (not (boundp 'el-spec:descriptions)))
  )

(ert-deftest el-spec:test-describe-it ()
  (describe "describe"
    (should (eq el-spec:full-context nil))
    (should (equal el-spec:descriptions '("\n" "describe")))
    (should (equal (ert-test-boundp (intern "describe\nexample")) nil))

    (it "it" (message "example"))
    (should (eq el-spec:full-context nil))
    (should (equal el-spec:descriptions '("\n" "describe")))
    (should (equal (ert-test-boundp (intern "describe\nit")) t))

    (let ((result (ert-run-test
                   (ert-get-test (intern "describe\nit")))))
      (should (ert-test-passed-p result))
      (should (equal (ert-test-result-messages result)
                     "example\n")))
    )
  )

(ert-deftest el-spec:test-describe-before ()
  (describe "describe"
    (should (eq el-spec:full-context nil))
    (should (equal el-spec:descriptions '("\n" "describe")))

    (before (message "before"))
    (should (equal el-spec:full-context
                   '((lambda (el-spec:example)
                       (message "before")
                       (funcall el-spec:example)))))
    (should (equal el-spec:descriptions '("\n" "describe")))
    )
  )

(ert-deftest el-spec:test-describe-after ()
  (describe "describe"
    (should (eq el-spec:full-context nil))
    (should (equal el-spec:descriptions '("\n" "describe")))

    (after (message "after"))
    (should (equal el-spec:full-context
                   '((lambda (el-spec:example)
                       (funcall el-spec:example)
                       (message "after")))))
    (should (equal el-spec:descriptions '("\n" "describe")))
    )
  )

(ert-deftest el-spec:test-describe-around ()
  (describe "describe"
    (should (eq el-spec:full-context nil))
    (should (equal el-spec:descriptions '("\n" "describe")))

    (around
     (message "around1")
     (funcall el-spec:example)
     (message "around2")
     )
    (should (equal el-spec:full-context
                   '((lambda (el-spec:example)
                       (message "around1")
                       (funcall el-spec:example)
                       (message "around2")))))
    (should (equal el-spec:descriptions '("\n" "describe")))
    )
  )

(ert-deftest el-spec:test-nested-before ()
  (let ((ex1 (intern "nested before\ncontext1\nit1"))
        (ex2 (intern "nested before\ncontext2\nit2"))
        )
    (describe "nested before"
      (before
       (message "before0"))
      (context "context1"
        (before
         (message "before1"))
        (it "it1"
          (message "example1")))
      (context "context2"
        (before
         (message "before2"))
        (it "it2"
          (message "example2")
          (should nil))))

    (let ((result (ert-run-test
                   (ert-get-test ex1))))
      (should (ert-test-passed-p result))
      (should (equal (ert-test-result-messages result)
                     "\
before0
before1
example1
")))

    (let ((result (ert-run-test
                   (ert-get-test ex2))))
      (should (ert-test-failed-p result))
      (should (equal (ert-test-result-messages result)
                     "\
before0
before2
example2
")))
    ))

(ert-deftest el-spec:test-nested-after ()
  (let ((ex1 (intern "nested after\ncontext1\nit1"))
        (ex2 (intern "nested after\ncontext2\nit2"))
        )
    (should (equal (ert-test-boundp ex1) nil))
    (should (equal (ert-test-boundp ex2) nil))
    (describe "nested after"
      (after
       (message "after0"))
      (context "context1"
        (after
         (message "after1"))
        (it "it1"
          (message "example1")))
      (context "context2"
        (after
         (message "after2"))
        (it "it2"
          (message "example2")
          )))

    (let ((result (ert-run-test
                   (ert-get-test ex1))))
      (should (ert-test-passed-p result))
      (should (equal (ert-test-result-messages result)
                     "\
example1
after1
after0
")))

    (let ((result (ert-run-test
                   (ert-get-test ex2))))
      (should (ert-test-passed-p result))
      (should (equal (ert-test-result-messages result)
                     "\
example2
after2
after0
")))
    ))

(ert-deftest el-spec:test-nested-around ()
  (let ((ex1 (intern "nested around\ncontext1\nit1"))
        (ex2 (intern "nested around\ncontext2\nit2"))
        )
    (should (equal (ert-test-boundp ex1) nil))
    (should (equal (ert-test-boundp ex2) nil))

    (describe "nested around"
      (around
       (message "around01")
       (funcall el-spec:example)
       (message "around02")
       )
      (context "context1"
        (around
         (message "around11")
         (funcall el-spec:example)
         (message "around12")
         )
        (it "it1"
          (message "example1")))
      (context "context2"
        (around
         (message "around21")
         (funcall el-spec:example)
         (message "around22")
         )
        (it "it2"
          (message "example2")
          )))

    (let ((result (ert-run-test
                   (ert-get-test ex1))))
      (should (ert-test-passed-p result))
      (should (equal (ert-test-result-messages result)
                     "\
around01
around11
example1
around12
around02
")))

    (let ((result (ert-run-test
                   (ert-get-test ex2))))
      (should (ert-test-passed-p result))
      (should (equal (ert-test-result-messages result)
                     "\
around01
around21
example2
around22
around02
")))
    ))

(ert-deftest el-spec:test-nested-mix ()
  (let ((ex1 (intern "nested mix\ncontext1\nit1"))
        (ex2 (intern "nested mix\ncontext2\nit2"))
        )
    (should (equal (ert-test-boundp ex1) nil))
    (should (equal (ert-test-boundp ex2) nil))
    (describe "nested mix"
      (around
       (message "around1")
       (funcall el-spec:example)
       (message "around2")
       )
      (context "context1"
        (before (message "before"))
        (it "it1"
          (message "example1")))
      (context "context2"
        (after (message "after"))
        (it "it2"
          (message "example2")
          )))

    (let ((result (ert-run-test
                   (ert-get-test ex1))))
      (should (ert-test-passed-p result))
      (should (equal (ert-test-result-messages result)
                     "\
around1
before
example1
around2
")))

    (let ((result (ert-run-test
                   (ert-get-test ex2))))
      (should (ert-test-passed-p result))
      (should (equal (ert-test-result-messages result)
                     "\
around1
example2
after
around2
")))
    ))

(ert-deftest el-spec:test-shared-context ()
  (let ((ex1 (intern "shared context\ncontext1\nit1"))
        (ex2 (intern "shared context\ncontext2\nit2"))
        )
    (should (equal (ert-test-boundp ex1) nil))
    (should (equal (ert-test-boundp ex2) nil))
    (describe "shared context"
      (shared-context "context0"
        (before
         (message "before01")
         )
        (after
         (message "after01")
         )
        )

      (context "context1"
        (include-context "context0")

        (around
         (message "around11")
         (funcall el-spec:example)
         (message "around12")
         )
        (it "it1"
          (message "example1")))

      (context "context2"
        (include-context "context0")

        (around
         (message "around21")
         (funcall el-spec:example)
         (message "around22")
         )
        (it "it2"
          (message "example2")
          )))

    (let ((result (ert-run-test
                   (ert-get-test ex1))))
      (should (ert-test-passed-p result))
      (should (equal (ert-test-result-messages result)
                     "\
before01
around11
example1
around12
after01
")))
    (let ((result (ert-run-test
                   (ert-get-test ex2))))
      (should (ert-test-passed-p result))
      (should (equal (ert-test-result-messages result)
                     "\
before01
around21
example2
around22
after01
")))
    ))

(ert-deftest el-spec:test-nested-shared-context ()
  (let ((ex1 (intern "nested shared context\ncontext1\nit1"))
        (ex2 (intern "nested shared context\ncontext2\nit2"))
        )
    (should (equal (ert-test-boundp ex1) nil))
    (should (equal (ert-test-boundp ex2) nil))

    (describe "nested shared context"
      (shared-context "context00"
        (before
         (message "before001"))
        (after
         (message "after001")))
      (shared-context "context0"
        (before
         (message "before01"))
        (after
         (message "after01"))

        (include-context "context00")

        (before
         (message "before02"))
        (after
         (message "after02"))
        )

      (context "context1"
        (include-context "context0")

        (around
         (message "around11")
         (funcall el-spec:example)
         (message "around12")
         )
        (it "it1"
          (message "example1")))

      (context "context2"
        (include-context "context0")

        (around
         (message "around21")
         (funcall el-spec:example)
         (message "around22")
         )
        (it "it2"
          (message "example2")
          )))

    (let ((result (ert-run-test
                   (ert-get-test ex1))))
      (should (ert-test-passed-p result))
      (should (equal (ert-test-result-messages result)
                     "\
before01
before001
before02
around11
example1
around12
after02
after001
after01
")))
    (let ((result (ert-run-test
                   (ert-get-test ex2))))
      (should (ert-test-passed-p result))
      (should (equal (ert-test-result-messages result)
                     "\
before01
before001
before02
around21
example2
around22
after02
after001
after01
")))
    ))

(ert-deftest el-spec:test-shared-examples ()
  (let ((ex1 (intern "shared examples\ncontext1\nexamples0\nit1"))
        (ex2 (intern "shared examples\ncontext1\nexamples0\nit2"))
        (ex3 (intern "shared examples\ncontext2\nexamples0\nit1"))
        (ex4 (intern "shared examples\ncontext2\nexamples0\nit2"))
        )
    (should (equal (ert-test-boundp ex1) nil))
    (should (equal (ert-test-boundp ex2) nil))
    (should (equal (ert-test-boundp ex3) nil))
    (should (equal (ert-test-boundp ex4) nil))

    (describe "shared examples"
      (shared-examples "examples0"
        (it "it1"
          (message "example1"))
        (it "it2"
          (message "example2"))
        )

      (context "context1"
        (around
         (message "around11")
         (funcall el-spec:example)
         (message "around12")
         )
        (include-examples "examples0"))

      (context "context2"
        (around
         (message "around21")
         (funcall el-spec:example)
         (message "around22")
         )
        (include-examples "examples0")
        ))

    (let ((result (ert-run-test
                   (ert-get-test
                    ex1))))
      (should (ert-test-passed-p result))
      (should (equal (ert-test-result-messages result)
                     "\
around11
example1
around12
")))
    (let ((result (ert-run-test
                   (ert-get-test
                    ex2))))
      (should (ert-test-passed-p result))
      (should (equal (ert-test-result-messages result)
                     "\
around11
example2
around12
")))
    (let ((result (ert-run-test
                   (ert-get-test
                    ex3))))
      (should (ert-test-passed-p result))
      (should (equal (ert-test-result-messages result)
                     "\
around21
example1
around22
")))
    (let ((result (ert-run-test
                   (ert-get-test
                    ex4))))
      (should (ert-test-passed-p result))
      (should (equal (ert-test-result-messages result)
                     "\
around21
example2
around22
")))
    )
  )

