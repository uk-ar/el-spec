(require 'el-spec)


(ert-deftest el-spec:test-nested-describes-should-create-tests ()
  (let ((num-calls 0))
    
    (flet ((inc-calls () (incf num-calls)))
      (describe "outer"
        (around
          (inc-calls)
          (funcall el-spec:example))
        
        (it "outer-it"
          'nothing-here)
        
          (describe "inner"
            (it "inner-it"
              'nothing-here-either)
            
            (describe "intestines"
              (it "intestines-it"
                'nothing-here-again))))
        
      (dolist (tst tests)
        (ert-run-test (ert-get-test tst)))
      
      (should (ert-test-boundp (intern "outer\nouter-it")))
      (should (ert-test-boundp (intern "outer\ninner\ninner-it")))
      
      (should (= num-calls 2)))))

(ert-deftest el-spec:contexts-should-also-do-what-we-want  ()
  (describe "wrap"
    (context "outer context"
      (it "x")
      (context "inner context"
        (it "y")))
    
    (context "ab"
      (it "aba")))
  
  (should (ert-test-boundp (intern "wrap\nouter context\nx")))
  (should (ert-test-boundp (intern "wrap\nouter context\minner context\ny")))
  )
