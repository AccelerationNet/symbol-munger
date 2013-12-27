(defpackage :symbol-munger-test
  (:use :cl :cl-user :symbol-munger :lisp-unit2))

(in-package :symbol-munger-test)

(define-test test-basic ()
  (let ((it "tHis Is My teSt Phrase"))
    (assert-equal "This Is My Test Phrase"
		  (normalize-capitalization-and-spacing it))
    (assert-equal "this-is-my-test-phrase" (english->lisp-name it))
    (assert-equal :this-is-my-test-phrase (english->lisp-symbol it :keyword))
    (assert-equal :this-is-my-test-phrase (english->keyword it))
    (assert-equal "This Is My Test Phrase" (lisp->english (english->keyword it)))
    (assert-equal "thisIsATest" (english->camel-case "This Is A Test"))
    (assert-equal "thisIsATest" (english->camel-case "This is a test"))
    (assert-equal "ThisIsATest" (english->studly-case "This is a test"))
    (assert-equal "this_is_a_test" (english->underscores "This is a test"))

    (assert-equal "This Is A Test" (camel-case->english "thisIsATest"))
    (assert-equal "This is a test" (camel-case->english "thisIsATest" :capitalize :first-word))
    (assert-equal "this-is-a-test" (camel-case->lisp-name "thisIsATest"))
    (assert-equal :this-is-a-test (camel-case->lisp-symbol "thisIsATest" :keyword))
    (assert-equal "thisIsATest"
		  (lisp->camel-case
		   (camel-case->keyword "thisIsATest")))
    (assert-equal "thisIsATest"
		  (lisp->camel-case
		   (camel-case->keyword
		    (lisp->camel-case
		     (camel-case->keyword "thisIsATest")))))
    (assert-equal :this-is-a-test (camel-case->keyword "thisIsATest"))
    (assert-equal "this_is_a_test" (camel-case->underscores "thisIsATest"))
    (assert-equal "this_is_a_test" (camel-case->underscores "ThisIsATest"))
    (assert-equal "THIS_IS_A_TEST" (camel-case->underscores "ThisIsATest" :capitalize T))
    (assert-equal "This_Is_A_Test" (camel-case->underscores "ThisIsATest" :capitalize :each-word))

    (assert-equal "This Is A Test" (underscores->english "this_is_a_test"))
    (assert-equal "This is a test" (underscores->english "this_is_a_test" :capitalize :first-word))
    (assert-equal "this-is-a-test" (underscores->lisp-name "this_is_a_test"))
    (assert-equal :this-is-a-test (underscores->lisp-symbol "this_is_a_test" :keyword))
    (assert-equal :this-is-a-test (underscores->keyword "this_is_a_test"))
    (assert-equal "thisIsATest" (underscores->camel-case "this_is_a_test"))
    (assert-equal "ThisIsATest" (underscores->studly-caps "this_is_a_test"))
    
    (assert-equal "this_is_a_test" (lisp->underscores :this-is-a-test))
    (assert-equal "This Is A Test" (lisp->english :this-is-a-test))
    (assert-equal :this-is-a-test (lisp->keyword 'this-is-a-test))
    (assert-equal "thisIsATest" (lisp->camel-case :this-is-a-test))
    (assert-equal "ThisIsATest" (lisp->studly-caps :this-is-a-test))
    
    ))

(define-test test-list-type-args ()
  (assert-equal :this-is-my-test-phrase
                (symbol-munger:lisp->keyword (list 'this-is "my-test" 'phrase)))
  (assert-equal "Test This Thing Being Tested"
                (symbol-munger:lisp->english (list "test" "this-thing-being" "tested")))
  (assert-equal :this-is-a-multispace-test-string
   (english->keyword "this  is  a multispace   test string"))
  (assert-equal :this-is-a-test-of-things
   (symbol-munger:lisp->keyword (list "this" (list "is" "a" "Test") "of" 'things)))
  (assert-equal :this-is-a-test-of-things-2
   (symbol-munger:lisp->keyword (list "this" (list "is" "a" "Test") "of" 'things 2))))
