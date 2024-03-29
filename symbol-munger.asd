;; -*- lisp -*-

(defsystem "symbol-munger"
  :description "Functions to convert between the spacing and
  capitalization conventions of various environments"
  :license "MIT"
  :version "1.0"
  :components ((:file "symbol-munger"))
  :depends-on ("iterate" "alexandria")
  :in-order-to ((test-op (load-op "symbol-munger/test")))
  :perform (test-op (op c)
             (let ((*package* (find-package :symbol-munger-test)))
               (eval (read-from-string "
                 (run-tests :package :symbol-munger-test
                   :name :symbol-munger
                   :run-contexts #'with-summary-context)")))))

(defsystem "symbol-munger/test"
  :description "Tests for Functions to convert between the spacing and
  capitalization conventions of various environments"
  :license "MIT"
  :version "1.0"
  :components ((:module :tests
                :serial t
                :components ((:file "symbol-munger"))))
  :depends-on ("symbol-munger" "lisp-unit2"))

;; Copyright (c) 2011 Russ Tyndall , Acceleration.net http://www.acceleration.net

;; Permission is hereby granted, free of charge, to any person obtaining
;; a copy of this software and associated documentation files (the
;; "Software"), to deal in the Software without restriction, including
;; without limitation the rights to use, copy, modify, merge, publish,
;; distribute, sublicense, and/or sell copies of the Software, and to
;; permit persons to whom the Software is furnished to do so, subject to
;; the following conditions:

;; The above copyright notice and this permission notice shall be
;; included in all copies or substantial portions of the Software.

;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
;; MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
;; NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE
;; LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION
;; OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION
;; WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
