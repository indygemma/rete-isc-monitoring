;; Copyright (c) 2018 Conrad Indiono
;;
;; This program is free software: you can redistribute it and/or modify it under
;; the terms of the GNU General Public License as published by the Free Software
;; Foundation, either version 3 of the License, or (at your option) any later
;; version.
;;
;; This program is distributed in the hope that it will be useful, but WITHOUT ANY
;; WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A
;; PARTICULAR PURPOSE.  See the GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License along with
;; this program (see file COPYING). If not, see <http://www.gnu.org/licenses/>.

;;; -*- mode: lisp ; syntax: ansi-common-lisp -*-
(load "unittest.lisp")

(defmacro with-single-rete-rule-test (name rule-name rule-salience rule-conds rule-handler &body body); {{{
  "We define single rete rule tests with this macro, which accepts the paramets
   for building a single rule"
  `(with-rete (,name)
     (let* ((r1 (make-rule ,name ,rule-name ,rule-salience ,rule-conds ,rule-handler)))
       (check (= (activated-production-nodes ,name) 0))
       ,@body)) ); }}}

(defun simple-rule-handler (ras); {{{
  (format t "This is a sample lisp rule handler~%")); }}}
(deftest test-rete-var-var-var (); {{{
  (with-single-rete-rule-test rete "simple-rule" 0 '((?little_fred ?attr ?value)) #'simple-rule-handler
      (progn
        (create-wme rete "little_fred" "name" "Fred")
        (create-wme rete "little_fred" "age" 15)
        (create-wme rete "little_fred" "group" "WST")
        (check (= (activated-production-nodes rete) 3))))); }}}
(deftest test-rete-id-var-var (); {{{
  (with-single-rete-rule-test rete "simple-rule" 0 '(("little_fred" ?attr ?value)) #'simple-rule-handler
    (progn
      (create-wme rete "little_fred" "name" "Fred")
      (create-wme rete "little_fred" "age" 15)
      (create-wme rete "little_fred" "group" "WST")
      (check (= (activated-production-nodes rete) 3))))); }}}
(deftest test-rete-var-attr-var (); {{{
  (with-single-rete-rule-test rete "simple-rule" 0 '((?little_fred "name" ?value)) #'simple-rule-handler
    (progn
      (create-wme rete "little_fred" "name" "Fred")
      (create-wme rete "little_fred" "age" 15)
      (create-wme rete "little_fred" "group" "WST")
      (check (= (activated-production-nodes rete) 1))))); }}}
(deftest test-rete-var-attr-var-join-tests (); {{{
  (with-single-rete-rule-test rete "simple rule" 0 '((?fred "name"     "Fred")
                                                     (?fred "position" ?fred_position)
                                                     (?joe  "name"     "Joe")
                                                     (?joe  "position" ?joe_position
                                                      ((=  ?joe_position 2)
                                                       (!= ?joe_position ?fred_position))))
                              #'simple-rule-handler
    (progn
      (create-wme rete "Fred" "name"     "Fred")
      (create-wme rete "Fred" "position" 2)
      (create-wme rete "Joe"  "name"     "Joe")
      (create-wme rete "Joe"  "position" 2)
      (check (= (activated-production-nodes rete) 0))))); }}}
(deftest test-rete-var-var-value (); {{{
  (with-single-rete-rule-test rete "simple rule" 0 '((?little_fred ?attr "Fred")) #'simple-rule-handler
    (progn
      (create-wme rete "little_fred" "name" "Fred")
      (create-wme rete "little_fred" "age"  15)
      (create-wme rete "little_fred" "group" "WST")
      (check (= (activated-production-nodes rete) 1))))); }}}
(deftest test-rete-var-attr-value (); {{{
  (with-single-rete-rule-test rete "simple rule" 0 '((?little_fred "name" "Fred")) #'simple-rule-handler
    (progn
      (create-wme rete "little_fred" "name" "Fred")
      (create-wme rete "little_fred" "age"  15)
      (create-wme rete "little_fred" "group" "WST")
      (create-wme rete "big_fred" "name" "Fred")
      (check (= (activated-production-nodes rete) 2))))); }}}
(deftest test-rete-id-attr-var (); {{{
  (with-single-rete-rule-test rete "simple rule" 0 '(("little_fred" "name" ?value)) #'simple-rule-handler
    (progn
      (create-wme rete "little_fred" "name" "Fred")
      (create-wme rete "little_fred" "age"  15)
      (create-wme rete "little_fred" "group" "WST")
      (create-wme rete "big_fred" "name" "Fred")
      (check (= (activated-production-nodes rete) 1))))); }}}
(deftest test-rete-id-var-value (); {{{
  (with-single-rete-rule-test rete "simple rule" 0
                              '((?x "heartrate" 80)
                                (?x "age"       ?t)
                                (?z "height"    ?d
                                 ((= ?z ?x)
                                  (!= ?d ?t))))
                              (lambda (ras)
                                ;(format t "HANDLER CALLED! with ~a~%" ras)
                                ;(format t "var (x): ~a~%" (lookup-var ras '?x :string))
                                ;(format t "var (t): ~a~%" (lookup-var ras '?t :integer))
                                ;(format t "var (height): ~a~%" (lookup-var ras '?d :integer))
                                (let ((x (lookup-var ras '?x :string))
                                      (z (lookup-var ras '?z :string)))
                                  (check (not (null (lookup-var ras '?x :string))))
                                  (check (not (null (lookup-var ras '?z :string))))
                                  (check (equal x "daniel"))
                                  (check (equal z "daniel")))
                                )
    (progn
      (create-wme rete "daniel" "heartrate" 80)
      (create-wme rete "daniel" "age"  25)
      (create-wme rete "daniel" "height" 30)
      (check (= (activated-production-nodes rete) 1))
      (trigger-activated-production-nodes rete)))); }}}
(deftest test-rete-id-attr-value (); {{{
  (with-single-rete-rule-test rete "simple rule" 0 '(("little_fred" "name" "Fred")) #'simple-rule-handler
    (progn
      (create-wme rete "little_fred" "name" "Fred")
      (create-wme rete "little_fred" "age"  15)
      (create-wme rete "little_fred" "group" "WST")
      (create-wme rete "big_fred" "name" "Fred")
      (check (= (activated-production-nodes rete) 1))))); }}}

(deftest run-all-rete-tests ()
  (combine-results
    (test-rete-var-var-var)
    (test-rete-id-var-var)
    (test-rete-var-attr-var)
    (test-rete-var-attr-var-join-tests)
    (test-rete-var-var-value)
    (test-rete-var-attr-value)
    (test-rete-id-attr-var)
    (test-rete-id-var-value)
    (test-rete-id-attr-value)
    ))

(run-all-rete-tests)
