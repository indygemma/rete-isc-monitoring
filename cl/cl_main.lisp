(require :asdf)
(require :ecl-quicklisp)
(require :uiop)
(require :cffi)

(compile-file "binding-loader.lisp")
(load "binding-loader")

; TODO: explicitly destroy the rete context
(defmacro with-rete (name &body body); {{{
  `(let ((,@name (rete-init)))
     ,@body
     (rete-destroy ,@name))); }}}

; TODO: you could define several rete instances and clear them all in the end -> (with-rete (rete1 rete2) ...)

(load "tests.lisp")

;(defun scratch ()
  ;(rete-destroy (rete-init))

  ;(setf *rete-id* (rete-init))
  ;(rete-destroy *rete-id*)

  ;(make-rule *rete-id* "test" 3 '() (lambda (ras) nil))
  ;(make-rule *rete-id* "test" 3 '((?fred "name" 4.5)) (lambda (ras) nil))
  ;(make-rule *rete-id* "test" 3 '((?x "heartrate" 80)
                                  ;(?x "age"       ?t)
                                  ;(?z "height"    ?d ((=  ?z ?x)
                                                      ;(!= ?d ?t))))
             ;(lambda (ras) nil))

  ;(traverse-list '(1 2 3 (4 5 6)));

  ;(create-wme *rete-id* "freddie" "name" 15.3)

  ;(with-rete (rete)
      ;(make-rule rete "description" 0 '() (lambda (ras) nil))
      ;(format t "activated: ~a~%" (activated-production-nodes rete))
      ;(create-wme rete "fred" "name" "Fred")
      ;(trigger-activated-production-nodes rete)))
