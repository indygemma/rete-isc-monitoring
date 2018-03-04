(require :asdf)
(require :ecl-quicklisp)
(require :uiop)
(require :cffi)
(require :local-time)

(compile-file "binding-loader.lisp")
(load "binding-loader")

; TODO: explicitly destroy the rete context
(defmacro with-rete (name &body body); {{{
  `(let ((,@name (rete-init)))
     ,@body
     (rete-destroy ,@name))); }}}

; TODO: you could define several rete instances and clear them all in the end -> (with-rete (rete1 rete2) ...)

;(load "tests.lisp")

;(defun scratch ()
  ;(rete-destroy (rete-init))

  ;(setf *rete-id* (rete-init))
  ;(rete-destroy *rete-id*)
  
  ;(to-json *rete-id*)
  ;(to-json-file *rete-id* "test-cl.json")

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

(with-rete (rete)
  (let (
        ;; we have one rule that is responsible for assigning the instance start time
        (r0 (make-rule rete "Assign process instance start time" 0
                       '((?id "type" "start")
                         (?id "timestamp" ?timestamp))
                       (lambda (ras)
                         (format t "adding instance-start-time for ~a~%" (lookup-var ras '?id :string))
                         (modify-wme rete
                                     (lookup-var ras '?id :string)
                                     "instance_start_time"
                                     (lookup-var ras '?timestamp :integer)))))
        (r1 (make-rule rete "original ISC - send alert threshold exceeded" 0
                       `((?id "type" "read-out meter end")
                         (?id "timestamp" ?timestamp
                           ((>= ?timestamp ,(local-time:timestamp-to-unix (local-time:parse-timestring "2018-01-01T00:00:00.000000")))
                            (<= ?timestamp ,(local-time:timestamp-to-unix (local-time:parse-timestring "2018-01-01T06:00:00.000000")))))
                         (?id "readout" ?readout_value)
                         (?id "instance_start_time" ?start_time)
                         ("vars" "accumulated_values" ?acc_values))
                       (lambda (ras)
                         (let* ((acc-values (lookup-var ras '?acc_values :integer))
                                (value      (lookup-var ras '?readout_value :integer))
                                (THRESHOLD  10000)
                                (acc-values2 (+ acc-values value)))
                           (format t "instance id: ~a accumulated value: ~a, next value: ~a~%"
                                   (lookup-var ras '?id :string)
                                   (lookup-var ras '?acc_values :integer)
                                   value)
                           (if (> acc-values2 THRESHOLD)
                             (format t "ALERT: ~a Readout Threshold exceeded (x): ~a. (Start time: ~a)~%"
                                     (lookup-var ras '?id :string)
                                     acc-values2
                                     (local-time:unix-to-timestamp (lookup-var ras '?start_time :integer)))
                             (modify-wme rete "vars" "accumulated_values" acc-values2)))))))

    ;(format t "~a~%" (to-json rete))
    (to-json-file rete "cl_1.json")

    ; initialize the accumulated values for readout meters
    (create-wme rete "vars" "accumulated_values" 0)

    ;(format t "~a~%" (to-json rete))
    (to-json-file rete "2.json")

    ; add the stream of facts now
    (create-wme rete "some-uuid" "type" "start")

    (to-json-file rete "3.json")

    (create-wme rete "some-uuid" "timestamp" (local-time:timestamp-to-unix (local-time:parse-timestring "2018-01-01T00:15:00.000000")))
    (to-json-file rete "4.json")

    (trigger-activated-production-nodes rete)

    (create-wme rete "some-uuid1" "type" "start")
    (to-json-file rete "5.json")
    (create-wme rete "some-uuid1" "timestamp" (local-time:timestamp-to-unix (local-time:parse-timestring "2018-01-01T00:25:00.000000")))
    (to-json-file rete "6.json")

    (trigger-activated-production-nodes rete)

    (create-wme rete "some-uuid" "type" "read-out meter end")
    (to-json-file rete "6_1.json")
    (create-wme rete "some-uuid" "timestamp" (local-time:timestamp-to-unix (local-time:parse-timestring "2018-01-01T02:00:00.000000")))
    (to-json-file rete "6_2.json")
    (create-wme rete "some-uuid" "readout" 10000)
    (to-json-file rete "7.json")

    (trigger-activated-production-nodes rete)

    (create-wme rete "some-uuid1" "type" "read-out meter end")
    (to-json-file rete "8.json")
    (create-wme rete "some-uuid1" "timestamp" (local-time:timestamp-to-unix (local-time:parse-timestring "2018-01-01T02:00:00.000000")))
    (to-json-file rete "9.json")
    (create-wme rete "some-uuid1" "readout" 1)
    (to-json-file rete "10.json")

    (trigger-activated-production-nodes rete)
    ;(trigger-activated-production-nodes rete)
    ;(trigger-activated-production-nodes rete)

    ))

;(with-rete (rete)
  ;(let (
        ;;; we have one rule that is responsible for assigning the instance start time
        ;(r0 (make-rule rete "Assign process instance start time" 0
                       ;'((?id "type" "start")
                         ;(?id "timestamp" ?timestamp))
                       ;(lambda (ras)
                         ;(modify-wme (lookup-var ras '?id :string) "instance_start_time" (lookup-var ras '?timestamp :string)))))
        ;(r1 (make-rule rete "original ISC - send alert threshold exceeded" 1
                       ;'((?id "type" "read-out meter end")
                         ;(?id "timestamp" ?timestamp
                           ;((= ?timestamp "2018-01-01T00:00:00")))
                         ;(?id "readout" ?readout_value)
                         ;("vars" "accumulated_values" ?acc_values))
                       ;(lambda (ras)
                         ;(let* ((acc-values (lookup-var ras '?acc_values :integer))
                                ;(value      (lookup-var ras '?readout_value :integer))
                                ;(THRESHOLD  10000)
                                ;(acc-values2 (+ acc-values value)))
                           ;(if (> acc-values2 THRESHOLD)
                             ;(format t "ALERT: Readout Threshold exceeded (x): ~a~%" acc-values2)
                             ;(modify-wme "vars" "accumulated_values" acc-values2)))))))
    ;(delete-rule rete r1)
    ;(let ((r2 (make-rule rete "original ISC routed - send alert threshold exceeded" 0
                         ;'((?id "type" "read-out meter end")
                           ;(?id "timestamp" ?timestamp
                                ;((= ?timestamp "2018-01-01T00:00:00")))
                           ;(?id "readout" ?readout_value)
                           ;;; this is part of the routing mechanism
                           ;(?id "instance-start-time" ?ist
                                ;((< ?ist "2018-01-01T02:00:00")))
                           ;("vars" "accumulated_values" ?acc_values))
                         ;(lambda (ras)
                           ;(let* ((acc-values (lookup-var ras '?acc_values :integer))
                                  ;(value      (lookup-var ras '?readout_value :integer))
                                  ;(THRESHOLD  10000)
                                  ;(acc-values2 (+ acc-values value)))
                             ;(if (> acc-values2 THRESHOLD)
                               ;(format t "ALERT: Readout Threshold exceeded (x): ~a~%" acc-values2)
                               ;(modify-wme "vars" "accumulated_values" acc-values2))))

    ;; initialize the accumulated values for readout meters
    ;(create-wme rete "vars" "accumulated_values" 0)

    ;; add the stream of facts now
    ;(create-wme rete "some-uuid" "type" "read-out meter end")
    ;(create-wme rete "some-uuid" "timestamp" "2018-01-01T00:00:00")
    ;(create-wme rete "some-uuid" "readout" 10000)

    ;(trigger-activated-production-nodes rete)))

;(require :local-time)
;`((?id "name" "Fred")
  ;(?id "timestamp" ?timestamp
    ;((>= ?timestamp ,(local-time:timestamp-to-unix (local-time:parse-timestring "2018-01-01T00:00:00.000000")))
     ;(<= ?timestamp ,(local-time:timestamp-to-unix (local-time:parse-timestring "2018-01-01T06:00:00.000000"))))))
