(require :asdf)
(require :ecl-quicklisp)
(require :uiop)
(require :cffi)
(require :local-time)

(compile-file "binding-loader.lisp")
(load "binding-loader")

(defmacro with-rete (name &body body); {{{
  `(let ((,@name (rete-init)))
     ,@body
     (rete-destroy ,@name))); }}}

; TODO: you could define several rete instances and clear them all in the end -> (with-rete (rete1 rete2) ...)

(load "tests.lisp")
(load "isc_evolution/scenario_builder.lisp")
(create-log-data *readout-meter* 1000 #p"/tmp/sample-logdata.txt")

;(create-log-data *readout-meter* 10 #p"/tmp/sample-logdata.txt")

(defun prepare-event-stream (filename)
  (let ((in (open filename :if-does-not-exist nil)))
    (when in
      (let ((all (loop for line = (read-line in nil)
                       while line collect (cl-json:decode-json-from-string line))))
        (sort all #'< :key #'(lambda (x)
                               (let ((y (cdr (assoc :TIMESTAMP x))))
                                 ;(format t "y: ~a~%" y)
                                 (local-time:timestamp-to-unix (local-time:parse-timestring y)))))
        (close in)
        all))))

(defun rule1-handler (ras); {{{
  (format t "adding instance-start-time for ~a~%" (lookup-var ras '?id :string))
  ; this somehow causes degradation of performance. What to do?
  (modify-wme rete
              (lookup-var ras '?id :string)
              "instance_start_time"
              (lookup-var ras '?timestamp :integer))
  ); }}}
(defun rule2-handler (ras); {{{
  (format t "TEST1~%")
  (let* ((acc-values (lookup-var ras '?acc_values :integer))
         (value      (lookup-var ras '?readout_value :integer))
         (THRESHOLD  1000)
         (acc-values2 (+ acc-values value)))
    (format t "instance id: ~a accumulated value: ~a, next value: ~a~%"
            (lookup-var ras '?id :string)
            (lookup-var ras '?acc_values :integer)
            value)
    (if (> acc-values2 THRESHOLD)
      (format t "ALERT: ~a Readout Threshold exceeded (x): ~a. (Start time: ~a, Timestamp: ~a)~%"
              (lookup-var ras '?id :string)
              acc-values2
              (local-time:unix-to-timestamp (lookup-var ras '?start_time :integer))
              (local-time:unix-to-timestamp (lookup-var ras '?timestamp :integer))
              )
      (modify-wme rete "vars" "accumulated_values" acc-values2)))); }}}
(defun sample-event-stream (); {{{
  (with-rete (rete)
             (let (
                   ;; we have one rule that is responsible for assigning the instance start time
                   (r0 (make-rule rete "Assign process instance start time" 0
                                  '((?id "type" "START-EVENT")
                                    (?id "timestamp" ?timestamp))
                                  #'(lambda (ras)
                                      (format t "adding instance-start-time for ~a~%" (lookup-var ras '?id :string))
                                      ; this somehow causes degradation of performance. What to do?
                                      (modify-wme rete
                                                  (lookup-var ras '?id :string)
                                                  "instance_start_time"
                                                  (lookup-var ras '?timestamp :integer))
                                      )))
                   (r1 (make-rule rete "original ISC - send alert threshold exceeded" 0
                                  `((?id "type" "READ-OUT-METER")
                                    (?id "timestamp" ?timestamp
                                         ((>= ?timestamp ,(local-time:timestamp-to-unix (local-time:parse-timestring "2018-01-01T00:00:00.000000")))
                                          (<= ?timestamp ,(local-time:timestamp-to-unix (local-time:parse-timestring "2018-01-10T06:00:00.000000")))))
                                    (?id "readout" ?readout_value)
                                    (?id "instance_start_time" ?start_time)
                                    ("vars" "accumulated_values" ?acc_values))
                                  #'(lambda (ras)
                                      (format t "TEST1~%")
                                      (let* ((acc-values (lookup-var ras '?acc_values :integer))
                                             (value      (lookup-var ras '?readout_value :integer))
                                             (THRESHOLD  1000)
                                             (acc-values2 (+ acc-values value)))
                                        (format t "instance id: ~a accumulated value: ~a, next value: ~a~%"
                                                (lookup-var ras '?id :string)
                                                (lookup-var ras '?acc_values :integer)
                                                value)
                                        (if (> acc-values2 THRESHOLD)
                                          (format t "ALERT: ~a Readout Threshold exceeded (x): ~a. (Start time: ~a, Timestamp: ~a)~%"
                                                  (lookup-var ras '?id :string)
                                                  acc-values2
                                                  (local-time:unix-to-timestamp (lookup-var ras '?start_time :integer))
                                                  (local-time:unix-to-timestamp (lookup-var ras '?timestamp :integer))
                                                  )
                                          (modify-wme rete "vars" "accumulated_values" acc-values2))))
                                      )))

               (let ((filename #p"/tmp/sample-logdata.txt")
                     (n 10)
                     (spec *readout-meter*))
                 (create-log-data spec 50 filename)

                 ;; initialize the accumulated values for readout meters
                 (create-wme rete "vars" "accumulated_values" 0)

                 (let ((event-stream (prepare-event-stream filename))
                       (lookup (lambda (key x) (cdr (assoc key x)))))
                   (loop
                     for event in event-stream
                     do (progn
                          (let ((instance-id (funcall lookup :INSTANCE-ID event))
                                (timestamp (local-time:timestamp-to-unix
                                             (local-time:parse-timestring (funcall lookup :TIMESTAMP event))))
                                (event-name (funcall lookup :EVENT event))
                                (state (funcall lookup :STATE event)))
                            (when (or (equal event-name "START-EVENT")
                                      (equal event-name "READ-OUT-METER"))
                              (format t "instance-id: ~s event: ~s at ~a ~a~%" instance-id event-name (local-time:unix-to-timestamp timestamp) event)
                              ;(format t "adding type wme~%")
                              (create-wme rete instance-id "type" event-name)
                              ;(trigger-activated-production-nodes rete)
                              ;(format t "adding timestamp wme~%")
                              (create-wme rete instance-id "timestamp" timestamp)
                              )
                            (when (and (equal event-name "READ-OUT-METER")
                                       (equal state "running"))
                              (let ((value (funcall lookup :READ-OUT-VALUE event)))
                                ;(format t "VALUE: ~a~%" value)
                                (create-wme rete instance-id "readout" value)
                                ;(trigger-activated-production-nodes rete)
                                ))
                            (trigger-activated-production-nodes rete)
                            )
                            )
                     )))
               ))); }}}
(defun test-correctness (); {{{
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

               ))); }}}
;(sample-event-stream)
;(test-correctness)

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
