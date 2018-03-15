
; DONE each activity has properties for the "setup" phase, "execution" phase and "post" phase. These properties determine how long they block
; DONE each activity needs to emit data specific for that activity as well

(ql:quickload :alexandria)
(ql:quickload :uuid)
(ql:quickload :flexi-streams)
(ql:quickload :split-sequence)
(ql:quickload :cl-json)

(defmacro with-normal-distribution (mean stddev name &body body); {{{
  (with-gensyms (result)
  `(let ((,@name (create-normal-distribution ,mean ,stddev)))
     (let ((,result (progn ,@body)))
       (destroy-normal-distribution ,@name)
       ,result)))); }}}

;(with-normal-distribution 60 20 (nd)
  ;(format t "1: ~a~%" (draw-from-normal-distribution nd))
  ;(format t "2: ~a~%" (draw-from-normal-distribution nd))
  ;(format t "3: ~a~%" (draw-from-normal-distribution nd)))

; notes: generated data belongs to the instance. Once generated it is always there, associated with an activity.
; These values can be looked up at a later activity to generate new data elements from

; TODO: define work schedules that should be followed when performing an
; activity that is handled by a human who is following a work schedule.

;(def-data-model some-process
  ;'())

(ql:quickload :cl-arrows)
(use-package :cl-arrows)

(define-condition unknown-property (base-error)
  ((text :initarg :text :reader text)))

(define-condition unknown-attribute (base-error)
  ((text :initarg :text :reader text)))

(define-condition unknown-data-operation (base-error)
  ((text :initarg :text :reader text)))

(defun generate-uuid ()
  "Generates a v4 uuid in URN format."
  (let ((id (uuid:make-v4-uuid)))
    (third (split-sequence:split-sequence #\: (uuid:format-as-urn nil id)))))

(defun range (max &key (min 0) (step 1))
  (loop for n from min below max by step
        collect n))

(defun random-integer (min max)
  (alexandria:random-elt (range max :min min :step 1)))

(defun random-ip-address ()
  (format nil "~a.~a.~a.~a"
          (random-integer 10 200)
          (random-integer 10 200)
          (random-integer 10 200)
          (random-integer 10 200)))

(defun clear-state (state)
  "Removes all other non-core keys from a state, but creating a new one and copying over the core elements."
  (let ((ns (make-hash-table)))
    (setf (gethash 'instance-id ns) (gethash 'instance-id state))
    (setf (gethash 'timestamp ns) (gethash 'timestamp state))
    (setf (gethash 'timestamp-unix ns) (gethash 'timestamp-unix state))
    (setf (gethash 'event ns) (gethash 'event state))
    ns))

(defun get-activity-name (entry)
  "Returns the activity part of a specification entry."
  (first entry))

(defun get-activity-properties (entry)
  "Returns the properties part of a specification entry."
  (first (rest entry)))

(defun process-spec-entry (entry stream state)
  "Main function to process a single specification entry."
  (let ((an (get-activity-name entry))
        (ap (get-activity-properties entry)))
    ;(format t "processing activity: ~a -> ~a~%" (symbol-name an) ap)
    (setf (gethash 'event state) (symbol-name an))
    (let ((new-state (process-spec-entry-properties an ap stream state)))
      new-state)))

(defun process-spec-entry-properties (an properties stream state)
  "Process all the properties for a given spec entry."
  (if (null properties)
    state
    (progn
      ;(format t "process-spec-entry-properties ~a~%" (first properties))
      ;(format t "process-spec-entry-properties (rest)~a~%" (rest properties))
      (->> (process-spec-entry-property an (first properties) stream state)
           (process-spec-entry-properties an (rest properties) stream)))))

(defun process-spec-entry-property (an prop stream state)
  "Main function to process a single property.
   Example:
    (:execute
      (duration :minutes :mean 60 :stddev 20)
      (data
        ((read-out-value
           (random-integer :mean 500 :stddev 100)))))
  "
  (let ((name (get-property-name prop))
        (attrs (get-property-attributes prop)))
    ;(format t "Processing ~a property ~a~%" an name)
    (cond ((equal name :is-start-event) (process-start-event an attrs stream state))
          ((equal name :is-end-event)   (process-end-event an stream state))
          ((equal name :before)         (process-before-event an attrs stream state))
          ((equal name :execute)        (process-execute-event an attrs stream state))
          ((equal name :after)          (process-after-event an attrs stream state))
          (t (error 'unknwon-property :text (format nil "unknown property: ~a~%" name))))))

(defun get-property-name (prop)
  "Returns the name of an activity propery."
  (first prop))

(defun get-property-attributes (prop)
  "Returns the list of parameters associated with a property."
  (rest prop))

(defun process-start-event (activity-name attrs stream state)
  "Emits the event data when a process start event occurs."
  ;(format t "processing ~a:start-event~%" activity-name)
  ;; create a random UUID for the instance id
  (setf (gethash 'instance-id state) (generate-uuid))
  ;(format t "UUID=~s~%" (gethash 'instance-id state))
  (let ((new-state (reduce #'process-attribute attrs :initial-value state)))
    (format stream "~a~%" (cl-json:encode-json-to-string new-state))
    new-state))

(defun process-end-event (activity-name stream state)
  "Emits the event data when a process end event occurs."
  ;(format t "handling ~a:end event~%" activity-name)
  (let ((ns (clear-state state)))
    (format stream "~a~%" (cl-json:encode-json-to-string ns))
    ns))

(defun process-before-event (activity-name attrs stream state)
  "Emits the event data when an activity is in the before state."
  ;(format t "processing ~a:before event...~%" activity-name)
  (let ((ns (clear-state state)))
    (setf (gethash 'state ns) "before")
    (let ((ns2 (reduce #'process-attribute attrs :initial-value ns)))
      (format stream "~a~%" (cl-json:encode-json-to-string ns2))
      ns2)))

(defun process-execute-event (activity-name attrs stream state)
  "Emits the event data when an activity is in the execute state."
  ;(format t "processing ~a:execute event...~%" activity-name)
  (let ((ns (clear-state state)))
    (setf (gethash 'state ns) "running")
    (let ((ns2 (reduce #'process-attribute attrs :initial-value ns)))
      (format stream "~a~%" (cl-json:encode-json-to-string ns2))
      ns2)))

(defun process-after-event (activity-name attrs stream state)
  "Emits the event data when an activity is in the after state."
  ;(format t "processing ~a:after event...~%" activity-name)
  (let ((ns (clear-state state)))
    (setf (gethash 'state ns) "after")
    (let ((ns2 (reduce #'process-attribute attrs :initial-value ns)))
      (format stream "~a~%" (cl-json:encode-json-to-string ns2))
      ns2)))

(defun process-attribute (state attr)
  "Main function to process a property attribute.
   E.g.:
   (:start-time \"2018-01-01T08:00:00.000000\" :hours mean 15 :stddev 3)
  "
  (let ((name (first attr)))
    (cond ((equal name :start-time) (progn
                                      (multiple-value-bind (_ start-dt time-unit _ mean _ stddev)
                                        (values-list attr)
                                        (let ((start-time (with-normal-distribution mean stddev (nd)
                                                            (let ((x (draw-from-normal-distribution nd)))
                                                            (local-time:timestamp+ (local-time:parse-timestring start-dt) (round x) time-unit)))))
                                          (setf (gethash 'timestamp state) (local-time:format-timestring nil start-time))
                                          (setf (gethash 'timestamp-unix state) (local-time:timestamp-to-unix start-time))
                                          state))))
          ((equal name :duration)   (progn
                                      ;(format t "handling duration attribute. ~a~%" attr)
                                      (multiple-value-bind (_ time-unit _ mean _ stddev)
                                        (values-list attr)
                                        ;(format t "time unit: ~a, mean: ~a, stddev: ~a~%" time-unit mean stddev)
                                        (let* ((current-ts (gethash 'timestamp state))
                                               (timestamp (with-normal-distribution mean stddev (nd)
                                                            (let ((x (draw-from-normal-distribution nd)))
                                                            (local-time:timestamp+ (local-time:parse-timestring current-ts) (round x) time-unit)))))
                                          (setf (gethash 'timestamp state) (local-time:format-timestring nil timestamp))
                                          (setf (gethash 'timestamp-unix state) (local-time:timestamp-to-unix timestamp))
                                          state))
                                      state))
          ((equal name :data)       (progn
                                      ;(format t "handling data attribute. ~a~%" attr)
                                      (reduce (lambda (state data-spec)
                                                (let* ((data-name (first data-spec))
                                                      (data-instruction (first (rest data-spec)))
                                                      (data-instruction-op (first data-instruction)))
                                                  ;(format t "data-spec: name=~a, instruction=~a~%" data-name data-instruction)
                                                  (cond ((equal data-instruction-op 'random-port)
                                                         ;(format t "genering new port number~%")
                                                         (multiple-value-bind (_ _ min _ max) (values-list data-instruction)
                                                           (setf (gethash (make-symbol data-name) state)
                                                                 (random-integer min max))))
                                                        ((equal data-instruction-op 'random-ip-address)
                                                         ;(format t "generating new ip address~%")
                                                         (setf (gethash (make-symbol data-name) state)
                                                               (random-ip-address)))
                                                        ((equal data-instruction-op 'random-integer)
                                                         ;(format t "generating random integer~%")
                                                         (multiple-value-bind (_ _ mean _ stddev) (values-list data-instruction)
                                                           (with-normal-distribution mean stddev (nd)
                                                             (let ((x (draw-from-normal-distribution nd)))
                                                               (setf (gethash (make-symbol data-name) state)
                                                                     (round x))))))
                                                        ((equal data-instruction-op 'random-uuid)
                                                         ;(format t "generating uuid~%")
                                                         (setf (gethash (make-symbol data-name) state) (generate-uuid)))
                                                        (t (error 'unknown-data-operation :text data-instrution-op)))
                                                  state))
                                              (first (rest attr))
                                              :initial-value state)))
          (t (error 'unknown-attribute :text (format nil "Unknown attribute: ~a~%" name))))))

(defun simulate (spec stream state)
  (if (null spec)
    nil
    (progn
      (->> (process-spec-entry (first spec) stream state)
           (simulate (rest spec) stream)))))

(defun create-log-data (spec n filename_or_stdout)
  (let ((ht (make-hash-table)))
    (if (equal filename_or_stdout t)
      (reduce (lambda (x y)
                (simulate spec t ht))
              (range n))
      (with-open-file (stream filename_or_stdout :direction :output)
        (reduce (lambda (x y)
                  (simulate spec stream ht))
                (range n))))))

;(create-log-data *readout-meter* 100 #p"/tmp/sample-logdata.txt")
;(create-log-data *readout-meter* 10 t) ; t means write to stdout
