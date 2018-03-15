(scenario-specification *readout-meter*
  '((start-event
      ((:is-start-event (:start-time "2018-01-01T08:00:00.000000"
                         :hour :mean 5 :stddev 3))))
    (load-client-profile
      ((:before  (:duration :minute :mean 8 :stddev 2))
       (:execute
         (:duration :minute :mean 25 :stddev 5)
         (:data
           (("client-id" (random-uuid)))))))
    (establish-connection
      ((:before  (:duration :minute :mean 5 :stddev 2))
       (:execute
         (:duration :minute :mean 5 :stddev 2)
         (:data
           (("ip-address" (random-ip-address))
            ("port"       (random-port :min 1000 :max 35000)))))))
    (read-out-meter
      ((:before (:duration :minute :mean 60 :stddev 20))
       (:execute
         (:duration :hour :mean 3 :stddev 2)
         (:data
           (("read-out-value" (random-integer :mean 500 :stddev 100)))))))
    (analysis-of-sampled-values
      ((:before (:duration :hour :mean 8 :stddev 1))
       (:execute
         (:duration :hour :mean 2 :stddev 1))))
    (update-profile-data
      ((:before (:duration :minute :mean 60 :stddev 20))
       (:execute (:duration :minute :mean 10 :stddev 2))))
    (end-event
      ((:is-end-event)))))
