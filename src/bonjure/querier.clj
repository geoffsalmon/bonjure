(ns bonjure.querier
  (:import [java.util.concurrent DelayQueue Delayed TimeUnit])
  (:use [clojure.contrib.def :only [defvar defvar-]])
  )

(defvar- listeners (atom (set [])) "Currently registered listener callbacks")
(defvar- querier (atom nil))

(defn get-data []
  (let [data (:data @querier)]
    (if (nil? data) nil @data)
    )
  )

(defn queue-thread
  "Create and start a thread to serve tasks from a given queue"
  [queue]
  (doto (Thread. (fn []
                   (try
                    (doseq [item (repeatedly #(.take queue)) :while (not (:shutdown item))]
                      (try
                       (println "Got item" item) (when-not (nil? (:action item)) ((:action item)))
                       (catch Exception e
                         (println "Exception while processing item" e)
                         )))
                    (finally (println "Finally Exiting.")))
                   ))
    (.start)
    )
  )

(deftype Delayer [target action shutdown] java.util.concurrent.Delayed
  (getDelay [unit]
            (.convert unit (- target (System/currentTimeMillis)) TimeUnit/MILLISECONDS))
  (compareTo [other] (println "compareTo" other) (- target (:target other))))


(defn- build-delay [millis action shutdown]
  (let [target (+ millis (System/currentTimeMillis))]
    (Delayer target action shutdown))
  )

(defn push-task [millis action]
  (let [q (:queue @querier)]
    (when-not (nil? q)
      (.add q (build-delay millis action false))
      )
    )
  )

(defn start
  "Starts the querier if not already started. Does nothing if querier
  is already running."
  []
  (swap! querier
         (fn [old]
           (if (nil? old)
             ; start new querier
             (let [d (atom {:rows []})
                   q (java.util.concurrent.DelayQueue.)
                   t (queue-thread q)
                   ]

               ; start data watcher
               (add-watch d nil
                          (fn [key r old new]
                            (println "Watcher fired. notifying" (count @listeners) "listeners")
                            (doseq [listener @listeners]
                              (listener new))
                            ))               
               
               (.add q (build-delay 0 #(println "First action!") false))
               {
                :queue q
                :thread t
                :data d
                })
             
             ; querier already running!
             old
             )
           ))
  )

(defn stop
  "Stops the querier. Cleans up and stops background threads. Does
  nothing if no querier is running"
  []
  (swap! querier
         (fn [old]
           (when-not (nil? old)
             (remove-watch (:data old) nil)
             (doto (:queue old)
               ; clear all previous tasks and add a new task telling the
               ; thread to exit
               (.clear)
               (.add (build-delay -1000 nil true)))
             )
           nil
           ))
  )

(defn register-listener [listener]
  (listener (get-data)) ; notify listener of current state
  (swap! listeners conj listener)
  nil
  )

(defn unregister-listener [listener]
  (swap! listeners disj listener)
  nil
  )


(defn trigger []
  (println "trigger")
  (let [data (:data @querier)]
    (when-not (nil? data)
      (swap! data #(hash-map :rows (conj (:rows %) (str "Row " (count (:rows %))))))))
  )
