(ns bonjure.querier
  (:use [clojure.contrib.def :only [defvar defvar-]])
  )

(defvar- data {})

(defvar- listeners (atom (set [])) "Currently registered listener callbacks")

(defn register-listener [listener]
  (listener data) ; notify listener of current state
  (swap! listeners conj listener)  
  )

(defn unregister-listener [listener]
  (swap! listeners disj listener)
  )
