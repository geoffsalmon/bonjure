(ns bonjure.main
  (:import (java.net MulticastSocket InetAddress DatagramPacket SocketTimeoutException))
  (:import (java.nio ByteBuffer ByteOrder))
  (:use [bonjure bytebuffer])
  (:use [clojure.contrib.seq :only [flatten]])
  (:use [clojure.contrib.string :only [join]])
  (:require [bonjure.core :as core]) 
  (:gen-class)
  )

(def group-ip (InetAddress/getByName "224.0.0.251"))
;"224.0.0.251"
(def group-port 5353)
;5353

(defn send-packet [] 
  (let [sock (MulticastSocket. group-port)]
    
    (doto sock
      (.joinGroup group-ip)
      (.send (bonjure.core/create-msg group-ip group-port))
      (.leaveGroup group-ip))))


(defn listen-once [sock]
  (let [pkt (DatagramPacket. (byte-array 1500) 1500)]
    (println "Calling receive")
    (.receive sock pkt)
    pkt))

(defn listen-many [sock event-fn]
  (try
   (loop [] (event-fn sock (listen-once sock)) (recur))
   (catch SocketTimeoutException e (println "Timeout " e)))
  )

(defn start-listen [sock event-fn]
  (let [out *out*
        err *err*] 
    (.start (Thread. #(binding
                          ; bind *out* and *err* to be the same as the
                          ; calling thread so that output appears in
                          ; the slime repl buffer
                          [*out* out *err* err]
                       (println "Running in thread") (listen-many sock event-fn)))))
  )

(defn create-msg [ip port data]
  (DatagramPacket. (.array data) (.position data) (.remaining data) ip port)
  )

(defn got-pkt [sock pkt]
  (println "received packet of size" (.getLength pkt))
  (let [pkt (bonjure.core/process-pkt (ByteBuffer/wrap (.getData pkt) (.getOffset pkt) (.getLength pkt)))
        rrs (filter (comp not nil?) (flatten [(:an pkt) (:ns pkt) (:ar pkt)]))
        ]
    ;(println "pkt:" pkt)
    ;(println "rrs:" rrs)
    (doseq [rr rrs]
      (println (get (get core/rr-info (:type rr)) :name "?") (:name rr) (:rd rr))

      ;(.send sock (create-msg group-ip group-port (bonjure.core/create-query-msg 33 (:rd rr))))
      )

    
    
;    (doseq [rr (filter (comp not nil?) (flatten (:an pkt) (:ns pkt) (:ar pkt)))])

    
    ;(println "an:" (:an pkt))
    )
  ) 

(defn go []
  (let [sock (MulticastSocket. group-port)]
    ;(.setLoopbackMode sock true)
    (.setSoTimeout sock 1000)
    (println "Send msg")
                                        ;(.send sock (bonjure.core/create-msg group-ip group-port))

;    (.send sock (create-msg group-ip group-port (bonjure.core/create-query-msg 33 ["_http" "_tcp" "local"])))

    (.send sock (create-msg group-ip group-port (bonjure.core/create-query-msg 33 ["_http" "_tcp"  "local"])))
    
    (.joinGroup sock group-ip)
    (listen-many sock got-pkt)
    ;(start-listen sock got-pkt)
    ))

(defn -main
  [& args]
  (println "main args" args)
  (go)
  
  )


