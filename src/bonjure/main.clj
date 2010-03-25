(ns bonjure.main
  (:import (java.net MulticastSocket InetAddress DatagramPacket SocketTimeoutException))
  (:import (java.nio ByteBuffer ByteOrder))
  (:use [bonjure bytebuffer])
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
  (loop [] (event-fn (listen-once sock)) (recur)))

(defn start-listen [sock event-fn]
  (.joinGroup sock group-ip)
  (.start (Thread. (partial listen-many sock event-fn))))

(defn got-pkt [pkt]
  (println "received packet of size" (.getLength pkt))
  (println "pkt:" (bonjure.core/process-pkt (ByteBuffer/wrap (.getData pkt) (.getOffset pkt) (.getLength pkt))))
  ) 

(defn go []
  (let [sock (MulticastSocket. group-port)]
    (.setSoTimeout sock 1000)
    ;(start-listen sock got-pkt)
    (println "Send msg")
    (.send sock (bonjure.core/create-msg group-ip group-port))
    ;(Thread/sleep 1000)

      (try
       (.joinGroup sock group-ip)
       (got-pkt (listen-once sock))
       (catch SocketTimeoutException e (println "Timeout " e)))
    
    ))

(defn -main
  [& args]
  (println "main args" args)
  (go)
  
  )


