(ns bonjure.core
  (:import (java.net MulticastSocket InetAddress DatagramPacket))
  (:import (java.nio ByteBuffer ByteOrder))
  (:import (java.nio.charset Charset))
  )

; Attempts to implement a continuous multicast dns querier as
; described in sectin 6.3 of
; http://files.multicastdns.org/draft-cheshire-dnsext-multicastdns.txt

; DNS http://www.faqs.org/rfcs/rfc1035.html
; SRV RR ftp://ftp.rfc-editor.org/in-notes/rfc2782.txt
; DNS-SD http://files.dns-sd.org/draft-cheshire-dnsext-dns-sd.txt


(def group-ip (InetAddress/getByName "224.0.0.251"))
;"224.0.0.251"
(def group-port 5353)
;5353

(def utf8-charset (Charset/forName "UTF-8"))

(defn add-query [buffer name]
  (reduce (fn [_ comp]
            (println (.length comp) comp)
            (.put buffer (byte (.length comp)))
            (.put buffer (.getBytes comp))
            nil)
          nil (seq (.split name "\\.")))
  
  (doto buffer
    (.put (byte 0))                    ; add final null byte
    (.putShort 12)                    ; QTYPE 12=PTR, 255=Request all records
    (.putShort 1))                     ; QCLASS 1=IN Internet
  )

(defn create-msg-data []
  (doto (ByteBuffer/allocate 100)
    (.order ByteOrder/BIG_ENDIAN)
    (.putShort 12)                      ; ID
    (.putShort 0)                       ; flags
    (.putShort 1)                       ; QDCOUNT
    (.putShort 0)                       ; ANCOUNT
    (.putShort 0)                       ; NSCOUNT
    (.putShort 0)                       ; ARCOUNT

    (add-query "_services._dns-sd._udp.local")
                                        ;(add-query "_daap._udp.local")
    (.flip)
    )
  )

(defn create-msg [ip port]
  (let [data (create-msg-data)
        pkt (DatagramPacket. (.array data) (.position data) (.remaining data) ip port)]
    pkt))


(defn send-packet [] 
  (let [sock (MulticastSocket. group-port)]
    
    (doto sock
      (.joinGroup group-ip)
      (.send (create-msg group-ip group-port))
      (.leaveGroup group-ip))))


(defn listen-once [sock]
  (let [pkt (DatagramPacket. (byte-array 1500) 1500)]
    (println "Block on receive")
    (.receive sock pkt)
    pkt))

(defn listen-many [sock event-fn]
  (loop [] (event-fn (listen-once sock)) (recur)))

(defn start-listen [sock event-fn]
  (.joinGroup sock group-ip)  
  (.start (Thread. (partial listen-many sock event-fn))))
  
(defn decode-flags [flags]
  (-> {}
      (assoc :qr (bit-test flags 15))
      (assoc :opcode (bit-shift-right (bit-and flags 0x7800) 11))
      (merge (zipmap [:aa :tc :rd :ra] (map (partial bit-test flags) (reverse (range 7 11)))))
      (assoc :rcode (bit-and flags 0xF))
      )
  )

(defn slice-off [buff len]
  "Create a new bytebuffer by slicing off the first len bytes. Also
consumes the bytes in the given buffer."
;  (println "slice" len "bytes from" (.remaining buff) "remaining")
  (when (> len (.remaining buff))
    (println "slice is too big")
    )
;  (when (> len (.remaining buff))
;    (println (.get buff) " " (.get buff) " " (.get buff) " " (.get buff) " " (.get buff) ))
  (let [rdbuf (-> buff (.slice) (.limit len))]
    (.position buff (+ (.position buff) len)) ; advance the actual buffer
    rdbuf
    )
  ) 
  
(defn read-label [buff length]
  (let [viewbuf (slice-off buff length)
        decoder (.newDecoder utf8-charset)]
    (.toString (.decode decoder viewbuf))
    )
  )


(defn read-labels
  ([buff] (read-labels buff []))
  ([buff labels]
     (let [num (bit-and 0xFF (.get buff))]
                                        ;     (println "read label num" num)
       (if (zero? num) (do
                         (println "Read labels" labels)
                         labels)
           (if (== 0xC0 (bit-and 0xC0 num)) ; is a pointer
             (let [index (bit-or (bit-shift-left (bit-and 0x3F num) 8) (.get buff))
                   dupbuff (.position (.duplicate buff) index)]
               (do
                 (println "Is pointer to " labels " index:" index)
                 (recur dupbuff labels)))
             (let [label (read-label buff num)]
                                        ;            (println "Got label" label)
               (recur buff (conj labels label))
               )
             ))
       ))
  )

(defn process-question [buff n]
;  (println "process-question")
  (doall (take n (repeatedly
                  #(-> {}
                       (assoc :name (read-labels buff))
                       (assoc :type (.getShort buff))
                       (assoc :class (.getShort buff))))))
  )



(defn process-rrs [buff n]
;  (println "process-rrs" n)
  (doall (take n (repeatedly
                  #(let [hdr (-> {}
                                 (assoc :name (read-labels buff))
                                 (assoc :type (.getShort buff))
                                 (assoc :class (.getShort buff))
                                 (assoc :ttl (.getInt buff))
                                 )
                         rdlen (bit-and 0xFFFF (int (.getShort buff)))
                         rdbuf (slice-off buff rdlen)
                         type (:type hdr)
                         ]
                     (condp = type
                           12           ; PTR
                           (assoc hdr :rd (read-labels rdbuf))

                           hdr          ; default
                           ))
                  )
               )))

(defn process-pkt-body [buff pinfo]
  ;(if (:qr (:flags pinfo))
  ;  pinfo
    (-> pinfo
        (assoc :qd (process-question buff (:qdcount pinfo)))
        (assoc :an (process-rrs buff (:ancount pinfo)))
        (assoc :ns (process-rrs buff (:nscount pinfo)))
        (assoc :ar (process-rrs buff (:arcount pinfo))))
  ;  )
  )

(defn process-pkt [buff]
  
  (let [pinfo (zipmap [:id :flags :qdcount :ancount :nscount :arcount] (take 6 (repeatedly #(bit-and 0xFFFF (.getShort buff)))))]
    (-> pinfo
        (assoc :flags (decode-flags (:flags pinfo)))
        ((partial process-pkt-body buff)))
    )
  )

(defn got-pkt [pkt]
  (println "received packet of size" (.getLength pkt))
  ( println "pkt:" (process-pkt (ByteBuffer/wrap (.getData pkt) (.getOffset pkt) (.getLength pkt))))
  ) 


(defn go []
  (let [sock (MulticastSocket. group-port)]
    (start-listen sock got-pkt)
    (dotimes [i 1]
      (println "Send msg")
      (.send sock (create-msg group-ip group-port))
      (Thread/sleep 1000)
      )))
                                        ;  (doto sock
                                        ;    (.joinGroup group)
                                        ;    (.send (create-msg group group-port))
                                        ;    (.leaveGroup group)))

    
                                        ;(go)

;(let [buffer (ByteBuffer/allocate 100)]
;  (add-query buffer "foo.m")
;  (.flip buffer)
;  (println "Remaining:" (.remaining buffer))
;)

(go)
