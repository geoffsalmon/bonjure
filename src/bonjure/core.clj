(ns bonjure.core
  (:import (java.net MulticastSocket InetAddress DatagramPacket))
  (:import (java.nio ByteBuffer ByteOrder))
  (:import (java.nio.charset Charset))
  (:use bonjure.bytebuffer)
  )

; Attempts to implement a continuous multicast dns querier as
; described in sectin 6.3 of
; http://files.multicastdns.org/draft-cheshire-dnsext-multicastdns.txt

; DNS http://www.faqs.org/rfcs/rfc1035.html
; SRV RR ftp://ftp.rfc-editor.org/in-notes/rfc2782.txt
; DNS-SD http://files.dns-sd.org/draft-cheshire-dnsext-dns-sd.txt


(def utf8-charset (Charset/forName "UTF-8"))

(defn add-domain-name [buff name]
  (doseq [comp (.split name "\\.")] 
    (let [len (count comp)]
      (when (or (> len 63) (< len 1))
        (throw (Exception. (str "Component \"" comp "\" of name \"" name "\" must be length 1-63")))
        )
      ;(println len comp)
      (put-byte buff len)
      (.put buff (.getBytes comp)))
    )
  (put-byte buff 0)                     ; add final null byte
  )

(defn add-query [buff name]
  (add-domain-name buff name)
  (put-short buff 12)          ; QTYPE 12=PTR, 255=Request all records
  (put-short buff 1)           ; QCLASS 1=IN Internet
  )

(defn create-query-msg [id name & names]
  (println id name names (cons name names))
  (let [buff (byte-buffer 100)]
    (.order buff ByteOrder/BIG_ENDIAN)
    (pack buff "ssssss"
          id                            ; ID
          0                             ; flags
          (inc (count names))           ; QDCOUNT
          0 0 0                         ; ANCOUNT NSCOUNT ARCOUNT
          )
    (doseq [n (cons name names)] (add-query buff n))
    (.flip buff)
    )
  )

(defn create-msg-data []
  (create-query-msg 16 "_services._dns-sd._udp.local"
                    ;"_daap._udp.local"
                    )
  )

(defn create-msg [ip port]
  (let [data (create-msg-data)
        pkt (DatagramPacket. (.array data) (.position data) (.remaining data) ip port)]
    pkt))


(defn decode-flags [flags]
  (-> {}
      (assoc :qr (bit-test flags 15))
      (assoc :opcode (bit-shift-right (bit-and flags 0x7800) 11))
      (merge (zipmap [:aa :tc :rd :ra] (map (partial bit-test flags) (reverse (range 7 11)))))
      (assoc :rcode (bit-and flags 0xF))
      )
  )

(defn hex-byte [x]
  (let [s (.toUpperCase (.toString (bigint x) 16))]
    (if (= 1 (count s))
      (str "0" s)
      s
      ))
  )

(defn read-label [buff length]
  (let [viewbuf (slice-off buff length)
        decoder (.newDecoder utf8-charset)]
    (.toString (.decode decoder viewbuf))
    )
  )

; holds a thread-local binding of the packet currently being processed
(def *curr-packet-body*)
(def *curr-packet-start*)

(defn read-labels
  ([buff] (read-labels buff []))
  ([buff labels]
     (let [num (take-ubyte buff)]
       ;(println "read label num" (hex-byte num))
       
       (if (zero? num)
        (do
          ;(println "Read labels" labels)
          labels)

        (condp = (bit-and 3 (bit-shift-right num 6))
          0                             ; is start of new label
          (let [label (read-label buff num)]
            ;(println "Got label" label)
            (recur buff (conj labels label)))
              
          3                             ; is a pointer
          (let [index (bit-or (bit-shift-left (bit-and 0x3F num) 8) (take-ubyte buff))
                dupbuff (.position (.duplicate *curr-packet-body*) (- index *curr-packet-start*))]
            (recur dupbuff labels))
          
          2 (do (println "Unsupported label byte" num) labels)
          1 (do (println "Unsupported label byte" num) labels)
          )
        )
       )
     )
  )

(defn process-question [buff n]
;  (println "process-question")
  (doall
   (take n
         (repeatedly
          #(hash-map
            :name (read-labels buff)
            :type (take-ushort buff)
            :class (take-ushort buff)))))
  )


(defn process-rrs [buff n]
  (println "process-rrs" n)
  (doall
   (take n
         (repeatedly
          #(let [hdr {:name (read-labels buff)
                      :type (take-ushort buff)
                      :class (take-ushort buff)
                      :ttl (take-int buff)}
                 rdlen (take-ushort buff)
                 rdbuf (slice-off buff rdlen)
                 ]
             (println "rdlen: " rdlen)
             ;(doseq [x (take rdlen (repeatedly (fn [] (take-ubyte rdbuf))))] (print (hex-byte x) ""))
             ;(println "")
             (case (:type hdr)
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
    ;)
  )

(defn process-pkt [buff]

  (binding [*curr-packet-body* buff
            *curr-packet-start* (.position buff)]
      (let [pinfo (zipmap
                   [:id :flags :qdcount :ancount :nscount :arcount]
                   (unpack buff "SSSSSS"))]

        (-> pinfo
            (assoc :flags (decode-flags (:flags pinfo)))
            ((partial process-pkt-body buff)))
        )
      )
  )

