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
; mDNS http://files.multicastdns.org/draft-cheshire-dnsext-multicastdns.txt


(def utf8-charset (Charset/forName "UTF-8"))




(def rr-info)

(defn split-name [name]
  (if (string? name)
    (.split name "\\.")
    name))

(defn add-domain-name [buff name]
  (doseq [comp (split-name name)] 
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


(defn encode-flags [{:keys [qr opcode aa tc rd ra rcode] :as flags}]
;  (when-not (map? flags)
;    (throw (IllegalArgumentException. "flags must be a map")))
  (pack-bits 1 qr
             4 opcode
             1 aa 1 tc 1 rd 1 ra
             3 0
             4 rcode)
  )

(defn decode-flags [flags]
  (zipmap [:qr :opcode :aa :tc :rd :ra :rcode]
          (unpack-bits flags \b 4 \b \b \b \b -3 4))
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
     (let [num (if (.hasRemaining buff) (take-ubyte buff) 0)]
       ;(println "read label num" (hex-byte num))

       (if (zero? num)
         labels

         (condp = (bit-and 3 (bit-shift-right num 6))
           0                            ; is start of new label
           (let [label (read-label buff num)]
                                        ;(println "Got label" label)
             (recur buff (conj labels label)))
              
           3                            ; is a pointer
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
  ;(println "process-rrs" n)
  (doall
   (take n
         (repeatedly
          #(let [hdr {:name (read-labels buff)
                      :type (take-ushort buff)
                      :class (take-ushort buff)
                      :ttl (take-int buff)}
                 rdlen (take-ushort buff)
                 rdbuf (slice-off buff rdlen)
                 parser (:parser (get rr-info (:type hdr)))
                 ]
                                        ;(println "rdlen: " rdlen)
                                        ;(doseq [x (take rdlen (repeatedly (fn [] (take-ubyte rdbuf))))] (print (hex-byte x) ""))
                                        ;(println "")
             
             (if (nil? parser)
               hdr
               (assoc hdr :rd (parser rdbuf))
               )
             )))))

(defn process-pkt-body [buff pinfo]
  (-> pinfo
      (assoc :qd (process-question buff (:qdcount pinfo)))
      (assoc :an (process-rrs buff (:ancount pinfo)))
      (assoc :ns (process-rrs buff (:nscount pinfo)))
      (assoc :ar (process-rrs buff (:arcount pinfo))))
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


(defn parse-rdata-srv [buf]
  {:priority (take-ushort buf)
   :weight (take-ushort buf)
   :port (take-ushort buf)
   :target (read-labels buf)}
  )

(defn parse-rdata-addr [buf]
  (let [bytes (byte-array (.remaining buf))]
    (.get buf bytes)
    (InetAddress/getByAddress bytes)
    )
  )

(def rr-info
     (reduce
      (fn [rrs [id name parser-fn]]
        (assoc rrs id
               {:id id
                :name name
                :parser parser-fn})
        )
      {}
      [[1 "A" parse-rdata-addr]
       [16 "TXT" read-labels]
       [12 "PTR" read-labels]
       [28 "AAAA" parse-rdata-addr]
       [33 "SRV" parse-rdata-srv]]
      )
     )
