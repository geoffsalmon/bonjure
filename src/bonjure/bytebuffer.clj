
(def *byte-buffer* nil)

(defn byte-buffer [n]
  (ByteBuffer/allocate n))

(defmacro with-buffer
  [buffer & body]
  `(binding [*byte-buffer* ~buffer]
     ~@body))

(defn put-byte
  ([val]
     (put-byte *byte-buffer* val))
  ([buff val]
;     (println "put-byte" val (class val))
     (.put buff (.byteValue val)))
  )

(defn put-short
  ([val]
     (put-short *byte-buffer* val))
  ([buff val]
     (.putShort buff (.shortValue val)))
  )

(defn put-int
  ([val]
     (put-int *byte-buffer* val))
  ([buff val]
     (.putInt buff (.intValue val)))
  )

(defn put-long
  ([val]
     (put-long *byte-buffer* val))
  ([buff val]
     (.putLong buff (.longValue val)))
  )

(defn take-byte
  ([]
     (take-byte *byte-buffer*))
  ([buff]
     (.get buff))
  )

(defn take-ubyte
  ([]
     (take-ubyte *byte-buffer*))
  ([buff]
     (bit-and 0xFF (short (.get buff))))
  )

(defn take-short
  ([]
     (take-short *byte-buffer*))
  ([buff]
     (.getShort buff))
  )

(defn take-ushort
  ([]
     (take-ushort *byte-buffer*))
  ([buff]
     (bit-and 0xFFFF (int (.getShort buff))))
  )

(defn take-int
  ([]
     (take-int *byte-buffer*))
  ([buff]
     (.getInt buff))
  )
  
(defn take-uint
  ([]
     (take-uint *byte-buffer*))
  ([buff]
     (bit-and 0xFFFFFFFF (long (.getInt buff))))
  )

(defn take-long
  ([]
     (take-long *byte-buffer*))
  ([buff]
     (.getLong buff))
  )
  
(defn take-ulong
  ([]
     (take-ulong *byte-buffer*))
  ([buff]
     (bit-and 0xFFFFFFFFFFFFFFFF (bigint (.getLong buff))))
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

(defn- pack-one [buff fmt val]
;  (println "packone" fmt val)
  (case fmt
        \b (put-byte buff val)
        \s (put-short buff val)
        \i (put-int buff val)
        \l (put-long buff val)
        (throw (IllegalArgumentException. (str "Unknown format symbol \"" fmt \")))
        ))

(defn pack
  [buff fmt & vals]
                                        ;  (println "pack" fmt vals)
  (when-not (= (count fmt) (count vals))
    (throw (IllegalArgumentException. "pack error. Number of format symbols must match number of values.")))

  (doseq [[f val] (partition 2 (interleave fmt vals))]
    (pack-one buff f val))
;  (doall (map #(pack-one buff %1 %2) fmt vals))
  buff
  )

(defn- unpack-one [buff fmt]
;  (println "unpack-one" fmt)
  (case fmt
        \b (take-byte buff)
        \B (take-ubyte buff)
        \s (take-short buff)
        \S (take-ushort buff)
        \i (take-int buff)
        \I (take-uint buff)
        \l (take-long buff)
        \L (take-ulong buff)
        (throw (IllegalArgumentException. (str "Unknown format symbol \"" fmt \")))
        ))

(defn unpack
  [buff fmt]
  (doall (map (partial unpack-one buff) fmt))
  )


(defn pack-bits
  "Packs multiple numbers into a single number using explicit bit lengths.

fields => bit-length value
"
  ([] 0)
  ([& fields]
     (when-not (zero? (mod (count fields) 2))
       (throw (IllegalArgumentException. (str "pack-bits Last field does not have a value.")))
       )

     (reduce (fn [acc [num-bits val]]
               (when-not (pos? num-bits)
                 (throw (IllegalArgumentException.
                         (str "pack-bits: Invalid bit length " num-bits ". Must be positive."))))

               (when (neg? val)
                 (throw (IllegalArgumentException.
                         (str "pack-bits: Invalid value " val ". Must be non-negative."))))
               
               ; ensure specified bit length was big enough
               (when-not (= val
                          (bit-and val (dec (bit-shift-left 1 num-bits)))
                          
                          )
                 (throw (IllegalArgumentException.
                         (str "pack-bits: Invalid value " val ". Must fit in " num-bits " bits as specified.")))
                 )
               
               (-> acc
                   (bit-shift-left num-bits)
                   (bit-or val)
                   )
               )
             0 (partition 2 fields))
     )
  )

(defn unpack-bits
  "Pulls apart a number into a list of fields of various bit lengths. Pass a non-positive bit length to skip that many bits without adding a corresponding value to the result list."
  [x & bit-lengths]
  (loop [val x
         results '()
         ; iterate bit lengths in reverse to continually pull 
         ; values from the low order bits
         rbit-lens (reverse bit-lengths)]

    ;(println "loop val:" val " results:" results "bitlens:" rbit-lens)
    
    (if (= '() rbit-lens)
      results

      (let [bits (first rbit-lens)]
        (if (pos? bits)
          (recur (bit-shift-right val bits)
                 (cons
                  (bit-and val (dec (bit-shift-left 1 bits)))
                  results)
                 (rest rbit-lens)
                 )
          ; skip bits
          (recur (bit-shift-right val (- bits))
                 results
                 (rest rbit-lens)
                 )
          )
        )
      )
    )
  )

(defn bin [x]
  (println (.toString (bigint x) 2))
  )
