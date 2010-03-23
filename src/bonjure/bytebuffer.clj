
(def *byte-buffer* nil)

(defmacro with-buffer
  [buffer & body]
  `(binding [*byte-buffer* ~buffer]
     ~@body))

(defn put-byte
  ([val]
     (put-byte *byte-buffer* val))
  ([buff val]
     (.put buff (byte val)))
  )

(defn put-short
  ([val]
     (put-short *byte-buffer* val))
  ([buff val]
     (.putShort buff (short val)))
  )

(defn put-int
  ([val]
     (put-int *byte-buffer* val))
  ([buff val]
     (.putInt buff (int val)))
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





