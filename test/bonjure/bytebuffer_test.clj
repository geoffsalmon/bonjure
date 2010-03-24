(ns bonjure.bytebuffer-test
  (:use [bonjure.bytebuffer] :reload-all)
  (:use [clojure.test])
  (:use [clojure.contrib math])
  (:import (java.nio ByteBuffer ByteOrder)))

; choose values that have a different value for each byte
(def b (byte 0x1))
(def s 0x302)
(def i 0x7060504)
(def l 0x0f0e0d0c0b0a0908)

(deftest test-packing
  "Check that buffers filled with pack are identical to those
filled callin the Java put* methods"
  (is (=
       (doto (ByteBuffer/allocate 100)
         (.put b)
         (.putShort s)
         (.putInt i)
         (.putLong l)
         (.flip)
         )
       (.flip (pack (byte-buffer 100) "bsil" b s i l))))
  )

(deftest test-packing-error
  (is (thrown? Exception
               (pack (byte-buffer 100) "x" 1)))

  (is (thrown? Exception
               (pack (byte-buffer 100) "bb" 1)))

  (is (thrown? Exception
               (pack (byte-buffer 100) "b" 1 1)))
  )

(deftest test-unpacking-error
  (let [buff (.flip (pack (byte-buffer 100) "i" 1))]
    (is (thrown? Exception
                 (unpack buff "x")))
    
    )
  )

(deftest test-unpacking
  "Check that unpack is the inverse of pack"
  (let [vals [b s i l]
        fmt "bsil"
        buff (.flip (apply pack (byte-buffer 100) fmt vals))]
    (is (=
         vals
         (unpack buff fmt)
         )))
  )

(deftest test-signed-unsigned
  (let [max-unsigned (map #(dec (expt 2 %)) [8 16 32 64])
        mid-unsigned (map #(expt 2 %) [7 15 31 63])
        min-signed (map - mid-unsigned)
        
        buff
        (.flip
         (apply pack (byte-buffer 100)
                "bsilbsilbsilbsil"
                
                (concat
                 (repeat 4 -1)
                 max-unsigned
                 mid-unsigned
                 min-signed
                 )
               ))]

    ; unpack as signed variables
    (is (= (repeat 8 -1)
           (unpack buff "bsilbsil")))
    
    (is (= (apply concat (repeat 2 min-signed))
           (unpack buff "bsilbsil")))

    (.position buff 0) ; reread buffer from start

    ; unpack as unsigned variables
    (is (= (apply concat (repeat 2 max-unsigned))
           (unpack buff "BSILBSIL")))
    
    (is (= (apply concat (repeat 2 mid-unsigned))
           (unpack buff "BSILBSIL")))
    )
  )

(def b15 (expt 2 15))
(def b16 (expt 2 16))
(def b31 (expt 2 31))
(def b32 (expt 2 32))
(def b63 (expt 2 63))
(def b64 (expt 2 64))

(defn- pack-flip [fmt & vars]
  (.flip (apply pack (byte-buffer 100) fmt vars))
  )

(deftest test-take-bytes
  (let [buff (pack-flip "bbbbbb" 0 0 -128 -128 -1 -1)]
    (is (= 0 (take-byte buff)))
    (is (= 0 (take-ubyte buff)))
  
    (is (= -128 (take-byte buff)))
    (is (= 128 (take-ubyte buff)))
  
    (is (= -1 (take-byte buff)))
    (is (= 255 (take-ubyte buff)))))

(deftest test-take-short
  (let [buff (pack-flip "ssssss" 0 0 (- b15) (- b15) -1 -1)]

    (is (= 0 (take-short buff)))
    (is (= 0 (take-ushort buff)))
  
    (is (= (- b15) (take-short buff)))
    (is (= b15 (take-ushort buff)))
  
    (is (= -1 (take-short buff)))
    (is (= (- b16 1) (take-ushort buff)))))

(deftest test-take-int
  (let [buff (pack-flip "iiiiii" 0 0 (- b31) (- b31) -1 -1)]
    
    (is (= 0 (take-int buff)))
    (is (= 0 (take-uint buff)))
  
    (is (= (- b31) (take-int buff)))
    (is (=  b31 (take-uint buff)))
  
    (is (= -1 (take-int buff)))
    (is (= (- b32 1) (take-uint buff)))))

(deftest test-take-long
  (let [buff (pack-flip "llllll" 0 0 (- b63) (- b63) -1 -1)]
    
    (is (= 0 (take-long buff)))
    (is (= 0 (take-ulong buff)))
  
    (is (= (- b63) (take-long buff)))
    (is (= b63 (take-ulong buff)))
  
    (is (= -1 (take-long buff)))
    (is (= (- b64 1) (take-ulong buff)))))


(deftest test-bit-pack
  (is (= 0x120428 (pack-bits 4 1 4 2 4 0 4 4 8 0x28)))

  (is (thrown? Exception
               (pack-bits 4 1 4 -5 4)) "field without value")
  
  (is (thrown? Exception
               (pack-bits 4 1 4 -5 4 2)) "negative values")

  (is (thrown? Exception
               (pack-bits 4 1 0 0 4 2)) "zero bit lengths")

  (is (thrown? Exception
               (pack-bits 4 1 -10 5 4 2)) "negative bit lengths")

  (is (thrown? Exception
               (pack-bits 4 1 4 16 4 2)) "insufficient bit lengths")
  )

(deftest test-bit-unpack
  (is (= [0 0 0x12 0x34 0x5] (unpack-bits 0x12345 3 9 8 8 4)))
  
  (is (= [0x12 0x5] (unpack-bits 0x12345 8 -8 4)) "Skip bits")
  
  (is (= [0x12 0x34 0x5] (unpack-bits 0x12345 0 0 8 8 0 4)) "Weird 0 bit length. Should this be an error instead?")

  )



