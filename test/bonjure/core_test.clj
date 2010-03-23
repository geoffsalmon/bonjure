(ns bonjure.core-test
  (:use [bonjure.core] :reload-all)
  (:use [clojure.test])
  (:use [clojure.contrib math])
;  [:require [clojure.contrib [math :as math]]] 
  (:import (java.nio ByteBuffer ByteOrder)))


(def pos-int (expt 2 20))


(def b15 (expt 2 15))
(def b16 (expt 2 16))
(def b31 (expt 2 31))
(def b32 (expt 2 32))
(def b63 (expt 2 63))
(def b64 (expt 2 64))

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
  (let [buff
        (.flip
         (apply pack (byte-buffer 100)
               "bbssiill"
               (mapcat #(list -1 (dec (expt 2 %))) [8 16 32 64])
               ))]

        (is (= (repeat 8 -1)
               (unpack buff "bbssiill")))

        (.position buff 0)
        
        (is (= (mapcat #(repeat 2 (dec (expt 2 %))) [8 16 32 64])
               (unpack buff "BBSSIILL")))
        )
  )

(deftest test-take-bytes
  (with-buffer
    (.flip (reduce (fn [buff b] (.put buff (byte b)))
                   (ByteBuffer/allocate 100)
                   [0 0 -128 -128 -1 -1 ]))
    
    (is (= 0 (take-byte)))
    (is (= 0 (take-ubyte)))
  
    (is (= -128 (take-byte)))
    (is (= 128 (take-ubyte)))
  
    (is (= -1 (take-byte)))
    (is (= 255 (take-ubyte)))))

(deftest test-take-short
  (with-buffer
    (.flip (reduce (fn [buff b] (.putShort buff (short b)))
                   (ByteBuffer/allocate 100)
                   [0 0 (- b15) (- b15) -1 -1 ]))    
    (is (= 0 (take-short)))
    (is (= 0 (take-ushort)))
  
    (is (= (- b15) (take-short)))
    (is (= b15 (take-ushort)))
  
    (is (= -1 (take-short)))
    (is (= (- b16 1) (take-ushort)))))

(deftest test-take-int
  (with-buffer
    (.flip (reduce (fn [buff b] (.putInt buff (int b)))
                   (ByteBuffer/allocate 100)
                   [0 0 (- b31) (- b31) -1 -1 ]))
    
    (is (= 0 (take-int)))
    (is (= 0 (take-uint)))
  
    (is (= (- b31) (take-int)))
    (is (=  b31 (take-uint)))
  
    (is (= -1 (take-int)))
    (is (= (- b32 1) (take-uint)))))

(deftest test-take-long
  (with-buffer
    (.flip (reduce (fn [buff b] (.putLong buff (long b)))
                   (ByteBuffer/allocate 100)
                   [0 0 (- b63) (- b63) -1 -1 ]))
    
    (is (= 0 (take-long)))
    (is (= 0 (take-ulong)))
  
    (is (= (- b63) (take-long)))
    (is (= b63 (take-ulong)))
  
    (is (= -1 (take-long)))
    (is (= (- b64 1) (take-ulong)))))

;(use-fixtures :each read-fix-1)



;(set! *load-tests* false)



