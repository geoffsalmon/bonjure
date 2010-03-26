(ns bonjure.core-test
  (:use [bonjure.core] :reload-all)
  (:use [clojure.test])
  )


(deftest test-flags
  (let [flags 0xff8f] ; the 8 is because 3 bits will always be 0 in the
                      ; encoded flag
    (is (= flags (encode-flags (decode-flags flags))))
    )
  )
