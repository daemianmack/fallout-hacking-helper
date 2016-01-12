(ns fallout-hacking-helper.core-test
  (:require [clojure.test :refer :all]
            [fallout-hacking-helper.core :refer :all]))


(deftest likeness-test
 (are [result a b] (= result (likeness a b))
   0 "abcde" "vwxyz"
   1 "awcde" "vwxyz"
   2 "vwcde" "vwxyz"
   3 "vbxdz" "vwxyz"
   4 "vwxye" "vwxyz"
   5 "vwxyz" "vwxyz"))

(deftest best-candidate-test
  (is (= ["dog" [[0 "cat"][1 "dub"][2 "dot"]]]
         (best-candidate #{"dog" "dub" "dot" "cat"})))

  (is (= ["talk" [[0 "whip"] [1 "warm"] [2 "wale"] [3 "walk"]]]
         (best-candidate #{"walk" "whip" "warm" "wale" "talk" }))))
