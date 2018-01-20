(ns fallout-hacking-helper.core
  (:require [clojure.pprint :as pp]
            [clojure.set :as s]
            [clojure.string :refer [join split]]))


(defn likeness [a b] (count (filter identity (map = a b))))

(defn blue [s] (str "\u001b[1;34m" s "\u001b[0m"))
(defn grey [s] (str "\u001b[1;30m" s "\u001b[0m"))

(defn print-relations
  [relations]
  (doseq [[word rels] relations
          :let [_ (println (grey word))]
          [cnt rels] (group-by first rels)
          :let [_ (print (grey (format "%s%s " 
                                       (join "" (repeat (inc (count word)) " "))
                                       cnt)))]] 
    (println (grey (join " " (map second rels))))))

(defn best-candidate
  "Given...
    #{dog dub dot cat}
  produce...
    '([dog [[0 cat][1 dub][2 dot]]]  ;; Three intersection counts (0,1,2).
      [dot [[1 dub][1 cat][2 dog]]]  ;; Two (1,2).
      [cat [[0 dog][0 dub][1 dot]]]) ;; Two (0,1).
  returning the pair with the highest number of distinct intersection counts."
  [words]
  (let [tree (apply merge-with (comp sort into)
                    (for [a words
                          b (s/difference words #{a})]
                      {a [[(likeness a b) b]]}))
        sorted (sort-by
                (fn [[_ rels]] (count (distinct (map first rels))))
                tree)]
    (print-relations sorted)
    (last sorted)))

(defn sanity-check
  [words]
  (assert (apply = (map count words)) "All words must be the same length.")
  words)

(defn get-words [] (-> (read-line) (split #" ") set sanity-check))

(defn -main
  []
  (println "=> Hit enter after each word. End with a Ctrl-D.")
  (let [words (get-words)]
    (loop [[candidate rels :as data] (best-candidate words)]
      (when (seq data)
        (println (format "=> How many likenesses does %s have?" (blue candidate)))
        (let [likenesses     (Integer. (read-line))
              new-candidates (map second (filter #(= likenesses (first %)) rels))]
          (if (< (count new-candidates) 2)
            (dorun (map #(->> % blue (format "=> Answer: %s") println) new-candidates))
            (recur (best-candidate (set new-candidates)))))))))

