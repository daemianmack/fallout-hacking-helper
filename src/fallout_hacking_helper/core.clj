(ns fallout-hacking-helper.core
  (:require [clojure.pprint :as pp]
            [clojure.set :as s]
            [clojure.string :refer [join split]]))


(defn likeness [a b] (count (filter identity (map = a b))))

(defn grey  [s] (str "\u001b[1;30m" s "\u001b[0m"))
(defn red   [s] (str "\u001b[1;31m" s "\u001b[0m"))
(defn green [s] (str "\u001b[1;32m" s "\u001b[0m"))
(defn blue  [s] (str "\u001b[1;34m" s "\u001b[0m"))

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
    '([dog [[0 cat][1 dub][2 dot]]]  ;; Three distinct intersection-group counts (0,1,2).
      [dot [[1 dub][1 cat][2 dog]]]  ;; Two (1,2).
      [cat [[0 dog][0 dub][1 dot]]]) ;; Two (0,1).
  returning the pair with the highest number of distinct intersection-group counts.

  Optimal outcome: we manage to find a word with the greatest number
  of intersection groups, sorting the candidates into groups as small
  as possible, so that when the Fallout game computer reports the
  number of letters that are correct, we minimize the search space by
  as much as possible. i.e.,

    dub
        0 cat
        1 dog dot
    dot
        1 cat dub
        2 dog
    cat
        0 dog dub
        1 dot
    dog
        0 cat
        1 dub
        2 dot

  Here, dog is the best first choice because it relates to the other
  candidates options across the greatest number of intersection
  groups, each of which is as shallow as possible (and in this toy
  example would produce a win in 2 moves, since each group narrows the
  space to a single obvious choice)."
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
  (println "=> Input candidates from game.")
  (println "=> Hit space after each word.")
  (println "=> End by pressing the" (red "Enter") "key.")
  (let [words (get-words)]
    (loop [[candidate rels :as data] (best-candidate words)]
      (when (seq data)
        (println (format "=> How many likenesses does %s have?" (blue candidate)))
        (let [likenesses     (Integer. (read-line))
              new-candidates (map second (filter #(= likenesses (first %)) rels))]
          (if (< (count new-candidates) 2)
            (dorun (map #(->> % green (format "=> Answer: %s") println) new-candidates))
            (recur (best-candidate (set new-candidates)))))))))