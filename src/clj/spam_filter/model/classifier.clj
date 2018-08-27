(ns spam-filter.model.classifier
  (:require [clojure.string :as st]))




(defn getwords
  "Divides the text on any character that isn't a letter.
  Converted to lowercase"
  [doc]
  (let [words (st/split doc #" ")
        less-than-20 (filter #(< (count %) 20) words)
        final-words (filter #(> (count %) 2) less-than-20)]
    (reduce (fn [final-map word]
              (assoc final-map word 1))
            {}
            final-words))) 



(defn incf
  "This function increases the count of feature/category pair"
  [f cat fc]
  (let [map1 (if (= (get fc f) nil)
               (assoc fc f {})
               fc)]
(if (get-in map1 [f cat])
  (update-in map1 [f cat] inc)
  (let [new-val (assoc-in map1 [f cat] 0)]
      (update-in new-val [f cat] inc)))))




(defn incc
  "Increase the count of a category"
  [cat]
  (if (get cc cat)
    (update cc cat inc)
    (let [new-val (assoc cc cat 0)]
      (update new-val cat inc))))



(defn fcount
  "The number of times a feature has appeared in a category"
  [f cat]
  (if (and (contains? fc f) (get-in fc [f cat]))
       (float (get-in fc [f cat]))
       0.0))



(defn catcount
  "The number of items in a category"
  [cat]
(if (contains? cc cat)
  (get cc cat)
  0))


(defn totalcount
  "The total number of items"
  []
  (reduce + (vals cc)))


(defn categories
  "The list of all categories"
  []
  (keys cc))


