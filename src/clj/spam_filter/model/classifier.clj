(ns spam-filter.model.classifier
  (:require [clojure.string :as st]))


(def fc (atom {}))


@fc

(def cc (atom {}))


@cc

; extracts features from the text
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


; increase the count of a feature/category pair
(defn incf
  [f cat]
(if (not (contains? @fc f))
  (swap! fc #(assoc % f {})))
(if (not (get-in @fc [f cat]))
  (swap! fc #(assoc-in % [f cat] 0)))
(swap! fc #(update-in % [f cat] inc)))


; increase the count of a category
(defn incc
 [cat]
(if (not (contains? @cc cat))
(swap! cc #(assoc % cat 0)))
(swap! cc #(update % cat inc)))



; The number of times a feature has appeared in a category
(defn fcount
  [f cat]
(let [cat (get-in @fc [f cat])]
  (if (not (nil? cat))
    cat
    0.0)))



; The number of items in a category
(defn catcount
  [cat]
(let [n-of-items (get @cc cat)]
  (if (not (nil? n-of-items))
    n-of-items
    0)))


; The total numbers of items
(defn totalcount
  []
(reduce + (vals @cc)))



; The list of all categories
(defn categories
[]
(keys @cc))



(defn train
  [t cat]
(incc cat)
(let [ws (keys (getwords t))]
  (for [w ws] (incf w cat))))

@fc
@cc

(train "the quick brown fox jumps over the lazy dog" "good")
(train "make quick money in the online casino" "bad")

(fcount "money" "good")

(fcount "online" "good")








