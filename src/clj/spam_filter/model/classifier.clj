(ns spam-filter.model.classifier
  (:require [clojure.string :as st]))

; Counts of feature/category combinations
(def fc (atom {}))


; Counts of documents in each category
; How many times every classification has been used
(def cc (atom {}))


; extracts features from the text
(defn getwords
  "Divides the text on any character that isn't a letter.
  Converted to lowercase"
  [doc]
  (let [words (st/split doc #" ")
        less-than-20 (filter #(< (count %) 20) words)
        final-words (filter #(> (count %) 2) less-than-20)]
    (reduce (fn [final-map word]
              (assoc final-map (.toLowerCase word) 1))
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


@cc

(defn train
  [t cat]
(incc cat)
(let [ws (keys (getwords t))]
  (for [w ws] (incf w cat))))



(defn train1
  [t cat]
  (incc cat)
  (let [features (keys (getwords t))]
    (map incf features (repeat (count features) cat))))



(defn sampletrain
[]
(do
  [(train "Nobody owns the water." "good")
  (train "the quick rabbit jumps fences" "good")
  (train "buy pharmaceuticals now" "bad")
  (train "make quick money at the online casino" "bad")
  (train "the quick brown fox jumps" "good")]))



(defn fprob
  [f cat]
(if (= (catcount cat) 0)
  0
(float (/ (fcount f cat) (catcount cat)))))




(defn weightedprob
  [f cat fprob]
(let [weight 1
      ap 0.5
      basicprob (fprob f cat)
      totals (reduce + (vals (get @fc f)))
      bp (/ (+ (* weight ap) (* totals basicprob)) (+ weight totals))]
bp))



; Extracts features and multiplies all
; their probabilities together to get
; an overall probability Pr(Document | Category)

(defn docprob
  [item cat]
  (let [features (keys (getwords item))]

  (loop [features features
         p 1]
    (if (empty? features)
      p
      (recur
       (rest features)
       (* p (weightedprob (first features) cat fprob)))))))



(defn prob
  [item cat]
  (let [catprob (/ (catcount cat) (totalcount))
        docprob (docprob item cat)]
    (* docprob catprob)))


(def thresholds (atom {}))


(defn setthreshold
  [cat t]
(swap! thresholds #(assoc % cat t)))


(defn getthreshold
  [cat]
(if (contains? @thresholds cat)
  (get @thresholds cat)
1.0))





































































