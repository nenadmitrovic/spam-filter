(ns spam-filter.model.classifier
  (:require [clojure.string :as st])
  (:use [clojure.java.io :only [file]]))

; Counts of feature/category combinations
(def fc (atom {}))
@fc




; Counts of documents in each category
; How many times every classification has been used
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
              (assoc final-map (.toLowerCase word) 1))
            {}
            final-words)))

(getwords "Danas je lep dan")


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
(let [num (get-in @fc [f cat])]
  (if (not (nil? num))
    num
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



(defn train1
  [t cat]
  (incc cat)
  (let [features (keys (getwords t))]
    (map incf features (repeat (count features) cat))))



(defn sampletrain
[]
[(train "Nobody owns the water." "good")
  (train "the quick rabbit jumps fences" "good")
  (train "buy pharmaceuticals now" "bad")
  (train "make quick money at the online casino" "bad")
  (train "the quick brown fox jumps" "good")])

@fc
@cc

(sampletrain)


; probability that a word is in particular category
; Pr(word | classification)
(defn fprob
  [f cat]
(if (= (catcount cat) 0)
  0
(float (/ (fcount f cat) (catcount cat)))))

(fprob "quick" "good")



; probability that a word is in particular category
; assumed probability 0.5
(defn weightedprob
  [f cat fprob]
(let [weight 1.0
      ap 0.5
      basicprob (fprob f cat)
      totals (reduce + (for [c (categories)] (fcount f c)))
      bp (/ (+ (* weight ap) (* totals basicprob)) (+ weight totals))]
bp))

(weightedprob "money" "good" fprob)

(weightedprob "nekarec" "good" fprob)
(sampletrain)



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


; returns product of Pr(Document | Category) and Pr(Category)
(defn prob
  [item cat]
  (let [catprob (/ (catcount cat) (totalcount))
        docprob (docprob item cat)]
    (* docprob catprob)))

(prob "quick rabbit" "good")
(prob "quick rabbit" "bad")


(def thresholds (atom {}))


(defn setthreshold
  [cat t]
(swap! thresholds #(assoc % cat t)))


(defn getthreshold
  [cat]
(if (contains? @thresholds cat)
  (get @thresholds cat)
1.0))


; calculate probability for each category
(defn classify
  [item]
(let [probs (reduce (fn [res cat] (assoc res cat (prob item cat))) {} (categories))
      max (val (apply max-key val probs))
      best (key (apply max-key val probs))]
(for [cat (filter #(not= % best) (keys probs))]
  (if (> (* (get probs cat) (getthreshold best)) (get probs best))
    "unknown"
    best))))


(classify "quick rabbit")
(classify "quick money")



; Fisher method

(defn cprob
  [f cat]
(let [clf (fprob f cat)]
  (if (= clf 0)
    0
    (let [freqsum (reduce + (for [c (categories)] (fprob f c)))
          p (/ clf freqsum)]
      p))))




(defn invchi2
  [chi df]
(let [m (/ chi 2.0)
      sum (atom (/ 1 (Math/exp m)))
      term (atom (/ 1 (Math/exp m)))]
(doseq [i (range 1 (quot df 2))]
  (do
    (swap! term #(* % (/ m i)))
    (swap! sum #(+ % @term))))
(min @sum 1.0)))


(defn fisherprob
  [item cat]
(let [features (keys (getwords item))
      p (reduce (fn [val f] (* val (weightedprob f cat cprob))) 1 features)
      fscore (* (Math/log p) -2)]
(invchi2 fscore (* 2 (count features)))))


(def minimus (agent {}))


(defn setminimum
  [cat mini]
(send minimus assoc cat mini))



(defn getminimum
  [cat]
(if-let [res (get @minimus cat)]
  res
  0.0))



(defn classify
  [item]
(let [best (atom nil)
      maxi (atom 0.0)
      p (atom 0.0)]
(doseq [c (categories)]
  (swap! p #(let [% (fisherprob item c)] %))
  (if (and (> @p (getminimum c)) (> @p @maxi))
    (do
      (swap! best #(let [% c] %))
      (swap! maxi #(let [% @p] %)))))
@best))




