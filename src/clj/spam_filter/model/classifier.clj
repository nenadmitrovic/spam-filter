(ns spam-filter.model.classifier
  (:require [clojure.string :as st]
            [clojure.java.jdbc :as sql])
  (:use [clojure.java.io :only [file]]))



(def db {:subprotocol "mysql"
         :subname "//localhost/spam_classifier"
         :user "root"
         :password ""})




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


(defn fcount
  [f cat]
(let [res (sql/query db ["select count from fc where feature=? and category=?" f cat])]
(if (= (:count (first res)) nil)
  0
  (float (:count (first res))))))



(defn incf
  [f cat]
(let [res (fcount f cat)]
(if (= res 0)
(sql/insert! db :fc {:feature f :category cat :count 1})
(sql/execute! db ["update fc set count=? where feature=? and category=?" (inc res) f cat]))))



(defn catcount
  [cat]
(let [res (:count (first (sql/query db ["select count from cc where category=?" cat])))]
  (if (= res nil)
    0
    res)))



(defn incc
  [cat]
(let [res (catcount cat)]
  (if (= res 0)
    (sql/insert! db :cc {:category cat :count 1})
    (sql/execute! db ["update cc set count=? where category=?" (inc res) cat]))))




(defn categories
  []
  (map :category (sql/query db ["select category from cc"])))



(defn totalcount
  []
(let [res (reduce + (map :count (sql/query db ["select count from cc"])))]
  (if (= res nil)
    0
    res)))




(def token-regex #"[a-zA-Z]{3,}")


(def header-fields
  ["To:"
   "From:"
   "Subject:"
   "Return-Path:"])




(defn header-token-regex
  [f]
(re-pattern (str f "(.*)\n")))



(defn extract-tokens-from-headers
  [text]
(for [field header-fields]
  (map #(str field %1)
       (mapcat (fn [x] (->> x second (re-seq token-regex)))
               (re-seq (header-token-regex field)
                       text)))))



(defn extract-tokens
  [text]
(filter #(< (count %) 20)
        (apply concat
       (re-seq token-regex text)
       (extract-tokens-from-headers text))))






(defn populate-emails
  "Returns a sequence of vectors of the form [filname type]"
[]
(letfn [(get-email-files [type1]
          (map (fn [f] [(.toString f) (.toString type1)])
               (rest (file-seq (file (str "corpus/" type1))))))]
  (mapcat get-email-files ["ham" "spam"])))


(defn populate-emails1
  []
  (letfn [(get-email-files [type1]
            (map (fn [f] [(.toString f) (.toString type1)])
                 (take 20 (rest (file-seq (file (str "corpus/" type1)))))))]
    (mapcat get-email-files ["ham" "spam"])))


(def hams (file "corpus/ham"))



(def spams (file "corpus/spam"))

(def hams-20 (take 20 (rest (file-seq hams))))

(def spams-20 (take 20 (rest (file-seq spams))))






(defn train
  [t cat]
(doseq [w (extract-tokens t)]
  (incf w cat))
  (incc cat))



(defn train-from-corpus!
  [corpus]
(doseq [v corpus]
  (let [[filename type] v]
    (train (slurp filename) type))))






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






; probability that a word is in particular category
; Pr(word | classification)
(defn fprob
  [f cat]
(if (= (catcount cat) 0)
  0
(float (/ (fcount f cat) (catcount cat)))))





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



(def thresholds (atom {}))


(defn setthreshold
  [cat t]
(swap! thresholds #(assoc % cat t)))


(defn getthreshold
  [cat]
(if (contains? @thresholds cat)
  (get @thresholds cat)
1.0))

(setthreshold "bad" 3)


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



(defn classify-fisher
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













