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

(incf "jumps" "")


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

(catcount "bad")


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

(train "jumps fox" "good")

@fc
@cc



(defn sampletrain
[]
(do
  [(train "Nobody owns the water." "good")
  (train "the quick rabit jumps fences" "good")
  (train "buy pharmaceuticals now" "bad")
  (train "make quick money at the online casino" "bad")
  (train "the quick brown fox jumps" "good")]))

(sampletrain)

@fc

@cc

(defn fprob
  [f cat]
(if (= (catcount cat) 0)
  0
(float (/ (fcount f cat) (catcount cat)))))

(fprob "the" "good")

(fprob "money" "bad")

(defn weightedprob
  [f cat]
(let [weight 1
      ap 0.5
      basicprob (atom 0)
      totals (atom 0)
      bp (atom 0)]
(reset! basicprob (fprob f cat))
(reset! totals (reduce + (vals (get @fc f))))
(reset! bp (/ (+ (* weight ap) (* @totals @basicprob)) (+ weight @totals)))
@bp))

(weightedprob "money" "good")
(weightedprob "money" "bad")

































          















