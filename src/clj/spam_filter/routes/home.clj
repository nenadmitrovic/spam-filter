
(ns spam-filter.routes.home
  (:require [spam-filter.layout :as layout]
            [compojure.core :refer [defroutes GET]]
            [ring.util.http-response :as response]
            [clojure.java.io :as io]
            [clojure.string :as st]))

(defn home-page []
  (layout/render
    "home.html" {:docs (-> "docs/docs.md" io/resource slurp)}))



(def doc "The reason Bayesian classifiers are often used for document classification is that they require far less computing power than other methods do. An email message might have hundreds or even thousands of words in it, and simply updating the counts takes vastly less memory and processor cycles than training a neural network of that size does; as shown, it can be done entirely within a database. Depending on the speed required for training and querying, and on the environment in which it is run, a neural network may be a viable alternative. ")



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

;; Promeniti sa update-in funkcijom

(defn incf
  "This function increases the count of feature/category pair"
  [f cat fc]
  (let [map1 (if (= (get fc f) nil)
               (assoc fc f {})
               fc)]
(if (get-in map1 [f cat])
  (assoc-in map1 [f cat] (inc (get-in map1 [f cat])))
  (do
    (let [new-val (assoc-in map1 [f cat] 0)]
      (assoc-in new-val [f cat] (inc (get-in new-val [f cat]))))))))


(def counter (agent 0))

(send counter inc)

@counter

(def ^:private feature-db
  (agent {} :error-handler #(println "Error: " %2)))

@feature-db


(defn about-page []
  (layout/render "about.html"))

(defroutes home-routes
  (GET "/" [] (home-page))
  (GET "/about" [] (about-page)))

