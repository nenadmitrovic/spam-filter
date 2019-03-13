(ns spam-filter.routes.home
  (:require [spam-filter.layout :as layout]
            [compojure.core :refer [defroutes GET POST]]
            [ring.util.http-response :as response]
            [clojure.java.io :as io]
            [clojure.string :as st]
            [spam-filter.model.classifier :as cls]))



(defn classify-page []
  (layout/render
   "classifier.html"))

(defn classify1 [{:keys [params]}]
 (layout/render
  "classifier.html"
  (let [text (get params :text)
          res (cls/classify text)]
      {:res (first res)})))



(defn classify [{:keys [params]}]
 (layout/render
  "classifier.html"
(let [task (get params :button)
      text (get params :text)
      cat (get params :class)
      res (if (= task "classify") (cls/classify text))]
(if (= task "train")
  (cls/train text cat))
{:res (first res)})))





(defroutes home-routes
  (GET "/" [] (classify-page))
  (POST "/classify" request (classify request)))


