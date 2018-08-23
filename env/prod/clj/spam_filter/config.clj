(ns spam-filter.config
  (:require [clojure.tools.logging :as log]))

(def defaults
  {:init
   (fn []
     (log/info "\n-=[spam-filter started successfully]=-"))
   :middleware identity})
