(ns spam-filter.config
  (:require [selmer.parser :as parser]
            [clojure.tools.logging :as log]
            [spam-filter.dev-middleware :refer [wrap-dev]]))

(def defaults
  {:init
   (fn []
     (parser/cache-off!)
     (log/info "\n-=[spam-filter started successfully using the development profile]=-"))
   :middleware wrap-dev})
