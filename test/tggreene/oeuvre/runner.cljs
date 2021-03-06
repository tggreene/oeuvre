(ns tggreene.oeuvre.runner
  (:require [cljs.test :as test]
            [cljs.nodejs :as nodejs]
            tggreene.oeuvre.core-test))

(nodejs/enable-util-print!)

(def status (atom nil))

(defn -main []
  (test/run-all-tests #"^tggreene.oeuvre.*-test$")
  (js/process.exit @status))

(defmethod test/report [:cljs.test/default :end-run-tests] [m]
  (reset! status (if (test/successful? m) 0 1)))

(set! *main-cli-fn* -main)
