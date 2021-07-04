(ns adasam.cljlox.cli
  (:require [adasam.cljlox.scanner :as scanner]
            [adasam.cljlox.parser :as parser]
            [adasam.cljlox.interpreter :as interpreter]
            [slingshot.slingshot :refer [throw+ try+]]))

(defn report [line where message]
  (.println *err* (format "[line %s] Error %s: %s"
                          line where message))
  (.flush *err*))

(defn error [err]
  (let [{:keys [line where message]} err]
    (report line where message)))

(defn run [source]
  (try+
    (let [tokens (scanner/scan-tokens source)
          exprs (parser/parse tokens)
          res (interpreter/evaluate exprs)]
      (println res))
    (catch Object e
      (println e))))

(defn run-prompt []
  (print "> ")
  (flush)
  (when-let [in (read-line)]
    (if (= in ":exit")
      nil
      (let [result (run in)
            err (:error result)]
        (when err
          (error err))
        (run-prompt)))))

(defn run-file [file]
  (let [contents (slurp file)
        result (run contents)]
    (when (:error result)
      (System/exit 65))))



(defn -main [& args]
  (case (count args)
    0 (run-prompt)
    1 (run-file (first args))
    (do
      (println "Usage: adasam.cljlox [script]")
      (System/exit 64))))

(comment
  (run-prompt))