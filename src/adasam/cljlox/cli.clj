(ns adasam.cljlox.cli
  (:require [adasam.cljlox.scanner :as scanner]
            [adasam.cljlox.parser :as parser]
            [adasam.cljlox.interpreter :as interpreter]
            [slingshot.slingshot :refer [throw+ try+]]))

(comment
  "
  TODO:
  - Get easy-to-run tests in place
  - Refactor function by function
  - Do one of the chapter 8 challenges
  - Tighten up my specs; consider switching to spec 1 + orchestra
  - Revisit error handling and solidify my approach
  - Update doc
  ")

(defn report [line where message]
  (.println *err* (format "[line %s] Error %s: %s"
                          line where message))
  (.flush *err*))

(defn error [err]
  (let [{:keys [line where message]} err]
    (report line where message)))

(defn run
  ([source] (run source [{}]))
  ([source env]
   (try+
     (let [tokens (scanner/scan-tokens source)
           statements (parser/parse tokens)]
       (interpreter/interpret statements env))
     #_(catch Object e
         (println e)))))

(defn run-prompt
  ([] (run-prompt [{}]))
  ([env]
   (print "> ")
   (flush)
   (when-let [in (read-line)]
     (if (= in ":exit")
       nil
       (let [env (run in env)]
         (run-prompt env))))))

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
  (run "var a = \"global a\";
        var b = \"global b\";
        var c = \"global c\";
        {
          var a = \"outer a\";
          var b = \"outer b\";
          {
            var a = \"inner a\";
            print a;
            print b;
            print c;
          }
          print a;
          print b;
          print c;
        }
        print a;
        print b;
        print c;")
  (run "var a=1;
        var b=2;
        print a+b;
        a=2;
        var c=a+b;
        print c;"))