(ns adasam.cljlox.parser
  (:require [clojure.alpha.spec :as s]
            [slingshot.slingshot :refer [throw+ try+]]
            [adasam.cljlox.spec]))

(comment
  "Things I've come across while working on this:
  - Desire to spec my central data model (`state` in the scanner,
    as well as `token`), both across boundaries and internally for
    debugging during dev.
  - Uncertainty how to deal with errors. To use exceptions? Monads?
    How to aggregate them?
  - Desire to improve the code; e.g. not call my CLI ('report-error')
    methods from my core scanner, parser. DATA is the interface! He
    mentions passing in an `ErrorReporter` class, but who in their
    right mind would do that rather than just... return the data of
    what errors were encountered, and let *that* be your interface,
    and let the client do anything in the world with that data.
  ")

(comment
  "TODO:
  - Cut over to the orig. `spec`, use orchestra?
  - Spec out ::expression
  - Spec out errors? Loosely?
  ")

(s/def ::state (s/cat :expr :adasam.cljlox.spec/expression
                      :tokens (s/coll-of :adasam.cljlox.spec/realized-token)))


;; Required due to mutual recursion:
;;  expression =calls=> equality => ... => expression
(declare expression)

(defn consume [tok-type tokens error]
  (let [[tok & more-toks :as tokens] tokens]
    (if (= tok-type (:token-type tok))
      [tok more-toks]
      (throw+ (assoc error :type :parse-error)))))

(defn synchronize [tokens]
  (loop [[tok & more-tokens :as tokens] tokens]
    (condp #(%1 %2) (:token-type tok)
      #{:SEMICOLON} more-tokens
      #{:CLASS
        :FUN
        :VAR
        :FOR
        :IF
        :WHILE
        :PRINT
        :RETURN
        :EOF} tokens
      nil? (do
             (println "TODO: synchronization error")
             [])
      (recur more-tokens))))

(defn primary [tokens]
  {:post [(vector? %)]}
  (let [literal #(identity {:expr-type :literal
                            :value %})
        [tok & more-toks] tokens]
    (condp #(%1 %2) (:token-type tok)
      #{:FALSE} [(literal false) more-toks]
      #{:TRUE} [(literal true) more-toks]
      #{:NIL} [(literal nil) more-toks]
      #{:NUMBER :STRING} [(literal (:literal tok)) more-toks]

      #{:IDENTIFIER} [{:expr-type :variable, :name tok}
                      more-toks]

      #{:LEFT_PAREN}
      (let [[expr [tok & more-toks :as tokens]] (expression more-toks)]
        (if (= :RIGHT_PAREN (:token-type tok))
          [{:expr-type :grouping
            :expression expr}
           more-toks]
          (throw+ {:type :parse-error
                   :token tok
                   :expected :RIGHT_PAREN
                   :tokens tokens})))

      (throw+ {:type :parse-error
               :message "Could not parse primary."
               :token tok
               :tokens tokens}))))

(defn unary [tokens]
  (let [[tok & more-toks] tokens]
    (if (#{:BANG :MINUS} (:token-type tok))
      (let [[expr more-toks] (unary more-toks)]
        [{:expr-type :unary
          :operator tok
          :right expr}
         more-toks])
      (primary tokens))))


(defn left-associative-binary-parser [token-types parse-operand]
  (fn [tokens]
    (let [[left tokens] (parse-operand tokens)]
      (loop [left left
             tokens tokens]
        (let [[tok & more-toks] tokens]
          (if (token-types (:token-type tok))
            (let [[right tokens] (parse-operand more-toks)]
              (recur {:expr-type :binary
                      :operator tok
                      :left left
                      :right right}
                     tokens))
            [left tokens]))))))

(def factor (left-associative-binary-parser
              #{:SLASH :STAR}
              unary))

(def term (left-associative-binary-parser
            #{:MINUS :PLUS}
            factor))

(def comparison (left-associative-binary-parser
                  #{:GREATER :GREATER_EQUAL :LESS :LESS_EQUAL}
                  term))

(def equality (left-associative-binary-parser
                #{:BANG_EQUAL :EQUAL_EQUAL}
                comparison))

(defn assignment [tokens]
  (let [[expr [tok & more-toks :as tokens]] (equality tokens)]
    (if (= :EQUAL (:token-type tok))
      (let [[value tokens] (assignment more-toks)]
        (if (= :variable (:expr-type expr))
          [{:expr-type :assign, :name (:name expr), :value value}
           tokens]
          (do
            (println "TODO: report error: invalid assignment target.")
            [expr tokens])))
      [expr tokens])))


(defn expression [tokens]
  (assignment tokens))

(defn print-statement [tokens]
  (let [[value tokens] (expression tokens)
        [_ tokens] (consume :SEMICOLON
                            tokens
                            {:message "';' must follow a print value."})]
    [{:stmt-type :print, :expression value}
     tokens]))

(defn expression-statement [tokens]
  (let [[expression tokens] (expression tokens)
        [_ tokens] (consume :SEMICOLON
                            tokens
                            {:message "';' must follow an expression."})]
    [{:stmt-type :expression, :expression expression}
     tokens]))

(defn var-declaration [tokens]
  (let [[name [tok & more-toks]] (consume :IDENTIFIER
                                          tokens
                                          {:message "Expected a variable name."})
         stmt {:stmt-type :var, :name name}]
    (if (= :EQUAL (:token-type tok))
      (let [[expression tokens] (expression more-toks)
            [_ tokens] (consume :SEMICOLON tokens {:message "Expected semicolon."})]
        [(assoc stmt :initializer expression) tokens])
      [stmt (second (consume :SEMICOLON more-toks {:message "Expected semicolon."}))])))


(declare statement)
(defn declaration [[tok & more-toks :as tokens]]
  (try+
    (if (= :VAR (:token-type tok))
      (var-declaration more-toks)
      (statement tokens))
    (catch Object e
      (prn e)
      [nil (synchronize tokens)])))


(defn block [tokens]
  (loop [[tok & _ :as tokens] tokens
         statements []]
    (if (and (not (#{:RIGHT_BRACE :EOF} (:token-type tok)))
             (not (nil? (:token-type tok))))
      (let [[stmt tokens] (declaration tokens)]
        (recur tokens (conj statements stmt)))
      (let [[_ tokens] (consume :RIGHT_BRACE tokens {:message "Expected right brace."})]
        [statements tokens]))))



(defn statement [[tok & more-toks :as tokens]]
  (case (:token-type tok)
    :PRINT (print-statement more-toks)
    :LEFT_BRACE (let [[statements tokens] (block more-toks)]
                  ;; TODO put this into `block`
                  [{:stmt-type :block
                    :statements statements}
                   tokens])
    (expression-statement tokens)))


(defn parse [tokens]
  (loop [statements []
         [tok & _ :as tokens] tokens]
    (cond
      (nil? tok) (throw+ {:type :parse-error
                          :message "Expected EOF token."})
      (= :EOF (:token-type tok)) statements
      :else (let [[statement tokens] (declaration tokens)
                  statements (if (some? statement)
                               (conj statements statement)
                               statements)]
              (recur statements tokens)))))


