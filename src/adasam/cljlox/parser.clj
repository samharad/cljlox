(ns adasam.cljlox.parser
  (:require [clojure.alpha.spec :as s]
            [slingshot.slingshot :refer [throw+ try+]]))

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

(defn expression [tokens]
  (equality tokens))

(defn parse [tokens]
  (try+
    (let [[expr [tok & more-tokens :as tokens]] (expression tokens)]
      (cond
        (nil? tok) (throw+ {:type :parse-error
                            :message "Expected EOF token."})
        (or (some? more-tokens)
            (not= :EOF (:token-type tok))) (throw+ {:type :parse-error
                                                    :message "Extra tokens."
                                                    :tokens tokens})
        :else expr))
    (catch [:type :parse-error] e
      (prn (format "Error: %s" e))
      (throw+))))

(let [res (parse [{:token-type :NUMBER :literal 1}
                  {:token-type :BANG_EQUAL}
                  {:token-type :MINUS}
                  {:token-type :NUMBER :literal 1}
                  {:token-type :EOF}])]
  (s/valid? :adasam.cljlox.spec/realized-expr res)
  (clojure.pprint/pprint res))

