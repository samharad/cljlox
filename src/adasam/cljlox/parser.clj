(ns adasam.cljlox.parser
  (:require [clojure.alpha.spec :as s]
            [slingshot.slingshot :refer [throw+ try+]]
            [adasam.cljlox.spec]))

(defmacro section [name & forms]
  (cons 'do forms))

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

  HERE'S WHAT'LL HAPPEN HERE:

  The expression-parser funcs need to... Hmmmm... I don't want
  to complicate the state model by having intermediate :exprs []
  accumulation.

  These methods could either:
    - return [expr tokens]
    - throw an error


  Macro view (statement parsing level):
  {:tokens []
   :statements []
   :errors []}

  But at the expression parsing level, we need to return:
  [expr tokens]
  Or else throw an exception :( and the caller can do the sync.

  And, perhaps we can get away with *just* a top-level catch
  out in the main parse-statement loop?

  I guess I need to understand where all we synchronize --
  where *don't* we throw out the entire statement none-the-wiser?

  ")

(s/def ::state (s/cat :expr :adasam.cljlox.spec/expression
                      :tokens (s/coll-of :adasam.cljlox.spec/realized-token)))


;; Required due to mutual recursion:
;;  expression =calls=> equality => ... => expression
(declare expression)

(defn synchronize-tokens [tokens]
  ;; Eat one token off the bat
  (let [tokens (rest tokens)]
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
        (recur more-tokens)))))

(defn synchronize [state]
  (update state :tokens synchronize-tokens))

(defn consume [tok-type error state]
  (let [{:keys [tokens]} state
        [tok & _] tokens]
    (if (= tok-type (:token-type tok))
      [tok (update state :tokens rest)]
      [nil (-> state
               (update :errors conj error)
               (synchronize))])))

#_(defn consume [tok-type tokens error]
    (let [[tok & more-toks :as tokens] tokens]
      (if (= tok-type (:token-type tok))
        [tok more-toks]
        (throw+ (assoc error :type :parse-error)))))

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

;; TODO: left off here, replacing a `tokens` argument with `state`
(defn var-declaration [state]
  (let [[name state] (consume :IDENTIFIER
                              {:message "Expected a variable name."}
                              state)
        [tok & _] (:tokens state)]
    (cond
      (not name) state

      (= :EQUAL (:token-type tok))
      (let [[expression tokens] (expression more-toks)]))

    #_(if name
        (let [stmt {:stmt-type :var :name name}])
        state)))
    ;     stmt {:stmt-type :var, :name name}]
    ;(if (= :EQUAL (:token-type tok))
    ;  (let [[expression tokens] (expression more-toks)
    ;        [_ tokens] (consume :SEMICOLON tokens {:message "Expected semicolon."})]
    ;    [(assoc stmt :initializer expression) tokens])
    ;  [stmt (second (consume :SEMICOLON more-toks {:message "Expected semicolon."}))])))


(declare statement)
(defn declaration [{:keys [tokens] :as state}]
  (let [[tok & _] tokens]
    (if (= :VAR (:token-type tok))
      (var-declaration (update state :tokens rest))
      (statement state)))
  #_(catch Object e
      (prn e)
      [nil (synchronize-tokens tokens)]))


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
  (loop [state {:tokens tokens
                :statements []
                :errors []}]
    (let [tok (get-in state [:tokens first])]
      (cond
        (nil? tok)
        (-> state
            (update :errors conj {:type :parse-error
                                  :message "Expected EOF token."})
            (dissoc :tokens))

        (= :EOF (:token-type tok))
        (dissoc state :tokens)

        :else (let [state (declaration state)]
                (recur state))))))


