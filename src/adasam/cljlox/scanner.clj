(ns adasam.cljlox.scanner
  "Lox scanner; scans source code into a sequence of tokens."
  (:require [clojure.alpha.spec :as s]
            [clojure.alpha.spec.test :as stest]
            [slingshot.slingshot :refer [throw+]]
            [clojure.string :as str]
            [adasam.cljlox.spec]))

(s/def ::tokens (s/coll-of :adasam.cljlox.spec/realized-token :kind vector?))
;;  TODO: spec out the types of errors
(s/def ::errors (s/coll-of map? :kind vector?))
;; TODO: consider replacing indexing with a list-of-chars approach.
;; Would this harm column derivation?
(s/def ::idx (s/and integer? #(not (neg? %))))
(s/def ::line-num (s/and integer? pos?))
(s/def ::source string?)

(s/def ::state (s/select (s/schema {:tokens ::tokens
                                    :errors ::errors
                                    :idx ::idx
                                    :line-num ::line-num
                                    :source ::source})
                         [*]))

(defn- safe-char-at [s i]
  (when (< i (count s))
    (.charAt s i)))

(defn- safe-parse-long [x]
  (try
    (Long/parseLong (str x))
    (catch NumberFormatException _ nil)))

(defn is-alpha-or-underscore [c]
  (and c
       (or (Character/isAlphabetic (int c))
           (= \_ c))))

(defn is-alpha-numeric [c]
  (and c
       (or (is-alpha-or-underscore c) (Character/isDigit ^char c))))

(defn peek-first [state]
  (safe-char-at (:source state) (:idx state)))

(defn peek-second [state]
  (safe-char-at (:source state) (inc (:idx state))))

(defn advance [n state]
  (update state :idx (partial + n)))

(defn consume-as [n token-type state]
  (let [{:keys [source idx line-num]} state
        state' (advance n state)]
    (update state' :tokens conj {:token-type token-type
                                 :lexeme (subs source idx (:idx state'))
                                 :line line-num})))

(defn scan-newline [state]
  (update (advance 1 state) :line-num inc))

(defn scan-string
  [state]
  (let [start-idx (:idx state)
        source (:source state)]
    (loop [state (advance 1 state)]
      (case (peek-first state)

        \"
        (let [fully-consumed (advance 1 state)
              idx (:idx fully-consumed)]
          (update fully-consumed :tokens conj
                  {:token-type :STRING
                   :lexeme     (subs source start-idx idx)
                   :literal    (subs source (inc start-idx) (dec idx))
                   :line       (:line-num fully-consumed)}))

        \newline
        (recur (scan-newline state))

        nil
        (update state :errors conj {:type ::unterminated-string
                                    :line (:line-num state)})

        (recur (advance 1 state))))))

(defn scan-possible-twofer [second-char if-so otherwise]
  (fn [state]
    (if (= second-char (peek-second state))
      (consume-as 2 if-so state)
      (consume-as 1 otherwise state))))


(defn scan-char [token-type]
  (fn [state]
    (consume-as 1 token-type state)))

(defn scan-comment [{:keys [source idx] :as state}]
  (let [newl (str/index-of source \newline idx)
        idx' (if newl newl (count source))]
    (assoc state :idx idx')))

(defn scan-backslash [state]
  (if (= \/ (peek-second state))
    (scan-comment state)
    (consume-as 1 :SLASH state)))

(defn scan-number [state]
  (let [consume-digits (fn [state] (if (safe-parse-long (peek-first state))
                                     (recur (advance 1 state))
                                     state))
        start-idx (:idx state)
        state' (consume-digits state)
        state' (if (and (= \. (peek-first state'))
                        (safe-parse-long (peek-second state')))
                 (consume-digits (advance 1 state'))
                 state')
        text (subs (:source state) start-idx (:idx state'))]
    (update state' :tokens conj {:token-type :NUMBER
                                 :lexeme     text
                                 :literal    (Double/valueOf text)
                                 :line       (:line-num state')})))

(defn scan-identifier [state]
  (let [start-idx (:idx state)
        state' (advance 1 state)
        state' (loop [state state']
                 (let [c (peek-first state)]
                   (if (is-alpha-numeric c)
                     (recur (advance 1 state))
                     state)))
        identifier (subs (:source state) start-idx (:idx state'))
        token-type (get {"and"    :AND
                         "class"  :CLASS
                         "else"   :ELSE
                         "false"  :FALSE
                         "for"    :FOR
                         "fun"    :FUN
                         "if"     :IF
                         "nil"    :NIL
                         "or"     :OR
                         "print"  :PRINT
                         "return" :RETURN
                         "super"  :SUPER
                         "this"   :THIS
                         "true"   :TRUE
                         "var"    :VAR
                         "while"  :WHILE}
                        identifier
                        :IDENTIFIER)]
    (update state' :tokens conj {:token-type token-type
                                 :lexeme     identifier
                                 :line       (:line-num state')})))

(defn err-char [state]
  (update state :errors conj {:type ::unexpected-char
                              :line (:line-num state)}))

(defn scan-token [state]
  (let [c (peek-first state)
        handler (case c
                  \( (scan-char :LEFT_PAREN)
                  \) (scan-char :RIGHT_PAREN)
                  \{ (scan-char :LEFT_BRACE)
                  \} (scan-char :RIGHT_BRACE)
                  \, (scan-char :COMMA)
                  \. (scan-char :DOT)
                  \- (scan-char :MINUS)
                  \+ (scan-char :PLUS)
                  \; (scan-char :SEMICOLON)
                  \* (scan-char :STAR)
                  \! (scan-possible-twofer \= :BANG_EQUAL :BANG)
                  \= (scan-possible-twofer \= :EQUAL_EQUAL :EQUAL)
                  \< (scan-possible-twofer \= :LESS_EQUAL :LESS)
                  \> (scan-possible-twofer \= :GREATER_EQUAL :GREATER)
                  \space (partial advance 1)
                  \return (partial advance 1)
                  \tab (partial advance 1)
                  \/ scan-backslash
                  \newline scan-newline
                  \" scan-string
                  (cond
                    (is-alpha-or-underscore c) scan-identifier
                    (Character/isDigit ^char c) scan-number
                    :else err-char))]
    (handler state)))

(defn done-scanning [{:keys [idx source]}]
  (>= idx (count source)))

(defn -scan-tokens [state]
  (let [{:keys [errors]} state
        done (done-scanning state)
        are-errs (not (empty? errors))]
    (cond
      (and done are-errs) (throw+ {:type ::scan-error :errors errors})
      done (:tokens (consume-as 0 :EOF state))
      :else (recur (scan-token state)))))

(s/fdef -scan-tokens
  :args (s/cat :state ::state))
(stest/instrument `-scan-tokens)


(defn scan-tokens [source]
  (-scan-tokens {:source source
                 :tokens []
                 :errors []
                 :idx 0
                 :line-num 1}))

(comment
  (scan-tokens "!!=()//foo
    !  // bar
    123()
    456.789
    {} // baz
    \"cat\"
    ors
    _sa_nd
    s_and
    and
    99.1
    .11
    else")
  (scan-tokens "")
  (scan-tokens "()/()")
  ,)

