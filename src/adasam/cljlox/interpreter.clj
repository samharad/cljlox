(ns adasam.cljlox.interpreter
  (:require [slingshot.slingshot :refer [throw+ try+]]))

(defn check-number-operands [operator & operands]
  (if (every? double? operands)
    operands
    (throw+ {:type  ::interpreter-error
             :message "Operators must be numbers."
             :token operator})))


(defn env-get [token envs]
  (let [var-name (:lexeme token)
        value (reduce (fn [_ env]
                        (let [value (get env var-name :not-present)]
                          (if (= value :not-present)
                            value
                            (reduced value))))
                      :not-present
                      envs)]
    (if (= value :not-present)
      (throw+ {:type ::interpreter-error
               :message (str "Undefined variable '" var-name "'.")
               :token token})
      value)))

(defn env-assign [token value [env & more-envs :as envs]]
  (let [var-name (:lexeme token)
        [env & more-envs :as envs] envs]
    (cond
      (contains? env var-name) (cons (assoc env var-name value) more-envs)
      (nil? env) (throw+ {:type ::interpreter-error
                          :message (str "Undefined variable '" var-name "'.")
                          :token token})
      :else (cons env (env-assign token value more-envs)))))

(defn env-set [var-name value [env & more-envs :as envs]]
  (cons (assoc env var-name value) more-envs))




(defmulti evaluate (fn [expr _] (:expr-type expr)))

(defmethod evaluate :literal [expr envs]
  [(:value expr) envs])

(defmethod evaluate :grouping [expr envs]
  (evaluate (:expression expr) envs))

(defmethod evaluate :unary [expr envs]
  (let [[right envs] (evaluate (:right expr) envs)
        operator (:operator expr)
        value (case (:token-type operator)
                :MINUS (- (check-number-operands operator right))
                ;; Same truthiness notion as Clojure's
                :BANG (not right))]
    [value envs]))

(defmethod evaluate :binary [expr envs]
  (let [{:keys [left right operator]} expr
        [left envs] (evaluate left envs)
        [right envs] (evaluate right envs)
        apply-dubs (fn [f]
                     (apply f (check-number-operands operator left right)))
        value (case (:token-type operator)
                :MINUS (apply-dubs -)
                :SLASH (apply-dubs /)
                :STAR (apply-dubs *)
                :PLUS (let [ts [(type left) (type right)]]
                        (if (= ts [Double Double])
                          (+ left right)
                          (if (= ts [String String])
                            (str left right)
                            (throw+ {:type ::interpreter-error
                                     :message "Operands must be numbers or strings."
                                     :token operator}))))
                :GREATER (apply-dubs >)
                :GREATER_EQUAL (apply-dubs >=)
                :LESS (apply-dubs <)
                :LESS_EQUAL (apply-dubs <=)
                ;; Same equality notion as Clojure's
                :EQUAL_EQUAL (apply-dubs =)
                :BANG_EQUAL (not (apply-dubs =)))]
    [value envs]))

(defmethod evaluate :variable [expr envs]
  (let [token (:name expr)
        var-name (:lexeme token)
        value (env-get token envs)]
    (if (= value :not-present)
      (throw+ {:type ::interpreter-error
               :message (str "Undefined variable '" var-name "'.")
               :token token})
      [value envs])))

(defmethod evaluate :assign [expr envs]
  (let [[value envs] (evaluate (:value expr) envs)
        token (:name expr)
        envs (env-assign token value envs)]
    [value envs]))





(defn stringify [x]
  (cond
    (nil? x) "nil"
    (double? x) (let [v (.toString x)]
                  (if (.endsWith v ".0")
                    (subs v 0 (- (count v) 2))
                    v))
    :else (.toString x)))




(defmulti execute (fn [stmt _] (:stmt-type stmt)))

(defmethod execute :expression [stmt envs]
  (second (evaluate (:expression stmt) envs)))

(defmethod execute :print [stmt envs]
  (let [[value envs] (evaluate (:expression stmt) envs)]
    (println (stringify value))
    envs))

(defmethod execute :var [stmt envs]
  (let [initializer (:initializer stmt)
        [value envs] (if initializer
                       (evaluate initializer envs)
                       [nil envs])
        var-name (-> stmt :name :lexeme)]
    (env-set var-name value envs)))

(declare interpret)

(defmethod execute :block [stmt envs]
  (let [envs (interpret (:statements stmt) (cons {} envs))]
    (rest envs)))



(defn interpret
  ([statements] (interpret statements [{}]))
  ([statements envs]
   (try+
     (reduce (fn [envs statement]
               (execute statement envs))
             envs
             statements))))