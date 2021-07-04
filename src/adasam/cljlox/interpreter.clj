(ns adasam.cljlox.interpreter
  (:require [slingshot.slingshot :refer [throw+ try+]]))

(defn check-number-operands [operator & operands]
  (if (every? double? operands)
    operands
    (throw+ {:type  ::interpreter-error
             :message "Operators must be numbers."
             :token operator})))

(defmulti evaluate :expr-type)

(defmethod evaluate :literal [expr]
  (:value expr))

(defmethod evaluate :grouping [expr]
  (evaluate (:expression expr)))

(defmethod evaluate :unary [expr]
  (let [right (evaluate (:right expr))
        operator (:operator expr)]
    (case (:token-type operator)
      :MINUS (- (check-number-operands operator right))
      ;; Same truthiness notion as Clojure's
      :BANG (not right))))

(defmethod evaluate :binary [expr]
  (let [{:keys [left right operator]} expr
        left (evaluate left)
        right (evaluate right)
        apply-dubs (fn [operator f]
                     (apply f (check-number-operands operator left right)))]
    (case (:token-type operator)
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
      :BANG_EQUAL (not (apply-dubs =)))))