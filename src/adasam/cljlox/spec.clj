(ns adasam.cljlox.spec
  (:require [clojure.alpha.spec :as s]))

(s/def ::token-type #{; Single-character tokens.
                      :LEFT_PAREN :RIGHT_PAREN :LEFT_BRACE :RIGHT_BRACE
                      :COMMA :DOT :MINUS :PLUS :SEMICOLON :SLASH :STAR

                      ; One or two character tokens.
                      :BANG :BANG_EQUAL
                      :EQUAL :EQUAL_EQUAL
                      :GREATER :GREATER_EQUAL
                      :LESS :LESS_EQUAL

                      ; Literals.
                      :IDENTIFIER :STRING :NUMBER

                      ; Keywords.
                      :AND :CLASS :ELSE :FALSE :FUN :FOR :IF :NIL :OR
                      :PRINT :RETURN :SUPER :THIS :TRUE :VAR :WHILE

                      :EOF})

(s/def ::token (s/schema {:token-type ::token-type
                          :lexeme string?
                          :literal (s/or :literal-string string?
                                         :literal-num number?)
                          :line integer?}))

(s/def ::realized-token
  (s/or :literal-token (s/select :adasam.cljlox.spec/token [*])
        :plain-token (s/select :adasam.cljlox.spec/token [:token-type
                                                          :lexeme
                                                          :line])))