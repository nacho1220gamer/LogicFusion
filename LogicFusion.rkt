#lang eopl
;******************************************************************************************
;;;;; Interpretador para lenguaje con condicionales, ligadura local, procedimientos y 
;;;;; procedimientos recursivos

;; La definición BNF para las expresiones del lenguaje:
;;
;;  <program>       ::= GLOBALS {{<expression>}*(;)} PROGRAM { proc main() {return <expression>;}} 
;;                      <a-program (globals body)>
;;  <expression>    ::= <number>
;;                      <lit-exp (datum)>
;;                  ::= <text>
;;                      <text-exp (txt)>
;;                  ::= <identifier>
;;                      <var-exp (id)>
;;                  ::= <type-exp> <identifier> = <expression>
;;                      <var-mut (type id exp)>
;;                  ::= const <type-exp> <identifier> = <expression>
;;                      <var-inmut (type id exp)>
;;                  ::= (<expression> <primitive> <expression>)
;;                      <primapp-exp (exp1 prim exp2)>
;;                  ::= proc <identifier> = lambda({{type-exp} {identifier}}*(,)){return <expression>}
;;                      <proc-exp (id types ids body)>
;;                  ::= BLOCK {{expression}*(;)}
;;                      <block-exp (body)>
;;                  ::= LOCALS {{<expression>}*(;)} {{expression}*(;)}
;;                      <locals-exp (exps body)
;;                  ::= for (<expression>; <expression>; <expression>;){{expression}*(;)}
;;                      <for-exp (exp1 exp2 exp3 body)>
;;                  ::= while (<expression>) {{expression}*(;)}
;;                      <while-exp (exp body)>
;;                  ::= if (<expression>) {<expression>} else {<expression>}
;;                      <if-exp (exp1 exp2 exp3)>
;;                  ::= switch (<identifier>) {{case <expression>: {<expression}}* default: {<expression>}}
;;                      <switch-exp (id exps1 exps2)>
;;                  ::= <sat-instance>
;;  <sat-instance>  ::= FNC <int> (<closure-sat> (and <closure-sat>)* )
;;  <closure-sat>   ::= (<literal-sat> (or <literal-sat>)* )
;;  <literal-sat>   ::= <int>
;;  <primitive>     ::= + | - | * | / | == | ! = | > | < | >= | <= | ->
;;  <type-exp>      ::= int | float | string | bool | void | sat 
;;                      list <<type-exp>> | vector <<type-exp>> 
;;                      dict <<type-exp>,<type-exp>>

;******************************************************************************************

;******************************************************************************************
;Especificación Léxica

(define scanner-spec-simple-interpreter
'((white-sp
   (whitespace) skip)
  (comment
   ("%" (arbno (not #\newline))) skip)
  (identifier
   (letter (arbno (or letter digit "?" "_"))) symbol)
  (text
   ("\"" (or letter "_") (arbno (or letter digit "_" ":")) "\"") string)
  (number
   (digit (arbno digit) "." digit (arbno digit)) number)
  (number
   ("-" digit (arbno digit) "." digit (arbno digit)) number)
  (number
   (digit (arbno digit)) number)
  (number
   ("-" digit (arbno digit)) number)))

(define grammar-simple-interpreter
'((program ("GLOBALS" "{" (arbno expression ";") "}" "PROGRAM" "{" "proc" "main" "(" ")" "{" "return" expression "}" "}") a-program)
  (expression (number) lit-exp)
  (expression (text) text-exp)
  (expression (identifier) var-exp)
  (expression (type-exp identifier "=" expression) var-mut)
  (expression ("const" type-exp identifier "=" expression) var-inmut)
  (expression ("(" expression primitive expression ")") primapp-exp)
  (expression ("proc" identifier "=" "lambda" "(" (separated-list type-exp identifier ",")")" "{" "return" expression "}") proc-exp)
  (expression ("BLOCK" "{" (arbno expression ";")"}") block-exp)
  (expression ("LOCALS" "{" (arbno expression ";") "}" "{" (arbno expression ";") "}") locals-exp)
  (expression ("for" "(" expression ";" expression ";" expression ")" "{" (arbno expression ";")"}") for-exp)
  (expression ("while" "(" expression ")" "{" (arbno expression ";") "}") while-exp)
  (expression ("if" "(" expression ")" "{" expression "}" "else" "{" expression "}") if-exp)
  (expression ("switch" identifier "{" (arbno "case" expression ":" "{" expression "}") "default" ":" "{" expression "}" "}") switch-exp)
  (expression (sat-instance) sat-exp)  
  (sat-instance ("FNC" number "(" sat-closure (arbno "and" sat-closure)")") sat-fnc)
  (sat-closure ("(" sat-literal (arbno "or" sat-literal)")") sat-and)
  (sat-literal (number) sat-or)
  (primitive ("+") add-prim)
  (primitive ("-") sub-prim)
  (primitive ("*") mult-prim)
  (primitive ("/") div-prim)
  (primitive ("==") eq-prim)
  (primitive ("!=") dif-prim)
  (primitive (">") greater-prim)
  (primitive ("<") less-prim)
  (primitive (">=") greater-eq-prim)
  (primitive ("<=") less-eq-prim)
  (primitive ("->") assign-prim)
  (type-exp ("int") int-type)
  (type-exp ("float") float-type)
  (type-exp ("string") string-type)
  (type-exp ("bool") bool-type)
  (type-exp ("void") void-type)
  (type-exp ("sat") sat-type)
  (type-exp ("list" "<" type-exp ">") list-type)
  (type-exp ("vector" "<" type-exp ">") vector-type)
  (type-exp ("dict" "<" type-exp "," type-exp ">") dict-type))
  )

;Construidos automáticamente:

(sllgen:make-define-datatypes scanner-spec-simple-interpreter grammar-simple-interpreter)

(define show-the-datatypes
  (lambda () (sllgen:list-define-datatypes scanner-spec-simple-interpreter grammar-simple-interpreter)))

;El FrontEnd (Análisis léxico (scanner) y sintáctico (parser) integrados)

(define scan&parse
  (sllgen:make-string-parser scanner-spec-simple-interpreter grammar-simple-interpreter))

;El Analizador Léxico (Scanner)

(define just-scan
  (sllgen:make-string-scanner scanner-spec-simple-interpreter grammar-simple-interpreter))

;El Interpretador (FrontEnd + Evaluación + señal para lectura )

(define interpretador
  (sllgen:make-rep-loop  "--> "
    (lambda (pgm) (eval-program  pgm)) 
    (sllgen:make-stream-parser 
      scanner-spec-simple-interpreter
      grammar-simple-interpreter)))

;El Interprete
(define eval-program
  (lambda (pgm)
    (cases program pgm
      (a-program (globals body)
                 (eval-expression body (init-env))))))

