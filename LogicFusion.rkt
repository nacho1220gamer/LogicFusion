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
   ("\"" (arbno (not #\")) "\"") string)
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
  (expression ("True") true-exp)
  (expression ("False") false-exp)
  (expression (fun-primitive "(" (separated-list expression ",") ")") primfun-exp)
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
  (fun-primitive ("longitud") longitud-prim)
  (fun-primitive ("concatenar") concatenar-prim)
  (fun-primitive ("print") print-prim)
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


;******************************************************************************************
;Referencias

;Definición del tipo de dato para referencias
(define-datatype reference reference?
  (a-ref (position integer?)
         (vec vector?)))

;Desreferenciación
(define deref
  (lambda (ref)
    (primitive-deref ref)))

(define primitive-deref
  (lambda (ref)
    (cases reference ref
      (a-ref (pos vec)
             (vector-ref vec pos)))))

;Setear valor
(define setref!
  (lambda (ref val)
    (primitive-setref! ref val)))

(define primitive-setref!
  (lambda (ref val)
    (cases reference ref
      (a-ref (pos vec)
             (vector-set! vec pos val)))))


;******************************************************************************************
;Ambientes

;Definición del tipo de dato ambiente
(define-datatype environment environment?
  (empty-env-record)
  (extended-env-record
   (syms (list-of symbol?))
   (types (list-of type-exp?))
   (consts (list-of boolean?))
   (vec vector?)
   (env environment?)))

;Ambiente vacío
(define empty-env
  (lambda ()
    (empty-env-record)))


;ambiente extendido
(define extend-env
  (lambda (syms types consts vals env)
    (extended-env-record syms types consts (list->vector vals) env)))

;Ambiente extendido recursivo
(define extend-env-rec
  (lambda (proc-names idss bodies old-env)
    (let ((len (length proc-names)))
      (let ((vec (make-vector len))
            (types (map (lambda (x) (void-type)) proc-names))
            (consts (map (lambda (x) #t) proc-names)))
        (let ((env (extended-env-record proc-names types consts vec old-env)))
          (for-each
           (lambda (pos ids body)
             (vector-set! vec pos (closure ids body env)))
           (iota len) idss bodies)
          env)))))

(define iota
  (lambda (end)
  (let loop ((next 0))
    (if (>= next end) '()
        (cons next (loop (+ 1 next)))))))

;buscar un símbolo en un ambiente
(define apply-env
  (lambda (env sym)
      (deref (apply-env-ref env sym))))

(define apply-env-ref
  (lambda (env sym)
    (cases environment env
      (empty-env-record ()
                        (eopl:error 'apply-env-ref "Variable no declarada: ~s" sym))
      (extended-env-record (syms types consts vec env)
                           (let ((pos (rib-find-position sym syms)))
                             (if (number? pos)
                                 (a-ref pos vec)
                                 (apply-env-ref env sym)))))))

;Consulta de inmutabilidad (const) y tipo registrado en el ambiente
(define apply-env-const?
  (lambda (env sym)
    (cases environment env
      (empty-env-record ()
                        (eopl:error 'apply-env-const? "Variable no declarada: ~s" sym))
      (extended-env-record (syms types consts vec env)
                           (let ((pos (rib-find-position sym syms)))
                             (if (number? pos)
                                 (list-ref consts pos)
                                 (apply-env-const? env sym)))))))

(define apply-env-type
  (lambda (env sym)
    (cases environment env
      (empty-env-record ()
                        (eopl:error 'apply-env-type "Variable no declarada: ~s" sym))
      (extended-env-record (syms types consts vec env)
                           (let ((pos (rib-find-position sym syms)))
                             (if (number? pos)
                                 (list-ref types pos)
                                 (apply-env-type env sym)))))))


;Auxiliares para localizar la posición de un identificador
(define rib-find-position
  (lambda (sym los)
    (list-index (lambda (sym1) (eqv? sym1 sym)) los)))

(define list-index
  (lambda (pred ls)
    (cond
      ((null? ls) #f)
      ((pred (car ls)) 0)
      (else (let ((list-index-r (list-index pred (cdr ls))))
              (if (number? list-index-r)
                (+ list-index-r 1)
                #f))))))


;Ambinete Inicial
(define init-env
  (lambda ()
    (empty-env)))


;******************************************************************************************
;Evaluador


;Evaluar programa
(define eval-program
  (lambda (pgm)
    (cases program pgm
      (a-program (globals body)
                 (eval-expression body (eval-declarations globals (init-env)))))))

;Evaluar expresión
(define eval-expression
  (lambda (exp env)
    (cases expression exp
      (lit-exp (datum) datum)
      (text-exp (txt) (substring txt 1 (- (string-length txt) 1)))
      (true-exp () #t)
      (false-exp () #f)
      (primfun-exp (prim args)
                   (apply-fun-primitive prim
                                        (map (lambda (a) (eval-expression a env)) args)))
      (var-exp (id) (apply-env env id))
      (locals-exp (decls exps)
                  (if (null? decls)
                      (eopl:error 'eval-expression
                                  "LOCALS debe contener al menos una declaracion")
                      (let ((local-env (eval-declarations decls env)))
                        (if (null? exps)
                            (eopl:error 'eval-expression
                                        "LOCALS debe contener al menos una expresion")
                            (let loop ((acc (eval-expression (car exps) local-env))
                                       (exps (cdr exps)))
                              (if (null? exps)
                                  acc
                                  (loop (eval-expression (car exps) local-env)
                                        (cdr exps))))))))
      (primapp-exp (exp1 prim exp2)
                   (cases primitive prim
                     (assign-prim ()
                                  (cases expression exp1
                                    (var-exp (id)
                                             (if (apply-env-const? env id)
                                                 (eopl:error 'eval-expression
                                                             "No se puede modificar la constante: ~s" id)
                                                 (let* ((var-type (apply-env-type env id))
                                                        (new-val (coerce-value var-type
                                                                               (eval-expression exp2 env))))
                                                   (if (validate-type var-type new-val)
                                                       (begin
                                                         (setref! (apply-env-ref env id) new-val)
                                                         new-val)
                                                       (eopl:error 'eval-expression
                                                                   "Tipo incompatible en la asignacion de ~s: se esperaba ~s y se recibio ~s"
                                                                   id (type->symbol var-type) (value->type-symbol new-val))))))
                                    (else
                                     (eopl:error 'eval-expression
                                                 "El lado izquierdo de -> debe ser una variable: ~s" exp1))))
                     (else
                      (eopl:error 'eval-expression
                                  "Primitiva no encontrada: ~s" prim))))
      (else
       (eopl:error 'eval-expression
                   "Expresion no definida: ~s" exp)))))

;Funciones auxiliares para evaluar declaraciones en GLOBALS y LOCALS


;Evaluar declaraciones
(define eval-declarations
  (lambda (decls env)
    (let loop ((decls decls)
               (env env))
      (if (null? decls)
          env
          (loop (cdr decls)
                (eval-declaration (car decls) env))))))


;Evaluar declaración
(define eval-declaration
  (lambda (decl env)
    (cases expression decl
      (var-mut (type id exp)
               (let ((val (coerce-value type (eval-expression exp env))))
                 (if (validate-type type val)
                     (extend-env (list id) (list type) (list #f) (list val) env)
                     (eopl:error 'eval-decl "Tipo incompatible en la declaracion de ~s: se esperaba ~s y se recibio ~s"
                                 id (type->symbol type) (value->type-symbol val)))))
      (var-inmut (type id exp)
                 (let ((val (coerce-value type (eval-expression exp env))))
                   (if (validate-type type val)
                       (extend-env (list id) (list type) (list #t) (list val) env)
                       (eopl:error 'eval-decl "Tipo incompatible en la declaracion const de ~s: se esperaba ~s y se recibio ~s"
                                   id (type->symbol type) (value->type-symbol val)))))
      (proc-exp (id types ids body)
                (extend-env-rec (list id) (list ids) (list body) env))
      (else
       (eopl:error 'eval-decl "Declaracion no valida: ~s" decl)))))

;******************************************************************************************
;Sistema de tipos primitivos y validación parcial

;Valida que un valor sea compatible con un tipo declarado.
(define validate-type
  (lambda (type val)
    (cases type-exp type
      (int-type ()    (and (number? val) (exact? val) (integer? val)))
      (float-type ()  (number? val))
      (string-type () (string? val))
      (bool-type ()   (boolean? val))
      (void-type ()   #t)
      (sat-type ()    #t)
      (list-type (t)  #t)
      (vector-type (t) #t)
      (dict-type (k v) #t))))

;Si la variable es float y el valor es un número exacto como 20 lo vuelve flotante como 20.0 sino devuelve el mismo valor

(define coerce-value
  (lambda (type val)
    (cases type-exp type
      (float-type ()
                  (if (and (number? val) (exact? val))
                      (exact->inexact val)
                      val))
      (else val))))

;Nombre legible de un tipo declarado (para mensajes de error)
(define type->symbol
  (lambda (type)
    (cases type-exp type
      (int-type ()    'int)
      (float-type ()  'float)
      (string-type () 'string)
      (bool-type ()   'bool)
      (void-type ()   'void)
      (sat-type ()    'sat)
      (list-type (t)  'list)
      (vector-type (t) 'vector)
      (dict-type (k v) 'dict))))

;Nombre legible del tipo de un valor en ejecución (para mensajes de error)
(define value->type-symbol
  (lambda (val)
    (cond
      ((boolean? val) 'bool)
      ((string? val) 'string)
      ((and (number? val) (exact? val) (integer? val)) 'int)
      ((number? val) 'float)
      (else 'otro))))

;Cadenas y salida
(define value->text
  (lambda (val)
    (cond
      ((eq? val #t) "True")
      ((eq? val #f) "False")
      ((string? val) val)
      ((number? val) (number->string val))
      (else "<valor>"))))

(define apply-fun-primitive
  (lambda (prim args)
    (cases fun-primitive prim
      (longitud-prim ()
                     (if (and (= (length args) 1) (string? (car args)))
                         (string-length (car args))
                         (eopl:error 'longitud "Espera exactamente una cadena")))
      (concatenar-prim ()
                       (if (and (= (length args) 2) (string? (car args)) (string? (cadr args)))
                           (string-append (car args) (cadr args))
                           (eopl:error 'concatenar "Espera exactamente 2 cadenas")))
      (print-prim ()
                  (begin
                    (for-each (lambda (a) (display (value->text a))) args)
                    (newline)
                    'nulo)))))

;******************************************************************************************
;Pruebas

;Prueba 1: Alcance lexico (GLOBALS -> LOCALS)
;x toma el valor local (99) y y toma el valor global (20). Retorna 99.
(eval-program
 (scan&parse
  "GLOBALS {
     int x = 10;
     const int y = 20;
   }
   PROGRAM {
     proc main() {
       return LOCALS {
         int x = 99;
       } {
         y;
         x;
       }
     }
   }"))

;Prueba 2: Mutacion de variables en memoria con el operador ->
;Modifica la variable global x a 50 y la local z a 30. Retorna 50.
(eval-program
 (scan&parse
  "GLOBALS {
     int x = 10;
   }
   PROGRAM {
     proc main() {
       return LOCALS {
         int z = 5;
       } {
         (x -> 50);
         (z -> 30);
         x;
       }
     }
   }"))

;Prueba 3 Error esperado: Intentar modificar una variable const con ->
;Retorna Error: "No se puede modificar la constante: pi"
#|
(eval-program
 (scan&parse
  "GLOBALS {
     const float pi = 3.14;
   }
   PROGRAM {
     proc main() {
       return LOCALS {
         int a = 1;
       } {
         (pi -> 3.15);
       }
     }
   }"))
|#

;Prueba 4 Error esperado: Consultar una variable no declarada ni en LOCALS ni en GLOBALS
;Retorna Error: "Variable no declarada: w"
#|
(eval-program
 (scan&parse
  "GLOBALS {
     int x = 10;
   }
   PROGRAM {
     proc main() {
       return LOCALS {
         int a = 1;
       } {
         w;
       }
     }
   }"))
|#