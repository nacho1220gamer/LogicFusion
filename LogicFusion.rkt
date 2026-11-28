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
  (expression ("empty") empty-exp)
  (expression ("'" "(" (separated-list expression ",") ")") list-exp)
  (expression ("[" (separated-list expression ",") "]") vector-exp)
  (expression ("{" (separated-list expression ":" expression ",") "}") dict-exp)
  (expression (fun-primitive "(" (separated-list expression ",") ")") primfun-exp)
  (expression (identifier var-prim) var-exp)
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
  (fun-primitive ("empty?") empty?-prim)
  (fun-primitive ("list?") list?-prim)
  (fun-primitive ("make-list") make-list-prim)
  (fun-primitive ("head") head-prim)
  (fun-primitive ("tail") tail-prim)
  (fun-primitive ("append") append-prim)
  (fun-primitive ("ref-list") ref-list-prim)
  (fun-primitive ("length") length-prim)
  (fun-primitive ("create-dict") create-dict-prim)
  (fun-primitive ("dict?") dict?-prim)
  (fun-primitive ("ref-dict") ref-dict-prim)
  (fun-primitive ("set-dict") set-dict-prim)
  (fun-primitive ("keys") keys-prim)
  (fun-primitive ("values") values-prim)
  (fun-primitive ("vector?") vector?-prim)
  (fun-primitive ("make-vector") make-vector-prim)
  (fun-primitive ("ref-vector") ref-vector-prim)
  (fun-primitive ("set-vector") set-vector-prim)
  (fun-primitive ("append-vector") append-vector-prim)
  (fun-primitive ("delete-val-vector") delete-val-vector-prim)
  (fun-primitive ("insert-val-vector") insert-val-vector-prim)
  (fun-primitive ("solve") solve-prim)
  (type-exp ("int") int-type)
  (type-exp ("float") float-type)
  (type-exp ("string") string-type)
  (type-exp ("bool") bool-type)
  (type-exp ("void") void-type)
  (type-exp ("sat") sat-type)
  (type-exp ("list" "<" type-exp ">") list-type)
  (type-exp ("vector" "<" type-exp ">") vector-type)
  (type-exp ("dict" "<" type-exp "," type-exp ">") dict-type)
  (var-prim () simple-var)
  (var-prim ("(" (separated-list expression ",") ")") app-exp))
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
                   (apply-fun-primitive prim args env))
      (var-exp (id v-prim)
               (cases var-prim v-prim
                 (simple-var () (apply-env env id))
                 (app-exp (vals)
                          (let ((proc (apply-env env id))
                                (args (eval-rands vals env)))
                            (if (procval? proc)
                                (apply-procedure proc args)
                                (eopl:error 'eval-expression
                                            "No es un procedimiento ~s" proc))))))
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
                              (var-exp (id v-prim)
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
                (let ((v1 (eval-expression exp1 env))
                      (v2 (eval-expression exp2 env)))
                  (cases primitive prim
                    (add-prim ()
                              (if (and (number? v1) (number? v2))
                                  (+ v1 v2)
                                  (eopl:error 'eval-expression "Operandos no numericos en +")))
                    (sub-prim ()
                              (if (and (number? v1) (number? v2))
                                  (- v1 v2)
                                  (eopl:error 'eval-expression "Operandos no numericos en -")))
                    (mult-prim ()
                               (if (and (number? v1) (number? v2))
                                   (* v1 v2)
                                   (eopl:error 'eval-expression "Operandos no numericos en *")))
                    (div-prim ()
                              (if (and (number? v1) (number? v2))
                                  (if (zero? v2)
                                      (eopl:error 'eval-expression "Division por cero")
                                      (/ v1 v2))
                                  (eopl:error 'eval-expression "Operandos no numericos en /")))
                    (eq-prim ()        (equal? v1 v2))
                    (dif-prim ()       (not (equal? v1 v2)))
                    (greater-prim ()
                                  (if (and (number? v1) (number? v2))
                                      (> v1 v2)
                                      (eopl:error 'eval-expression "Operandos no numericos en >")))
                    (less-prim ()
                               (if (and (number? v1) (number? v2))
                                   (< v1 v2)
                                   (eopl:error 'eval-expression "Operandos no numericos en <")))
                    (greater-eq-prim ()
                                     (if (and (number? v1) (number? v2))
                                         (>= v1 v2)
                                         (eopl:error 'eval-expression "Operandos no numericos en >=")))
                    (less-eq-prim ()
                                  (if (and (number? v1) (number? v2))
                                      (<= v1 v2)
                                      (eopl:error 'eval-expression "Operandos no numericos en <=")))
                    (else
                     (eopl:error 'eval-expression "Primitiva no reconocida")))))))
      (block-exp (exps)
           (if (null? exps)
               'nulo
               (let loop ((acc (eval-expression (car exps) env))
                          (rest (cdr exps)))
                 (if (null? rest)
                     acc
                     (loop (eval-expression (car rest) env)
                           (cdr rest))))))
      (proc-exp (id types ids body)
          (closure ids body env))
      (if-exp (test-exp true-exp false-exp)
        (if (eval-expression test-exp env)
            (eval-expression true-exp env)
            (eval-expression false-exp env)))
      (empty-exp () '())
      (list-exp (exps)
                (map (lambda (e) (eval-expression e env)) exps))
      (vector-exp (exps)
                  (list->vector (map (lambda (e) (eval-expression e env)) exps)))
      (dict-exp (keys vals)
                (build-dict
                 (map (lambda (k) (eval-expression k env)) keys)
                 (map (lambda (v) (eval-expression v env)) vals)))
      (else
       (eopl:error 'eval-expression
                   "Expresion no definida: ~s" exp)))))

(define eval-rands
  (lambda (rands env)
    (map (lambda (x) (eval-expression x env)) rands)))

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

;Auxiliar para verificar que todos los elementos de una lista cumplan una condicion
(define all-elements-valid?
  (lambda (pred lst)
    (cond
      ((null? lst) #t)
      ((pred (car lst)) (all-elements-valid? pred (cdr lst)))
      (else #f))))

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
      (list-type (t)
                 (and (list? val)
                      (all-elements-valid? (lambda (elem) (validate-type t elem)) val)))
      (vector-type (t)
                   (and (vector? val)
                        (all-elements-valid? (lambda (elem) (validate-type t elem))
                                             (vector->list val))))
      (dict-type (k v)
                 (and (dictionary? val)
                      (cases dictionary val
                        (a-dict (pairs)
                                (all-elements-valid?
                                 (lambda (p)
                                   (and (validate-type k (car p))
                                        (validate-type v (cdr p))))
                                 pairs))))))))

;Si la variable es float y el valor es un número exacto como 20 lo vuelve flotante como 20.0 sino devuelve el mismo valor

(define coerce-value
  (lambda (type val)
    (cases type-exp type
      (float-type ()
                  (if (and (number? val) (exact? val))
                      (exact->inexact val)
                      val))
      (list-type (t)
                 (if (list? val)
                     (map (lambda (elem) (coerce-value t elem)) val)
                     val))
      (vector-type (t)
                   (if (vector? val)
                       (list->vector
                        (map (lambda (elem) (coerce-value t elem)) (vector->list val)))
                       val))
      (dict-type (k v)
                 (if (dictionary? val)
                     (cases dictionary val
                       (a-dict (pairs)
                               (a-dict
                                (map (lambda (p)
                                       (cons (coerce-value k (car p))
                                             (coerce-value v (cdr p))))
                                     pairs))))
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
      ((list? val) 'list)
      ((vector? val) 'vector)
      ((dictionary? val) 'dict)
      (else 'otro))))

;Cadenas y salida
(define value->text
  (lambda (val)
    (cond
      ((eq? val #t) "True")
      ((eq? val #f) "False")
      ((string? val) val)
      ((number? val) (number->string val))
      ((list? val)
       (string-append "("
                      (let loop ((l val))
                        (cond
                          ((null? l) "")
                          ((null? (cdr l)) (value->text (car l)))
                          (else (string-append (value->text (car l)) " " (loop (cdr l))))))
                      ")"))
      ((vector? val)
       (string-append "["
                      (let loop ((l (vector->list val)))
                        (cond
                          ((null? l) "")
                          ((null? (cdr l)) (value->text (car l)))
                          (else (string-append (value->text (car l)) ", " (loop (cdr l))))))
                      "]"))
      ((dictionary? val)
       (cases dictionary val
         (a-dict (pairs)
                 (string-append "{"
                                (let loop ((ps pairs))
                                  (cond
                                    ((null? ps) "")
                                    ((null? (cdr ps))
                                     (string-append (value->text (caar ps)) ": " (value->text (cdar ps))))
                                    (else
                                     (string-append (value->text (caar ps)) ": " (value->text (cdar ps))
                                                    ", " (loop (cdr ps))))))
                                "}"))))
      (else "<valor>"))))

;******************************************************************************************
;Estructuras de Datos (Listas, Vectores y Diccionarios)

;Tipo de dato diccionario
(define-datatype dictionary dictionary?
  (a-dict (pairs (list-of pair?))))

;Construye un diccionario a partir de una lista de claves y una de valores
(define build-dict
  (lambda (keys vals)
    (let loop ((ks keys) (vs vals) (res '()))
      (if (null? ks)
          (a-dict (reverse res))
          (loop (cdr ks)
                (cdr vs)
                (put-pair res (car ks) (car vs)))))))

;Inserta o actualiza una clave en una lista de pares (claves unicas)
(define put-pair
  (lambda (lst k v)
    (cond
      ((null? lst) (list (cons k v)))
      ((equal? (caar lst) k) (cons (cons k v) (cdr lst)))
      (else (cons (car lst) (put-pair (cdr lst) k v))))))

;Busca una clave en el diccionario
(define lookup-dict
  (lambda (dict k)
    (cases dictionary dict
      (a-dict (pairs)
              (let ((entry (assoc k pairs)))
                (if entry
                    (cdr entry)
                    (eopl:error 'ref-dict "Clave no encontrada en el diccionario: ~s" k)))))))

;Actualiza o agrega una clave en el diccionario
(define update-dict
  (lambda (dict k v)
    (cases dictionary dict
      (a-dict (pairs)
              (a-dict (put-pair pairs k v))))))

;Funciones auxiliares para vectores
(define check-index
  (lambda (who idx len)
    (if (and (integer? idx) (exact? idx) (>= idx 0) (< idx len))
        #t
        (eopl:error who "Indice fuera de rango: ~s" idx))))

(define add-to-vector
  (lambda (vec val)
    (list->vector (append (vector->list vec) (list val)))))

(define remove-from-vector
  (lambda (vec idx)
    (let loop ((lst (vector->list vec)) (i 0) (res '()))
      (cond
        ((null? lst) (list->vector (reverse res)))
        ((= i idx) (loop (cdr lst) (+ i 1) res))
        (else (loop (cdr lst) (+ i 1) (cons (car lst) res)))))))

(define insert-into-vector
  (lambda (vec idx val)
    (let loop ((lst (vector->list vec)) (i 0) (res '()))
      (cond
        ((= i idx) (list->vector (append (reverse res) (list val) lst)))
        ((null? lst) (list->vector (reverse (cons val res))))
        (else (loop (cdr lst) (+ i 1) (cons (car lst) res)))))))

;Valida que el nuevo elemento cumpla el tipo T de vector<T> (si viene de una variable)
(define check-vector-elem-type
  (lambda (target-exp val env who)
    (cases expression target-exp
      (var-exp (id v-prim)
               (let ((t (apply-env-type env id)))
                 (cases type-exp t
                   (vector-type (elem-type)
                                (if (validate-type elem-type val)
                                    #t
                                    (eopl:error who "Tipo incompatible al modificar el vector ~s" id)))
                   (void-type () #t)
                   (else #t))))
      (else #t))))

;Valida que clave y valor cumplan K y V de dict<K, V> (si viene de una variable)
(define check-dict-entry-type
  (lambda (target-exp k v env who)
    (cases expression target-exp
      (var-exp (id v-prim)
               (let ((t (apply-env-type env id)))
                 (cases type-exp t
                   (dict-type (key-type val-type)
                              (if (and (validate-type key-type k)
                                       (validate-type val-type v))
                                  #t
                                  (eopl:error who "Tipo incompatible de clave o valor en el diccionario ~s" id)))
                   (void-type () #t)
                   (else #t))))
      (else #t))))

;Verifica que la variable no sea const y actualiza su referencia en el ambiente
(define save-if-variable!
  (lambda (target-exp new-val env who)
    (cases expression target-exp
      (var-exp (id v-prim)
               (if (apply-env-const? env id)
                   (eopl:error who "No se puede modificar la estructura constante: ~s" id)
                   (begin
                     (setref! (apply-env-ref env id) new-val)
                     new-val)))
      (else new-val))))


(define apply-fun-primitive
  (lambda (prim raw-args env)
    (let ((args (map (lambda (a) (eval-expression a env)) raw-args)))
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
                      'nulo))
        (empty?-prim ()
                     (if (and (= (length args) 1) (list? (car args)))
                         (null? (car args))
                         (eopl:error 'empty? "Espera una lista")))
        (list?-prim ()
                    (if (= (length args) 1)
                        (list? (car args))
                        (eopl:error 'list? "Espera un argumento")))
        (make-list-prim ()
                        (if (and (= (length args) 2) (list? (cadr args)))
                            (cons (car args) (cadr args))
                            (eopl:error 'make-list "Espera un elemento y una lista")))
        (head-prim ()
                   (if (and (= (length args) 1) (list? (car args)))
                       (if (null? (car args))
                           (eopl:error 'head "La lista esta vacia")
                           (caar args))
                       (eopl:error 'head "Espera una lista")))
        (tail-prim ()
                   (if (and (= (length args) 1) (list? (car args)))
                       (if (null? (car args))
                           (eopl:error 'tail "La lista esta vacia")
                           (cdar args))
                       (eopl:error 'tail "Espera una lista")))
        (append-prim ()
                     (if (and (= (length args) 2) (list? (car args)) (list? (cadr args)))
                         (append (car args) (cadr args))
                         (eopl:error 'append "Espera 2 listas")))
        (ref-list-prim ()
                       (if (and (= (length args) 2) (list? (car args)))
                           (begin
                             (check-index 'ref-list (cadr args) (length (car args)))
                             (list-ref (car args) (cadr args)))
                           (eopl:error 'ref-list "Espera una lista y un indice")))
        (length-prim ()
                     (if (and (= (length args) 1) (list? (car args)))
                         (length (car args))
                         (eopl:error 'length "Espera una lista")))
        (vector?-prim ()
                      (if (= (length args) 1)
                          (vector? (car args))
                          (eopl:error 'vector? "Espera un argumento")))
        (make-vector-prim ()
                          (cond
                            ((and (= (length args) 1) (integer? (car args)) (>= (car args) 0))
                             (make-vector (car args) 0))
                            ((and (= (length args) 2) (integer? (car args)) (>= (car args) 0))
                             (make-vector (car args) (cadr args)))
                            (else
                             (eopl:error 'make-vector "Argumentos invalidos"))))
        (ref-vector-prim ()
                         (if (and (= (length args) 2) (vector? (car args)))
                             (begin
                               (check-index 'ref-vector (cadr args) (vector-length (car args)))
                               (vector-ref (car args) (cadr args)))
                             (eopl:error 'ref-vector "Espera un vector y un indice")))
        (set-vector-prim ()
                         (if (and (= (length args) 3) (vector? (car args)))
                             (let ((vec (car args))
                                   (idx (cadr args))
                                   (val (caddr args)))
                               (check-index 'set-vector idx (vector-length vec))
                               (check-vector-elem-type (car raw-args) val env 'set-vector)
                               (save-if-variable! (car raw-args) vec env 'set-vector)
                               (vector-set! vec idx val)
                               vec)
                             (eopl:error 'set-vector "Espera un vector, un indice y un valor")))
        (append-vector-prim ()
                            (if (and (= (length args) 2) (vector? (car args)))
                                (let ((vec (car args))
                                      (val (cadr args)))
                                  (check-vector-elem-type (car raw-args) val env 'append-vector)
                                  (let ((new-vec (add-to-vector vec val)))
                                    (save-if-variable! (car raw-args) new-vec env 'append-vector)))
                                (eopl:error 'append-vector "Espera un vector y un valor")))
        (delete-val-vector-prim ()
                                (if (and (= (length args) 2) (vector? (car args)))
                                    (let ((vec (car args))
                                          (idx (cadr args)))
                                      (check-index 'delete-val-vector idx (vector-length vec))
                                      (let ((new-vec (remove-from-vector vec idx)))
                                        (save-if-variable! (car raw-args) new-vec env 'delete-val-vector)))
                                    (eopl:error 'delete-val-vector "Espera un vector y un indice")))
        (insert-val-vector-prim ()
                                (if (and (= (length args) 3) (vector? (car args)))
                                    (let* ((vec (car args))
                                           (idx (cadr args))
                                           (val (caddr args))
                                           (len (vector-length vec)))
                                      (if (and (integer? idx) (exact? idx) (>= idx 0) (<= idx len))
                                          (begin
                                            (check-vector-elem-type (car raw-args) val env 'insert-val-vector)
                                            (let ((new-vec (insert-into-vector vec idx val)))
                                              (save-if-variable! (car raw-args) new-vec env 'insert-val-vector)))
                                          (eopl:error 'insert-val-vector "Indice fuera de rango: ~s" idx)))
                                    (eopl:error 'insert-val-vector "Espera un vector, un indice y un valor")))
        (create-dict-prim ()
                          (cond
                            ((null? args) (a-dict '()))
                            ((and (= (length args) 2) (list? (car args)) (list? (cadr args))
                                  (= (length (car args)) (length (cadr args))))
                             (build-dict (car args) (cadr args)))
                            (else
                             (eopl:error 'create-dict "Argumentos invalidos para diccionario"))))
        (dict?-prim ()
                    (if (= (length args) 1)
                        (dictionary? (car args))
                        (eopl:error 'dict? "Espera un argumento")))
        (ref-dict-prim ()
                       (if (and (= (length args) 2) (dictionary? (car args)))
                           (lookup-dict (car args) (cadr args))
                           (eopl:error 'ref-dict "Espera un diccionario y una clave")))
        (set-dict-prim ()
                       (if (and (= (length args) 3) (dictionary? (car args)))
                           (let ((dict (car args))
                                 (k (cadr args))
                                 (v (caddr args)))
                             (check-dict-entry-type (car raw-args) k v env 'set-dict)
                             (let ((new-dict (update-dict dict k v)))
                               (save-if-variable! (car raw-args) new-dict env 'set-dict)))
                           (eopl:error 'set-dict "Espera un diccionario, una clave y un valor")))
        (keys-prim ()
                   (if (and (= (length args) 1) (dictionary? (car args)))
                       (cases dictionary (car args)
                         (a-dict (pairs) (map car pairs)))
                       (eopl:error 'keys "Espera un diccionario")))
        (values-prim ()
                     (if (and (= (length args) 1) (dictionary? (car args)))
                         (cases dictionary (car args)
                           (a-dict (pairs) (map cdr pairs)))
                         (eopl:error 'values "Espera un diccionario")))

        (else
         (eopl:error 'apply-fun-primitive "Primitiva no definida: ~s" prim))
        ))))
;*******************************************************************************************
;Procedimientos
(define-datatype procval procval?
  (closure
   (ids (list-of symbol?))
   (body expression?)
   (env environment?)))

;apply-procedure: evalua el cuerpo de un procedimientos en el ambiente extendido correspondiente
(define apply-procedure
  (lambda (proc args)
    (cases procval proc
      (closure (ids body env)
               (let ((types (map (lambda (x) (void-type)) ids))
                     (consts (map (lambda (x) #f) ids)))
               (eval-expression body (extend-env ids types consts args env)))))))

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

;Prueba 5: tipos validos, int convertido a float, void que cambia de tipo y que retorna 7.0
(eval-program
 (scan&parse
  "GLOBALS {
     float a = 20;
     const float pi = 3.14;
     string s = \"hola mundo\";
     bool b = True;
     void v = 5;
   }
   PROGRAM {
     proc main() {
       return LOCALS {
         int z = 1;
       } {
         (v -> \"ahora soy texto\");
         (a -> 7);
         a;
       }
     }
   }"))

;Prueba 6: cadenas y print. Imprime 'Texto: Hola mundo' y 'Longitud: 10' y que retorna 10
(eval-program
 (scan&parse
  "GLOBALS {
     int n = 0;
   }
   PROGRAM {
     proc main() {
       return LOCALS {
         string s = concatenar(\"Hola \", \"mundo\");
       } {
         print(\"Texto: \", s);
         print(\"Longitud: \", longitud(s));
         longitud(s);
       }
     }
   }"))

;Prueba 7 Error esperado: asignar un tipo distinto a una variable bool
;Retorna Error: "Tipo incompatible en la asignacion de b: se esperaba bool y se recibio int"
#|
(eval-program
 (scan&parse
  "GLOBALS {
     bool b = True;
   }
   PROGRAM {
     proc main() {
       return LOCALS {
         int z = 1;
       } {
         (b -> 5);
       }
     }
   }"))
|#

;Prueba 8 Error esperado: declarar un int con valor float
;Retorna Error: "Tipo incompatible en la declaracion de x : se esperaba int y se recibio float"
#|
(eval-program
 (scan&parse
  "GLOBALS {
     int x = 3.5;
   }
   PROGRAM {
     proc main() {
       return 1
     }
   }"))
|#
;Prueba 9: BLOCK evalua secuencialmente y retorna la ultima expresion (True)
(eval-program
 (scan&parse
  "GLOBALS {
     int a = 10;
   }
   PROGRAM {
     proc main() {
       return BLOCK {
         (20.5 * 2);
         ((100 / 4) - 3);
         (a == 10);
       }
     }
   }"))

;Prueba 10: Operaciones relacionales dentro de BLOCK. Retorna False
(eval-program
 (scan&parse
  "GLOBALS {
     int x = 5;
   }
   PROGRAM {
     proc main() {
       return BLOCK {
         (x > 3);
         (x != 5);
       }
     }
   }"))

;Prueba 11: Llamada a procedimiento con var-prim. Retorna 17
(eval-program
 (scan&parse
  "GLOBALS {
     int x = 7;
     proc sumar = lambda(int a, int b) {
       return (a + b)
     };
   }
   PROGRAM {
     proc main() {
       return BLOCK {
         (sumar(x, 3) + x);
       }
     }
   }"))

;Prueba 12: Procedimiento recursivo factorial. Retorna 120
(eval-program
 (scan&parse
  "GLOBALS {
     proc factorial = lambda(int n) {
       return if ((n == 0)) { 1 } else { (n * factorial((n - 1))) }
     };
   }
   PROGRAM {
     proc main() {
       return factorial(5)
     }
   }"))

;Prueba 13: Listas inmutables (empty, make-list, head, tail, append, ref-list, length)
;Retorna 40
(eval-program
 (scan&parse
  "GLOBALS {
     list<int> base = '(20, 30);
   }
   PROGRAM {
     proc main() {
       return LOCALS {
         list<int> l1 = make-list(10, base);
         list<int> l2 = append(l1, make-list(40, empty));
       } {
         head(l2);
         tail(l2);
         length(l2);
         ref-list(l2, 3);
       }
     }
   }"))

;Prueba 14: Vectores mutables y diccionarios (set-vector, append-vector, delete-val-vector, set-dict, ref-dict)
;Retorna 95
(eval-program
 (scan&parse
  "GLOBALS {
     vector<int> notas = [70, 80, 90];
     dict<string, int> edades = {\"Ana\": 25};
   }
   PROGRAM {
     proc main() {
       return BLOCK {
         set-vector(notas, 1, 85);
         append-vector(notas, 100);
         delete-val-vector(notas, 0);
         set-dict(edades, \"Luis\", 10);
         (ref-vector(notas, 0) + ref-dict(edades, \"Luis\"));
       }
     }
   }"))

;Prueba 15 Error esperado: Intentar mutar un vector declarado como const
;Retorna Error: "No se puede modificar la estructura constante: fijos"
#|
(eval-program
 (scan&parse
  "GLOBALS {
     const vector<int> fijos = [1, 2, 3];
   }
   PROGRAM {
     proc main() {
       return BLOCK {
         set-vector(fijos, 0, 99);
       }
     }
   }"))
|#

;Prueba 16 Error esperado: Tipo incompatible en coleccion (list<int> con string)
;Retorna Error: "Tipo incompatible en la declaracion de malos: se esperaba list y se recibio list"
#|
(eval-program
 (scan&parse
  "GLOBALS {
     list<int> malos = '(1, \"dos\", 3);
   }
   PROGRAM {
     proc main() {
       return 1
     }
   }"))
|#