#lang eopl
(require "auxiliar.rkt")  
;; ============================Primitivas============================
(define lexica
  '((white-sp
   (whitespace) skip)
  (comment
   ("//" (arbno (not #\newline))) skip)
  (identificador
   (letter (arbno (or letter digit "?"))) symbol)
  (digitoBinario
   ("b" (or "0" "1") (arbno (or "0" "1"))) string)
  (digitoBinario
   ("-" "b" (or "0" "1") (arbno (or "0" "1"))) string)
  (digitoDecimal
   (digit (arbno digit)) number)
  (digitoDecimal
   ("-" digit (arbno digit)) number)
  (digitoOctal
   ("0x" (or "0" "1" "2" "3" "4" "5" "6" "7")(arbno (or "0" "1" "2" "3" "4" "5" "6" "7"))) string)
  (digitoOctal
   ("-" "0x" (or "0" "1" "2" "3" "4" "5" "6" "7") (arbno (or "0" "1" "2" "3" "4" "5" "6" "7"))) string)
  (digitoHexadecimal
   ("hx" (or "0" "1" "2" "3" "4" "5" "6" "7" "8" "9" "A" "B" "C" "D" "E" "F") (arbno (or "0" "1" "2" "3" "4" "5" "6" "7" "8" "9" "A" "B" "C" "D" "E" "F"))) string)
  (digitoHexadecimal
   ("-" "hx" (or "0" "1" "2" "3" "4" "5" "6" "7" "8" "9" "A" "B" "C" "D" "E" "F") (arbno (or "0" "1" "2" "3" "4" "5" "6" "7" "8" "9" "A" "B" "C" "D" "E" "F"))) string) 
  (flotante
   (digit (arbno digit) "." digit (arbno digit)) number)
  (flotante
   ("-" digit (arbno digit) "." digit (arbno digit)) number)
  ))
(define gramatica
  '(
    (programa ((arbno struct-decl) expresion) a-programa)
    (expresion (bool-expresion) bool-exp)
    (expresion (identificador) var-exp)
    (expresion (numero-exp) num-exp)    
    (expresion ("\"" identificador (arbno identificador) "\"") cadena-exp)
    (expresion (var-decl) decl-exp)
    (expresion ("void") void-exp)

    ;;Listas y arrays
    (expresion ("list" "(" (separated-list expresion ",") ")") lista-exp)
    (expresion ("cons" "(" expresion expresion ")") cons-exp)
    (expresion ("empty") empty-list-exp)
    (expresion ("array" "(" (separated-list expresion ",") ")") array-exp)

    ;;Expresion primitivas
    ;;Primitiva numerica
    (expresion ("(" expresion primitiva expresion ")") prim-num-exp)
    ;;Primitiva booleana
    (expresion (primitivaBooleana "(" (separated-list expresion ",") ")") prim-bool-exp)
    ;;Primitiva listas
    (expresion (primitivaListas "(" expresion ")") prim-list-exp)
    ;;Primitiva array
    (expresion (primitivaArray "(" (separated-list expresion ",") ")") prim-array-exp)
    ;;Primitiva de cadenas
    (expresion (primitivaCadena "(" (separated-list expresion ",") ")") prim-cad-exp)


    ;;Condicionales
    (expresion ("if" expresion "{" expresion "else" expresion "}") if-exp)


    ;;Iteradores
    (expresion ("for" identificador "from" expresion "until" expresion "by" expresion "do" expresion) for-exp)
    (expresion ("while" expresion "{" expresion "}") while-exp)

    ;;Switch
    (expresion ("switch" "(" expresion ")" "{" (arbno "case" expresion ":" expresion) "default" ":" expresion "}") switch-exp)

    ;;Secuenciación y asignación
    (expresion ("begin" expresion (arbno ";" expresion) "end") begin-exp)
    (expresion ("set" identificador "=" expresion) set-exp)

    ;;Funciones
    (expresion ("func" "(" (separated-list identificador ",") ")" expresion) func-exp)
    (expresion ("call" expresion "(" (separated-list expresion ",") ")") call-exp)


    ;;Numero-exp
    (numero-exp (digitoDecimal) decimal-num)
    (numero-exp (digitoOctal) octal-num)
    (numero-exp (digitoBinario) bin-num)
    (numero-exp (digitoHexadecimal) hex-num)
    (numero-exp (flotante) float-num)
    
    ;;Bool-exp
    (bool-expresion ("true") true-exp)
    (bool-expresion ("false") false-exp)

    ;;primitivas numéricas
    (primitiva ("+") sum-prim)
    (primitiva ("-") minus-prim)
    (primitiva ("*") mult-prim)
    (primitiva ("mod") mod-prim)
    (primitiva ("pow") elevar-prim)
    (primitiva ("<") menor-prim)
    (primitiva (">") mayor-prim)
    (primitiva ("<=") menorigual-prim)
    (primitiva (">=") mayorigual-prim)
    (primitiva ("!=") diferente-prim)
    (primitiva ("==") igual-prim)

    ;;primitiva booleana
    (primitivaBooleana ("and") and-prim)
    (primitivaBooleana ("or") or-prim)
    (primitivaBooleana ("xor") xor-prim)
    (primitivaBooleana ("not") not-prim)

    ;;Primitiva listas
    (primitivaListas ("first") first-primList)
    (primitivaListas ("rest") rest-primList)
    (primitivaListas ("empty?") empty-primList)

    ;;Primitiva arrays
    (primitivaArray ("length") length-primArr)
    (primitivaArray ("index") index-primArr)
    (primitivaArray ("slice") slice-primArr)
    (primitivaArray ("setlist") setlist-primArr)

    ;;Primitiva cadenas
    (primitivaCadena ("concat") concat-primCad)
    (primitivaCadena ("string-length") length-primCad)
    (primitivaCadena ("elementAt") index-primCad)
    
    ;;Variables
    (var-decl ("var" (arbno identificador "=" expresion) "in" expresion) lvar-exp)
    (var-decl ("let" (arbno identificador "=" expresion) "in" expresion) let-exp)
    
    ;;Estructuras de datos
    (struct-decl ("struct" identificador "{" (arbno identificador) "}") struct-exp)
    )
  )
(sllgen:make-define-datatypes lexica gramatica)

(define show-the-datatypes
  (lambda () (sllgen:list-define-datatypes lexica gramatica)))
;El FrontEnd (Análisis léxico (scanner) y sintáctico (parser) integrados)

(define scan&parse
  (sllgen:make-string-parser lexica gramatica))

;El Analizador Léxico (Scanner)

(define just-scan
  (sllgen:make-string-scanner lexica gramatica))

(define eval-program
  (lambda (pgm)
    (cases programa pgm
      (a-programa (lstrc body)
        (if (null? lstrc)
            (eval-expression body (init-env))
            (let* ([structs (map eval-struct lstrc)]
                   [ids (map car structs)]
                   [values (map cadr structs)])
              (eval-expression body (extend-env ids values (init-env)))))))))

;El Interpretador (FrontEnd + Evaluación + señal para lectura +
(define interpretador
  (sllgen:make-rep-loop ">>> "
    (lambda (pgm) (eval-program  pgm))
    (sllgen:make-stream-parser 
      lexica
      gramatica)))



;; ============================Evaluación============================
(define eval-struct 
  (lambda (structs)
    (cases struct-decl structs
      (struct-exp (id lids) 
          (list id lids)
      )
    )
  )
)

(define eval-expression
  (lambda (exp env)
    (cases expresion exp
      ; Expresiones básicas
      (bool-exp (bool) 
        (cases bool-expresion bool
          (true-exp () #T)
          (false-exp () #F)
          ))
      (void-exp () 'void)
      (var-exp (id) (apply-env env id))
      (num-exp (num) 
        (cases numero-exp num
          (decimal-num (num) num)
          (octal-num (num) (parse-string num))
          (bin-num (num) (parse-string num))
          (hex-num (num) (parse-string num))
          (float-num (num) num)))
      (cadena-exp (id1 id2)
        (letrec
            [(crear_string (lambda(lids)
                            (cond
                              [(null? lids) "" ]
                              [else( string-append " " (symbol->string(car lids))(crear_string(cdr lids)))])))]
          (string-append (symbol->string id1)(crear_string id2))
         )
       )
      (decl-exp (dcl) 
        (cases var-decl dcl
          (lvar-exp (ids rands body)
               (let ((args (eval-rands rands env)))
                 (eval-expression body
                                  (extend-env ids args env))))


          (let-exp (ids rands body) 
              (let ((args (eval-rands rands env)))
                (if (contains-set? body)
                    (eopl:error 'decl-exp "No se puede asignar a una variable en una expresión let")
                 (eval-expression body
                                  (extend-env ids args env)))))
          )
       )

      ; Listas y arrays
      (lista-exp(lexps) (
        map (lambda (exp) (eval-expression exp env)) lexps
      ))
      (cons-exp (exp1 exp2) 
        (cons (eval-expression exp1 env) (eval-expression exp2 env)))
      (empty-list-exp () '())
      (array-exp (lexp)
                 (let (
                       (evaluated (map (lambda (exp) (eval-expression exp env)) lexp)))
                       (list->vector evaluated)))

      ; Expresion primitivas
      (prim-num-exp (exp1 prim exp2) (
        apply-num-primitive prim (eval-expression exp1 env) (eval-expression exp2 env))
      )
      (prim-bool-exp (prim lexps)
            (apply-bool-primitive prim 
              (map (lambda (exp) (let ([value (eval-expression exp env)])
                (if (boolean? value) value
                (eopl:error 'apply-bool-primitive "No es un valor booleano"))
              )) lexps)
            )
      )
      (prim-list-exp (prim exp) 
       (apply-list-primitive prim (eval-expression exp env)) 
      )
      (prim-array-exp (prim lexps)
                      (let (
                            (args (eval-rands lexps env)))
                        (apply-array-primitive prim args)))
      (prim-cad-exp (prim lexps)
                     (apply-cadena-primitive prim
                                            (map (lambda (lexps)
                                                   (eval-expression lexps env))
                                                 lexps)))
      ; Condicionales
      (if-exp (test-exp true-exp false-exp) 
              (if (eval-expression test-exp env)
                  (eval-expression true-exp env)
                  (eval-expression false-exp env)))

      ; Iteradores
      (for-exp (cond-exp from-exp until-exp by-exp do-exp)
                (let loop (
                           (env (extend-env (list cond-exp) (list (eval-expression from-exp env)) env))
                           (until-val (eval-expression until-exp env))
                           (by-val (eval-expression by-exp env))
                           (result (eval-expression (void-exp) env)))
                  (let ((current-val (apply-env env cond-exp)))
                    (if (< current-val until-val) 
                        (let* ((do-val (eval-expression do-exp env))
                               (new-env (extend-env (list cond-exp) (list (+ current-val by-val)) env)))
                          (set! result do-val)
                          (loop new-env until-val by-val result))
                       result))))
       (while-exp (cond-exp exp) (
                                  let loop (
                                            [cond (eval-expression cond-exp env)]
                                            )
                                   (if (not cond) #f
                                       (begin
                                         (eval-expression exp env)
                                         (loop (eval-expression cond-exp env))
                                         )
                                       )
                                   ))
      
      ;Switch
      (switch-exp (cond-exp case-exp lexps default-exp) (
        let loop (
          [match-case (eval-expression cond-exp env)]
          [cases (map (lambda (exp) (eval-expression exp env)) lexps)]
          [lexps lexps]
          )
           (cond
            [(null? cases) (eval-expression default-exp env)]
            [(eq? match-case (car cases)) (eval-expression (car lexps) env)]
            [else (loop match-case (cdr cases) (cdr lexps))]
            )
          )
      )

      ; Secuenciación y asignación
      (begin-exp (exp exps) 
                 (let loop (
                            (acc (eval-expression exp env))
                             (exps exps))
                    (if (null? exps) acc
                        (loop (eval-expression (car exps) env)
                              (cdr exps)))))
      (set-exp (id rhs-exp)
              (eval-expression (begin
                 (setref!
                  (apply-env-ref env id)
                  (eval-expression rhs-exp env))
                 (void-exp)) env))

      ; Funciones
      (func-exp (lids exp)
                  (lambda (args env)
                    (let (
                          (new-env (extend-env lids args env))) 
                          (eval-expression exp new-env))
                  ))
      (call-exp (exp args)
                (let (
                      (func-val (eval-expression exp env))
                      (eval-args (map (lambda (arg) (eval-expression arg env)) args)))
                  (apply func-val (list eval-args env))))

      )
      
    ))
(define lista-index
  (lambda (pred ls)
    (let aux ((ls ls) (idx 0)) ; Función auxiliar con contador de índice
      (cond
        ((null? ls) #f) ; Caso base: lista vacía
        ((pred (car ls)) idx) ; Si el predicado es verdadero, devuelve el índice actual
        (else (aux (cdr ls) (+ idx 1))))))) ; Caso recursivo: avanza en la lista e incrementa el índice

(define contains-set?
  (lambda (expr)
    (cases expresion expr
      (set-exp (id rhs-exp) #t)
      (begin-exp (id rhs-exp) (contains-set? id))
      (for-exp (cond-exp from-exp until-exp by-exp do-exp) (contains-set? do-exp))
      (while-exp (cond-exp exp) (contains-set? exp))
      (else #f))))


;; ============================Primitivas============================
(define apply-num-primitive
  (lambda (prim num1 num2)
    (cases primitiva prim
      (sum-prim () (operation-numerical + num1 num2 #F))
      (minus-prim () (operation-numerical - num1 num2 #F))
      (mult-prim () (operation-numerical * num1 num2 #F))
      (mod-prim () (operation-numerical modulo num1 num2 #F) )
      (elevar-prim () (operation-numerical expt num1 num2 #F ))
      (menor-prim () (operation-numerical < num1 num2 #T))
      (mayor-prim () (operation-numerical > num1 num2 #T))
      (menorigual-prim () (operation-numerical <= num1 num2 #T))
      (mayorigual-prim () (operation-numerical >= num1 num2 #T))
      (diferente-prim () (not (operation-numerical eq? num1 num2 #T)))
      (igual-prim () (operation-numerical eq? num1 num2 #T))
      )
  ))
(define apply-bool-primitive
  (lambda (prim args)
    (cases primitivaBooleana prim
      (and-prim () (let loop ([args args])
                    (if (null? args) #t
                        (if (not (car args)) #f (loop (cdr args))))))
      (or-prim () (let loop ([args args])
          (if (null? args) #f
              (if (car args) #t (loop (cdr args)))
          )))
      (xor-prim () (and (or (car args) (cadr args)) (not (and (car args) (cadr args)))))
      (not-prim () (not (car args)))
      )
    ))
(define apply-list-primitive
  (lambda (prim args)
    (cases primitivaListas prim
      (first-primList () (car args))
      (rest-primList () (cdr args))
      (empty-primList () (null? args))
    )
  ))
(define apply-array-primitive
  (lambda (prim args)
    (cases primitivaArray prim
      (length-primArr () (vector-length (car args)))
      (index-primArr () (let (
                              (arr (car args))
                              (index (cadr args)))
                           (vector-ref arr index)))
      (slice-primArr() (let (
                             (vec (car args))
                             (start (cadr args))
                             (end (caddr args)))
                         (array-slice vec start end)))
      (setlist-primArr () (let (
                                (vec (car args))
                                (pos (cadr args))
                                (val (caddr args)))
                            (vector-set! vec pos val)
                             vec))
    )
  ))
(define apply-cadena-primitive  
  (lambda (prim args)
      (cases primitivaCadena prim
        (concat-primCad() (apply string-append args))
        (length-primCad() (string-length (car args)))
        (index-primCad() (string (string-ref (car args)(cadr args)))))))

;; ============================Evaluar expresiones============================
(define eval-rands
  (lambda (rands env)
    (map (lambda (x) (eval-rand x env)) rands)))

(define eval-rand
  (lambda (rand env)
    (eval-expression rand env)))

;; ============================Representacion de Datos============================

;; Implementacion basada en los videos de la clase
;; https://youtu.be/AUq-XtehFjU?si=VC2UnPN4oTfrq7E6


(define-datatype environment environment?
  (empty-env-record) ; Entorno vacío
  (extended-env-record ; Entorno extendido
   (syms (list-of symbol?)) ; Lista de símbolos
   (vec vector?) ; Vector de valores
   (env environment?))) ; Referencia al entorno

(define-datatype referencia referencia?
  (a-ref (position integer?) ; Posición del valor en el vector
         (vec vector?))) ; Vector que contiene los valores

(define empty-env
  (lambda ()
    (empty-env-record))) ; Entorno vacío

(define extend-env
  (lambda (syms vals env)
    (extended-env-record syms (list->vector vals) env)))

(define apply-env-ref
  (lambda (env sym)
    (cases environment env
      (empty-env-record ()
                        (eopl:error 'apply-env-ref "No se encuentra la variable ~a" sym))
      (extended-env-record (syms vals env)
                           (let ((pos (rib-find-position sym syms)))
                             (if pos ; Si pos es un número, entonces se encontró la posición.
                                 (a-ref pos vals)
                                 (apply-env-ref env sym)))))))

(define deref
  (lambda (ref)
    (primitive-deref ref)))

(define primitive-deref
  (lambda (ref)
    (cases referencia ref
      (a-ref (pos vec)
             (vector-ref vec pos)))))

(define setref!
  (lambda (ref val)
    (primitive-setref! ref val)))

(define primitive-setref!
  (lambda (ref val)
    (cases referencia ref
      (a-ref (pos vec)
             (vector-set! vec pos val)))))

(define rib-find-position 
  (lambda (sym los)
    (lista-posicion sym los)))

(define lista-posicion
  (lambda (sym los)
    (lista-index (lambda (sym1) (eqv? sym1 sym)) los)))


(define apply-env
  (lambda (env sym)
    (deref (apply-env-ref env sym))))

(define init-env
  (lambda ()
    (extend-env
     '(x y z) ; Símbolos
     '(1 2 3) ; Valores
     (empty-env))))



(interpretador)

