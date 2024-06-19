#lang eopl
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

    ;;Instanciación y uso de estructuras
    (expresion ("new" identificador "(" (separated-list expresion ",") ")") new-struct-exp)
    (expresion ("get" expresion "." identificador) get-struct-exp)
    (expresion ("set-struct" expresion "." identificador "=" expresion) set-struct-exp)

    ;;Reconocimiento de patrones
    (expresion ("match" expresion "{" (arbno regular-exp "=>" expresion) "}") match-exp)

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

    ;;Expresiones regulares
    (regular-exp (identificador "::" identificador) list-match-exp)
    (regular-exp ("numero" "(" identificador ")") num-match-exp)
    (regular-exp ("cadena" "(" identificador ")") cad-match-exp)
    (regular-exp ("boolean" "(" identificador ")") bool-match-exp)
    (regular-exp ("array" "(" (separated-list identificador ",") ")") array-match-exp)
    (regular-exp ("empty") empty-match-exp)
    (regular-exp ("default") default-match-exp)
    )
  )

;; ============================Representacion de Datos============================

;; Implementacion basada en los videos de la clase
;; https://youtu.be/AUq-XtehFjU?si=VC2UnPN4oTfrq7E6


;; Representa un entorno.
(define-datatype environment environment?
  (empty-env-record) ; Entorno vacío
  (extended-env-record ; Entorno extendido
   (syms (list-of symbol?)) ; Lista de símbolos
   (vec vector?) ; Vector de valores
   (env environment?))) ; Referencia al entorno padre

;; Representa una referencia a un valor en el entorno.
(define-datatype referencia referencia?
  (a-ref (position integer?) ; Posición del valor en el vector
         (vec vector?))) ; Vector que contiene los valores

;; Crea un entorno vacío.
(define empty-env
  (lambda ()
    (empty-env-record))) ; Entorno vacío

;; Extiende el entorno agregando símbolos y sus valores correspondientes.
(define extend-env
  (lambda (syms vals env)
    (extended-env-record syms (list->vector vals) env)))

;; Aplica la referencia del entorno a un símbolo y devuelve el valor correspondiente.
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

;; Desreferencia una referencia y devuelve el valor al que se refiere.
(define deref
  (lambda (ref)
    (primitive-deref ref)))

;; Desreferencia una referencia y devuelve el valor al que se refiere.
(define primitive-deref
  (lambda (ref)
    (cases referencia ref
      (a-ref (pos vec)
             (vector-ref vec pos)))))

;; Establece el valor de una referencia.
(define setref!
  (lambda (ref val)
    (primitive-setref! ref val)))

;; Establece el valor de una referencia.
(define primitive-setref!
  (lambda (ref val)
    (cases referencia ref
      (a-ref (pos vec)
             (vector-set! vec pos val)))))

;; Encuentra la posición de un símbolo en una lista.
(define rib-find-position 
  (lambda (sym los)
    (lista-posicion sym los)))

;; Encuentra la posición de un símbolo en una lista.
(define lista-posicion
  (lambda (sym los)
    (lista-index (lambda (sym1) (eqv? sym1 sym)) los)))

;; Encuentra el índice de un elemento en una lista.
(define lista-index
  (lambda (pred ls)
    (let aux ((ls ls) (idx 0)) ; Función auxiliar con contador de índice
      (cond
        ((null? ls) #f) ; Caso base: lista vacía
        ((pred (car ls)) idx) ; Si el predicado es verdadero, devuelve el índice actual
        (else (aux (cdr ls) (+ idx 1))))))) ; Caso recursivo: avanza en la lista e incrementa el índice

;; Aplica el entorno a un símbolo y devuelve el valor correspondiente.
(define apply-env
  (lambda (env sym)
    (deref (apply-env-ref env sym))))

;; Inicializa el entorno con símbolos y valores predefinidos.
(define init-env
  (lambda ()
    (extend-env
     '(x y z) ; Símbolos
     '(1 2 3) ; Valores
     (empty-env))))

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


(define eval-struct 
  (lambda (structs)
    (cases struct-decl structs
      (struct-exp (id lids) 
          (list id lids)
      )
    )
  )
)

(define eval-expression '')

(define eval-rands
  (lambda (rands env)
    (map (lambda (x) (eval-rand x env)) rands)))

(define eval-rand
  (lambda (rand env)
    (eval-expression rand env)))

(interpretador)

