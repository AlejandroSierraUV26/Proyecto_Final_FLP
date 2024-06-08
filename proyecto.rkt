#lang eopl

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

(sllgen:make-define-datatypes lexica gramatica)

(define show-the-datatypes
  (lambda () (sllgen:list-define-datatypes lexica gramatica)))



;El FrontEnd (Análisis léxico (scanner) y sintáctico (parser) integrados)

(define scan&parse
  (sllgen:make-string-parser lexica gramatica))

;El Analizador Léxico (Scanner)

(define just-scan
  (sllgen:make-string-scanner lexica gramatica))


;El Interpretador (FrontEnd + Evaluación + señal para lectura +
(define interpretador
  (sllgen:make-rep-loop "--> "
    (lambda (pgm)  pgm)
    (sllgen:make-stream-parser 
      lexica
      gramatica)))

(define evaluar-programa
  (lambda (pgm)
    (cases programa pgm
      (a-programa (structs exp)
        (evaluar-expresion exp empty-env)))))
  
(define evaluar-expresion 
  (lambda (exp env)
    (cases expresion exp
      (var-exp (id) (apply-env env id))
      (bool-exp (b) b)
      (num-exp (n) n)
      (cadena-exp (c) c)
      (decl-exp (decl) (evaluar-declaracion decl env))
      (lista-exp (l) (evaluar-lista l env))
      (cons-exp (e1 e2) (cons (evaluar-expresion e1 env) (evaluar-expresion e2 env)))
      (empty-list-exp () '())
      (array-exp (l) (evaluar-array l env))
      (prim
        (op e1 e2)
        (apply-primitiva op (evaluar-expres
                              ion e1 env) (evaluar-expresion e2 env)))

)))
(define tipos-de-expresiones
  (lambda (exp tenv)
    (cases expresion exp
      (var-exp (id) (apply-env tenv id))
      (bool-exp (b) 'bool)
      (num-exp (n) 'num)
      (cadena-exp (c) 'cadena)
      (decl-exp (decl) (tipos-de-declaracion decl tenv))
      (lista-exp (l) (tipos-de-lista l tenv))
      (cons-exp (e1 e2) (tipos-de-cons e1 e2 tenv))
      (empty-list-exp () 'list)
      (array-exp (l) (tipos-de-array l tenv))
      (prim
        (op e1 e2)
        (tipos-de-primitiva op e1 e2 tenv)
        )))
  )

(define tipos-de-primitivas
  (lambda (prim)
    (cases primitiva prim
      (prim-num-exp () (proc-type (list int-type int-type) int-type))
      (prim-bool-exp () (proc-type (list bool-type bool-type) bool-type))
      (prim-list-exp () (proc-type (list list-type) list-type))
      (prim-array-exp () (proc-type (list array-type) array-type))
      (prim-cad-exp () (proc-type (list string-type) string-type))
      )))
(define apply-primitiva
  (lambda (prim v1 v2)
    (cases primitiva prim
      (sum-prim () (+ v1 v2))
      (minus-prim () (- v1 v2))
      (mult-prim () (* v1 v2))
      (mod-prim () (modulo v1 v2))
      (elevar-prim () (expt v1 v2))
      (menor-prim () (< v1 v2))
      (mayor-prim () (> v1 v2))
      (menorigual-prim () (<= v1 v2))
      (mayorigual-prim () (>= v1 v2))
      (diferente-prim () (not (= v1 v2)))
      (igual-prim () (= v1 v2))
      (and-prim () (and v1 v2))
      (or-prim () (or v1 v2))
      (xor-prim () (xor v1 v2))
      (not-prim () (not v1))
      (first-primList () (car v1))
      (rest-primList () (cdr v1))
      (empty-primList () (null? v1))
      (length-primArr () (length v1))
      (index-primArr () (list-ref v1 v2))
      (slice-primArr () (sublist v1 v2))
      (setlist-primArr () (setlist v1 v2))
      (concat-primCad () (string-append v1 v2))
      (length-primCad () (string-length v1))
      (index-primCad () (string-ref v1 v2))
      )))




(define empty-env '())
