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
    (expresion (texto) str-exp)
    (expresion (numero) lit-exp)
    (expresion ("\"" identificador (arbno identificador) "\"") cadena-exp)
    (expresion (var-decl) let-exp)

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
    (expresion ("let" (arbno identificador "=" expresion) "in" expresion) let-exp)

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
(define show-the-datatypes
  (lambda () (sllgen:list-define-datatypes lexica gramatica)))
;El FrontEnd (Análisis léxico (scanner) y sintáctico (parser) integrados)

(define scan&parse
  (sllgen:make-string-parser lexica gramatica))

;El Analizador Léxico (Scanner)

(define just-scan
  (sllgen:make-string-scanner lexica gramatica))

(define evaluar-programa
  (lambda (pgm)
    (cases programa pgm
      (a-programa (structs exp)
        (evaluar-expresion exp ambiente-inicial)
        ))))

(define evaluar-expresion
  (lambda (exp amb)
    (cases expresion exp
      ;Numeros
      (str-exp (str) (substring str 1 (-(string-length str) 1)))
      (lit-exp (dato) dato)
      (bool-exp (bool-expresion) bool-exp)
      (var-exp (identificador) (apply-env amb identificador))
      (cadena-exp (identificador otro-identificador) (string-append identificador (apply-env amb otro-identificador)))
      (let-exp (var-decl)
                (letrec
                    ([lvalues (map (lambda (x) (evaluar-expresion x amb)) (cdr var-decl))]
                     [ids (map (lambda (x) (car x)) (cdr var-decl))]
                     )
                  (evaluar-expresion (car (cdr (cdr var-decl))) (ambiente-extendido ids lvalues amb))
                  )
                )
      
      ;(cadena-exp (identificador (arbno identificador)) (string-append identificador (apply-env amb identificador)))

      (lista-exp (list-exp) (list (map (lambda (x) (evaluar-expresion x amb)) list-exp)))
      (cons-exp (exp1 exp2) (cons (evaluar-expresion exp1 amb) (evaluar-expresion exp2 amb)))
      (empty-list-exp () '())
      (array-exp (list-exp) (vector->list (vector (map (lambda (x) (evaluar-expresion x amb)) list-exp))))

      ;Primitivas
      

      ;Condicionales    
      (if-exp (condicion consecuencia else)
              (let (
                    [condicion-val (evaluar-expresion condicion amb)]
                    )
                (if (boolean? condicion-val)
                    (if condicion-val (evaluar-expresion consecuencia amb) (evaluar-expresion else amb))
                    (eopl:error "Se esperaba un valor booleano para el IF"))
                )
              )
      ; Iteradores
      ; for
      (for-exp (init-expr test-expr update-expr body-expr else-expr)
                (letrec
                    ;area de definiciones
                    ;init = la expresion de inicializacion
                    ;test = la expresion de prueba
                    ;update = la expresion de actualizacion
                    ;body = la expresion que se va a ejecutar
                    ([init (evaluar-expresion init-expr amb)]
                      [test (evaluar-expresion test-expr amb)]
                      [update (evaluar-expresion update-expr amb)]
                      [body (evaluar-expresion body-expr amb)]
                      ;iterar = funcion que se encarga de ejecutar el cuerpo del for mientras la expresion de prueba
                      ;sea verdadera
                      [iterar (lambda ()
                                (if test
                                    (begin
                                      body
                                      (evaluar-expresion update-expr amb)
                                      (iterar))
                                    1)
                                )
                      ]
                      ;area de ejecucion
                      (init)
                      (iterar)
                      )
                  )
               )
      ; while
      (while-exp (exp1 body)
                 (letrec
                     ;area de definiciones
                     ;iterar = mientras que la exp1 sea verdadera en el ambiente actual, se ejecuta el cuerpo del while
                     ;cuando ya deja de serlo, simplemente retornamos un 1
                     ([iterar (lambda ()
                                (if (evaluar-expresion exp1 amb)
                                    (begin
                                      (evaluar-expresion body amb)
                                      (iterar))
                                    1)
                                )])
                   ;area de ejecucion
                   (iterar)))
      ;Switch
      (switch-exp (exp1 cases default amb) ; Añadir 'amb' como cuarto campo
  (letrec
      ([value (evaluar-expresion exp1 amb)]
       [cases (map (lambda (x) (car x)) cases)]
       [consecuencias (map (lambda (x) (car (cdr x))) cases)]
       [default-consecuencia (car (cdr default))]
       [iterar (lambda (cases consecuencias default-consecuencia)
                 (cond
                   [(null? cases) (evaluar-expresion default-consecuencia amb)]
                   [(= value (car cases)) (evaluar-expresion (car consecuencias) amb)]
                   [else (iterar (cdr cases) (cdr consecuencias) default-consecuencia)]
                 ))
       ]
      (iterar cases consecuencias default-consecuencia)
  )))
      ;Secuenciación y asignación
      ;Begin
      ;
      (begin-exp (exp lexp)
                 (if
                  (null? lexp)
                  (evaluar-expresion exp amb)
                  (begin
                    (evaluar-expresion exp amb)
                    (letrec
                        ([evaluar-begin (lambda (lexp)
                                          (cond
                                            [(null? (cdr lexp)) (evaluar-expresion (car lexp) amb)]
                                            [else
                                             (begin
                                               (evaluar-expresion (car lexp) amb)
                                               (evaluar-begin (cdr lexp)))]
                                            ))])                         
                      (evaluar-begin lexp)))))
      ;;set
      (set-exp (id exp)
               (begin
                 (setref! (apply-env-ref amb id) (evaluar-expresion exp amb))
                 1))
      ;Funciones
      (func-exp (ids body) (closure ids body amb))
      (call-exp (rator rands)
                (let
                    ;area de definiciones
                    ;lrands = evaluamos cada una de los rands que serian los identificadores del procedimiento para
                    ;conocer sus valores, o eventualmente el valor que tomen al evaluarse
                    ;proc = evaluamos el rator que seria el nombre del procedimiento, para verificar que efectivamente lo sea
                    ([lrands (map (lambda (x) (evaluar-expresion x amb)) rands)]
                     [proc (evaluar-expresion rator amb)])
                  (if
                   ;si proc es un procedimiento, entramos a verificar
                   (procval? proc)
                   (cases procval proc
                     (closure (lid body old-env)
                              ;si el numero de variables ingresadas coniciden con las esperadas
                              ;si lo hace, evaluamos el cuerpo de el closure, osea del proc, con las variables
                              ;que estamos ingresando, ademas de el ambiente anterior
                              ;si no lo son simplemente lo retornamos en un mensaje de error
                              (if (= (length lid) (length lrands))
                                  (evaluar-expresion body (ambiente-extendido lid lrands old-env))
                                  (eopl:error "Se espearaban " (length lid) "parametros y se han recibido " (length lrands))))
                     )
                   (eopl:error proc "No corresponde a un procedimiento") 
                   )))
      ;Instanciación y uso de estructuras
      (new-struct-exp (id lexp)
                      (let (
                            [estructura (apply-env amb id)]
                            [lvalues (map (lambda (x) (evaluar-expresion x amb)) lexp)]
                            )
                        (begin
                          (cases struct estructura
                            (a-struct (lids) (ambiente-extendido lids lvalues (ambiente-vacio)))
                            )
                        )))
      (get-struct-exp (id varId)
                      (let (
                            [estructura (apply-env amb id)]
                            )
                        (apply-env estructura varId)
                        )
                      )
      (set-struct-exp (id varId exp)
                      (begin
                        (setref! (apply-env-ref (apply-env amb id) varId) (evaluar-expresion exp amb))
                        1)
                      )
      ;Reconocimiento de patrones
      (match-exp (exp cases default) ; Añadir 'default' como tercer campo
        (letrec
            ([value (evaluar-expresion exp amb)]
            [cases (map (lambda (x) (car x)) cases)]
            [consecuencias (map (lambda (x) (car (cdr x))) cases)]
            [default-consecuencia (car (cdr default))] ; Asegurarse de que 'default' se maneja correctamente
            [iterar (lambda (cases consecuencias default-consecuencia)
                      (cond
                        [(null? cases) (evaluar-expresion default-consecuencia amb)]
                        [(= value (car cases)) (evaluar-expresion (car consecuencias) amb)]
                        [else (iterar (cdr cases) (cdr consecuencias) default-consecuencia)]
                      ))
            ]
            (iterar cases consecuencias default-consecuencia)
        )))
      ; Numero-exp
      ; Bool-exp
      
      ;Ligaduras locales
      (let-exp (ids rands body)
               (let
                   (
                    [lvalues (map (lambda (x) (evaluar-expresion x amb)) rands)]
                    )
                 (evaluar-expresion body (ambiente-extendido ids lvalues amb))
                 )
               )
      (let-exp (ids rands body)
               (let
                   (
                    [lvalues (map (lambda (x) (evaluar-expresion x amb)) rands)]
                    )
                 (evaluar-expresion body (ambiente-extendido ids lvalues amb))
                 )
               )
      (else exp) 
      )
    )
  )

(define evaluar-primitiva
  (lambda (prim)
    (cases primitiva prim
      ;;Primitivas aritmeticas
      (sum-prim () (lambda (a b)(+ a b)))
      (minus-prim () (lambda (a b)(- a b)))
      (mult-prim () (lambda (a b)(* a b)))
      (div-prim () (lambda (a b)(/ a b)))
      (mod-prim () (lambda (a b)(modulo a b)))
      (elevar-prim () (lambda (a b)(expt a b)))
      (add-prim () (lambda (a)(+ a 1)))
      (sub-prim () (lambda (a)(- a 1)))
      ;;primitivas booleanas
      (mayor-prim ()(lambda (a b)(> a b)))
      (mayorigual-prim ()(lambda (a b)(>= a b)))
      (menor-prim ()(lambda (a b)(< a b)))
      (menorigual-prim ()(lambda (a b)(<= a b)))
      (igual-prim ()(lambda (a b)(= a b)))
      (diferente-prim ()(lambda (a b)(not (= a b))))

      ;;primitivas sobre listas
      (first-primList ()(lambda (a)(car a)))
      (rest-primList ()(lambda (a)(cdr a)))
      (empty-primList ()(lambda (a)(null? a)))

      ;;primitivas sobre arrays
      (length-primArr ()(lambda (a)(length a)))
      (index-primArr ()(lambda (a b)(list-ref a b)))
      (slice-primArr ()(lambda (a b c)(list-tail (list-drop a b) c)))
      (setlist-primArr ()(lambda (a b c)(list-set a b c)))


      ;;primitivas sobre cadenas
      (length-primCad ()(lambda (a)(string-length a)))
      (concat-primCad ()(lambda (a b)(string-append a b)))
      (index-primCad ()(lambda (a b)(string-ref a b)))

        

      ;Primitivas booleanas
      (and-prim () (lambda (a b) (and a b)))
      (or-prim () (lambda (a b) (or a b)))
      (xor-prim () (lambda (a b) (xor a b)))
      (not-prim () (lambda (a) (not a)))

      (prim-num-exp (exp1 prim exp2) ((evaluar-primitiva prim) (evaluar-expresion exp1 amb) (evaluar-expresion exp2 amb)))
      (prim-num-exp (exp1 prim exp2) ((evaluar-primitiva prim) (evaluar-expresion exp1 amb) (evaluar-expresion exp2 amb)))
      (prim-bool-exp (primitivaBooleana) (evaluar-primitiva primitivaBooleana))
      (prim-list-exp (primitivaListas) (evaluar-primitiva primitivaListas))
      (prim-array-exp (primitivaArray) (evaluar-primitiva primitivaArray))
      (prim-cad-exp (primitivaCadena) (evaluar-primitiva primitivaCadena))

     )
    )
  )
(define-datatype struct struct?
  (a-struct (l list?)))

(define-datatype procval procval?
  (closure (lid (list-of symbol?))
           (body expresion?)
           (amb ambiente?)))

(define-datatype ambiente ambiente?
  (ambiente-vacio)
  (ambiente-extendido-ref
   (lids (list-of symbol?))
   (lvalue vector?)
   (old-env ambiente?)))

(define ambiente-extendido
  (lambda (lids lvalue old-env)
    (ambiente-extendido-ref lids (list->vector lvalue) old-env)))

(define apply-env
  (lambda (env var)
    (deref (apply-env-ref env var))))

(define apply-env-ref
  (lambda (env var)
    (cases ambiente env
      (ambiente-vacio () (eopl:error "No se encuentra la variable " var))
      (ambiente-extendido-ref (lid vec old-env)
                          (letrec
                              (
                               (buscar-variable (lambda (lid vec pos)
                                                  (cond
                                                    [(null? lid) (apply-env-ref old-env var)]
                                                    ;cuando la encuentra construye es una referencia
                                                    [(equal? (car lid) var) (a-ref pos vec)]
                                                    [else
                                                      ;recorriendo el vector aumentando la posicion en el caso recursivo
                                                     (buscar-variable (cdr lid) vec (+ pos 1)  )]
                                                    )
                                                  )
                                                )
                               )
                            (buscar-variable lid vec 0)
                            )
                          
                          )
      
      )
    )
  )
(define ambiente-inicial
  (ambiente-extendido '(x y z) '(1 2 3)
                      (ambiente-extendido '(a b c) '(4 5 6)
                                          (ambiente-vacio))))


(define-datatype referencia referencia?
  (a-ref (pos number?)
         (vec vector?)))

(define deref
  (lambda (ref)
    (primitiva-deref ref)))

(define primitiva-deref
  (lambda (ref)
    (cases referencia ref
      (a-ref (pos vec)))))

(define setref!
  (lambda (ref val)
    (primitiva-setref! ref val)))

(define primitiva-setref!
  (lambda (ref val)
    (cases referencia ref
      (a-ref (pos vec)
             (vector-set! vec pos val)))))        
;El Interpretador (FrontEnd + Evaluación + señal para lectura +
(define interpretador
  (sllgen:make-rep-loop ">>>> "
    (lambda (pgm)  pgm)
    (sllgen:make-stream-parser 
      lexica
      gramatica)))

(sllgen:list-define-datatypes lexica gramatica)
(interpretador)

