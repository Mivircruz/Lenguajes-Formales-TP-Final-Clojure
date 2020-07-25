(declare evaluar)
(declare aplicar)
(declare controlar-aridad)
(declare igual?)
(declare cargar-arch)
(declare imprimir)
(declare actualizar-amb)
(declare revisar-f)
(declare revisar-lae)
(declare buscar)
(declare evaluar-cond)
(declare evaluar-secuencia-en-cond)
(declare macroOr)
(declare macroIf)
(declare macroLoad)
(declare subTLCLISP)
(declare reverseTLCLISP)
(declare appendTLCLISP)
(declare consTLCLISP)
(declare equalTLCLISP)
(declare lengthTLCLISP)
(declare geTLCLISP)
(declare gtTLCLISP)
(declare listTLCLISP)
(declare ltTLCLISP)
(declare notTLCLISP)
(declare nullTLCLISP)
(declare restTLCLISP)
(declare evalTLCLISP)
(declare readTLCLISP)
(declare terpriTLCLISP)
(declare prin3TLCLISP)


; REPL (read–eval–print loop).
; Aridad 0: Muestra mensaje de bienvenida y se llama recursivamente con el ambiente inicial.
; Aridad 1: Muestra >>> y lee una expresion y la evalua.
; Si la 2da. posicion del resultado es nil, retorna true (caso base de la recursividad).
; Si no, imprime la 1ra. pos. del resultado y se llama recursivamente con la 2da. pos. del resultado. 
(defn repl
   ([]
      (println "Interprete de TLC-LISP en Clojure")
    (println "Trabajo Practico de 75.14/95.48 - Lenguajes Formales 2020")
    (println "Inspirado en:")
      (println "TLC-LISP Version 1.51 for the IBM Personal Computer")
      (println "Copyright (c) 1982, 1983, 1984, 1985 The Lisp Company") (flush)
      (repl '(add add append append cond cond cons cons de de env env equal equal eval eval exit exit
        first first ge ge gt gt if if lambda lambda length length list list load load lt lt nil nil not not
        null null or or prin3 prin3 quote quote read read rest rest reverse reverse setq setq sub sub
        t t terpri terpri + add - sub)))
   ([amb]  
      (print ">>> ") (flush)
      (try (let [res (evaluar (read) amb nil)]
              (if (nil? (fnext res))
            true
          (do (imprimir (first res)) (repl (fnext res)))))
           (catch Exception e (println) (print "*error* ") (println (get (Throwable->map e) :cause)) (repl amb))))
)

; Carga el contenido de un archivo.
; Aridad 3: Recibe los ambientes global y local y el nombre de un archivo
; (literal como string o atomo, con o sin extension .lsp, o el simbolo ligado al nombre de un archivo en el ambiente), abre el archivo 
; y lee un elemento de la entrada (si falla, imprime nil), lo evalua y llama recursivamente con el (nuevo?) amb., nil, la entrada y un arg. mas: el resultado de la evaluacion.
; Aridad 4: lee un elem. del archivo (si falla, imprime el ultimo resultado), lo evalua y llama recursivamente con el (nuevo?) amb., nil, la entrada y el resultado de la eval.
(defn cargar-arch
  ([amb-global amb-local arch]
    (let [nomb (first (evaluar arch amb-global amb-local))]
      (if (and (coll? nomb) (igual? (first nomb) '*error*))
      (do (imprimir nomb) amb-global) 
        (let [nm (clojure.string/lower-case (str nomb)),
              nom (if (and (> (count nm) 4)(clojure.string/ends-with? nm ".lsp")) nm (str nm ".lsp")),
              ret (try (with-open [in (java.io.PushbackReader. (clojure.java.io/reader nom))]
                             (binding [*read-eval* false] (try (let [res (evaluar (read in) amb-global nil)]
                                                      (cargar-arch (fnext res) nil in res))
                                                             (catch Exception e (imprimir nil) amb-global))))
                 (catch java.io.FileNotFoundException e (imprimir (list '*error* 'file-open-error 'file-not-found nom '1 'READ)) amb-global))]
           ret))))
  ([amb-global amb-local in res]
    (try (let [res (evaluar (read in) amb-global nil)] (cargar-arch (fnext res) nil in res))
         (catch Exception e (imprimir (first res)) amb-global)))
)

; Evalua una expresion usando los ambientes global y local. Siempre retorna una lista con un resultado y un ambiente.
; Si la evaluacion falla, el resultado es una lista con '*error* como primer elemento, por ejemplo: (list '*error* 'too-many-args) y el ambiente es el ambiente global.
; Si la expresion es un escalar numero o cadena, retorna la expresion y el ambiente global.
; Si la expresion es otro tipo de escalar, la busca (en los ambientes local y global) y retorna el valor y el ambiente global.
; Si la expresion es una secuencia nula, retorna nil y el ambiente global.
; Si el primer elemento de la expresion es '*error*, retorna la expresion y el ambiente global.
; Si el primer elemento de la expresion es una forma especial o una macro, valida los demas elementos y retorna el resultado y el (nuevo?) ambiente.
; Si no lo es, se trata de una funcion en posicion de operador (es una aplicacion de calculo lambda), por lo que se llama a la funcion aplicar,
; pasandole 4 argumentos: la evaluacion del primer elemento, una lista con las evaluaciones de los demas, el ambiente global y el ambiente local. 

(defn evaluar [expre amb-global amb-local]
  (if (not (coll? expre))
    (if (or (number? expre) (string? expre)) (list expre amb-global) (list (buscar expre (concat amb-local amb-global)) amb-global))
    (cond (igual? expre nil) (list nil amb-global)
          (igual? (first expre) '*error*) (list expre amb-global)
          (igual? (first expre) 'exit) (if (< (count (next expre)) 1) (list nil nil) (list (list '*error* 'too-many-args) amb-global))
            (igual? (first expre) 'setq) (cond (< (count (next expre)) 2) (list (list '*error* 'list 'expected nil) amb-global)
                                           (igual? (fnext expre) nil) (list (list '*error* 'cannot-set nil) amb-global)
                                           (not (symbol? (fnext expre))) (list (list '*error* 'symbol 'expected (fnext expre)) amb-global)
                         (= (count (next expre)) 2) (let [res (evaluar (first (nnext expre)) amb-global amb-local)]
                                                                         (list (first res) (actualizar-amb amb-global (fnext expre) (first res))))
                         true (let [res (evaluar (first (nnext expre)) amb-global amb-local)]
                                                           (evaluar (cons 'setq (next (nnext expre))) (actualizar-amb amb-global (fnext expre) (first res)) amb-local)))
        (igual? (first expre) 'de) (cond (< (count (next expre)) 2) (list (list '*error* 'list 'expected nil) amb-global)
                         (and (not (igual? (first (nnext expre)) nil)) (not (coll? (first (nnext expre))))) (list (list '*error* 'list 'expected (first (nnext expre))) amb-global)
                                         (igual? (fnext expre) nil) (list (list '*error* 'cannot-set nil) amb-global)
                                         (not (symbol? (fnext expre))) (list (list '*error* 'symbol 'expected (fnext expre)) amb-global)
                         true (list (fnext expre) (actualizar-amb amb-global (fnext expre) (cons 'lambda (nnext expre)))))
        (igual? (first expre) 'quote) (list (if (igual? (fnext expre) nil) nil (fnext expre)) amb-global)
        (igual? (first expre) 'lambda) (cond (< (count (next expre)) 1) (list (list '*error* 'list 'expected nil) amb-global)
                             (and (not (igual? (fnext expre) nil)) (not (coll? (fnext expre)))) (list (list '*error* 'list 'expected (fnext expre)) amb-global)
                             true (list expre amb-global))
        (igual? (first expre) 'if) (macroIf (next expre) amb-global amb-local)
        (igual? (first expre) 'or) (macroOr (next expre) amb-global amb-local)
        (igual? (first expre) 'load) (macroLoad (next expre) amb-global amb-local)
        (igual? (first expre) 'cond) (evaluar-cond (next expre) amb-global amb-local)
        true (aplicar (first (evaluar (first expre) amb-global amb-local)) (map (fn [x] (first (evaluar x amb-global amb-local))) (next expre)) amb-global amb-local)))
)

; Aplica una funcion a una lista de argumentos evaluados, usando los ambientes global y local. Siempre retorna una lista con un resultado y un ambiente.
; Si la aplicacion falla, el resultado es una lista con '*error* como primer elemento, por ejemplo: (list '*error* 'arg-wrong-type) y el ambiente es el ambiente global.
; Aridad 4: Recibe la func., la lista de args. evaluados y los ambs. global y local. Se llama recursivamente agregando 2 args.: la func. revisada y la lista de args. revisada.
; Aridad 6: Si la funcion revisada no es nil, se la retorna con el amb. global.
; Si la lista de args. evaluados revisada no es nil, se la retorna con el amb. global.
; Si no, en caso de que la func. sea escalar (predefinida o definida por el usuario), se devuelven el resultado de su aplicacion (controlando la aridad) y el ambiente global.
; Si la func. no es escalar, se valida que la cantidad de parametros y argumentos coincidan, y:
; en caso de que se trate de una func. lambda con un solo cuerpo, se la evalua usando el amb. global intacto y el local actualizado con los params. ligados a los args.,  
; en caso de haber multiples cuerpos, se llama a aplicar recursivamente, pasando la funcion lambda sin el primer cuerpo, la lista de argumentos evaluados,
; el amb. global actualizado con la eval. del 1er. cuerpo (usando el amb. global intacto y el local actualizado con los params. ligados a los args.) y el amb. local intacto. 
(defn aplicar
   ([f lae amb-global amb-local]
      (aplicar (revisar-f f) (revisar-lae lae) f lae amb-global amb-local))
   ([resu1 resu2 f lae amb-global amb-local]
      (cond resu1 (list resu1 amb-global)
        resu2 (list resu2 amb-global)
        true  (if (not (coll? f))
                  (list (cond
                        (igual? f 'env) (if (> (count lae) 0)
                                  (list '*error* 'too-many-args)
                        (concat amb-global amb-local))
              (igual? f 'first) (let [ari (controlar-aridad lae 1)]
                                    (cond (coll? ari) ari
                                (igual? (first lae) nil) nil
                                (not (coll? (first lae))) (list '*error* 'list 'expected (first lae))
                                    true (ffirst lae)))
              (igual? f 'add) (if (< (count lae) 2)
                                  (list '*error* 'too-few-args)
                                  (try (reduce + lae) 
                             (catch Exception e (list '*error* 'number-expected))))
              (igual? f 'sub) (subTLCLISP lae)
              (igual? f 'reverse) (reverseTLCLISP lae)
              (igual? f 'append) (appendTLCLISP lae)
              (igual? f 'cons) (consTLCLISP lae)
              (igual? f 'equal) (equalTLCLISP lae)
              (igual? f 'length) (lengthTLCLISP lae)
              (igual? f 'ge) (geTLCLISP lae)
              (igual? f 'gt) (gtTLCLISP lae)
              (igual? f 'list) (listTLCLISP lae)
              (igual? f 'lt) (ltTLCLISP lae)
              (igual? f 'not)(notTLCLISP lae)
              (igual? f 'null) (nullTLCLISP lae)
              (igual? f 'rest) (restTLCLISP lae)
              (igual? f 'eval) (evalTLCLISP lae amb-global amb-local)
              (igual? f 'read) (readTLCLISP lae)
              (igual? f 'terpri) (terpriTLCLISP lae)
              (igual? f 'prin3) (prin3TLCLISP lae)
              true (let [lamb (buscar f (concat amb-local amb-global))]
                    (cond (or (number? lamb) (igual? lamb 't) (igual? lamb nil)) (list '*error* 'non-applicable-type lamb)
                                      (or (number? f) (igual? f 't) (igual? f nil)) (list '*error* 'non-applicable-type f)
                                      (igual? (first lamb) '*error*) lamb
                                      true (aplicar lamb lae amb-global amb-local)))) amb-global)
          (cond (< (count lae) (count (fnext f))) (list (list '*error* 'too-few-args) amb-global)
                (> (count lae) (count (fnext f))) (list (list '*error* 'too-many-args) amb-global)
                true (if (nil? (next (nnext f)))
                         (evaluar (first (nnext f)) amb-global (concat (reduce concat (map list (fnext f) lae)) amb-local))
                       (aplicar (cons 'lambda (cons (fnext f) (next (nnext f)))) lae (fnext (evaluar (first (nnext f)) amb-global (concat (reduce concat (map list (fnext f) lae)) amb-local))) amb-local))))))
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;MACROS


(defn wrapperOrTLCLISP [expre amb-global amb-local]
  (let [elem (first expre)]
    (cond
	    (empty? expre) nil
	    (igual? 't elem) 't
	    (coll? elem) (cond 
	                   (igual? 't (first (evaluar elem amb-global amb-local))) 't 
	                   (igual? nil (first (evaluar elem amb-global amb-local))) (wrapperOrTLCLISP (next expre) amb-global amb-local)
	                   true (first (evaluar elem amb-global amb-local)) ;Caso de una evaluacion fallida
	                  )
	    true (wrapperOrTLCLISP (next expre) amb-global amb-local)
  		)
   )
)

(defn macroOr [lis amb-global amb-local]
    (if (igual? nil lis) (list lis amb-global))
    (list (wrapperOrTLCLISP lis amb-global amb-local) amb-global)
)

(defn macroIf [lis amb-global amb-local]
  (let [len (count lis) condicion (first (evaluar (first lis) amb-global amb-local))]
	  (cond 
	    (>  len 3) (list (list '*error* 'too-many-args) amb-global)
	    (< len 2) (list (list '*error* 'not-enough-args) amb-global)
	    (igual? 't condicion) (evaluar (nth lis 1) amb-global amb-local)
	    (and (igual? 'nil condicion) (= len 2)) nil
	    (and (igual? 'nil condicion) (= len 3)) (evaluar (nth lis 2) amb-global amb-local)
	    true (list condicion amb-global) ;Caso de error
	  ) 
  )
)


(defn macroLoad [lis amb-global amb-local]
  (cond 
  (> (count lis) 1) (list (list '*error* 'too-many-args) amb-global)
  (empty? lis) (list (list '*error* 'not-enough-args) amb-global)
  true (list nil (cargar-arch amb-global amb-local (first lis))))
)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;FUNCIONES

(defn prin3TLCLISP [lae]
  (let [aridad (controlar-aridad lae 1)]
	  (cond
	    (coll? aridad) aridad
	    true (do (println (first lae)) (first lae))
	  ) 
  )
)


(defn terpriTLCLISP [lae]
  (let [aridad (controlar-aridad lae 0)]
	  (cond
	    (coll? aridad) aridad
	    true (imprimir nil nil)
	  ) 
  )
)


(defn readTLCLISP [lae]
  (let [aridad (controlar-aridad lae 0)]
	  (cond
	    (coll? aridad) aridad
	    true (read)
	  )
  )
)

(defn evalTLCLISP [lae amb-global amb-local]
  (let [aridad (controlar-aridad lae 1)]
	  (cond
	    (coll? aridad) aridad
	    true (first (evaluar (first lae) amb-global amb-local))
	  ) 
  )
)

(defn restTLCLISP [lae]
  (let [aridad (controlar-aridad lae 1) lis (first lae)]
	   (cond
	    (coll? aridad) aridad
	    (igual? nil lis) '()
	    (not (coll? lis)) (list '*error* 'arg-wrong-type)
	    true (rest lis)
	  ) 
  )
)


(defn nullTLCLISP [lae]
  (let [aridad (controlar-aridad lae 1)]
	    (if (coll? aridad) 
		    aridad
		    (if (igual? "nil" (first lae))
			    't
			    nil)
	  		)
  )
)

(defn notTLCLISP [lae]
  (let [aridad (controlar-aridad lae 1) bool (first lae)]
	  (cond
	    (coll? aridad) aridad
	    (not (or (igual? 't bool) (igual? nil bool))) (list '*error* 'boolean-expected)
	    true (if (igual? 't bool)
	      nil
	      't)
	  )
  )
)

(defn ltTLCLISP [lae]
  (let [aridad (controlar-aridad lae 2)]
	  (if (coll? aridad) 
	    aridad
	    (try (cond
	          (reduce < lae) 't 
	          true nil
	        )
	      (catch Exception e (list '*error* 'number-expected))
	    )
	  )
  )
)

(defn listTLCLISP 
  ([lae] lae)
  ([] nil)
)

(defn gtTLCLISP [lae]
	(let [aridad (controlar-aridad lae 2)]
		(if (coll? aridad) 
		    aridad
		    (try (cond
		          (reduce > lae) 't 
		          true nil
		        )
		      (catch Exception e (list '*error* 'number-expected))
		    )
		  )
  )
)

(defn geTLCLISP [lae]
	(let [aridad (controlar-aridad lae 2)]
		(if (coll? aridad) 
		    aridad
		    (try (cond
		          (reduce >= lae) 't 
		          true nil
		        )
		      (catch Exception e (list '*error* 'number-expected))
		    )
		  )
  )
)


(defn lengthTLCLISP [lae]
  (let [aridad (controlar-aridad lae 1) lis (first lae)]
	  (cond
	    (coll? aridad) aridad
	    (and (not (coll? lis)) (not (igual? nil (first lae))))(list '*error* 'arg-wrong-type)
	    true (count (first lae))
	  )  
  )
)

(defn equalTLCLISP [lae]
  (let [aridad (controlar-aridad lae 2)]
	  (if (coll? aridad)
	    aridad
	    (if (igual? true (igual? (first lae) (nth lae 1)))
	    't
	    nil
	    )
	  ) 
  )
)

(defn consTLCLISP [lae]
  (let [aridad (controlar-aridad lae 2) elem (first lae) lis (nth lae 1)]
	  (cond
	    (coll? aridad) aridad
	    (igual? nil lis) (cons elem nil)
	    (not (coll? lis)) (list '*error* 'not-implemented)
	    true (cons elem lis)
	  )    
  )
)

(defn appendTLCLISP [lae]
 (let [aridad (controlar-aridad lae 2) listOne (first lae) listTwo (nth lae 1)]
		(cond
		    (coll? aridad) aridad
		    (and (not (coll? listOne)) (not (igual? nil listOne))) (list '*error* 'arg-wrong-type)
		    (and (not (coll? listTwo)) (not (igual? nil listTwo))) (list '*error* 'not-implemented)
		    true (concat  listOne listTwo)
		  )   
  )
)

(defn subTLCLISP [lae]
  (if (< (count lae) 2)
    (list '*error* 'too-few-args)
    (try (reduce - lae) 
      (catch Exception e (list '*error* 'number-expected))
    )
  )
)

(defn reverseTLCLISP [lae]
  (let [lis (first lae)]
	  (if (and (not (coll? lis)) (not (igual? nil lis)))
	    (list '*error* 'arg-wrong-type)
	    (reverse lis)
	  ) 
  )

)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;FUNCIONES AUXILIARES

; Controla la aridad (cantidad de argumentos de una funcion)
; Recibe una lista y un numero. Si la longitud de la lista coincide con el numero, retorna el numero.
; Si es menor, retorna (list '*error* 'too-few-args).
; Si es mayor, retorna (list '*error* 'too-many-args).
(defn controlar-aridad [lis valor-esperado]
  (cond
    (== valor-esperado (count lis)) valor-esperado 
    (<= valor-esperado (count lis)) (list '*error* 'too-many-args)
    true (list '*error* 'too-few-args)
  )
)


; Revisa una lista que representa una funcion.
; Recibe la lista y, si esta comienza con '*error*, la retorna. Si no, retorna nil.
(defn revisar-f [lis]
  (cond
    (not (coll? lis)) nil
    (igual? '*error* (first lis)) lis
    true nil
  )
)

;Devuelve el equivalente para comparar
;Transforma una lista vacía o "nil" (ya sea en cadena o como simbolo) en el valor nil
;Transforma el símbolo o cadena "t" en el valor true
;Transforma la cadena o el símbolo en cadena en minúsculas.
;Si no, lo devuelve como está
(defn equivalentToCompare [x]
  (cond
    (or (= x '()) (= nil x) (= false x)) "nil"
    (true? x) "t"
    (or (symbol? x) (instance? String x)) (clojure.string/lower-case (str x))
    true x
  )
)

; Compara la igualdad de dos simbolos.
; Recibe dos simbolos a y b. Retorna true si se deben considerar iguales; si no, false.
; Se utiliza porque TLC-LISP no es case-sensitive y ademas no distingue entre nil y la lista vacia.
(defn igual? [a b]
  (= (equivalentToCompare a) (equivalentToCompare b))
)

; Revisa una lista de argumentos evaluados.
; Recibe la lista y, si esta contiene alguna sublista que comienza con '*error*, retorna esa sublista. Si no, retorna nil.
(defn revisar-lae [lis] 
  (cond
    (empty? lis) nil
    (not (nil? (revisar-f (first lis)))) (first lis)
    true (revisar-lae (next lis))
  )
)

;Agrega el elemento elem entre cada elemento de la lista
;Al final, agrega un salto de línea
(defn addBetweenElements [lis elem]
  (concat (butlast (flatten (map (fn [x] (concat x elem)) (map list lis)))) '(\newline) )
)

;Compara si el simbolo x es un igual a los simbolos 1 o 2 
;Si x es igual a uno de ellos, devuelve true
;Caso contrario, false
(defn compareSymbols [x a b]
    (or (= a x) (= b x))
)

;(or (compareStrings strA string) (compareStrings strB string))

;Imprime el contenido de la lista con espacios en el medio y un salto de línea al final
(defn printListContent [lis]
  (map (fn [x] (if (compareSymbols x \space \newline) (print x) (pr x))) (addBetweenElements lis '(\space)))
)

;Imprime primer elemento de lis y luego, imprime un espacio.
;Después, llama a imprimir con el resto de lis y el segundo parametro intacto
(defn imprimirWrapper [lis orig]
  (pr (first lis))
  (print \space)
  (imprimir (rest lis) orig)
)

; Imprime, con salto de linea, atomos o listas en formato estandar (las cadenas con comillas) y devuelve su valor. Muestra errores sin parentesis.
; Aridad 1: Si recibe un escalar, lo imprime con salto de linea en formato estandar (pero si es \space no lo imprime), purga la salida y devuelve el escalar.
; Si recibe una secuencia cuyo primer elemento es '*error*, se llama recursivamente con dos argumentos iguales: la secuencia recibida.
; Si no, imprime lo recibido con salto de linea en formato estandar, purga la salida y devuelve la cadena.
; Aridad 2: Si el primer parametro es nil, imprime un salto de linea, purga la salida y devuelve el segundo parametro.
; Si no, imprime su primer elemento en formato estandar, imprime un espacio y se llama recursivamente con la cola del primer parametro y el segundo intacto.

(defn imprimir
  ([elem]
    (let [x elem]
      (cond
        (igual? '\space x) \newline
        (coll? x) (if (igual? '*error* (first x)) (dorun (printListContent x)) (prn x))
        true (prn x)
      )
      x
    )
  )
  ([lis orig] 
    (let [x lis y orig]
      (cond
        (igual? x nil) (print \newline)
        true (imprimirWrapper x y)
      )
      y
    )
  )
)

; Busca una clave en un ambiente (una lista con claves en las posiciones impares [1, 3, 5...] y valores en las pares [2, 4, 6...] y retorna el valor asociado.
;Si no la encuentra, retorna una lista con '*error* en la 1ra. pos., 'unbound-symbol en la 2da. y el elemento en la 3ra.
(defn buscar [elem lis] 
  (cond
    (empty? lis) (list '*error* 'unbound-symbol elem)
    (igual? elem (first lis)) (nth lis 1)
    true (buscar elem (nthnext lis 2))
  )
)

;Transpone una matriz (lista de listas)
(defn transponer [m]
  (apply map list m)
)

;Devuelve una lista de vectores que contienen la clave y su posicion en el arreglo lis
(defn getKeysList [lis]
  (let [length (count lis)]
	  (for [[x i] (transponer (list lis (range length)))
	      :when (even? i)]
	 	[x i])
  )
)

;Obtiene las posiciones de la lista en las que se encuentra el elemento elem
(defn getPosition [lis elem]
  (let [keysList (getKeysList lis)]
	  (for [x keysList
	      :when (igual? (first x) elem)]
	  (nth x 1))   
  )
)

;Reemplaza el valor de la lista que se encuentra en la posicion pos por el valor pasado por parametro
(defn actualizarValor [lis pos valor]
  (let [primeraParte (take pos lis) segundaParte (nthnext lis (+ pos 1))]
    (concat (concat primeraParte (list valor)) segundaParte)
  )
)

; Actualiza un ambiente (una lista con claves en las posiciones impares [1, 3, 5...] y valores en las pares [2, 4, 6...]
; Recibe el ambiente, la clave y el valor.
; Si el valor no es escalar y en su primera posicion contiene '*error*, retorna el ambiente intacto.
; Si no, coloca la clave y el valor en el ambiente (puede ser un alta o una actualizacion) y lo retorna.
(defn actualizar-amb [amb-global clave valor]
  (if (coll? valor) 
    (if (igual? '*error* (first valor)) 
      (amb-global) 
    )
  )
  (let [listKeysPositions (getPosition amb-global clave)]
    (if (empty? listKeysPositions)
	    (concat amb-global (list clave valor))
	    (actualizarValor amb-global (+ 1 (first listKeysPositions)) valor)
	  )
  )
)


; Evalua (con evaluar) secuencialmente las sublistas de una lista y retorna el valor de la ultima evaluacion.
(defn evaluar-secuencia-en-cond [lis amb-global amb-local]
  (let [resultado (evaluar (first lis) amb-global amb-local)]
	  (if (== (count lis) 1) ;Condición de corte
	    resultado
	    (evaluar-secuencia-en-cond (rest lis) (nth resultado 1) amb-local)
	  )
  )
)

; Evalua el cuerpo de una macro COND. Siempre retorna una lista con un resultado y un ambiente.
; Recibe una lista de sublistas (cada una de las cuales tiene una condicion en su 1ra. posicion) y los ambientes global y local.
; Si la lista es nil, el resultado es nil y el ambiente retornado es el global.
; Si no, evalua (con evaluar) la cabeza de la 1ra. sublista y, si el resultado no es nil, retorna el res. de invocar a evaluar-secuencia-en-cond con la cola de esa sublista.
; En caso contrario, sigue con las demas sublistas.

(defn matchesCondition [amb-global amb-local lis]
  (first (evaluar (first lis) amb-global amb-local))
)

; Evalua el cuerpo de una macro COND. Siempre retorna una lista con un resultado y un ambiente.
(defn evaluar-cond [lis amb-global amb-local]
  (if (nil? lis)
    (list nil amb-global)
    (let [matched (filter (fn[x] (matchesCondition amb-global amb-local x)) lis)]
    (if (empty? matched)
      (list nil amb-global)
      (evaluar-secuencia-en-cond (rest (first matched)) amb-global amb-local))))
)



(true? true)