(require '[clojure.test :refer [is deftest run-tests]])

(load-file "tlc-lisp.clj")

(deftest test-evaluar
  (is (=  '(3 (+ add r 3)) (evaluar '(setq r 3) '(+ add) nil)))
  (is (= '(doble (+ add doble (lambda (x) (+ x x)))) (evaluar '(de doble (x) (+ x x)) '(+ add) nil))) 
  (is (=  '(5 (+ add)) (evaluar '(+ 2 3) '(+ add) nil))) 
  (is (= '((*error* unbound-symbol +) (add add)) (evaluar '(+ 2 3) '(add add) nil)))
  (is (= '(6 (+ add doble (lambda (x) (+ x x)))) (evaluar '(doble 3) '(+ add doble (lambda (x) (+ x x))) nil)))
  (is (= '(8 (+ add r 4 doble (lambda (x) (+ x x)))) (evaluar '(doble r) '(+ add r 4 doble (lambda (x) (+ x x))) nil)))
  (is (= '(6 (+ add)) (evaluar '((lambda (x) (+ x x)) 3) '(+ add) nil)))
  (is (= '(8 (+ add r 4 doble (lambda (x) (+ x x)))) (evaluar '(doble r) '(+ add r 4 doble (lambda (x) (+ x x))) nil)))
  (is (= '(nil (equal equal)) (evaluar '(if (equal 1 2) (equal 3 3) (equal 4 5)) '(equal equal) nil)))
  (is (= '(nil (equal equal)) (evaluar '(if (equal 1 1) (equal 3 4) (equal x 5)) '(equal equal) nil)))
  (is (= '(t (equal equal))  (evaluar '(if (equal 1 1) (equal 3 3) (equal x 5)) '(equal equal) nil)))
  (is (= '(t (equal equal))  (evaluar '(if (equal 1 1) (equal 3 3) (equal 4 5)) '(equal equal) nil)))
  (is (= '(t (equal equal))  (evaluar '(if (equal 1 1) (equal 3 3) (equal 4 5)) '(equal equal) nil)))
  (is (= '((*error* unbound-symbol x) (equal equal))  (evaluar '(if (equal 1 1) (equal x 3) (equal 4 5)) '(equal equal) nil)))
  (is (= '((*error* unbound-symbol x) (equal equal))  (evaluar '(if (equal x 1) (equal 3 3) (equal 4 5)) '(equal equal) nil)))
  (is (= '(t (equal equal)) (evaluar '(or (equal 1 2) (equal 3 4) (equal 5 5)) '(equal equal) nil)))
  (is (= '(t (equal equal)) (evaluar '(or (equal 1 1) (equal 3 4) (equal x 5)) '(equal equal) nil)))
  (is (= '(nil (equal equal)) (evaluar '(or (equal 1 2) (equal 3 4) (equal 5 7)) '(equal equal) nil)))
  (is (= '(nil (equal equal x 8)) (evaluar '(or (equal 2 1) (equal 3 4) (equal x 5)) '(equal equal x 8) nil)))
  (is (= '((*error* unbound-symbol x) (equal equal))  (evaluar '(or (equal 2 1) (equal x 3) (equal 4 5)) '(equal equal) nil)))
)

(deftest test-igual?
  (is (= 'true (igual? nil 'NIL)))
  (is (= 'true (igual? nil "NIL")))
  (is (= 'true (igual? nil ())))
  (is (= 'true (igual? () 'NIL)))
)

(deftest test-actualizar-amb
	(is (= '(+ add - sub x 1) (actualizar-amb '(+ add - sub) 'x 1)))
 (is (= '(+ add - sub x 3 y 2) (actualizar-amb '(+ add - sub x 1 y 2) 'x 3)))
 (is (= '(+ add - sub y x x 3) (actualizar-amb '(+ add - sub y x x 1) 'x 3)))
)

(deftest test-revisar-f
  (is (= 'nil (revisar-f 'doble)))
  (is (= '(*error* too-few-args) (revisar-f '(*error* too-few-args))))
)

(deftest test-revisar-lae
  (is (= 'nil (revisar-lae '(1 add first))))
  (is (= '(*error* too-many-args) (revisar-lae '(1 add (*error* too-many-args) first))))
  (is (= '(*error* too-few-args) (revisar-lae '((*error* too-few-args) 4))))
)

(deftest test-evaluar-cond
  (is (= '(nil (equal equal setq setq)) (evaluar-cond nil '(equal equal setq setq) nil)))
  (is (= '(nil (equal equal first first)) (evaluar-cond '(((equal 'a 'b) (setq x 1))) '(equal equal first first) nil)))
  (is (= '(2 (equal equal setq setq y 2)) (evaluar-cond '(((equal 'a 'b) (setq x 1)) ((equal 'a 'a) (setq y 2))) '(equal equal setq setq) nil)))
  (is (= '(3 (equal equal setq setq y 2 z 3)) (evaluar-cond '(((equal 'a 'b) (setq x 1)) ((equal 'a 'a) (setq y 2) (setq z 3))) '(equal equal setq setq) nil))) 
)

(deftest test-buscar
  (is (= 'sub (buscar '- '(+ add - sub))))
  (is (= '(*error* unbound-symbol doble) (buscar 'doble '(+ add - sub))))
  (is (= 'y (buscar 'x '(+ z y x x y))))
  (is (= '(*error* unbound-symbol x) (buscar 'x '(+ z y x + y))))
)

(deftest test-evaluar-secuencia-en-cond
  (is (= '(2 (setq setq y 2)) (evaluar-secuencia-en-cond '((setq y 2)) '(setq setq) nil)))
  (is (= '(3 (setq setq y 2 z 3)) (evaluar-secuencia-en-cond '((setq y 2) (setq z 3)) '(setq setq) nil)))
)

(deftest test-controlar-aridad
  (is (= '(*error* too-few-args) (controlar-aridad '(a b c) 4)))
  (is (= 4 (controlar-aridad '(a b c d) 4)))
  (is (= '(*error* too-many-args) (controlar-aridad '(a b c d e) 4)))
)

(deftest test-aplicar
  (is (= '((a b) (cons cons)) (aplicar 'cons '(a (b)) '(cons cons) nil)))
  (is (= '(9 (+ add r 5)) (aplicar 'add '(4 5) '(+ add r 5) nil)))
  (is (= '(8 (+ add r 4 doble (lambda (x) (+ x x)))) (aplicar '(lambda (x) (+ x x)) '(4) '(+ add r 4 doble (lambda (x) (+ x x))) nil)))
  (is (= '(-1 (- sub r 5)) (aplicar 'sub '(4 5) '(- sub r 5) nil)))
  (is (= '(t nil) (aplicar 'equal '(nil NIL) nil nil)))
  (is (= '(t nil) (aplicar 'equal '(nil niL) nil nil)))
  (is (= '(t nil) (aplicar 'equal '(() NIL) nil nil)))
  (is (= '(nil nil) (aplicar 'equal '(hola chau) nil nil)))
  (is (= '(nil nil) (aplicar 'equal '(() CHAU) nil nil)))
  (is (= '(t nil) (aplicar 'equal '(CHau chau) nil nil)))
  (is (= '(t nil) (aplicar 'ge '(5 4) nil nil)))
  (is (= '(t nil) (aplicar 'ge '(5 5) nil nil)))
  (is (= '(nil nil) (aplicar 'ge '(4 5) nil nil)))
  (is (= '(t nil) (aplicar 'gt '(85 5) nil nil)))
  (is (= '(nil nil) (aplicar 'gt '(5 5) nil nil)))
  (is (= '(nil nil) (aplicar 'gt '(1 5) nil nil)))
  (is (= '(nil nil) (aplicar 'lt '(85 5) nil nil)))
  (is (= '(nil nil) (aplicar 'lt '(5 5) nil nil)))
  (is (= '(t nil) (aplicar 'lt '(1 5) nil nil)))
 	(is (= '(nil nil) (aplicar 'lt '(85 5) nil nil)))
  (is (= '(nil nil) (aplicar 'lt '(5 5) nil nil)))
  (is (= '(t nil) (aplicar 'lt '(1 5) nil nil)))
  (is (= '(niL (equal equal) (aplicar 'not '((equal 1 1)) (equal equal) nil))))
)

(run-tests)