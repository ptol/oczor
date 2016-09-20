(print (lambda (x) (progn (princ x) (princ "\n"))))

(add-any (lambda (x y) (+ x y)))

(mul-any (lambda (x y) (* x y)))

(sub-any (lambda (x y) (- x y)))

(div-any (lambda (x y) (/ x y)))

(eq-any (lambda (x y) (eql x y)))


(unit '())
(empty-object '())

(eq-int eq-any)
(eq-double eq-any)
(eq-string (lambda (x y) (string= x y)))
(eq-char eq-any)
(eq-bool eq-any)

(show-int 'number-to-string)

(show-double 'number-to-string)
(show-string (lambda (x) x))
(show-char (lambda (x) x))
(show-bool (lambda (x) (if x "true" "false")))

(add-int add-any)
(mul-int mul-any)
(sub-int mul-any)
(div-int mul-any)

(add-double add-any)
(mul-double mul-any)
(sub-double mul-any)
(div-double mul-any)


(or-bool (lambda (x y) (or x y)))
(and-bool (lambda (x y) (and x y)))

(add-bool 'orBool)
(mul-bool 'andBool)

(_not 'not)

(append-string 'concat)
