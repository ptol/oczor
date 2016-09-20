;;; -*- lexical-binding: t -*-
(require 'cl)
(setq oc (make-hash-table :test 'equal))

; (defun oc-hash-from-alist (alist)
;   (let ((hash (make-hash-table :test 'equal)))
;     (mapcar (lambda (element)
;               (puthash (car element) (cdr element) hash))
;             alist)
;     hash))

(defun oc-hash-from-alist (plist)
  (let ((pl plist)
        (result (make-hash-table :test 'equal)))
    (while pl
      (puthash (car pl) (cadr pl) result)
      (setq pl (cddr pl)))
    result))

(puthash 'instances (make-hash-table :test 'equal) oc)

(puthash 'clone-object (lambda (obj) (copy-hash-table obj)) oc)

; (puthash 'clone-object (lambda (obj)
;   (let ((result (make-hash-table :test 'equal)))
;     (maphash (lambda (key val) (puthash key val result)) obj)
;     result)) oc)

(progn
  (if
    (eq (gethash 'std oc) nil)
    (puthash 'std (oc-hash-from-alist '()) oc)
    )
  (puthash 'ff (let*
    ((print (lambda (x) (progn (princ x) (princ "\n"))))

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
)
    (oc-hash-from-alist (list
      'unit unit
      'show-int show-int
      'show-double show-double
      'show-string show-string
      'show-char show-char
      'show-bool show-bool
      'eq-any eq-any
      'eq-int eq-int
      'eq-double eq-double
      'eq-string eq-string
      'eq-char eq-char
      'eq-bool eq-bool
      '_not _not
      'and-bool and-bool
      'or-bool or-bool
      'add-int add-int
      'mul-int mul-int
      'sub-int sub-int
      'div-int div-int
      'add-double add-double
      'mul-double mul-double
      'sub-double sub-double
      'div-double div-double
      'append-string append-string
      'print print))) (gethash 'std oc)))
(progn
  (if
    (eq (gethash 'std oc) nil)
    (puthash 'std (oc-hash-from-alist '()) oc)
    )
  (puthash 'numbers (let*
    (
    (add nil)
    (zero nil)
    (mul nil)
    (one nil)
    (sub nil)
    (div nil)
    (negate nil))
    (puthash 'add (oc-hash-from-alist '()) (gethash 'instances oc))
    (setq add (lambda (x p1 p2) (funcall x p1 p2)))
    (puthash 'zero (oc-hash-from-alist '()) (gethash 'instances oc))
    (setq zero (lambda (x) x))
    (puthash 'mul (oc-hash-from-alist '()) (gethash 'instances oc))
    (setq mul (lambda (x p1 p2) (funcall x p1 p2)))
    (puthash 'one (oc-hash-from-alist '()) (gethash 'instances oc))
    (setq one (lambda (x) x))
    (puthash 'sub (oc-hash-from-alist '()) (gethash 'instances oc))
    (setq sub (lambda (x p1 p2) (funcall x p1 p2)))
    (puthash 'div (oc-hash-from-alist '()) (gethash 'instances oc))
    (setq div (lambda (x p1 p2) (funcall x p1 p2)))
    (puthash 'int (gethash 'add-int (gethash 'ff (gethash 'std oc))) (gethash 'add (gethash 'instances oc)))
    (puthash 'int (gethash 'mul-int (gethash 'ff (gethash 'std oc))) (gethash 'mul (gethash 'instances oc)))
    (puthash 'int 0 (gethash 'zero (gethash 'instances oc)))
    (puthash 'int 1 (gethash 'one (gethash 'instances oc)))
    (puthash 'int (gethash 'sub-int (gethash 'ff (gethash 'std oc))) (gethash 'sub (gethash 'instances oc)))
    (puthash 'int (gethash 'div-int (gethash 'ff (gethash 'std oc))) (gethash 'div (gethash 'instances oc)))
    (puthash 'double (gethash 'add-double (gethash 'ff (gethash 'std oc))) (gethash 'add (gethash 'instances oc)))
    (puthash 'double (gethash 'mul-double (gethash 'ff (gethash 'std oc))) (gethash 'mul (gethash 'instances oc)))
    (puthash 'double 0.0 (gethash 'zero (gethash 'instances oc)))
    (puthash 'double 1.0 (gethash 'one (gethash 'instances oc)))
    (puthash 'double (gethash 'sub-double (gethash 'ff (gethash 'std oc))) (gethash 'sub (gethash 'instances oc)))
    (puthash 'double (gethash 'div-double (gethash 'ff (gethash 'std oc))) (gethash 'div (gethash 'instances oc)))
    (setq negate (lambda (_nzero _nsub x) (funcall _nsub (funcall zero _nzero) x)))
    (oc-hash-from-alist (list
      'add add
      'zero zero
      'mul mul
      'one one
      'sub sub
      'div div
      'negate negate))) (gethash 'std oc)))
(progn
  (if
    (eq (gethash 'std oc) nil)
    (puthash 'std (oc-hash-from-alist '()) oc)
    )
  (puthash 'prelude (let*
    (
    (flip nil)
    (cnst nil)
    (id nil)
    (fst nil)
    (snd nil)
    (show nil)
    (_eq nil)
    (mempty nil)
    (append nil)
    (map nil)
    (apply nil)
    (pure nil)
    (bind nil)
    (traverse nil))
    (setq flip (lambda (f) (lambda (a b) (funcall f b a))))
    (setq cnst (lambda (x) (lambda (y) x)))
    (setq id (lambda (x) x))
    (setq fst (lambda (x y) x))
    (setq snd (lambda (x y) y))
    (puthash 'show (oc-hash-from-alist '()) (gethash 'instances oc))
    (setq show (lambda (x p1) (funcall x p1)))
    (puthash '_eq (oc-hash-from-alist '()) (gethash 'instances oc))
    (setq _eq (lambda (x p1 p2) (funcall x p1 p2)))
    (puthash 'mempty (oc-hash-from-alist '()) (gethash 'instances oc))
    (setq mempty (lambda (x) x))
    (puthash 'append (oc-hash-from-alist '()) (gethash 'instances oc))
    (setq append (lambda (x p1 p2) (funcall x p1 p2)))
    (puthash 'map (oc-hash-from-alist '()) (gethash 'instances oc))
    (setq map (lambda (x p1 p2) (funcall x p1 p2)))
    (puthash 'apply (oc-hash-from-alist '()) (gethash 'instances oc))
    (setq apply (lambda (x p1 p2) (funcall x p1 p2)))
    (puthash 'pure (oc-hash-from-alist '()) (gethash 'instances oc))
    (setq pure (lambda (x p1) (funcall x p1)))
    (puthash 'bind (oc-hash-from-alist '()) (gethash 'instances oc))
    (setq bind (lambda (x p1 p2) (funcall x p1 p2)))
    (puthash 'traverse (oc-hash-from-alist '()) (gethash 'instances oc))
    (setq traverse (lambda (x p1 p2) (funcall x p1 p2)))
    (puthash 'int (gethash 'show-int (gethash 'ff (gethash 'std oc))) (gethash 'show (gethash 'instances oc)))
    (puthash 'double (gethash 'show-double (gethash 'ff (gethash 'std oc))) (gethash 'show (gethash 'instances oc)))
    (puthash 'string (gethash 'show-string (gethash 'ff (gethash 'std oc))) (gethash 'show (gethash 'instances oc)))
    (puthash 'char (gethash 'show-char (gethash 'ff (gethash 'std oc))) (gethash 'show (gethash 'instances oc)))
    (puthash 'bool (gethash 'show-bool (gethash 'ff (gethash 'std oc))) (gethash 'show (gethash 'instances oc)))
    (puthash 'int (gethash 'eq-int (gethash 'ff (gethash 'std oc))) (gethash '_eq (gethash 'instances oc)))
    (puthash 'double (gethash 'eq-double (gethash 'ff (gethash 'std oc))) (gethash '_eq (gethash 'instances oc)))
    (puthash 'string (gethash 'eq-string (gethash 'ff (gethash 'std oc))) (gethash '_eq (gethash 'instances oc)))
    (puthash 'char (gethash 'eq-char (gethash 'ff (gethash 'std oc))) (gethash '_eq (gethash 'instances oc)))
    (puthash 'bool (gethash 'eq-bool (gethash 'ff (gethash 'std oc))) (gethash '_eq (gethash 'instances oc)))
    (puthash 'string "" (gethash 'mempty (gethash 'instances oc)))
    (puthash 'string (gethash 'append-string (gethash 'ff (gethash 'std oc))) (gethash 'append (gethash 'instances oc)))
    (oc-hash-from-alist (list
      'flip flip
      'cnst cnst
      'id id
      'fst fst
      'snd snd
      'show show
      '_eq _eq
      'mempty mempty
      'append append
      'map map
      'apply apply
      'pure pure
      'bind bind
      'traverse traverse))) (gethash 'std oc)))
(puthash 'test_code (let*
  (
  (x1 nil)
  (x2 nil)
  (func nil)
  (x3 nil)
  (func2 nil))
  (setq x1 1)
  (setq x2 (oc-hash-from-alist (list
    'foo 1
    'bar 1)))
  (setq func (lambda (x y) (+ (+ x y) 1)))
  (setq x3 (funcall func 1 2))
  (setq func2 (lambda (x) (let* (
    
    (temp nil)
  ) (progn
    (setq temp (eql x 1))
    (not (and temp nil))))))
  (oc-hash-from-alist (list
    'x1 x1
    'x2 x2
    'func func
    'x3 x3
    'func2 func2
    'temp1 x1
    'temp2 x2
    'temp3 x3))) oc)
