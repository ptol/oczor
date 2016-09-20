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
(puthash 'testing (let*
  (
  (check nil)
  (check-eq nil))
  (setq check (lambda (b) (funcall (gethash 'print (gethash 'ff (gethash 'std oc))) (if b "Pass" "FAIL!!!"))))
  (setq check-eq (lambda (_neq x y) (funcall check (funcall _neq x y))))
  (oc-hash-from-alist (list
    'check check
    'check-eq check-eq))) oc)
(progn
  (if
    (eq (gethash 'std oc) nil)
    (puthash 'std (oc-hash-from-alist '()) oc)
    )
  (puthash 'foldable (let*
    (
    (foldl nil)
    (any nil)
    (all nil)
    (or nil)
    (and nil)
    (sum nil)
    (product nil)
    (contains nil)
    (count nil)
    (fold-map nil)
    (concat nil))
    (puthash 'foldl (oc-hash-from-alist '()) (gethash 'instances oc))
    (setq foldl (lambda (x p1 p2 p3) (funcall x p1 p2 p3)))
    (setq any (lambda (_qfoldl f l) (funcall _qfoldl (lambda (a e) (or a (funcall f e))) nil l)))
    (setq all (lambda (_aefoldl f l) (funcall _aefoldl (lambda (a e) (and a (funcall f e))) t l)))
    (setq or (lambda (_amfoldl l) (funcall any _amfoldl (gethash 'id (gethash 'prelude (gethash 'std oc))) l)))
    (setq and (lambda (_aufoldl l) (funcall all _aufoldl (gethash 'id (gethash 'prelude (gethash 'std oc))) l)))
    (setq sum (lambda (_azfoldl _bmzero _bmadd l) (funcall _azfoldl _bmadd (funcall (gethash 'zero (gethash 'numbers (gethash 'std oc))) _bmzero) l)))
    (setq product (lambda (_brfoldl _ceone _cemul l) (funcall _brfoldl _cemul (funcall (gethash 'one (gethash 'numbers (gethash 'std oc))) _ceone) l)))
    (setq contains (lambda (_cteq _cufoldl x l) (funcall any _cufoldl (lambda (y) (funcall _cteq x y)) l)))
    (setq count (lambda (_czfoldl l) (funcall _czfoldl (lambda (a e) (+ a 1)) 0 l)))
    (setq fold-map (lambda (_dymempty _dyappend _dlfoldl f l) (funcall _dlfoldl (lambda (a e) (funcall _dyappend a (funcall f e))) (funcall (gethash 'mempty (gethash 'prelude (gethash 'std oc))) _dymempty) l)))
    (setq concat (lambda (_ecfoldl _ehmempty _ehappend l) (funcall fold-map _ehmempty _ehappend _ecfoldl (gethash 'id (gethash 'prelude (gethash 'std oc))) l)))
    (oc-hash-from-alist (list
      'foldl foldl
      'any any
      'all all
      'or or
      'and and
      'sum sum
      'product product
      'contains contains
      'count count
      'fold-map fold-map
      'concat concat))) (gethash 'std oc)))
(progn
  (if
    (eq (gethash 'std oc) nil)
    (puthash 'std (oc-hash-from-alist '()) oc)
    )
  (puthash 'maybes (let*
    (
    (none nil)
    (none-maybe nil)
    (pure-maybe nil)
    (maybe nil)
    (is-none nil)
    (is-just nil)
    (from-maybe nil)
    (map-maybe nil)
    (bind-maybe nil)
    (apply-maybe nil)
    (show-maybe nil)
    (eq-maybe nil)
    (foldl-maybe nil))
    (setq none (lambda))
    (puthash 'none (gethash 'eq-any (gethash 'ff (gethash 'std oc))) (gethash '_eq (gethash 'instances oc)))
    (setq none-maybe none)
    (setq pure-maybe (lambda (x) (oc-hash-from-alist (list
      'just x))))
    (setq maybe (lambda (_a1 _a2 _a3) (if
      (funcall (lambda (x f _a) (funcall (gethash 'none (gethash '_eq (gethash 'instances oc))) _a none)) _a1 _a2 _a3)
      (funcall (lambda (x f _a) x) _a1 _a2 _a3)
      (if
        (funcall (lambda (x f z) (gethash 'just z)) _a1 _a2 _a3)
        (funcall (lambda (x f z) (funcall f (gethash 'just z))) _a1 _a2 _a3)
        (error "cases error")))))
    (setq is-none (lambda (x) (funcall maybe t (funcall (gethash 'cnst (gethash 'prelude (gethash 'std oc))) nil) x)))
    (setq is-just (lambda (x) (funcall maybe nil (funcall (gethash 'cnst (gethash 'prelude (gethash 'std oc))) t) x)))
    (setq from-maybe (lambda (x m) (funcall maybe x (gethash 'id (gethash 'prelude (gethash 'std oc))) m)))
    (setq map-maybe (lambda (f x) (funcall maybe none-maybe (lambda (y) (oc-hash-from-alist (list
      'just (funcall f y)))) x)))
    (setq bind-maybe (lambda (f x) (funcall maybe none f x)))
    (setq apply-maybe (lambda (f x) (funcall maybe none (lambda (y) (funcall map-maybe y x)) f)))
    (setq show-maybe (lambda (_cvshow x) (funcall maybe "none" (lambda (p1) (funcall (gethash 'show (gethash 'prelude (gethash 'std oc))) _cvshow p1)) x)))
    (setq eq-maybe (lambda (_edeq _a1 _a2) (if
      (funcall (lambda (_b _c) (and (funcall (gethash 'none (gethash '_eq (gethash 'instances oc))) _b none) (funcall (gethash 'none (gethash '_eq (gethash 'instances oc))) _c none))) _a1 _a2)
      (funcall (lambda (_b _c) t) _a1 _a2)
      (if
        (funcall (lambda (_d y) (funcall (gethash 'none (gethash '_eq (gethash 'instances oc))) _d none)) _a1 _a2)
        (funcall (lambda (_d y) nil) _a1 _a2)
        (if
          (funcall (lambda (x _e) (funcall (gethash 'none (gethash '_eq (gethash 'instances oc))) _e none)) _a1 _a2)
          (funcall (lambda (x _e) nil) _a1 _a2)
          (if
            (funcall (lambda (x y) (and (gethash 'just x) (gethash 'just y))) _a1 _a2)
            (funcall (lambda (x y) (funcall _edeq (gethash 'just x) (gethash 'just y))) _a1 _a2)
            (error "cases error")))))))
    (setq foldl-maybe (lambda (f i x) (funcall maybe i (lambda (y) (funcall f i y)) x)))
    (puthash 'maybe show-maybe (gethash 'show (gethash 'instances oc)))
    (puthash 'maybe eq-maybe (gethash '_eq (gethash 'instances oc)))
    (puthash 'maybe foldl-maybe (gethash 'foldl (gethash 'instances oc)))
    (puthash 'maybe pure-maybe (gethash 'pure (gethash 'instances oc)))
    (puthash 'maybe map-maybe (gethash 'map (gethash 'instances oc)))
    (puthash 'maybe apply-maybe (gethash 'apply (gethash 'instances oc)))
    (puthash 'maybe bind-maybe (gethash 'bind (gethash 'instances oc)))
    (puthash 'maybe none (gethash 'mempty (gethash 'instances oc)))
    (oc-hash-from-alist (list
      'none none
      'none-maybe none-maybe
      'pure-maybe pure-maybe
      'maybe maybe
      'is-none is-none
      'is-just is-just
      'from-maybe from-maybe
      'map-maybe map-maybe
      'bind-maybe bind-maybe
      'apply-maybe apply-maybe
      'show-maybe show-maybe
      'eq-maybe eq-maybe
      'foldl-maybe foldl-maybe))) (gethash 'std oc)))
(puthash 'test_maybe (let*
  (
  (m nil)
  (m2 nil)
  (test-none nil)
  (maybe-bind-func nil))
  (setq m (funcall (gethash 'maybe (gethash 'pure (gethash 'instances oc))) 2))
  (setq m2 (funcall (gethash 'maybe (gethash 'pure (gethash 'instances oc))) 3))
  (setq test-none (gethash 'none (gethash 'maybes (gethash 'std oc))))
  (setq maybe-bind-func (lambda (_a1) (if
    (funcall (lambda (x) (funcall (gethash 'eq-int (gethash 'ff (gethash 'std oc))) x 1)) _a1)
    (funcall (lambda (x) (gethash 'none (gethash 'maybes (gethash 'std oc)))) _a1)
    (if
      (funcall (lambda (x) (funcall (gethash 'eq-int (gethash 'ff (gethash 'std oc))) x 2)) _a1)
      (funcall (lambda (x) (oc-hash-from-alist (list
        'just 3))) _a1)
      (error "cases error")))))
  (oc-hash-from-alist (list
    'm m
    'm2 m2
    'test-none test-none
    'maybe-bind-func maybe-bind-func
    'item1 (funcall (gethash 'check-eq (gethash 'testing oc)) (gethash 'bool (gethash '_eq (gethash 'instances oc))) (funcall (gethash 'is-just (gethash 'maybes (gethash 'std oc))) m) t)
    'item2 (funcall (gethash 'check-eq (gethash 'testing oc)) (gethash 'bool (gethash '_eq (gethash 'instances oc))) (funcall (gethash 'is-none (gethash 'maybes (gethash 'std oc))) test-none) t)
    'item3 (funcall (gethash 'check (gethash 'testing oc)) (eql (funcall (gethash 'maybe (gethash 'maybes (gethash 'std oc))) 1 (funcall (gethash 'cnst (gethash 'prelude (gethash 'std oc))) 2) (gethash 'none (gethash 'maybes (gethash 'std oc)))) 1))
    'item4 (funcall (gethash 'check (gethash 'testing oc)) (eql (funcall (gethash 'maybe (gethash 'maybes (gethash 'std oc))) 1 (lambda (x) (+ x 1)) m2) 4))
    'item5 (funcall (gethash 'check (gethash 'testing oc)) (eql (funcall (gethash 'from-maybe (gethash 'maybes (gethash 'std oc))) 0 (gethash 'none (gethash 'maybes (gethash 'std oc)))) 0))
    'item6 (funcall (gethash 'check (gethash 'testing oc)) (eql (funcall (gethash 'from-maybe (gethash 'maybes (gethash 'std oc))) 0 (funcall (gethash 'pure-maybe (gethash 'maybes (gethash 'std oc))) 2)) 2))
    'item7 (funcall (gethash 'check (gethash 'testing oc)) (eql (funcall (gethash 'from-maybe (gethash 'maybes (gethash 'std oc))) 0 m) 2))
    'item8 (funcall (gethash 'check (gethash 'testing oc)) (eql (funcall (gethash 'from-maybe (gethash 'maybes (gethash 'std oc))) 0 (funcall (gethash 'maybe (gethash 'map (gethash 'instances oc))) (lambda (x) (+ x 1)) m)) 3))
    'item9 (funcall (gethash 'check (gethash 'testing oc)) (eql (funcall (gethash 'from-maybe (gethash 'maybes (gethash 'std oc))) 0 (funcall (gethash 'maybe (gethash 'bind (gethash 'instances oc))) maybe-bind-func m)) 3))
    'item10 (funcall (gethash 'check (gethash 'testing oc)) (eql (funcall (gethash 'maybe (gethash 'foldl (gethash 'instances oc))) (lambda (a e) (+ a e)) 1 m) 3))
    'item11 (funcall (gethash 'check (gethash 'testing oc)) (string= (funcall (gethash 'show-maybe (gethash 'maybes (gethash 'std oc))) (gethash 'int (gethash 'show (gethash 'instances oc))) test-none) "none"))
    'item12 (funcall (gethash 'check (gethash 'testing oc)) (string= (funcall (lambda (p1) (funcall (gethash 'maybe (gethash 'show (gethash 'instances oc))) (gethash 'int (gethash 'show (gethash 'instances oc))) p1)) m) "2"))
    'item13 (funcall (gethash 'check (gethash 'testing oc)) (funcall (lambda (p1 p2) (funcall (gethash 'maybe (gethash '_eq (gethash 'instances oc))) (gethash 'int (gethash '_eq (gethash 'instances oc))) p1 p2)) m m))))) oc)
