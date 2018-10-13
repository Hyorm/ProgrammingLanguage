#lang plai

; Problem 2:
; Solved by myself: Y
; Time taken: 1 hour
; [contract] subst: FWAE symbol FWAE -> FWAE
;            interp: FWAE -> FWAE
;            num-op: (number number -> number) -> (FWAE FWAE -> FWAE)
;            parse: sexp -> FWAE
;            lookup: symbol DefrdSub -> number
; [purpose] Implement FAE with deferred substitution
; [tests] (test(interp (app (fun 'x (app (fun 'f (app (fun 'x (app (id 'f) (num 4))) (num 5))) (fun 'y (add (id 'x) (id 'y))))) (num 3)) (mtSub))(num 7))
;         (test (interp (parse '{with {x 3} {with {f {fun {y} {+ x y}}} {with {x 5} {f 4}}}})(mtSub))(num 7))
;         (test (interp (parse '{with {y 10} {fun {x} {+ y x}}}) (mtSub))(closureV 'x (add (id 'y) (id 'x))(aSub 'y (numV 10) (mtSub))))

(define-type FAE
    [num    (n number?)]
    [add     (lhs FAE?) (rhs FAE?)]
    [sub     (lhs FAE?) (rhs FAE?)]
    [id         (name symbol?)]
    [fun      (param symbol?) (body FAE?)]
    [app     (ftn FAE?) (arg FAE?)])

(define-type FAE-Value
    [numV         (n number?)]
    [closureV    (param symbol?) (body FAE?) (ds DefrdSub?)])

(define-type DefrdSub
    [mtSub]
    [aSub (name symbol?) (value FAE-Value?) (ds DefrdSub?)])

; [contract] lookup: symbol DefrdSub -> number
(define (lookup name ds)
      (type-case DefrdSub ds
        [mtSub () (error 'lookup "free identifier")]
        [aSub (i v saved) (if (symbol=? i name) v (lookup name saved))]))

; [contract]  interp: FAE DefrdSub -> FAE-Value
(define (interp fae ds)
    (type-case FAE fae
       [num   (n)      (numV n)]
       [add    (l r)    (num+ (interp l ds) (interp r ds))]
       [sub    (l r)    (num- (interp l ds) (interp r ds))]
       [id       (s)     (lookup s ds)]
       [fun     (p b)  (closureV p b ds)]
       [app    (f a)   (local [(define f-val (interp f ds))
                                      (define a-val (interp a ds))]
                               (interp (closureV-body f-val)
                                           (aSub (closureV-param f-val)
                                                      a-val
                                                      (closureV-ds f-val))))]))

; [contract] num-op: (number number -> number) -> (FWAE FWAE -> FWAE)
(define (num-op op)
     (lambda (x y)
          (numV (op (numV-n x) (numV-n y)))))
(define num+ (num-op +))
(define num- (num-op -))

; [contract] parse: sexp -> FWAE
(define (parse sexp)
  (cond
    [(number? sexp)(num sexp)]
    [(symbol? sexp)(id sexp)]
    [(list? sexp)
     (case(first sexp)
       [(+)(add(parse (second sexp))
               (parse (third sexp)))]
       [(-)(sub(parse (second sexp))
               (parse (third sexp)))]
       [(with)(app(fun (first (second sexp))(parse (third sexp)))
                   (parse (second (second sexp))))]
       [(fun)(fun(first(second sexp))
                 (parse (third sexp)))]
       [else (app (parse (first sexp))
                  (parse (second sexp)))])]
    [else (error 'parse "bad syntax: ~a" sexp)]))

(test(interp (app (fun 'x (app (fun 'f (app (fun 'x (app (id 'f) (num 4))) (num 5))) (fun 'y (add (id 'x) (id 'y))))) (num 3)) (mtSub))(numV 7))
(test (interp (parse '{with {x 3} {with {f {fun {y} {+ x y}}} {with {x 5} {f 4}}}})(mtSub))(numV 7))
(test (interp (parse '{with {y 10} {fun {x} {+ y x}}}) (mtSub))(closureV 'x (add (id 'y) (id 'x))(aSub 'y (numV 10) (mtSub))))
(test (interp (parse '(with (x 3) (- ((fun (x) (- x 5)) 9) 6))) (mtSub))(numV -2))