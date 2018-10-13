#lang plai

; Problem 1:
; Solved by myself: Y
; Time taken: 4 hour
; [contract] subst: FWAE symbol FWAE -> FWAE
;            interp: FWAE -> FWAE
;            num-op: (number number -> number) -> (FWAE FWAE -> FWAE)
;            parse: sexp -> FWAE
; [purpose] Implement FWAE parser and interpreter as learned in class
; [tests] must be covered branch / number of minimum branch
; Test case 1: num / 1
;            (test (interp (parse '1)) (num 1))
; Test case 2: fun_only / 1
;            (test (parse '{fun {x} {+ x 1}})(fun 'x (add (id 'x) (num 1))))
; Test case 3: id / 1
;            (test/exn (interp (parse 'x)) "interp: free identifier")
; Test case 4: num, add / 2
;            (test(interp (parse '(+ 1 2))) (num 3))
; Test case 5: with, sub / 2
;            (test(interp(parse '{with {x 10} {- x 5}}))(num 5))
; Test case 6: with, fun, app / 3
;            (test (interp(parse '{with {x {{fun {x} {+ x 1}} 1}}{+ x x}}))(num 4))
; Test case 7: with, fun, app, and sub / 4
;            (test (interp(parse '{with {x {{fun {x} {- x 1}} 5}}{- x 2}}))(num 2))
; Test case 8: all branches but not add and sub / 5
;            (test/exn (interp(parse '{with {x {{fun {x} {num 1}} 1}}{num x}})) "interp: free identifier")
; Test case 9: all branches but not add / 6
;            (test (interp(parse '(-{with {x {{fun {x} {- x 5}} 10}}{- x 1}} 4)))(num 0))
; Test case 10: all branches / 7
;            (test (interp(parse '{with {x {{fun {x} {+ x 10}} 10}}{- x 5}}))(num 15))

(define-type FWAE
    [num    (n number?)]
    [add     (lhs FWAE?) (rhs FWAE?)]
    [sub     (lhs FWAE?) (rhs FWAE?)]
    [with    (name symbol?) (named-expr FWAE?) (body FWAE?)]
    [id         (name symbol?)]
    [fun      (param symbol?) (body FWAE?)]
    [app     (ftn FWAE?) (arg FWAE?)])

; [contract] subst: FWAE symbol FWAE -> FWAE
(define (subst expr sub-id val)
  (type-case FWAE expr
    [num (n) expr]
    [add (l r)(add (subst l sub-id val)
                   (subst r sub-id val))]
    [sub (l r)(sub (subst l sub-id val)
                   (subst r sub-id val))]
    [with (bound-id named-expr bound-body)
          (if(symbol=? bound-id sub-id)
             (with bound-id
                   (subst named-expr sub-id val)
                   bound-body)
             (with bound-id
                   (subst named-expr sub-id val)
                   (subst bound-body sub-id val)))]
    [id (v) (cond [(equal? v sub-id ) val]
                  [else expr])]
    [app (fun-name arg-expr)(app (subst fun-name sub-id val)
                                 (subst fun-name sub-id val))]
    [fun  (id body)  (if (equal? sub-id id)
                         exp
                         (fun id (subst body sub-id val)))]))

; [contract] interp: FWAE -> FWAE
(define (interp fwae)
     (type-case FWAE fwae
          [num   (n)          fwae]
          [add    (l r)          (num+ (interp l) (interp r))]
          [sub    (l r)          (num- (interp l) (interp r))]
          [with   (i v e)      (interp (subst e i (interp v)))]
          [id       (s)            (error 'interp "free identifier")]
          [fun    (p b)         fwae]
          [app    (f a)         (local [(define ftn (interp f))]
                                            (interp (subst (fun-body ftn)
                                                                      (fun-param ftn)  
                                                                      (interp a))))]))

; [contract] num-op: (number number -> number) -> (FWAE FWAE -> FWAE)
(define (num-op op)
     (lambda (x y)
          (num (op (num-n x) (num-n y)))))
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
       [(with)(with(first (second sexp))
                   (parse (second (second sexp)))
                   (parse (third sexp)))]
       [(fun)(fun(first(second sexp))
                 (parse (third sexp)))]
       [else (app (parse (first sexp))
                  (parse (second sexp)))])]
    [else (error 'parse "bad syntax: ~a" sexp)]))

; Test case 1: num / 1
(test (interp (parse '1)) (num 1))
; Test case 2: fun_only / 1
(test (interp(parse '{fun {x} {+ x 1}}))(fun 'x (add (id 'x) (num 1))))
; Test case 3: id / 1
(test/exn (interp (parse 'x)) "interp: free identifier")
; Test case 4: num, add / 2
(test(interp (parse '(+ 1 2))) (num 3))
; Test case 5: with, sub / 2
(test(interp(parse '{with {x 10} {- x 5}}))(num 5))
; Test case 6: with, fun, app / 3
(test (interp(parse '{with {x {{fun {x} {+ x 1}} 1}}{+ x x}}))(num 4))
; Test case 7: with, fun, app, and sub / 4
(test (interp(parse '{with {x {{fun {x} {- x 1}} 5}}{- x 2}}))(num 2))
; Test case 8: all branches but not add and sub / 5
(test/exn (interp(parse '{with {x {{fun {x} {num 1}} 1}}{num x}})) "interp: free identifier")
; Test case 9: all branches but not add / 6
(test (interp(parse '(-{with {x {{fun {x} {- x 5}} 10}}{- x 1}} 4)))(num 0))
; Test case 10: all branches / 7
(test (interp(parse '{with {x {{fun {x} {+ x 10}} 10}}{- x 5}}))(num 15))