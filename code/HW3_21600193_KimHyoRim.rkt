#lang plai

; Problem 1:
; Solved by myself: Y
; Time taken: 4 hour
; [contract] remDup: list → list
;            subst: WAE → WAE symbol number → WAE
;            free-ids: WAE → list-of-symbols
; [purpose] To find free ids in WAE
; [tests](test (free-ids (id 'y)) '(y))
;        (test (free-ids (add (id 'y) (num 4))) '(y))
;        (test (free-ids (with 'x (num 3) (id 'y))) '(y))
;        (test (free-ids (with 'x (num 3) (add (id 'y) (num 1)))) '(y))
;        (test (free-ids (with 'x (num 3) (add (id 'x) (sub (num 3) (id 'x))))) '())
;        (test (free-ids (with 'x (num 3) (sub (id 'a) (add (num 4) (id 'x))))) '(a))
;        (test (free-ids (with 'x (num 3) (sub (id 'b) (sub (id 'a) (id 'x))))) '(b a))
;        (test (free-ids (with 'x (num 3) (sub (id 'a) (sub (id 'b) (add (id 'x) (id 'b)))))) '(a b))   
;        (test (free-ids (with 'x (num 3) (sub (id 'y) (with 'y (num 7) (add (id 'x) (sub (id 'b) (id 'a))))))) '(y b a))
;        (test (free-ids (with 'x (id 't) (sub (id 'x) (with 'y (id 'y) (add (id 'x) (sub (id 'b) (id 'a))))))) '(b a y t))
;        (test (free-ids (with 'x (with 'y (num 3) (sub (id 'x) (id 'y))) (add (id 'x) (id 'y)))) '(y x))
;        (test (free-ids (add (with 'x (num 10) (with 'x (num 3) (sub (id 'y) (with 'y (num 7) (add (id 'x) (sub (id 'c) (id 'b))))))) (with 'a (id 'a) (id 'a)))) '(y c b a))
;        (test (free-ids (add (with 'x (num 10) (with 'x (num 3) (sub (id 'y) (with 'y (num 7) (add (id 'x) (sub (id 'c) (id 'b))))))) (with 'a (id 'd) (id 'a)))) '(y c b d))
;        (test (free-ids (add (with 'x (num 10) (with 'x (num 3) (sub (id 'y) (with 'y (num 7) (add (id 'x) (sub (id 'c) (id 'b))))))) (with 'a (id 'd) (id 'z)))) '(y c b z d))

(define-type WAE
	[num (n number?)]
	[add (lhs WAE?) (rhs WAE?)]
	[sub (lhs WAE?) (rhs WAE?)]
	[with (name symbol?) (named-expr WAE?) (body WAE?)]
	[id (name symbol?)])

;; [contract] remDup: list → list
(define (remDup lst)(cond
                      [(empty? lst)empty]
                      [(member (first lst)(rest lst))(remDup (rest lst))]
                      [else (cons (first lst)(remDup (rest lst)))]))

;; [contract] subst: WAE → WAE symbol number → WAE
(define(subst expr sub-id val)
  (type-case WAE expr
    [num (n) expr]
    [add (l r)(add (subst l sub-id val)(subst r sub-id val))]
    [sub (l r)(sub (subst l sub-id val)(subst r sub-id val))]
    [with (bound-id named-expr bound-body)
          (if(symbol=? bound-id sub-id)
             (with bound-id
                   (subst named-expr sub-id val)bound-body)
             (with bound-id
                   (subst named-expr sub-id val)
                   (subst bound-body sub-id val)))]
    [id (v)(if(symbol=? v sub-id) val expr)]))

;; [contract] free-ids: WAE → list-of-symbols
(define (free-ids expr)(remDup(type-case WAE expr
                         [num (n)  empty]
                         [add (l r)(append(free-ids l)(free-ids r))]
                         [sub (l r)(append(free-ids l)(free-ids r))]
                         [with (bound-id named-expr bound-body)
                               (append (free-ids (subst bound-body bound-id named-expr)) (free-ids named-expr))]
                         [id (v) (list v)])))
