#lang plai
(define-type AE
  [num (n number?)]
  [add (lhs AE?)
       (rhs AE?)]
  [sub (lhs AE?)
       (rhs AE?)])
;;time taken: 30min
;; [contract]parse: sexp -> AE 
;; [purpose] to convert s-expressions into AEs using 'match'
(define (parse sexp)(cond
                      [(number? sexp)(num sexp)]
                      [(match sexp
                                       [(list '+ a b)
                                        (add (parse a)
                                             (parse b))]
                                       [(list '- a b)
                                        (sub (parse a)
                                             (parse b))]
                         [_ (error 'parse "bad syntax: ~a" sexp)]
                      )]
                      )) 
;; [tests]
(test(parse '3)(num 3))
(test(parse '{+ 3 4})(add (num 3)(num 4)))
(test(parse '{- 3 {- 4 3}})(sub (num 3)(sub (num 4)(num 3))))
(test/exn (parse '{+ 3 4 5})"parse: bad syntax: (+ 3 4 5)")