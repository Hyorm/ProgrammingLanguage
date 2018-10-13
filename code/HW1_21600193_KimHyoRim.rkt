#lang plai
; Problem 1:
; Solved by myself: Y
; Time taken: 1 min
; [contract] mile→km: number -> number
; [purpose] To convert mile to km
; [tests] (test (mile->km 1) 1.6)
;             (test (mile->km 2) 3.2)
(define (mile->km mile)(cond [(number? mile)(* mile 1.6)]
                             [else (error "input only number")]
                             ))
(test (mile->km 1) 1.6)
(test (mile->km 2) 3.2)
(test (mile->km "r") "input only number")

; Problem 2:
; Solved by myself: Y
; Time taken: 1 min
; [contract] volume-cuboid: number, number, number -> number
; [purpose] To calculate length * height * width
; [tests] (test (volume-cuboid 1 2 3) 6)
;             (test (volume-cuboid 80 90 40) 288000)
(define (volume-cuboid fi se th)(cond[(number? fi)
                                      (number? se)
                                      (number? th)
                                            (* fi se th)]
                                     [else (error "input only number")]))
(test (volume-cuboid 1 2 3) 6)
(test (volume-cuboid 80 90 40) 288000)
(test (volume-cuboid "r" 90 40) "input only number")

; Problem 3:
; Solved by myself: Y
; Time taken: 1 min
; [contract] is-odd: number -> boolean or string
; [purpose] To convert mile to km
; [tests] (test (is-odd 3) “Odd”)
;             (test (is-odd 2) “Even”)
(define (is-odd intNum)(cond[(number? intNum)
                           (cond [(equal? 0 intNum)"0 is not even and odd"]
                                 [else (cond [(equal? 0 (modulo intNum 2))false]
                                    [else true])]
                                 )]
                            [else "input only number"]))
(test (is-odd 3) true)
(test (is-odd 2) false)
(test (is-odd 0) "0 is not even and odd")
(test (is-odd "d") "input only number")

; Problem 4:
; Solved by myself: Y
; Time taken: 10 min
; [contract] gcd: number number -> number
; [purpose] To return the greatest common divisor
; [tests] (test (gcd 99 30) 3)
;             (test (gcd 50 10) 10)
(define (gcd intNum1 intNum2)(cond [(> intNum1 intNum2)
                                     (cond [(zero? intNum2) intNum1]
                                           [else (gcd intNum2 (- intNum1 intNum2))])]
                                   [(< intNum1 intNum2)
                                    (cond[ (zero? intNum1) intNum2]
                                         [else (gcd intNum1 (- intNum2 intNum1))]
                                         )]
                                   [else intNum1]))  
(test (gcd 99 30) 3)
(test (gcd 50 10) 10)

; Problem 5:
; Solved by myself: Y
; Time taken: 5 min
; [contract] gcd: number number -> number
; [purpose] To return the lowest common multiple
; [tests] (test (lcm  0 20) 0)
;             (test (lcm  50 10) 50)
(define (lcm intNum1 intNum2)(cond[(zero? intNum2) intNum2]
                                  [(zero? intNum1) intNum1]
                                  [else (*(/ intNum1 (gcd intNum2 intNum1))
                                          (/ intNum2 (gcd intNum1 intNum2))
                                          (gcd intNum1 intNum2)
                                          )]))
(test (lcm  0 20) 0)
(test (lcm  50 10) 50)

; Problem 6:
; Solved by myself: Y
; Time taken: 30 min
; [contract] 1. define-type COURSE: course-name, course-labs, course-homework, course-projects
;            2. have-homework: COURSE -> boolean
;            3. have-project: COURSE -> boolean
; [purpose] define-type COURSE and find have-homework number and have-project boolean
; [tests] (test(have-homework ITP40001)2)
;             (test(have-projects ITP40001)true)
;             (test(have-homework ECE20016)5)
;             (test(have-projects ECE20016)false)
;             (test(have-homework ITP20005)1)
;             (test(have-projects ITP20005)false)
(define-type COURSE[ITP2 (name string?)(homework number?)]
                   [ITP4 (name string?)(homework number?)(projects number?)]
                   [ECE2 (name string?)(labs number?)(homework number?)]
  )
(define (have-homework C)(cond[(ITP4? C)(ITP4-homework C)]
                              [(ITP2? C)(ITP2-homework C)]
                              [(ECE2? C)(ECE2-homework C)]))
(define (have-projects C)(cond[(ITP4? C)(cond[(>= (ITP4-projects C) 2)])]
                              [else false]))
(define ITP40001(ITP4 "ITP40001" 2 3 ))
(define ECE20016(ECE2 "ECE20016" 1 0 ))
(define ITP20005(ITP2 "ITP20005" 1))

(test(have-homework ITP40001)2)
(test(have-projects ITP40001)true)
(test(have-homework ECE20016)0)
(test(have-projects ECE20016)false)
(test(have-homework ITP20005)1)
(test(have-projects ITP20005)false)

; Problem 7:
; Solved by myself: Y
; Time taken: 5 min
; [contract] name-pets: list -> list
; [purpose] To consumes a list of pets and produces a corresponding list of pets with names
; [tests] (test(name-pets '("dog" "pig" "bird" "cat"))'("happy" "pinky" "unnamed" "smart"))
;             (test(name-pets '("bear" "cat" "cat" "cat"))'("unnamed" "smart" "smart" "smart"))
(define (name-pets lst)
	(map (lambda(i)
               (cond [(equal? "dog" i)"happy"]
                   [(equal? "cat" i)"smart"]
                   [(equal? "pig" i)"pinky"]
                   [else "unnamed"]))lst))

(test(name-pets '("dog" "pig" "bird" "cat"))
     '("happy" "pinky" "unnamed" "smart"))
(test(name-pets '("bear" "cat" "cat" "cat"))
     '("unnamed" "smart" "smart" "smart"))

; Problem 8:
; Solved by myself: Y
; Time taken: 1 min
; [contract] give-name: string string list -> list
; [purpose] To produces a list of symbols by replacing all occurrences of old by new
; [tests] (test(give-name 'bear 'pooh (cons 'pig (cons 'cat (cons 'bear empty))))'(pig cat pooh))
;             (test(give-name 'cat 'smart (cons 'pig (cons 'cat (cons 'bear empty))))'(pig smart bear))
(define (give-name old new lst)
	(map (lambda(i)
               (cond [(equal? old i) new]
                   [else i]))lst))

(test(give-name 'bear 'pooh (cons 'pig (cons 'cat (cons 'bear empty))))
'(pig cat pooh))
(test(give-name 'cat 'smart (cons 'pig (cons 'cat (cons 'bear empty))))
'(pig smart bear))
