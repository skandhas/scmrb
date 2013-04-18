require 'test/unit'
require_relative 'scmrb'

include Test::Unit::Assertions

def scmrb_check(act, exp, msg = nil)
  assert_equal(exp, ScmRb.evaluate(ScmRb.parse(act)), msg) 
end

class ScmRbTestAssertion < Test::Unit::TestCase
  def test_scmrb
    scmrb_check("(+ 2 2)", 4)
    scmrb_check("(+ (* 2 100) (* 1 10))", 210)
    scmrb_check("(if (> 6 5) (+ 1 1) (+ 2 2))", 2)
    scmrb_check("(if (< 6 5) (+ 1 1) (+ 2 2))", 4)
    scmrb_check("(define x 3)", nil) 
    scmrb_check("x", 3)   
    scmrb_check("(+ x x  )", 6)
    scmrb_check("(begin (define x 1) (set! x (+ x 1)) (+ x 1))", 3)
    scmrb_check("((lambda (x) (+ x x)) 5)", 10)
    scmrb_check("(define twice (lambda (x) (* 2 x)))", nil) 
    scmrb_check("(twice 5)", 10)
    scmrb_check("(define compose (lambda (f g) (lambda (x) (f (g x)))))", nil)
    scmrb_check("((compose list twice) 5)", [10])
    scmrb_check("(define repeat (lambda (f) (compose f f)))", nil)
    scmrb_check("((repeat twice) 5)", 20)
    scmrb_check("((repeat (repeat twice)) 5)", 80)
    scmrb_check("(define fact (lambda (n) (if (<= n 1) 1 (* n (fact (- n 1))))))", nil)
    scmrb_check("(fact 3)", 6)
    scmrb_check("(fact 50)", 30414093201713378043612608166064768844377641568960512000000000000)
    scmrb_check("(define abs (lambda (n) ((if (> n 0) + -) 0 n)))", nil)
    scmrb_check("(list (abs -3) (abs 0) (abs 3))", [3, 0, 3])
    scmrb_check((<<-EOF
                (define combine (lambda (f)
                (lambda (x y)
                  (if (null? x) (quote ())
                      (f (list (car x) (car y))
                         ((combine f) (cdr x) (cdr y)))))))
                EOF
                ),nil)
    scmrb_check("(define zip (combine cons))", nil)
    scmrb_check("(zip (list 1 2 3 4) (list 5 6 7 8))", [[1, 5], [2, 6], [3, 7], [4, 8]])
    scmrb_check((<<-EOF
                (define riff-shuffle (lambda (deck) (begin
                   (define take (lambda (n seq) (if (<= n 0) (quote ()) (cons (car seq) (take (- n 1) (cdr seq))))))
                   (define drop (lambda (n seq) (if (<= n 0) seq (drop (- n 1) (cdr seq)))))
                   (define mid (lambda (seq) (/ (length seq) 2)))
                   ((combine append) (take (mid deck) deck) (drop (mid deck) deck)))))
                 EOF
                ),nil)
    scmrb_check("(riff-shuffle (list 1 2 3 4 5 6 7 8))", [1, 5, 2, 6, 3, 7, 4, 8])
    scmrb_check("((repeat riff-shuffle) (list 1 2 3 4 5 6 7 8))",  [1, 3, 5, 7, 2, 4, 6, 8])
    scmrb_check("(riff-shuffle (riff-shuffle (riff-shuffle (list 1 2 3 4 5 6 7 8))))", [1,2,3,4,5,6,7,8])

    scmrb_check("(define lyst (lambda items items))", nil)
    scmrb_check("(lyst 1 2 3 (+ 2 2))", [1,2,3,4])
    scmrb_check("(if 1 2)", 2)
    scmrb_check("(if (= 3 4) 2)", nil)
    scmrb_check("(define ((account bal) amt) (set! bal (+ bal amt)) bal)", nil)
    scmrb_check("(define a1 (account 100))", nil)
    scmrb_check("(a1 0)", 100) 
    scmrb_check("(a1 10)", 110)
    scmrb_check("(a1 10)", 120)
    scmrb_check((<<-EOF
                (define (newton guess function derivative epsilon)
                    (define guess2 (- guess (/ (function guess) (derivative guess))))
                    (if (< (abs (- guess guess2)) epsilon) guess2
                        (newton guess2 function derivative epsilon)))
                EOF
                ),nil)
    scmrb_check((<<-EOF
                (define (square-root a)
                (newton 1 (lambda (x) (- (* x x) a)) (lambda (x) (* 2 x)) 1e-8))
                EOF
                ),nil)
    scmrb_check("(> (square-root 200.) 14.14213)", true)
    scmrb_check("(< (square-root 200.) 14.14215)", true)
    scmrb_check("(= (square-root 200.) (sqrt 200.))", true)
    scmrb_check((<<-EOF
                  (define (sum-squares-range start end)
                       (define (sumsq-acc start end acc)
                          (if (> start end) acc (sumsq-acc (+ start 1) end (+ (* start start) acc))))
                       (sumsq-acc start end 0))
                  EOF
                  ),nil)
    scmrb_check("(sum-squares-range 1 3000)", 9004500500)
    scmrb_check("(let ((a 1) (b 2)) (+ a b))", 3)
    scmrb_check("(define-macro unless (lambda args `(if (not ,(car args)) (begin ,@(cdr args))))) ; test `", nil)
    scmrb_check("(unless (= 2 (+ 1 1)) (display 2) 3 4)", nil)
    scmrb_check('(unless (= 4 (+ 1 1)) (display 2) (display "\n") 3 4)', 4)
    scmrb_check("(quote x)", :x)  
    scmrb_check("(quote (1 2 three))", [1, 2, :three]) 
    scmrb_check("'x", :x)
    scmrb_check("'(one 2 3)", [:one, 2, 3])
    scmrb_check("(define L (list 1 2 3))", nil)
    scmrb_check("`(testing ,@L testing)", [:testing,1,2,3,:testing])
    scmrb_check("`(testing ,L testing)", [:testing,[1,2,3],:testing])
  end
end
