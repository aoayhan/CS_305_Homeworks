(define (twoOperatorCalculator lst)
  (define (evaluate lst acc)
    (cond
      ((null? lst) acc)
      ((number? (car lst)) (evaluate (cdr lst) (car lst)))
      ((eq? '+ (car lst)) (evaluate (cddr lst) (+ acc (cadr lst))))
      ((eq? '- (car lst)) (evaluate (cddr lst) (- acc (cadr lst))))
      (else acc)
      )
    )
  (evaluate lst 0)
)
(define (fourOperatorCalculator lst)
  (define (process-mul-div lst)
    (if (null? lst)
        lst
        (let ((next (cdr lst)))
          (cond ((and (pair? next) (eq? '* (cadr lst))) (process-mul-div (cons (* (car lst) (caddr lst)) (cdddr lst))))
                ((and (pair? next) (eq? '/ (cadr lst))) (process-mul-div (cons (/ (car lst) (caddr lst)) (cdddr lst))))
                (else (cons (car lst) (process-mul-div next)))))))

  (process-mul-div lst))

(define (Nested element)
  (cond
    ((list? element) (twoOperatorCalculator (fourOperatorCalculator (calculatorNested element))))
    (else element)))

(define (calculatorNested sequence)
  (map (lambda (item) (Nested item)) sequence))

(define (checkOperators expr)
  (define (valid-operator? op)
    (member op '(+ - * /)))

  (define (process-expr lst)
    (cond
      ((null? lst) #f)
      ((and (number? (car lst)) (or (null? (cdr lst)) (valid-operator? (cadr lst))))
       (if (null? (cdr lst))
           #t
           (process-expr (cddr lst))))
      ((list? (car lst))
       (and (checkOperators (car lst)) 
            (or (null? (cdr lst)) 
                (and (valid-operator? (cadr lst)) (process-expr (cddr lst))))))
      (else #f)))

  (if (list? expr) (process-expr expr) #f))
(define (calculator expr)
  (if (checkOperators expr)
      (twoOperatorCalculator (fourOperatorCalculator (calculatorNested expr)))
      #f))
