; Checks if the given expression is a 'define' statement.
; A 'define' statement should be a list with three elements:
; 'define', a symbol for the variable name, and the expression defining the variable.
(define define-stmt? (lambda (expr)
    (and (list? expr) (= (length expr) 3) (eq? (car expr) 'define) (symbol? (cadr expr)) (expr? (caddr expr)))))
; Determines if an expression is valid.
; An expression can be a number, a symbol, or a list representing a complex expression like a function call or a special form.
(define expr? (lambda (expr)
 (or 
  (number? expr)
  (symbol? expr)
  (and (list? expr) (or (symbol? (car expr)) (operation? expr) (if-expr? expr) (let-expr? expr) (lambda? expr) (lambda? (car expr))))
)))
; Checks if the expression is an 'if' expression.
; An 'if' expression should be a list with four elements: 'if', a condition, a consequent expression, and an alternate expression.
(define if-expr? 
  (lambda (expr)
     (and
       (list? expr)
       (= (length expr) 4)
       (eq? (car expr) 'if)
     )))  
(define let-expr? (lambda (expr)
     (and
       (= (length expr) 3)
       (eq? (car expr) 'let)
       (or (eq? (cadr expr) '() ) (var-binding-list? (cadr expr)))
     )))

; Processes a 'let' expression by evaluating its bindings and then its body in the new environment.
(define let-func (lambda (expr env)
  (define (evaluate-bindings bindings)
    (if (null? bindings)
        '()
        (let ((binding (car bindings)))
          (cons (cons (car binding) (s7-interpret (cadr binding) env))
                (evaluate-bindings (cdr bindings))))))
  (let ((new-bindings (evaluate-bindings (cadr expr))))
    (s7-interpret (caddr expr) (append new-bindings env)))))


(define symbol-in-env? (lambda (expr env)
  (and (not (null? env)) (if (eq? (caar env) expr) 
      #t 
      (if (> (length env) 1) 
        (symbol-in-env? expr (cdr env))
        #f
      )))
))
; Maps built-in operators to their corresponding Scheme procedures.
(define built_in_operator (lambda (op)
   (cond
     ( (eq? op '+) +) 
     ( (eq? op '*) *) 
     ( (eq? op '/) /) 
     ( (eq? op '-) -) 
     ( else ((display "cs305: ERROR \n\n") (cs305 env)))
  )))

(define extend-env (lambda (var value old-env)
       (cons (cons var value) old-env)))


    


; Retrieves the value of a variable from the environment.
(define get-value (lambda (var old-env new-env)
    (cond
      ((null? new-env) (display "cs305: ERROR \n\n") (cs305 old-env))

      ((equal? (caar new-env) var) (cdar new-env))

      (else (get-value var old-env (cdr new-env))))))
      
; Checks if an expression represents an operation (+, -, *, /) with the correct structure.
(define operation? (lambda (expr)
  (and 
		(list? expr)
		(or (eq? '+ (car expr)) (eq? '- (car expr)) (eq? '* (car expr)) (eq? '/ (car expr)))
		(> (length expr) 2)
	) 
))
(define formal-binding-list? (lambda (expr)
	(and 
    (list? expr)
		(symbol? (car expr))
		(or (null? (cdr expr)) (formal-binding-list? (cdr expr)))
	)
))

(define operation-func(lambda (expr env)
  (let (
    (operands (map s7-interpret (cdr expr) (make-list (length (cdr expr)) env)))
    (operator (built_in_operator (car expr))))
        (apply operator operands)
  ))
)

(define var-binding-list? (lambda (expr)
	(and 
		(= (length (car expr)) 2)
		(symbol? (caar expr))
		(if (> (length expr) 1) (var-binding-list? (cdr expr)))
	)
))





(define lambda? (lambda (expr)
  (and 
    (list? expr) 
    (and (eq? 'lambda (car expr)) (formal-binding-list? (cadr expr)) (expr? (caddr expr)) (not (define-stmt? (caddr expr)))) 
  )))

(define lambda-func(lambda (expr env)
(if (= (length (cadar expr)) (length (cdr expr)))
											(let* (
                        (parameters (map s7-interpret (cdr expr) (make-list (length (cdr expr)) env))) 
                        (new-env (append (map cons (cadar expr) parameters) env)))
                        (s7-interpret (caddar expr) new-env))
											((display "cs305: ERROR \n\n") (cs305 env)))
))


(define s7-interpret (lambda (expr env)
  (if (expr? expr)
    (cond
      ((number? expr) expr)
      ((and (symbol? expr) (symbol-in-env? expr env)) (get-value expr env env))
      ((not (list? expr)) (display "cs305: ERROR \n\n") (cs305 env))
      ((null? expr) expr)
      ((if-expr? expr) (let ((value 
                  (if (not (= (s7-interpret (cadr expr) env) 0))
                    (s7-interpret (caddr expr) env)
                    (s7-interpret (cadddr expr) env)))) value))
      ((let-expr? expr) (let-func expr env))
      ((lambda? expr) expr)
      ((lambda? (car expr)) (lambda-func expr env))
      ((operation? expr) (operation-func expr env))
      (else (s7-interpret (append (list (get-value (car expr) env env)) (cdr expr))env ))
      )
    ((display "cs305: ERROR \n\n") (cs305 env))
    )))

; The cs305 function that reads, evaluates, and prints expressions, looping indefinitely.
(define cs305 (lambda (env)
  (let* (
      (dummy1 (display "cs305> "))
      (expr (read))  
      (new-env (if (define-stmt? expr) 
        (extend-env (cadr expr) (s7-interpret (caddr expr) env) env) env)) 
      (value (if (define-stmt? expr)
        (cadr expr)
        (s7-interpret expr env)))
      (dummy2 (display "cs305: "))
      (dummy3 (display value))
      (dummy4 (newline))
      (dummy5 (newline)))
    (cs305 new-env))))

(cs305 '())
