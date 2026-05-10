#lang racket

(require (for-syntax syntax/parse racket/syntax ) data/gvector racket/match racket/list "msf-core.rkt")



(provide (rename-out (module-begin #%module-begin) (def define))  #%app circuit 
    state superposition state-ref super-ref is-state? is-W? state-print
    hadamard X CNOT Z fill-negative flatten-superposition is-eq?  CZ 
    make-state make-superposition I
    (rename-out (datum #%datum)))

; (begin-for-syntax
;     (define W 'W)
;     (define B 'B))


; (begin-for-syntax 
;     (struct state (vec neg-marker) #:transparent)
;     (struct superposition (vec) #:transparent))


(define-syntax module-begin 
    (lambda (stx)
        (syntax-parse stx
            ((_ body ...)
                (define W (format-id stx "W"))
                (define B (format-id stx "B"))
                (define w (format-id stx "w"))
                (define b (format-id stx "b"))
                #`(#%module-begin 
                    (define #,W 'W)
                    (define #,B 'B)
                    (define #,w 'w)
                    (define #,b 'b)
                    body ...)))))

; (struct state (vec neg-marker) #:transparent)
; (struct superposition (vec) #:transparent)

(define-syntax datum 
    (lambda (stx)
        (syntax-parse stx
            ((_ . n:number) 
                #'(#%datum . n)))))

(define-syntax def
    (lambda (stx)
        (syntax-parse stx
            ((_ name thingie)
                #'(define name thingie)))))

(define-syntax I
    (lambda (stx)
        (syntax-parse stx
            ((_ state num ...)
                #'state))))

(define-syntax is-negative?
    (lambda (stx)
        (syntax-parse stx
            ((_ in-state)
                #'(state-neg-marker in-state)))))



(define-syntax make-state 
    (lambda (stx)
        (syntax-parse stx
          ((_ x ...)  
            #`(state (gvector x ...) #f)))))

(define-syntax make-superposition
    (lambda (stx)
        (syntax-parse stx
            ((_ x ...)
                #'(superposition (gvector x ...))))))


(define-syntax circuit
    (lambda (stx)
        (syntax-parse stx
            ((_ in-state gates ...)
                (letrec ((loop
                    (lambda (gate-list)
                        (cond
                            ((eq? (cdr gate-list) '())
                                (syntax-parse (car gate-list)
                                    ((gate num ...)
                                        ; (println #'gate)
                                        #'(gate in-state num ...))))
                            (else
                                (syntax-parse (car gate-list)
                                    ((gate num ...)
                                        #`(gate #,(loop (cdr gate-list)) num ...))))
                            ))))
                ; (println #'(list gates ...))
                (loop (reverse (syntax-e #'(gates ...)))))
                ))))

