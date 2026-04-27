#lang racket


(require (for-syntax syntax/parse racket/syntax racket/match racket/base "def.rkt"))

(provide (rename-out (module-begin #%module-begin) (def define))  (all-defined-out)
    (rename-out (datum #%datum)))

; (begin-for-syntax 
;     (struct state (list neg-marker))
;     (struct superposition (list)))

; (struct state (list neg-marker))
; (struct superposition (list))

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


(define-syntax state 
    (lambda (stx)
        (syntax-parse stx
            ((_ state-list neg?)
                #'(list 'state state-list neg?)))))

(define-syntax make-state
    (lambda (stx)
        (syntax-parse stx
            ((_ state-list ...)
                #'(state (list state-list ...) #f)))))

(define-syntax print-state
    (lambda (stx)
        (syntax-parse stx
            ((_ in-state)
                (define in-state-datum (syntax->datum (local-expand #'in-state 'expression '())))
                (println in-state-datum)
                (match 
                    (`(#%app list 'state ,state-list _)
                        #`#,(datum->syntax stx state-list)))
            ;    #`#,(datum->syntax stx in-state-datum)
                ))))

; (define-syntax X
;     (lambda (stx)
;         (syntax-parse stx
;             )))

; (define-syntax make-state
;     (lambda (stx)
;         (syntax-parse stx
;             ((_ stuff ...)
;                 (datum->syntax #'(stuff ...) (state (syntax->datum #'(list stuff ...)) #f))))))

; (define-syntax print-state
;     (lambda (stx)
;         (syntax-parse stx
;             ((_ state-in)
;                 (datum->syntax (state-list (local-expand (syntax->datum #'state-in) 'expression '())))))))