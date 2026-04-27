#lang racket

(require (for-syntax syntax/parse racket/syntax racket/match))

(provide (rename-out (module-begin #%module-begin) (def define))  
    make-state make-superposition display-state state-ref super-ref is-state? is-W? pretty-print
    hadamard X CNOT Z is-negative? fill-negative flatten-superposition is-eq?  CZ I circuit
    (rename-out (datum #%datum)))

; (begin-for-syntax
;     (define W 'W)
;     (define B 'B))

(begin-for-syntax 
    (struct state (vec neg-marker) #:transparent)
    (struct superposition (vec) #:transparent))

(struct state (vec neg-marker) #:transparent)
(struct superposition (vec) #:transparent)

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


(define-syntax app 
    (lambda (stx)
        (syntax-parse stx
            ((_ f args ...) 
                #'(#%app f args ...)))))


(define-syntax make-state 
    (lambda (stx)
        (syntax-parse stx
          ((_ x ...)  
            #`(state (vector x ...) #f)))))

(define-syntax make-superposition
    (lambda (stx)
        (syntax-parse stx
            ((_ x ...)
                #'(superposition (vector x ...))))))


(define-syntax display-state
    (lambda (stx)
        (syntax-parse stx
            ((_ whatever)
                   
                    #`whatever))))

(define-syntax state-ref
    (lambda (stx)
        (syntax-parse stx
            ((_ state mem)
                #'(vector-ref (state-vec state) mem)))))

(define-syntax super-ref
    (lambda (stx)
        (syntax-parse stx
            ((_ superposition mem)
                #'(vector-ref (superposition-vec superposition) mem)))))


(define-syntax is-state?
    (lambda (stx)
        (syntax-parse stx
            ((_ thingie)
                #`(state? thingie)))))

(define-syntax is-W?
    (lambda (stx)
        (syntax-parse stx
            ((_ item)
                #'(or (eq? item 'w) (eq? item 'W))))))

(define-syntax is-B?
    (lambda (stx)
        (syntax-parse stx
            ((_ item)
                #'(or (eq? item 'b) (eq? item 'B))))))

(define-syntax is-eq-micro? 
    (lambda (stx)
        (syntax-parse stx
            ((_ i1 i2)
                #'(or (and (is-B? i1) (is-B? i2)) (and (is-W? i1) (is-W? i2)))))))

(define-syntax is-eq?
    (lambda (stx)
        (syntax-parse stx
            ((_ state1 state2)
                #'(letrec ((loop 
                    (lambda (counter)
                        (if (eq? counter (vector-length (state-vec state1)))
                            #t
                            (if (is-eq-micro? (vector-ref (state-vec state1) counter) (vector-ref (state-vec state2) counter))
                                (loop (+ 1 counter))
                                #f))
                    )))
                    (loop 0))))))

(define-syntax pretty-print
    (lambda (stx)
        (syntax-parse stx
            ((_ to-print)
                #`(letrec ((loop
                    (lambda (current)
                        (match current
                            ((superposition sup-vec)
                                (remove-duplicates (for/list ((v sup-vec) #:unless (eq? v (void)))
                                    (loop v))))
                            ((state state-vec _)
                                (for/list ((v state-vec) #:unless (eq? v (void))) (loop v)))
                            (_ current)))))
                    (loop to-print))))))

(define-syntax vector-set 
    (lambda (stx)
        (syntax-parse stx
            ((_ vec new-thing pos)
                #'(begin
                    (let ((v (vector-copy vec)))
                        (vector-set! v pos new-thing)
                        v))))))




(define-syntax hadamard->collapse
    (lambda (stx)
        (syntax-parse stx
            ((_ in-state vec)
                #'(letrec ((loop-2 
                    (lambda (curr-state curr-vec)
                        (if (>= curr-vec (vector-length vec))
                            (superposition (vector))
                            (if (eq? (vector-ref vec curr-vec) (void))
                                (loop-2 curr-state (+ curr-vec 1)) 
                                (scan-collapse-outer (loop-2 curr-state (+ curr-vec 1)) (vector-ref vec curr-vec)))
                    )
                )))
                 (loop-2 in-state 0))))))


(define-syntax hadamard
    (lambda (stx)
        (syntax-parse stx
            ((_ in-state targ)
                #`(begin 
                    (let ((ret (vector)))
                    (letrec ((loop 
                    (lambda (current target frmr)
                        (match current
                        ((state state-vec _)
                                ; (vector-set! state-vec targ (loop (vector-ref state-vec targ) 0))
                                (loop (vector-ref state-vec target) target current))
                        ((superposition sup-vec)
                                (superposition (for/vector ((sup sup-vec))
                                    (loop sup target current))))
                        ('W 
                            (begin 
                                ; (println "arrived")
                                (vector-add! ret (state (vector-set (state-vec frmr) 'B target) #f))
                                (vector-add! ret (state (vector-set (state-vec frmr) 'W target) #f))))
                        ('B 
                            (begin 
                                ; (println "arrived")
                                (vector-add! ret (state (vector-set (state-vec frmr) 'b target) #f))
                                (vector-add! ret (state (vector-set (state-vec frmr) 'W target) #f))))

                        ('w
                            (begin 
                                ; (println "arrived")
                                (vector-add! ret (state (vector-set (state-vec frmr) 'b target) #f))
                                (vector-add! ret (state (vector-set (state-vec frmr) 'w target) #f))))
                        ('b 
                            (begin 
                                ; (println "arrived")
                                (vector-add! ret (state (vector-set (state-vec frmr) 'B target) #f))
                                (vector-add! ret (state (vector-set (state-vec frmr) 'w target) #f))))    
                        
                        ))))
                (begin
                    
                    (loop in-state targ '())
                    ; (println ret)
                    (flatten-superposition (hadamard->collapse in-state ret))
                    ))))))))



; (define-syntax hadamard--
;     (lambda (stx)
;         (syntax-parse stx
;             ((_ in-state target)
;                 #`(collapse (hadamard-- in-state target))))))

(define-syntax X
    (lambda (stx)
        (syntax-parse stx
            ((_ in-state target)
                #`(letrec ((loop 
                    (lambda (current targ frmr)
                        (match current 
                            ((state state-vec _)
                                (loop (vector-ref state-vec targ) targ state-vec))
                            ((superposition sup-vec)
                                (superposition (for/vector ((v sup-vec)) (loop v targ sup-vec))))
                            ('W (state (vector-set frmr 'B targ) #f))
                            ('B (state (vector-set frmr 'W targ) #f))
                            ('w (state (vector-set frmr 'b targ) #f))
                            ('b (state (vector-set frmr 'w targ) #f))
                            ))))
                (loop in-state target '()))))))

(define-syntax CNOT-help 
    (lambda (stx)
        (syntax-parse stx
            ((_ in-state control target)
                #`(begin
                    (if (is-B? (vector-ref in-state control))
                        (X (state in-state #f) target)
                        (state in-state #f)))))))

(define-syntax CNOT 
    (lambda (stx)
        (syntax-parse stx
            ((_ in-state control target)
                    #`(letrec ((loop 
                        (lambda (current cont targ)
                            (match current 
                                ((state vec _)
                                    (CNOT-help vec cont targ))
                                ((superposition sup-vec)
                                    (superposition (for/vector ((v sup-vec)) (loop v cont targ))))
                                ))))
                (loop in-state control target))))))

(define-syntax Z-help
    (lambda (stx)
        (syntax-parse stx
            ((_ in-vec target)
                #`(match (vector-ref in-vec target)
                    ('B (state (vector-set in-vec 'b target) #f))
                    ('b (state (vector-set in-vec 'B target) #f))
                    (_ (state in-vec #f)))))))


(define-syntax Z
    (lambda (stx)
        (syntax-parse stx 
            ((_ in-state target) 
                #`(letrec ((loop 
                    (lambda (current tar)
                        (match current 
                            ((state vec _)
                                (Z-help vec tar))
                            ((superposition sup-vec)
                                (superposition (for/vector ((v sup-vec)) (loop v target)))))))) 
                    (loop in-state target))))))

(define-syntax CZ 
    (lambda (stx) 
        (syntax-parse stx
            ((_ in-state target control)
                #`(letrec ((loop
                    (lambda (current)
                        (match current
                            ((superposition sup-vec)
                                (superposition (for/vector ((v sup-vec)) (loop v))))
                            ((state state-vec _)
                                (if (and (is-B? (vector-ref state-vec target)) (is-B? (vector-ref state-vec control)))
                                    (Z current control)
                                    current))
                            (_ 
                                current)))))
                (loop in-state))))))

(define-syntax toggle-bool
    (lambda (stx)
        (syntax-parse stx
            ((_ bool)
                #'(if bool
                    #f
                    #t)))))

(define-syntax fill-negative-inner
    (lambda (stx)
        (syntax-parse stx
            ((_ in-state)
                (define var (gensym 'var))
                #`(letrec ((#,var #f))
                    (begin (for ((s (state-vec in-state)))
                        (if (or (eq? s 'b) (eq? s 'w))
                            (set! #,var (toggle-bool #,var))
                            (void)))
                        (state (state-vec in-state) #,var)))))))

(define-syntax fill-negative
    (lambda (stx)
        (syntax-parse stx
            ((_ state-in)
                #`(letrec ((loop 
                    (lambda (current)
                        (match current
                            ((state state-vec _)
                                (fill-negative-inner current))
                            ((superposition sup-vec)
                                (superposition (for/vector ((v sup-vec)) (loop v))))))))
                    (loop state-in))))))

(define-syntax is-negative?
    (lambda (stx)
        (syntax-parse stx
            ((_ in-state)
                #'(state-neg-marker in-state)))))


; not the most efficient but easier to implement
;   these vector->list and list->vector functions are defintely eating up a good chunk of time
(define-syntax vector-add!
    (lambda (stx)
        (syntax-parse stx
            ((_ vect to-add)
                #'(set! vect (list->vector (append (vector->list vect) (list to-add))))))))

(define-syntax vector-add
    (lambda (stx)
        (syntax-parse stx
            ((_ v ta)
                #`(begin  (vector-add! v ta)  v)))))


(define-syntax flatten-superposition
    (lambda (stx)
        (syntax-parse stx
            ((_ in-super)
                (define vec-name (format-id stx "~a" (gensym 'vec)))
                #`(let ((#,vec-name (vector))) (letrec ((loop
                    (lambda (current)
                        (match current 
                            
                            ((state _ _) 
                                (vector-add! #,vec-name current))
                            ((superposition sup-vec)
                                (begin (for ((s sup-vec)) (loop s))))
                            ((vector vecs syntax/ellipses)
                                (begin (for ((s vecs)) (loop s))))
                            (_ 
                                ; (println current)
                                current)))))
                    (begin (loop in-super) (superposition #,vec-name))))))))

(define-syntax to-collapse?
    (lambda (stx)
        (syntax-parse stx
            ((_ state1 state2)
                #`(if (and (not (eq? state1 (void))) (not (eq? state1 (void))))
                    (if (and (is-eq? state1 state2) 
                    (eq? (toggle-bool (state-neg-marker state1)) (state-neg-marker state2)))
                        1
                        (if (is-eq? state1 state2)
                            2
                            0))
                    0)))))


(define-syntax scan-collapse-outer
    (lambda (stx)
        (syntax-parse stx
            ((_ in-super in-state)
                #'(scan-collapse (superposition-vec (fill-negative (flatten-superposition in-super))) (fill-negative in-state)) ))
))

(define-syntax vector-remove 
    (lambda (stx) 
        (syntax-parse stx
            ((_ vec to-remove)
                #'(list->vector (remove to-remove (vector->list vec)))))))

(define-syntax scan-collapse
    (lambda (stx)
        (syntax-parse stx
            ((_ in-super in-state)
                #`(letrec ((collapse-inner
                    (lambda (sup target current)
                        (if (>= current (vector-length sup))
                            (vector-add sup target)
                            (if (eq? (void) (vector-ref sup current))
                                (collapse-inner sup target (+ 1 current))
                                (cond 
                                    ((eq? (to-collapse? (vector-ref sup current) target) 1)
                                        (vector-remove sup (vector-ref sup current)))
                                    ; ((eq? (to-collapse? (vector-ref sup current) target) 2)
                                    ;     (collapse-inner (vector-remove sup target) target (+ 1 current)))
                                    (else 
                                        (collapse-inner sup target (+ 1 current)))))))))
                (superposition (collapse-inner in-super in-state 0)))))))

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

