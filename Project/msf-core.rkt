#lang racket

(require  racket/match racket/list)

(provide state superposition state-ref super-ref is-state? is-W? state-print
    hadamard X CNOT Z fill-negative flatten-superposition is-eq?  CZ  )

(struct state (vec neg-marker) #:transparent)
(struct superposition (vec) #:transparent)

(define (state-ref state mem)
  (vector-ref (state-vec state) mem))

(define super-ref
    (lambda (superposition mem)
        (vector-ref (superposition-vec superposition) mem)))

(define is-state?
    (lambda (thingie)
        (state? thingie)))

(define is-W?
    (lambda (item)
        (or (eq? item 'w) (eq? item 'W))))

(define is-B?
    (lambda (item)
        (or (eq? item 'b) (eq? item 'B))))

(define is-eq-micro?
    (lambda (i1 i2)
        (or (and (is-B? i1) (is-B? i2)) (and (is-W? i1) (is-W? i2)))))

(define (is-eq? state1 state2)
   (let loop ([counter 0])
     (if (eq? counter (vector-length (state-vec state1)))
         #t
         (if (is-eq-micro? (vector-ref (state-vec state1) counter) (vector-ref (state-vec state2) counter))
             (loop (+ 1 counter))
            #f))))

(define state-print
    (lambda (to-print)
        (letrec ((loop
                    (lambda (current)
                        ; (println current)
                        (match current
                            ((superposition sup-vec)
                                (remove-duplicates (for/list ((v sup-vec) #:unless (eq? v (void)))
                                    (loop v))))
                            ((state state-vec _)
                                (for/list ((v state-vec) #:unless (eq? v (void))) (loop v)))
                            (_ current)))))
                    (loop to-print))))

(define vector-set
    (lambda (vec new-thing pos)
        (begin
                    (let ((v (vector-copy vec)))
                        (vector-set! v pos new-thing)
                        v))))


(define hadamard->collapse
    (lambda (in-state vec)
        (letrec ((loop-2 
                    (lambda (curr-state curr-vec)
                        (if (>= curr-vec (vector-length vec))
                            (superposition (vector))
                            (if (eq? (vector-ref vec curr-vec) (void))
                                (loop-2 curr-state (+ curr-vec 1)) 
                                (scan-collapse-outer (loop-2 curr-state (+ curr-vec 1)) (vector-ref vec curr-vec)))
                    )
                )))
                 (loop-2 in-state 0))))

(define hadamard
    (lambda (in-state targ)
        
                (define ret (vector))
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
                                ; (println (state (vector-set (state-vec frmr) 'B target) #f))
                                (println ret)
                                (vector-add! ret (state (vector-set (state-vec frmr) 'B target) #f))
                                (vector-add! ret (state (vector-set (state-vec frmr) 'W target) #f))
                                (println ret)
                                ))
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
                    ))))

(define X
    (lambda (in-state target)
        (letrec ((loop 
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
                (loop in-state target '()))
    ))

(define CNOT-help
    (lambda (in-state control target)
        (begin
                    (if (is-B? (vector-ref in-state control))
                        (X (state in-state #f) target)
                        (state in-state #f)))))

(define CNOT
    (lambda (in-state control target)
        (letrec ((loop 
                        (lambda (current cont targ)
                            (match current 
                                ((state vec _)
                                    (CNOT-help vec cont targ))
                                ((superposition sup-vec)
                                    (superposition (for/vector ((v sup-vec)) (loop v cont targ))))
                                ))))
                (loop in-state control target))))

(define Z-help
    (lambda (in-vec target)
        (match (vector-ref in-vec target)
                    ('B (state (vector-set in-vec 'b target) #f))
                    ('b (state (vector-set in-vec 'B target) #f))
                    (_ (state in-vec #f)))))

(define Z
    (lambda (in-state target)
        (letrec ((loop 
                    (lambda (current tar)
                        (match current 
                            ((state vec _)
                                (Z-help vec tar))
                            ((superposition sup-vec)
                                (superposition (for/vector ((v sup-vec)) (loop v target)))))))) 
                    (loop in-state target))))

(define CZ
    (lambda (in-state target control)
        (letrec ((loop
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
                (loop in-state))))

(define toggle-bool
    (lambda (bool)
        (if bool
                    #f
                    #t)))

(define fill-negative 
    (lambda (state-in)
        ; (println state-in)
        (letrec ((loop 
                    (lambda (current)
                        (match current
                            ((state state-vec _)
                                (fill-negative-inner current))
                            ((superposition sup-vec)
                                (superposition (for/vector ((v sup-vec)) (loop v))))))))
                    (loop state-in))))




(define to-collapse?
    (lambda (state1 state2)
        (if (and (not (eq? state1 (void))) (not (eq? state1 (void))))
                    (if (and (is-eq? state1 state2) 
                    (eq? (toggle-bool (state-neg-marker state1)) (state-neg-marker state2)))
                        1
                        (if (is-eq? state1 state2)
                            2
                            0))
                    0)))

(define scan-collapse-outer
    (lambda (in-super in-state)
        ; (println in-super)
        (scan-collapse (superposition-vec (fill-negative (flatten-superposition in-super))) (fill-negative in-state)) ))

(define vector-remove 
    (lambda (vec to-remove)
        ; (println vec)

        (list->vector (remove to-remove (vector->list vec)))))

    (define scan-collapse
    (lambda (in-super in-state)
        (letrec ((collapse-inner
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
                (superposition (collapse-inner in-super in-state 0)))))

; (define-syntax vector-add
;     (lambda (stx)
;         (syntax-parse stx
;             ((_ v ta)
;                 #`(begin  (vector-add! v ta)  v)))))

(define vector-add 
    (lambda (v ta)
        (begin  (vector-add! v ta)  v)))

; (define-syntax vector-add!
;     (lambda (stx)
;         (syntax-parse stx
;             ((_ vect to-add)
;                 #'(set! vect (list->vector (append (vector->list vect) (list to-add))))))))

(define vector-add!
    (lambda (vect to-add)
        ; (println vect)
        ; (println (list->vector (append (vector->list vect) (list to-add))))
        (set! vect (list->vector (append (vector->list vect) (list to-add))))
        (println vect)))

; (define-syntax flatten-superposition
;     (lambda (stx)
;         (syntax-parse stx
;             ((_ in-super)
;                 (define vec-name (format-id stx "~a" (gensym 'vec)))
;                 #`(let ((#,vec-name (vector))) (letrec ((loop
;                     (lambda (current)
;                         (match current 
;                             ((state _ _) 
;                                 (vector-add! #,vec-name current))
;                             ((superposition sup-vec)
;                                 (begin (for ((s sup-vec)) (loop s))))
;                             ((vector vecs syntax/ellipses)
;                                 (begin (for ((s vecs)) (loop s))))
;                             (_ 
;                                 current)))))
;                     (begin (loop in-super) (superposition #,vec-name))))))))

(define flatten-superposition 
    (lambda (in-super)
        (let ((vec-name (vector))) (letrec ((loop
                    (lambda (current)
                        (match current 
                            ((state _ _) 
                                (vector-add! vec-name current))
                            ((superposition sup-vec)
                                (begin (for ((s sup-vec)) (loop s))))
                            ((vector vecs syntax/ellipses)
                                (begin (for ((s vecs)) (loop s))))
                            (_ 
                                current)))))
                    (begin (loop in-super) (superposition vec-name))))))

; (define-syntax fill-negative-inner
;     (lambda (stx)
;         (syntax-parse stx
;             ((_ in-state)
;                 (define var (gensym 'var))
;                 #`(letrec ((#,var #f))
;                     (begin (for ((s (state-vec in-state)))
;                         (if (or (eq? s 'b) (eq? s 'w))
;                             (set! #,var (toggle-bool #,var))
;                             (void)))
;                         (state (state-vec in-state) #,var)))))))

(define fill-negative-inner
    (lambda (in-state)
        (letrec ((var #f))
                    (begin (for ((s (state-vec in-state)))
                        (if (or (eq? s 'b) (eq? s 'w))
                            (set! var (toggle-bool var))
                            (void)))
                        (state (state-vec in-state)  var)))))