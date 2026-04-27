#lang racket


(provide all-defined-out)

; (begin-for 
;     (define W 'W)
;     (define B 'B))

; (begin-for  
;     (struct state (vec neg-marker) #:transparent)
;     (struct superposition (vec) #:transparent))

(struct state (vec neg-marker) #:transparent)
(struct superposition (vec) #:transparent)

; (define  datum 
;     (lambda (stx)
;         (syntax-parse stx
;             ((_ . n:number) 
;                 (#%datum . n)))))

; (define  def
;     (lambda (stx)
;         (syntax-parse stx
;             ((_ name thingie)
;                  (define name thingie)))))

(define  I
    (lambda (stx)
        (match stx
            ((_ state num ...)
                state))))

; (define  module-begin 
;     (lambda (stx)
;         (syntax-parse stx
;             ((_ body ...)
;                 (define W (format-id stx "W"))
;                 (define B (format-id stx "B"))
;                 (define w (format-id stx "w"))
;                 (define b (format-id stx "b"))
;                  (#%module-begin 
;                     (define  W 'W)
;                     (define  B 'B)
;                     (define  w 'w)
;                     (define  b 'b)
;                     body ...)))))


; (define  app 
;     (lambda (stx)
;         (syntax-parse stx
;             ((_ f args ...) 
;                  (#%app f args ...)))))


(define  make-state 
    (lambda (stx)
        (match stx
          ((_ x ...)  
             (state (vector x ...) #f)))))

(define  make-superposition
    (lambda (stx)
        (match stx
            ((_ x ...)
                 (superposition (vector x ...))))))


(define  display-state
    (lambda (stx)
        (match stx
            ((_ whatever)
                   
                    whatever))))

(define  state-ref
    (lambda (stx)
        ( match stx
            ((_ state mem)
                 (vector-ref (state-vec state) mem)))))

(define  super-ref
    (lambda (stx)
        ( match stx
            ((_ superposition mem)
                 (vector-ref (superposition-vec superposition) mem)))))


(define  is-state?
    (lambda (stx)
        ( match stx
            ((_ thingie)
                 (state? thingie)))))

(define  is-W?
    (lambda (stx)
        ( match stx
            ((_ item)
                 (or (eq? item 'w) (eq? item 'W))))))

(define  is-B?
    (lambda (stx)
        ( match stx
            ((_ item)
                 (or (eq? item 'b) (eq? item 'B))))))

(define  is-eq-micro? 
    (lambda (stx)
        ( match stx
            ((_ i1 i2)
                 (or (and (is-B? i1) (is-B? i2)) (and (is-W? i1) (is-W? i2)))))))

(define  is-eq?
    (lambda (stx)
        ( match stx
            ((_ state1 state2)
                 (letrec ((loop 
                    (lambda (counter)
                        (if (eq? counter (vector-length (state-vec state1)))
                            #t
                            (if (is-eq-micro? (vector-ref (state-vec state1) counter) (vector-ref (state-vec state2) counter))
                                (loop (+ 1 counter))
                                #f))
                    )))
                    (loop 0))))))

(define  pretty-print
    (lambda (stx)
        ( match stx
            ((_ to-print)
                 (letrec ((loop
                    (lambda (current)
                        (match current
                            ((superposition sup-vec)
                                (remove-duplicates (for/list ((v sup-vec) #:unless (eq? v (void)))
                                    (loop v))))
                            ((state state-vec _)
                                (for/list ((v state-vec) #:unless (eq? v (void))) (loop v)))
                            (_ current)))))
                    (loop to-print))))))

(define  vector-set 
    (lambda (stx)
        ( match stx
            ((_ vec new-thing pos)
                 (begin
                    (let ((v (vector-copy vec)))
                        (vector-set! v pos new-thing)
                        v))))))


; Old hadamard function - this was rebuilt whenever i discovered my original collapse algorithm
;    was evil as fuck

; (define  hadamard-one
;     (lambda (stx)
;         ( match stx
;             ((_ in-state target)
;                  (letrec ((loop 
;                     (lambda (current targ frmr)
;                         (match current
;                             ('W 
;                             ;     (make-superposition 
;                             ;         (state (vector-set frmr 'W targ) #f)
;                             ;         (state (vector-set frmr 'B targ) #f))) 
;                                 ; (define inside (scan-collapse-outer (make-superposition frmr) (state (vector-set frmr 'W targ) #f)))
;                                 ; (println inside)
;                                 (scan-collapse-outer 
;                                     (scan-collapse-outer (superposition (vector )) (state (vector-set frmr 'W targ) #f)) 
;                                     (state (vector-set frmr 'B targ) #f)))
;                             ('B
;                                 ; (make-superposition 
;                                 ;     (state (vector-set frmr 'W targ) #f)
;                                 ;     (state (vector-set frmr 'b targ) #f)))
;                                 (scan-collapse-outer 
;                                     (scan-collapse-outer (superposition (vector )) (state (vector-set frmr 'W targ) #f)) 
;                                     (state (vector-set frmr 'b targ) #f)))
;                             ('b 
;                                 (make-superposition 
;                                     (state (vector-set frmr 'w targ) #f)
;                                     (state (vector-set frmr 'B targ) #f))
;                                 )
;                             ('w 
;                                 (make-superposition 
;                                     (state (vector-set frmr 'w targ) #f)
;                                     (state (vector-set frmr 'b targ) #f)))
;                             ((state state-vec _)
;                                 ; (vector-set! state-vec targ (loop (vector-ref state-vec targ) 0))
;                                 (loop (vector-ref state-vec targ) targ state-vec))
;                             ((superposition sup-vec)
;                                 (superposition (for/vector ((sup sup-vec))
;                                     (loop sup targ current))))))))
;                     (loop in-state target '()))))))

(define  hadamard->collapse
    (lambda (stx)
        ( match stx
            ((_ in-state vec)
                 (letrec ((loop-2 
                    (lambda (curr-state curr-vec)
                        (if (>= curr-vec (vector-length vec))
                            (superposition (vector))
                            (if (eq? (vector-ref vec curr-vec) (void))
                                (loop-2 curr-state (+ curr-vec 1)) 
                                (scan-collapse-outer (loop-2 curr-state (+ curr-vec 1)) (vector-ref vec curr-vec)))
                        )
                        )))
                 (loop-2 in-state 0))))))

(define-for  hadamard-helper
    (lambda (in-state targ)
        (begin 
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
                    ))))))

(define  hadamard--
    (lambda (stx)
        ( match stx
            ((_ in-state targ)
                 (begin 
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

(define  hadamard 
    (lambda (stx)
        ( match stx
            ((_ in-state targ)
                  (hadamard-helper in-state targ)))))

; (define  hadamard--
;     (lambda (stx)
;         ( match stx
;             ((_ in-state target)
;                  (collapse (hadamard-- in-state target))))))

(define  X
    (lambda (stx)
        ( match stx
            ((_ in-state target)
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
                (loop in-state target '()))))))

(define  CNOT-help 
    (lambda (stx)
        ( match stx
            ((_ in-state control target)
                 (begin
                    (if (is-B? (vector-ref in-state control))
                        (X (state in-state #f) target)
                        (state in-state #f)))))))

(define  CNOT 
    (lambda (stx)
        ( match stx
            ((_ in-state control target)
                     (letrec ((loop 
                        (lambda (current cont targ)
                            (match current
                                ((state vec _)
                                    (CNOT-help vec cont targ))
                                ((superposition sup-vec)
                                    (superposition (for/vector ((v sup-vec)) (loop v cont targ))))
                                ))))
                (loop in-state control target))))))

(define  Z-help
    (lambda (stx)
        ( match stx
            ((_ in-vec target)
                 (match (vector-ref in-vec target)
                    ('B (state (vector-set in-vec 'b target) #f))
                    ('b (state (vector-set in-vec 'B target) #f))
                    (_ (state in-vec #f)))))))


(define  Z
    (lambda (stx)
        ( match stx 
            ((_ in-state target) 
                 (letrec ((loop 
                    (lambda (current tar)
                        (match current 
                            ((state vec _)
                                (Z-help vec tar))
                            ((superposition sup-vec)
                                (superposition (for/vector ((v sup-vec)) (loop v target)))))))) 
                    (loop in-state target))))))

(define  CZ 
    (lambda (stx) 
        ( match stx
            ((_ in-state target control)
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
                (loop in-state))))))

(define  toggle-bool
    (lambda (stx)
        ( match stx
            ((_ bool)
                 (if bool
                    #f
                    #t)))))

(define  fill-negative-inner
    (lambda (stx)
        ( match stx
            ((_ in-state)
                (define var (gensym 'var))
                 (letrec (( var #f))
                    (begin (for ((s (state-vec in-state)))
                        (if (or (eq? s 'b) (eq? s 'w))
                            (set!  var (toggle-bool  var))
                            (void)))
                        (state (state-vec in-state)  var)))))))

(define  fill-negative
    (lambda (stx)
        ( match stx
            ((_ state-in)
                 (letrec ((loop 
                    (lambda (current)
                        (match current
                            ((state state-vec _)
                                (fill-negative-inner current))
                            ((superposition sup-vec)
                                (superposition (for/vector ((v sup-vec)) (loop v))))))))
                    (loop state-in))))))

(define  is-negative?
    (lambda (stx)
        ( match stx
            ((_ in-state)
                 (state-neg-marker in-state)))))


; not the most efficient but easier to implement
;   these vector->list and list->vector functions are defintely eating up a good chunk of time
(define  vector-add!
    (lambda (stx)
        ( match stx
            ((_ vect to-add)
                 (set! vect (list->vector (append (vector->list vect) (list to-add))))))))

(define  vector-add
    (lambda (stx)
        ( match stx
            ((_ v ta)
                 (begin  (vector-add! v ta)  v)))))


(define  flatten-superposition
    (lambda (stx)
        ( match stx
            ((_ in-super)
                (define vec-name (format-id stx "~a" (gensym 'vec)))
                 (let (( vec-name (vector))) (letrec ((loop
                    (lambda (current)
                        (match current 
                            
                            ((state _ _) 
                                (vector-add!  vec-name current))
                            ((superposition sup-vec)
                                (begin (for ((s sup-vec)) (loop s))))
                            ((vector vecs syntax/ellipses)
                                (begin (for ((s vecs)) (loop s))))
                            (_ 
                                ; (println current)
                                current)))))
                    (begin (loop in-super) (superposition  vec-name))))))))

(define to-collapse?
    (lambda (stx)
        ( match stx
            ((_ state1 state2)
                 (if (and (not (eq? state1 (void))) (not (eq? state1 (void))))
                    (if (and (is-eq? state1 state2) 
                    (eq? (toggle-bool (state-neg-marker state1)) (state-neg-marker state2)))
                        1
                        (if (is-eq? state1 state2)
                            2
                            0))
                    0)))))


; old evil collapse function
;    this functions in some crazy high O(n) time so let's leave it commented

; (define  collapse-body
;     (lambda (stx)
;         ( match stx 
;             ((_ in-super)
;                  (if (state? in-super)
;                     (begin (print in-super) in-super)
;                     (letrec 
;                     ((collapse-outer
;                         (lambda (current sup)
                            
;                             (cond 
;                                 ((>= current (vector-length (superposition-vec in-super)))
;                                    (superposition sup))
;                                 ((eq? (vector-ref sup current) (void))
;                                     (collapse-outer (+ 1 current) sup))
;                                 (else 
;                                     (collapse-inner current current sup)))))
;                     (collapse-inner 
;                         (lambda (current-outer current-inner superposition)
                        
;                             (cond
;                                 ((>= current-inner (vector-length (superposition-vec in-super)))
;                                     (collapse-outer (+ 1 current-outer) superposition))
;                                 ((eq? (vector-ref superposition current-inner) (void))
;                                     (collapse-inner current-outer (+ 1 current-inner) superposition))
                                
;                                 (else 
                                    
;                                     (if (to-collapse? 
;                                             (vector-ref superposition current-outer) 
;                                             (vector-ref superposition current-inner))
;                                         (collapse-inner current-outer (+ 1 current-inner) 
;                                             (vector-set (vector-set superposition (void) current-outer) 
;                                                 (void) current-inner))
;                                         (collapse-inner current-outer (+ 1 current-inner) superposition)))))))
                                        
;                     (collapse-outer 0 (superposition-vec in-super))))))))

; (define  collapse
;     (lambda (stx)
;         ( match stx
;             ((_ in-super)
;                  (collapse-body (flatten-superposition (fill-negative in-super)))))))

(define scan-collapse-outer
    (lambda (stx)
        ( match stx
            ((_ in-super in-state)
                (scan-collapse (superposition-vec (fill-negative (flatten-superposition in-super))) (fill-negative in-state)) ))
))

(define vector-remove-help
    (lambda (vec to-remove)
        (list->vector (remove to-remove (vector->list vec)))))
        

(define vector-remove 
    (lambda (stx) 
        ( match stx
            ((_ vec to-remove)
                (vector-remove-help vec to-remove)))))

(define scan-collapse-help
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

(define scan-collapse
    (lambda (stx)
        ( match stx
            ((_ in-super in-state)
                (scan-collapse-help in-super in-state)))))