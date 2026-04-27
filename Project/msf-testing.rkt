#lang s-exp "msf-eval.rkt"

; (define s1 (make-state W B))

; (define super1 (make-superposition (make-state W) (make-state B)))

; (display-state s1)

; (display-state super1)

; (state-ref s1 1)

; (super-ref super1 0)

; (define s2 (make-state W (make-superposition (make-state W) (make-state B))))

; s2

; (is-W? (state-ref (super-ref (state-ref s2 1) 0) 0))

; (is-state? s2)

; (pretty-print s2)


; (pretty-print (hadamard (make-state W B W) 0))

; (pretty-print (hadamard (hadamard (make-state W B W) 0) 2))

; ; (define s3 (make-state (make-superposition (make-state W B) (make-state W W)) B))

; (define s3 (make-superposition (make-state W B W) (make-state B W B)))

; (pretty-print (X s3 0))

; (pretty-print (CNOT (make-state W B) 1 0))

; (define ent-test-1 (CNOT (hadamard (make-state W W) 0) 0 1))

; (pretty-print ent-test-1)
; (pretty-print (Z ent-test-1 1))
; (define et2 (hadamard (hadamard ent-test-1 0) 1))
; (pretty-print et2)

; (fill-negative et2)
; (is-negative? (fill-negative (make-state W b)))
; (is-negative? (fill-negative (make-state b b)))

; (define et3 (fill-negative (make-state W b)))



; (pretty-print (flatten-superposition et2))



; (pretty-print ent-test-1)
; (pretty-print (flatten-superposition et2))



; (define rt (hadamard (hadamard (make-state W W) 0) 1))
; (pretty-print (flatten-superposition rt))
; (pretty-print (flatten-superposition (hadamard (hadamard rt 1) 0)))

; (is-eq? (make-state W B) (make-state W B))
; (is-eq? (make-state W W) (make-state W B))


; (fill-negative (flatten-superposition (hadamard (hadamard rt 1) 0)))

; (pretty-print (collapse (flatten-superposition (fill-negative (hadamard (hadamard rt 1) 0)))))

; (pretty-print (collapse (fill-negative (flatten-superposition (hadamard (hadamard (make-state W) 0) 0)))))

; (collapse (fill-negative (flatten-superposition (hadamard (hadamard (X (X (CZ (X (X (hadamard (hadamard (CZ (hadamard (hadamard (make-state W W) 1) 0) 0 1) 0) 1) 1) 0) 1 0) 0) 1) 1) 0))))


; (pretty-print (I (flatten-superposition (fill-negative (hadamard (hadamard (I (I (CZ (Z (Z (hadamard (hadamard (CZ (hadamard (hadamard (make-state W W) 1) 0) 0 1) 0) 1) 1) 0) 1 0) 0) 1) 1) 0)))))
; 
; (pretty-print (collapse (fill-negative (hadamard (hadamard (I (I (CZ (Z (Z (I (hadamard (hadamard (CZ (collapse (hadamard (hadamard (make-state W W) 1) 0)) 0 1) 0) 1)) 1) 0) 1 0) 0) 1) 1) 0))))

; (hadamard (make-state W) 0)

; (pretty-print (flatten-superposition (Z (Z (hadamard (hadamard (CZ (hadamard (hadamard (make-state W W) 1) 0) 0 1) 0) 1) 1) 0)))

; (I (make-state W))
; (pretty-print (hadamard (hadamard (hadamard (hadamard (make-state W) 0) 0) 0) 0))

; (pretty-print (I (fill-negative (hadamard (hadamard (I (I (CZ (Z (Z (I (hadamard (hadamard (CZ (I (hadamard (hadamard (make-state W W) 1) 0)) 0 1) 0) 1)) 1) 0) 1 0) 0) 1) 1) 0))))

; (pretty-print (hadamard (hadamard (CZ (hadamard (hadamard (CZ (hadamard (hadamard (make-state W W) 0) 1) 0 1) 0) 1) 0 1) 0) 1))
; (pretty-print (hadamard (hadamard (CZ (hadamard (hadamard (make-state W W) 0) 1) 0 1) 0) 1))

; (pretty-print (hadamard (hadamard (CZ (Z (Z (hadamard (hadamard (CZ (hadamard (hadamard (make-state W W) 0) 1) 0 1) 0) 1) 0) 1) 0 1) 0 ) 1))

; (define ent-circ (circuit (make-state W W) (hadamard 0) (CNOT 0 1) ))

; (pretty-print ent-circ)

(define grover-init (circuit (make-state W W) (hadamard 0) (hadamard 1)))

(define grover-oracle-WB (circuit grover-init (X 0) (CZ 0 1) (X 0)))

(define grover-oracle-BB (circuit grover-init (CZ 0 1)))

(define grover-search-WB (circuit grover-oracle-WB (hadamard 1) (hadamard 0) (Z 0) (Z 1) (CZ 0 1) (hadamard 0) (hadamard 1)))

(define grover-search-BB (circuit grover-oracle-BB (hadamard 1) (hadamard 0) (Z 0) (Z 1) (CZ 0 1) (hadamard 0) (hadamard 1)))

(pretty-print grover-search-WB)

(pretty-print grover-search-BB)

; add deutsch-jozca and entanglement swapping

(define init-DJ (circuit (make-state B B) (hadamard 1) (hadamard 0)))

(define oracle-DJ-unbalanced (circuit init-DJ (CNOT 0 1)))

(define oracle-DJ-balanced (circuit init-DJ (I)))

(define DJ-ub (circuit oracle-DJ-unbalanced (hadamard 0) (hadamard 1)))

(define DJ-b (circuit oracle-DJ-balanced (hadamard 0) (hadamard 1)))

(pretty-print DJ-ub)

(pretty-print DJ-b)



; (pretty-print (CNOT (hadamard (make-state W W) 0) 0 1))
; (pretty-print (hadamard (hadamard (CZ (hadamard (hadamard (make-state W W) 0) 1) 0 1) 0) 1))
; (hadamard (X (Z (make-state B) 0) 0) 0)

;(define basic-entanglement (CNOT (hadamard (make-state W W) 0) 0 1))

;(pretty-print basic-entanglement)


;(pretty-print (hadamard (CNOT (X (I basic-entanglement 0) 0) 0 1) 0))