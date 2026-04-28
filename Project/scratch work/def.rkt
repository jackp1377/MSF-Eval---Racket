#lang racket
    (provide (struct-out state) (struct-out superposition))
    (struct state (vec neg-marker) #:transparent)
    (struct superposition (vec) #:transparent)

 