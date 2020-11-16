(define (accumulate operation term initial-value-before-accumulation from-value get-next to-value)
  (define (loop i result)
    (if (<= i to-value) 
        (loop (get-next i) (operation (term i) result))
        result
    )
  )
  (loop from-value initial-value-before-accumulation)
)

(define (accumulate-iterative operation term
                              initial-value-before-accumulation from-value get-next to-value)
  (define (loop i)
    (if (<= i to-value) 
        (operation (term i) (loop (get-next i)))
        initial-value-before-accumulation
    )
  )
  (loop from-value)
)

(define (print-square-line from to symbol-to-draw-from symbol-to-draw-to)
  (lambda (index _)
    (cond
      ((= from to) (display ""))
      ((= index from) (display symbol-to-draw-from))
      ((= index to) (display symbol-to-draw-to))
      (else (display "─"))
    )
  )
)


(define (print-even-odd-line from to symbol-for-even symbol-for-odd)
  (lambda (index _)
    (cond
      ((= from to) (display ""))
      ((= (remainder index 2) 0) (display symbol-for-even))
      (else (display symbol-for-odd))
    )
  )
)




(define (print-upper-segment from to)
  (accumulate (print-square-line from to "┌" "┐" ) (lambda (x) x) from from (lambda (n) (+ n 1)) to)
)

(define (print-lower-segment from to)
  (accumulate (print-square-line from to "└" "┘" ) (lambda (x) x) from from (lambda (n) (+ n 1)) to)
)

(define (print-cell-blank from to)
  (accumulate (print-even-odd-line from to " "  "│") (lambda (x) x) from from (lambda (n) (+ n 1)) to)
)

(define (print-upper-square number-of-squares)
  (lambda (n _)
    (define index (- number-of-squares n))   
    (define square-horizontal-length (+ (* (- (* n 2) 1) 2) 1))
  
    (print-cell-blank 1 (* index 2))
    (print-upper-segment 1 square-horizontal-length)
    (print-cell-blank 0 (* index 1 2))
    (display "\n")
  )
)

(define (print-lower-square number-of-squares)
  (lambda (n _)
    (define index (- number-of-squares n))   
    (define square-horizontal-length (+ (* (- (* n 2) 1) 2) 1))
  
    (print-cell-blank 1 (* index 2))
    (print-lower-segment 1 square-horizontal-length)
    (print-cell-blank 0 (* index 1 2))
    (display "\n")
  )
)

(define (square n)
  (accumulate-iterative (print-upper-square n) (lambda (x) x) 0 1 (lambda (n) (+ n 1)) n)
  (accumulate (print-lower-square n) (lambda (x) x) 0 1 (lambda (n) (+ n 1)) n)
)