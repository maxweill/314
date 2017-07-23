
; *********************************************
; *  314 Principles of Programming Languages  *
; *  Spring 2017                              *
; *  Student Version                          *
; *********************************************

;; contains "ctv", "A", and "reduce" definitions
(load "include.ss")

;; contains simple dictionary definition
(load "dictionary.ss")

;; -----------------------------------------------------
;; HELPER FUNCTIONS
(define bitvect
  (lambda (hashlist dict)
    (cond
      (
       (null? dict) '()
      )
      (else
       (cons (wordhash (car dict) hashlist) (bitvect hashlist (cdr dict)) )
      )
    )
  )
)

(define (delete n lst clean)
  (cond
    ((null? lst) clean)
    ((equal? n (car lst)) (delete n (cdr lst) clean))
    (else
      (delete n (cdr lst) (append clean (list (car lst)))))))

(define wordhash
  (lambda (w hashlist)
    (cond
      (
       (null? hashlist) '()
      )
      (else
       (cons ((car hashlist) w) (wordhash w (cdr hashlist))
      )
     )
    )
  )
)



(define compare
  (lambda (hashlist bitvector hashes)
    (cond ((null? hashes) '())
       (else (cons(compareone hashlist bitvector (car hashes)) (compare hashlist bitvector (cdr hashes))))
       )
    )
)

(define compareone
  (lambda (hashlist bitvector hash)
    (cond ((null? bitvector) #f)
       ((= (car bitvector) hash) #t)
       (else
        (compareone hashlist (cdr bitvector) hash)
       )
     )
))
(define and
(lambda (input1 input2) (and input1 input2)))

(define flatten
  (lambda (l)
     (cond((null? l) '())
       ((pair?(car l))(append (flatten (car l))(flatten (cdr l))))
       (else (append(list(car l))(flatten(cdr l))))
  )))
;; *** CODE FOR ANY HELPER FUNCTION GOES HERE ***


;; -----------------------------------------------------
;; KEY FUNCTION

(define key
  (lambda (w)
    (cond((null? w) '5187)
    (else(+ (ctv (car w)) (* (key (cdr w)) 29) )
          ))
     ;; *** FUNCTION BODY IS MISSING ***
))

;; -----------------------------------------------------
;; EXAMPLE KEY VALUES
;;   (key '(h e l l o))       = 106402241991
;;   (key '(m a y))           = 126526810
;;   (key '(t r e e f r o g)) = 2594908189083745

;; -----------------------------------------------------
;; HASH FUNCTION GENERATORS

;; value of parameter "size" should be a prime number
(define gen-hash-division-method
  (lambda (size)
    (lambda (w)
      (modulo (key w) size))

      ;; range of values: 0..size-1
      ;; *** FUNCTION BODY IS MISSING ***
))

;; value of parameter "size" is not critical
;; Note: hash functions may return integer values in "real"
;;       format, e.g., 17.0 for 17

(define gen-hash-multiplication-method
  (lambda (size) ;; range of values: 0..size-1
     (lambda (w)
       (floor(* size (-(* A (key w))(floor(* A (key w)))))))
       ;; *** FUNCTION BODY IS MISSING ***
))


;; -----------------------------------------------------
;; EXAMPLE HASH FUNCTIONS AND HASH FUNCTION LISTS

(define hash-1 (gen-hash-division-method 70111))
(define hash-2 (gen-hash-division-method 89997))
(define hash-3 (gen-hash-multiplication-method 7224))
(define hash-4 (gen-hash-multiplication-method 900))

(define hashfl-1 (list hash-1 hash-2 hash-3 hash-4))
(define hashfl-2 (list hash-1 hash-3))
(define hashfl-3 (list hash-2 hash-3))

;; -----------------------------------------------------
;; EXAMPLE HASH VALUES
;;   to test your hash function implementation
;;
;;  (hash-1 '(h e l l o))       ==> 35616
;;  (hash-1 '(m a y))           ==> 46566
;;  (hash-1 '(t r e e f r o g)) ==> 48238
;;
;;  (hash-2 '(h e l l o))       ==> 48849
;;  (hash-2 '(m a y))           ==> 81025
;;  (hash-2 '(t r e e f r o g)) ==> 16708
;;
;;  (hash-3 '(h e l l o))       ==> 6331.0
;;  (hash-3 '(m a y))           ==> 2456.0
;;  (hash-3 '(t r e e f r o g)) ==> 1806.0
;;
;;  (hash-4 '(h e l l o))       ==> 788.0
;;  (hash-4 '(m a y))           ==> 306.0
;;  (hash-4 '(t r e e f r o g)) ==> 225.0


;; -----------------------------------------------------
;; SPELL CHECKER GENERATOR

(define gen-checker
  (lambda (hashfunctionlist dict)
    (define bv (flatten(bitvect hashfunctionlist dict)))
    (lambda (word)
      (define w (wordhash word hashfunctionlist))
     (reduce and (compare hashfunctionlist bv w)  #t)
    ;; *** FUNCTION BODY IS MISSING ***
  
)))

;;(delete -1(flatten(bitvect hashfl-1 dictionary))'())
;;(compare hashfl-1(delete -1(flatten(bitvect hashfl-1 dictionary))'()) (delete -1(flatten(wordhash '(h e l l o) hashfl-1))'()))
;; -----------------------------------------------------
;; EXAMPLE SPELL CHECKERS

(define checker-1 (gen-checker hashfl-1 dictionary))
(define checker-2 (gen-checker hashfl-2 dictionary))
(define checker-3 (gen-checker hashfl-3 dictionary))

;; EXAMPLE APPLICATIONS OF A SPELL CHECKER
;;
;;  (checker-1 '(a r g g g g)) ==> #f
;;  (checker-2 '(h e l l o)) ==> #t
;;  (checker-2 '(a r g g g g)) ==> #t  // false positive

