#!/afs/cats.ucsc.edu/courses/cmps112-wm/usr/racket/bin/mzscheme -qr
;; $Id: sbi.scm,v 1.3 2016-09-23 18:23:20-07 - - $
;;
;; NAME
;;    sbi.scm - silly basic interpreter
;;
;; SYNOPSIS
;;    sbi.scm filename.sbir
;;
;; DESCRIPTION
;;    The file mentioned in argv[1] is read and assumed to be an SBIR
;;    program, which is the executed.  Currently it is only printed.
;;

(define *stderr* (current-error-port))

; create our symbol tables
(define *function-table* (make-hash))
(define *label-table* (make-hash))
(define *var-table* (make-hash))

;*****************************************************************
; returns value in hash at key
;*****************************************************************
(define (symbol-get hash key)
        (hash-ref hash key))

;*****************************************************************
; puts value in hash with key
;*****************************************************************
(define (symbol-put! key value hash)
        (hash-set! hash key value))


;*****************************************************************
; uhhh, runs files or somethings
;*****************************************************************
(define *run-file*
    (let-values
        ((
          (dirpath basepath root?)
          (split-path (find-system-path 'run-file))
          )
        )
        (path->string basepath))
)
;*****************************************************************
; print key label pairs
;*****************************************************************
(define (show item)
        (newline)
        ;(display label) (display ":") (newline)
        (display (car item) ) (newline))

;*****************************************************************
; prints to stderr in the event of program failure...or something
;*****************************************************************
(define (die list)
    (for-each (lambda (item) (display item *stderr*)) list)
    (newline *stderr*)
    (exit 1))

;*****************************************************************
; calls usage-exit in the event of program failure. 
;*****************************************************************
(define (usage-exit)
    (die `("Usage: " ,*run-file* " filename")))

;*****************************************************************
; read from the input file and return list of the lines
;*****************************************************************
(define (readlist-from-inputfile filename)
    (let ((inputfile (open-input-file filename)))
         (if (not (input-port? inputfile))
             (die `(,*run-file* ": " ,filename ": open failed"))
             (let ((program (read inputfile)))
                  (close-input-port inputfile)
                         program)
          )
    )
)

;*****************************************************************
; put contents of a list into a hash-table (hash)
;*****************************************************************
(define (put-in-hash list hash)
  (when (not (null? list))
    (let ((first (car list)))
      (when (not (symbol? first) )
          (hash-set! hash first list)
      )
    )
    (put-in-hash (cdr list) hash)
  )
)

;*****************************************************************
; initialize variable table
;*****************************************************************
(for-each
    (lambda (pair)
      (symbol-put! (car pair) (cadr pair) *var-table*))
    '(
      (pi   3.141592653589793238462643383279502884197169399)
      (e    2.718281828459045235360287471352662497757247093)
    )
)

;*****************************************************************
; initialize function table
;*****************************************************************
(for-each
    (lambda (pair)
      (symbol-put! (car pair) (cadr pair) *function-table*))
    '(
      (print   ,(lambda (msg) (display msg) (newline) ) ) 
    )
)


;*****************************************************************
; The function evalexpr outlines how to evaluate a list recursively.
;*****************************************************************
(define (evalexpr expr)
   (cond ((number? expr) expr)
         ;((symbol? expr) (hash-ref *function-table* expr #f))
         ((pair? expr)   (apply (hash-ref *function-table* (car expr))
                                (map evalexpr (cdr expr))
                         )
         )
         (else #f))
)

;*****************************************************************
; nifty lambda for doing stuff to all values in hash table
;*****************************************************************
;(hash-for-each *label-table*
;              (lambda (key value) (show key value)))



;*****************************************************************
; checks type of value and prints it to stdout
;*****************************************************************
(define (check-stmt value) 
  (cond
   [(pair? value) (display "list")]
   [(list? value) (display "pair")]
   [else (display "not list or pair")])
)

;*****************************************************************
; main function to execute (interpreter)
;*****************************************************************
(define (main arglist)
    (if (or (null? arglist) (not (null? (cdr arglist))))
        (usage-exit)
        (begin ;then
          (let* (
                  (sbprogfile (car arglist))
                  (program (readlist-from-inputfile sbprogfile))
                )
            (put-in-hash program *label-table*); initialize var-table
            (hash-for-each *label-table*
              (lambda (key value) (show value)))
          )
        )
    )
)


(main (vector->list (current-command-line-arguments)))
