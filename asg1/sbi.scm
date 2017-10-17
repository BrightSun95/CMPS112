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
;(define (show label item)
;        (newline)
;        (display label) (display ":") (newline)
;        (display (car item) ) (newline))

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
    (let ((first (caar list)))
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
      (dim   ,(lambda () () ))
      (let   ,(lambda () () ))
      (goto   ,(lambda () () ))
      (if   ,(lambda () () ))
      (print ,(lambda (x) (display x)(newline)))
      (input   ,(lambda () () ))
      (abs   ,(lambda () () ))
      (acos   ,(lambda () () ))
      (asin   ,(lambda () () ))
      (atan   ,(lambda () () ))
      (abs   ,(lambda () () ))
      (ceil   ,(lambda () () ))
      (cos   ,(lambda () () ))
      (exp   ,(lambda () () ))
      (floor   ,(lambda () () ))
      (log   ,(lambda () () ))
      (log10   ,(lambda () () ))
      (log2   ,(lambda () () ))
      (round   ,(lambda () () ))
      (sin   ,(lambda () () ))
      (sqrt   ,(lambda () () ))
      (tan   ,(lambda () () ))
      (trunc   ,(lambda () () ))
    )
)


;*****************************************************************
; The function eval-expr outlines how to evaluate a list recursively.
;*****************************************************************
(define (eval-expr expr)
   (cond ((number? expr) expr)
          ((string? expr) (list->string expr))
          ((symbol? expr) (hash-ref *function-table* expr #f))
          ((pair? expr)   (apply (hash-ref *function-table* (car expr))
                                  (map eval-expr (cdr expr))
                          )
          )
          (else #f)
    )
)

;*****************************************************************
; nifty lambda for doing stuff to all values in hash table
;*****************************************************************
;(hash-for-each *label-table*
;              (lambda (key value) (show key value)))

;*****************************************************************
; print key=value
;*****************************************************************
(define (show key value)
    (display key)
    (display " = ")
    (display value)
    (newline))

;*****************************************************************
;
;*****************************************************************
(define (atom? x)
    (and (not (pair? x))
        (not (null? x))))

;*****************************************************************
; evaluate a line from program
;*****************************************************************
(define (eval-line program line_num)
    ; is the line denoted by line_num in the program?
    (when (> (length program) line_num)
  
        (let ((line (list-ref program line_num)))
            
            (cond
                ; (Linenr Label Statement)
                ((= (length line) 3)
                    ;(printf "line length is 3: ~s~n" line) 
                    (set! line (cddr line))
                    (display "3: ")(display line)(newline))
                    ;(execute-line (car line) program line_num))

                ;; Linenr Label|Statement
                ((and (= (length line) 2) (list? (cadr line)))
                  
                  (set! line (cdr line))
                  (display "2: ")(display (cdar line) ) (newline)
                  (eval-expr (car line) )
                )                
                ;; otherwise evaluate next line
                (else
                  (display "1: ")(display (length line) )(newline)
                  (eval-line program (+ line_num 1)))                
             )
         )
     )
)

;*****************************************************************
; reads program (a proper list) and puts labels into *label-table*
;*****************************************************************
(define (hash-labels program)
    (map  (lambda (line)
            
            (when (not (null? (cdr line)))
              (when (atom? (cadr line))
                ;(display(cddr line))(newline)
                (hash-set! *label-table* (cadr line)  line)))
          ) program
    )
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
            (hash-labels program)
            (eval-line program 0)
          )
        )
    )
)


(main (vector->list (current-command-line-arguments)))
