;*****************************************************************
; checks type of value and prints it to stdout
;*****************************************************************
(define (check-stmt value) 
  (cond
   [(pair? value) (display "pair")]
   [(list? value) (display "list")]
   [(string? value) (display "string")]
   [else (display "not list or pair")])
)