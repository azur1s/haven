(define (_hvn_nequal? x y) (not (equal? x y)))

(define (_hvn_string-join lst sep)
  (if (null? lst)
      ""
      (let loop ((lst (cdr lst)) (acc (car lst)))
        (if (null? lst)
            acc
            (loop (cdr lst) (string-append acc sep (car lst)))))))

(define (_hvn_any->string x)
  (cond
    ((number? x) (number->string x))
    ((string? x) x)
    ((boolean? x) (if x "true" "false"))
    ((null? x) "()")
    ; format list as = [1, 2, 3...]
    ((list? x) (string-append "[" (_hvn_string-join (map _hvn_any->string x) ", ") "]"))
    (else (error "Unsupported type" x))))
; ---
