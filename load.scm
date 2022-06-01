(define-syntax dbg
  (syntax-rules ()
    ((_ fst ...)
     (begin (let loop ((lst (list fst ...))
                       (symbol-names (map symbol->string (quote (fst ...)))))
              (if (not (null? lst))
                  (begin (display (string-append " " (car symbol-names) ": "))
                         (display (car lst))
                         (display ",\t")
                         ;; when it's the last element of the list,
                         ;; print a newline
                         (if (null? (cdr lst)) (newline))
                         (loop (cdr lst) (cdr symbol-names)))))))))
