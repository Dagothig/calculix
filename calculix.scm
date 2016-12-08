#! /usr/bin/env gsi -:dR

(define-macro errorf
    (lambda parts
        (let ((str-parts
            (cons 'string-append (map
                (lambda (part) (if (string? part) part `(list->string ,part)))
                parts))))
        `(lambda _ (raise ,str-parts)))))

(define foldl
    (lambda (f base lst)
        (if (null? lst)
            base
            (foldl f (f base (car lst)) (cdr lst)))))

(define lst-eq? (lambda (lst1 lst2)
    (if (and (null? lst1) (null? lst2))
        #t
        (let*((next1 (if (null? lst1) '() (car lst1)))
              (next2 (if (null? lst2) '() (car lst2)))
              (r (eq? next1 next2)))
            (if r
                (lst-eq? (cdr lst1) (cdr lst2))
                #f)))))

(define find
    (lambda (f lst cont fail)
        (if (null? lst)
            (if (procedure? fail) (fail) fail)
            (if (f (car lst))
                (if (procedure? cont) (cont (car lst)) cont)
                (find f (cdr lst) cont fail)))))

(define every (lambda (f lst) (find (lambda (x) (not (f x))) lst #f #t)))

(define split-at
    (lambda (f lst cont)
        (define split-at-with
            (lambda (head tail)
                (if (and (not (null? tail))
                        (f (cons (car tail) head) (cdr tail)))
                    (split-at-with (cons (car tail) head) (cdr tail))
                    (cont (reverse head) tail))))
        (split-at-with '() lst)))

(define depile
    (lambda (pile num cont fail)
        (if (>= (length pile) num)
            (split-at (lambda (args _) (<= (length args) num)) pile cont)
            (fail))))

(define new-pile (lambda (state pile) (cons pile (cdr state))))
(define new-dict (lambda (state dict) (cons (car state) dict)))

(define add-to-pile
    (lambda (state to-add) (new-pile state (cons to-add (car state)))))

(define list->number (lambda (lst) (string->number (list->string lst))))
(define number->list (lambda (num) (string->list (number->string num))))

(define digit? (lambda (c) (and (char>=? c #\0) (char<=? c #\9))))
(define non-0-digit? (lambda (c) (and (digit? c) (not (eq? c #\0)))))
(define char? (lambda (c) (and (char>=? c #\a) (char<=? c #\z))))
(define char-or-digit? (lambda (c) (or (char? c) (digit? c))))

(define number? (lambda (n) (and
    (non-0-digit? (car n))
    (every digit? (cdr n)))))


(define operators (list
    (list #\+ +)
    (list #\- -)
    (list #\- *)))

(define operator?
    (lambda (n) (and
        (null? (cdr n))
        (case (car n) ((#\+ #\- #\/ #\*) #t) (else #f)))))

(define operator
    (lambda (token) (case (car token)
        ((#\+) +) ((#\-) -) ((#\*) *) ((#\/) /)
        (else (errorf "Operation \"" token "\" not supported")))))

(define varassignation? (lambda (n) (and
    (eq? (car n) #\=)
    (varname? (cdr n)))))

(define varname? (lambda (n) (and
    (char? (car n))
    (every char-or-digit? (cdr n)))))

(define tokenize
    (lambda (expr)
        ; Tokenize operates on the expression in a reversed order; it also prepends as it creates the tokens since its a simpler operation.
        ; Thus, once the expression has been tokenized, it is reversed and its' contents are reversed
        ; The tokenization is done by prepending every character to the ongoing token and to begin a new token every time whitespace is found
        (define f
            (lambda (expr tokens token)
                (if (null? expr)
                    (if (null? token) tokens (cons token tokens))
                    (if (char-whitespace? (car expr))
                        (if (null? token)
                            (f (cdr expr) tokens token)
                            (f (cdr expr) (cons token tokens) '()))
                        (f (cdr expr) tokens (cons (car expr) token))))))
        (map reverse (reverse (f expr '() '())))))

(define process-operator
    (lambda (state token)
        (depile (car state) 2
            (lambda (args depiled)
                (add-to-pile
                    (new-pile state depiled)
                    (number->list (apply
                        (operator token)
                        (reverse (map list->number args))))))
            (errorf "Not enough arguments for \"" token "\" (need 2)"))))

(define process-set
    (lambda (state token)
        (depile (car state) 1
            (lambda (args _)
                (new-dict state
                    (foldl
                        (lambda (newdict pair)
                            (if (lst-eq? (car pair) (cdr token))
                                newdict
                                (cons pair newdict)))
                        (list (list (cdr token) (car args)))
                        (cdr state))))
            (errorf "Not enough arguments for assignation (need 1)"))))

(define process-ref
    (lambda (state token)
        (add-to-pile state
            (find
                (lambda (pair) (lst-eq? (car pair) token))
                (cdr state)
                cadr
                (errorf "Variable \"" token "\" not bound")))))

(define processors (list
    (list number? add-to-pile)
    (list operator? process-operator)
    (list varassignation? process-set)
    (list varname? process-ref)))

(define process-token
    (lambda (state token) (find
        (lambda (processor) ((car processor) token))
        processors
        (lambda (processor) ((cadr processor) state token))
        (errorf "Syntax error \"" token "\""))))

(define traiter
    (lambda (expr dict)
        (if (null? expr)
            (cons '(#\0) dict)
            (with-exception-catcher
                (lambda (e) (cons (string->list e) dict))
                (lambda ()
                    (let ((result (foldl
                            process-token
                            (cons '() dict)
                            (tokenize expr))))
                        (if (<= (length (car result)) 1)
                            (cons (caar result) (cdr result))
                            (raise "Stack not empty"))))))))

;;;----------------------------------------------------------------------------

(define repl
    (lambda (dict)
        (print "# ")
        (let ((ligne (read-line)))
            (if (string? ligne)
                (let ((r (traiter-ligne ligne dict)))
                    (for-each write-char (car r))
                    (print #\newline)
                    (repl (cdr r)))))))

(define traiter-ligne
    (lambda (ligne dict)
        (traiter (string->list ligne) dict)))

(define main (lambda () (repl '()))); dictionnaire initial est vide

;;;----------------------------------------------------------------------------