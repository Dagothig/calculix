#! /usr/bin/env gsi -:dR

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
    (lambda (f default lst)
        (if (null? lst)
            default
            (if (f (car lst))
                (car lst)
                (find f default (cdr lst))))))

(define every
    (lambda (f lst)
        (not (find (lambda (e) (not (f e))) #f lst))))

(define list->number (lambda (lst) (string->number (list->string lst))))
(define number->list (lambda (num) (string->list (number->string num))))

(define digit? (lambda (c) (and (char>=? c #\0) (char<=? c #\9))))
(define number? (lambda (n) (every digit? n)))
(define non-0-digit? (lambda (c) (and (digit? c) (not (eq? c #\0)))))
(define operator?
    (lambda (c) (case c
        ((#\+ #\- #\/ #\* #\^) #t)
        (else #f))))
(define varassignation? (lambda (c) (eq? c #\=)))
(define varname? (lambda (c) (and (char>=? c #\a) (char<=? c #\z))))

(define tokenize
    (lambda (expr)
        (map (lambda (token) (reverse token))
            (reverse
                (foldl
                    (lambda (tokens c)
                        (let ((current (car tokens)) (others (cdr tokens)))
                            (if (char-whitespace? c)
                                (if (null? current)
                                    tokens
                                    (cons '() tokens))
                                (cons (cons c current) others))))
                    (list '())
                    expr)))))

(define process-number
    (lambda (state token)
        (if (number? token)
            (cons (cons token (car state)) (cdr state))
            (raise (string-append
                "Invalid number \""
                (list->string token)
                "\"")))))

(define process-operator
    (lambda (state token)
        (if (eq? (length token) 1)
            (let ((func (case (car token)
                        ((#\+) +) ((#\-) -) ((#\*) *)
                        (else (raise (string-append
                            "Operation \""
                            (list->string token)
                            "\" not supported")))))
                  (pile (car state)))
                (if (>= (length pile) 2)
                    (let*((depiled (cdr (cdr pile)))
                          (arg1 (list->number (car (cdr pile))))
                          (arg2 (list->number (car pile)))
                          (result (number->list (func arg1 arg2))))
                        (cons
                            (cons result depiled)
                            (cdr state)))
                    (raise (string-append
                        "Not enough arguments for \""
                        (list->string token)
                        "\" (need 2)"))))
            (raise (string-append
                "Invalid operation \""
                (list->string token)
                "\"")))))

(define process-set
    (lambda (state token)
        (if (and (eq? (length token) 2)
                (varname? (car (cdr token))))
        (if (>= (length (car state)) 1)
            (let*((pile (car state))
                  (dict (cdr state))
                  (varname (cdr token))
                  (arg (car pile))
                  (newdict (foldl
                    (lambda (newdict pair)
                        (if (and (not (null? pair)) (eq? (car pair) varname))
                            newdict
                            (cons pair newdict)))
                    (list (list varname arg))
                    dict)))
                (cons pile newdict))
            (raise (string-append
                "Not enough arguments for assignation (need 1)")))
            (raise (string-append
                "Invalid variable name \""
                (list->string (cdr token))
                "\"")))))

(define process-ref
    (lambda (state token)
        (if (eq? (length token) 1)
            (let ((pile (car state))
                  (dict (cdr state)))
                (let ((pair (find
                        (lambda (pair) (lst-eq? (car pair) token))
                        #f
                        dict)))
                    (if pair
                        (cons (cons (car (cdr pair)) pile) dict)
                        (raise (string-append
                            "Variable \""
                            (list->string token)
                            "\" not bound")))))
        (raise (string-append
            "Invalid variable name \""
            (list->string token)
            "\"")))))

(define process-token
    (lambda (state token)
        (let ((top (car token)))
            (cond
                ((non-0-digit? top) (process-number state token))
                ((operator? top) (process-operator state token))
                ((varassignation? top) (process-set state token))
                ((varname? top) (process-ref state token))
                (else (raise (string-append
                    "Unknown token \""
                    (list->string token)
                    "\"")))))))

(define traiter
    (lambda (expr dict)
        (if (null? expr)
            (cons '(#\0) dict)
            (with-exception-catcher
                (lambda (e)
                    (cons (string->list e) dict))
                (lambda ()
                    (let ((result
                            (foldl
                                (lambda (state token)
                                    (process-token state token))
                                (cons '() dict)
                                (tokenize expr))))
                        (cons (car (car result)) (cdr result))))))))

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