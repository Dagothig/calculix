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

(define find-cont
    (lambda (f lst cont fail)
        (if (null? lst)
            (fail)
            (if (f (car lst))
                (cont (car lst))
                (find-cont f (cdr lst) cont fail)))))

(define list->number (lambda (lst) (string->number (list->string lst))))
(define number->list (lambda (num) (string->list (number->string num))))

(define digit? (lambda (c) (and (char>=? c #\0) (char<=? c #\9))))
(define non-0-digit? (lambda (c) (and (digit? c) (not (eq? c #\0)))))
(define char? (lambda (c) (and (char>=? c #\a) (char<=? c #\z))))
(define char-or-digit? (lambda (c) (or (char? c) (digit? c))))

(define number? (lambda (n) (and
    (non-0-digit? (car n))
    (every digit? (cdr n)))))

(define operator?
    (lambda (n) (and
        (null? (cdr n))
        (case (car n) ((#\+ #\- #\/ #\* #\^) #t) (else #f)))))

(define varassignation? (lambda (n) (and
    (eq? (car n) #\=)
    (varname? (cdr n)))))

(define varname? (lambda (n) (and
    (char? (car n))
    (every char-or-digit? (cdr n)))))

(define tokenize
    (lambda (expr)
        (let ((reversed
            (foldl
                (lambda (tokens c)
                    (let ((current (car tokens)) (others (cdr tokens)))
                        (if (char-whitespace? c)
                            (if (null? current)
                                tokens
                                (cons '() tokens))
                            (cons (cons c current) others))))
                (list '())
                expr)))
            (map reverse (reverse reversed)))))

(define process-number
    (lambda (state token) (cons (cons token (car state)) (cdr state))))

(define process-operator
    (lambda (state token)
        (let ((func (case (car token)
                ((#\+) +) ((#\-) -) ((#\*) *)
                (else (raise (string-append
                    "Operation \""
                    (list->string token)
                    "\" not supported")))))
              (pile (car state)))
            (if (>= (length pile) 2)
                (let*((depiled (cddr pile))
                      (arg1 (list->number (cadr pile)))
                      (arg2 (list->number (car pile)))
                      (result (number->list (func arg1 arg2))))
                    (cons
                        (cons result depiled)
                        (cdr state)))
                (raise (string-append
                    "Not enough arguments for \""
                    (list->string token)
                    "\" (need 2)"))))))

(define process-set
    (lambda (state token)
        (if (>= (length (car state)) 1)
            (let*((pile (car state))
                  (dict (cdr state))
                  (varname (cdr token))
                  (arg (car pile)))
                  
                  (cons pile (foldl
                    (lambda (newdict pair)
                        (if (and (not (null? pair)) (lst-eq? (car pair) varname));;if la paire est non-nulle et la variable est 
                            newdict
                            
                            (cons pair newdict))) ;; f
                    (list (list varname arg));; base
                    dict)));; lst
            (raise (string-append
                "Not enough arguments for assignation (need 1)")))))

(define process-ref
    (lambda (state token)
        (let ((pile (car state)) (dict (cdr state)))
            (find-cont
                (lambda (pair) (lst-eq? (car pair) token))
                dict
                (lambda (pair) (cons (cons (cadr pair) pile) dict))
                (lambda () (raise (string-append
                    "Variable \""
                    (list->string token)
                    "\" not bound")))))))

(define processors (list
    (list number? process-number)
    (list operator? process-operator)
    (list varassignation? process-set)
    (list varname? process-ref)))

(define process-token
    (lambda (state token) (find-cont
        (lambda (pair) ((car pair) token));; appliquer la première fonction sur token, la fonction de test
        processors
        (lambda (processor) ((cadr processor) state token))
        (lambda () (raise
            (string-append
                "Unknown token \""
                (list->string token)
                "\""))))))

(define traiter
    (lambda (expr dict)
        (if (null? expr)
            (cons '(#\0) dict)
            (with-exception-catcher
                (lambda (e) (cons (string->list e) dict)) ;; Procedure si on a une exception (catch)
                (lambda () ;; Try:
                    (let ((result
                            (foldl
                                process-token ;; f
                                (cons '() dict) ;; base
                                (tokenize expr)));; lst
                           )
                        (if (<= (length (car result)) 1)    
                            (cons (car (car result)) (cdr result))
                            (raise "Stack not empty")
                        )
                        ))))))

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
