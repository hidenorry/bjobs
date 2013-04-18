#!/usr/bin/env gosh

(use gauche.process)
(use gauche.sequence)
(use util.match)
(use sxml.ssax)
(use slib)
(require 'format)

(define (command->sxml command)
  (ssax:xml->sxml
   (open-input-string
    (call-with-input-process command port->string))
   '()))

(define (get-all-jobs lis)
  (cdadr lis))
(define (get-one-job jobs) (cdar jobs))

(define (element-with-number lis)
  (map-with-index (lambda args args) lis))

(define (select-jobs cont all-jobs)
  (define alis (element-with-number (get-one-job all-jobs)))
  (define (get-contents lis) (cdr lis))
  (define (cont->key cont alis)
    (map (lambda (c) (if-let1 it (assoc c alis) (caadr it) it)) cont))
  (define (get-jobs all-jobs)
    (map (lambda (in) (map (lambda (k) (assoc k (get-contents in)))
                           (cont->key cont alis))) all-jobs))
  (get-jobs all-jobs))

(define (main args)
  (define inp (command->sxml "qstat -x"))
  (if (null? (cdr args))
      (for-each
       (lambda (l) (match l ((c (n . rest)) (format #t "~d ~s~%" c n))))
       (element-with-number (get-one-job (get-all-jobs inp))))
      (for-each
       (lambda (line) (format #t "~{~a:~a ~}~%" (apply append line)))
       (select-jobs (map string->number (cdr args)) (get-all-jobs inp)))))
