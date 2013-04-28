#!/usr/bin/env gosh

(use gauche.process)
(use gauche.sequence)
(use util.match)
(use sxml.ssax)
(use srfi-1)
(use slib)
(require 'format)

(define (command->sxml command)
  (ssax:xml->sxml
   (open-input-string
    (call-with-input-process command port->string))
   '()))

(define (get-all-jobs lis) (cdadr lis))
(define (get-one-job jobs) (cdar jobs))

(define (element-with-number lis)
  (map-with-index (lambda args args) lis))

(define (select-jobs conts all-jobs)  
  (define (conts->keys conts alis)
    (map (lambda (c) (if-let1 it (assoc c alis) (caadr it) it)) conts))
  (define (get-jobs keys all-jobs)
    (define (get-contents lis) (cdr lis))
    (map (lambda (job)
           (map (lambda (key) (assoc key (get-contents job))) keys)) all-jobs))
  (let1 keys (conts->keys conts (element-with-number (get-one-job all-jobs)))
    (get-jobs keys all-jobs)))

(define (main allargs)
  (define inp (command->sxml "qstat -x"))
  (define args (cdr allargs))
  (if (null? args)
      (for-each
       (lambda (l) (match l ((c (n . rest)) (format #t "~d ~s~%" c n))))
       (element-with-number (get-one-job (get-all-jobs inp))))
      (for-each
       (lambda (line)
         (format #t "~{~a:~a ~}~%"
                 (append-map (lambda (l) (if l
                                             (list (car l) (cdr l))
                                             '()))
                             line)))
       (select-jobs (map string->number args) (get-all-jobs inp)))))
