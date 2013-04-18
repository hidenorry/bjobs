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

(define (show-element-with-number input)
  (define (get-one-job jobs) (cdar jobs))
  (map-with-index
   (lambda (c n) (list c n))
   (get-one-job (get-all-jobs input))))

(define (select-jobs cont input)
  (define (get-contents lis) (cdr lis))
  (define (get-jobs input)
    (map (lambda (in)
           (apply append (map (lambda (c) (ref (get-contents in) c)) cont))) input))
  (get-jobs input))

(define (main args)
  (define inp (command->sxml "qstat -x"))
  (if (null? (cdr args))
      (for-each
       (lambda (l) (match l ((c (n . rest)) (format #t "~d ~s~%" c n))))
       (show-element-with-number inp))
      (for-each (lambda (line) (format #t "~{~a:~a ~}~%" line))
                (select-jobs (map string->number (cdr args))
                             (get-all-jobs inp)))))
