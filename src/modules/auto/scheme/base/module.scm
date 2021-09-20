;;  This file is part of the 'AutoScheme' project.
;;  Copyright 2021 Steven Wiley <s.wiley@katchitek.com> 
;;  SPDX-License-Identifier: BSD-2-Clause

(define-library (auto scheme base)
  (import  (auto scheme))
		;; * + / < <= = => > >= - abs and append apply assoc assq assv begin boolean? caar cadr call-with-current-continuation call-with-values car case cdar cddr cdr ceiling char->integer char-ready? char<=? char<? char=? char>=? char>? char? close-input-port close-output-port close-port cond cons current-input-port current-output-port define define-syntax do dynamic-wind else eof-object? eq? equal? eqv? error even? exact? expt exact->inexact floor for-each gcd get-output-string if inexact->exact inexact? input-port? integer->char integer? lambda lcm length let let* let-syntax letrec letrec* letrec-syntax list list->string list->vector list-ref list-tail list? make-string make-vector map max member memq memv min modulo negative? newline not null? number->string number? odd? open-input-output-string open-input-string open-output-string or output-port? pair? peek-char port? positive? procedure? quasiquote quote quotient read-char real? remainder reverse round set! set-car! set-cdr! string string->list string->number string->symbol string-append string-copy string-fill! string-length string-ref string-set! string<=? string<? string=? string>=? string>? string? substring symbol->string symbol? syntax-rules truncate unless values vector vector->list vector-fill! vector-length vector-ref vector-set! vector? when write-char zero?
		;; ))

  (export       * + / < <= = => > >= - abs and append apply assoc assq assv begin boolean? caar cadr call-with-current-continuation call-with-values car case cdar cddr cdr ceiling char->integer char-ready? char<=? char<? char=? char>=? char>? char? close-input-port close-output-port close-port cond cons current-input-port current-output-port define define-syntax do dynamic-wind else eof-object? eq? equal? eqv? error even? exact? expt exact->inexact floor for-each gcd get-output-string if inexact->exact inexact? input-port? integer->char integer? lambda lcm length let let* let-syntax letrec letrec* letrec-syntax list list->string list->vector list-ref list-tail list? make-string make-vector map max member memq memv min modulo negative? newline not null? number->string number? odd? open-input-output-string open-input-string open-output-string or output-port? pair? peek-char port? positive? procedure? quasiquote quote quotient read-char real? remainder reverse round set! set-car! set-cdr! string string->list string->number string->symbol string-append string-copy string-fill! string-length string-ref string-set! string<=? string<? string=? string>=? string>? string? substring symbol->string symbol? syntax-rules truncate unless values vector vector->list vector-fill! vector-length vector-ref vector-set! vector? when write-char zero?
		)
  )