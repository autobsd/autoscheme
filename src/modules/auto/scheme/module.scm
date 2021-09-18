;;  This file is part of the 'AutoScheme' project.
;;  Copyright 2021 Steven Wiley <s.wiley@katchitek.com> 
;;  SPDX-License-Identifier: BSD-2-Clause

(define auto_scheme-identifiers 
  '( 
    ;; begin

    define-macro eval quit caddr newline display write define for-each reverse eq? set! symbol? if and list? = length cadr cddr lambda let letrec quote gensym atom? macro-expand macro? closure? get-closure-code make-closure defined? assoc assv assq member memv memq set-output-port set-input-port peek-char read-char interaction-environment close-port close-input-port open-input-output-string open-input-string open-input-output-file open-output-file open-input-file with-output-to-file with-input-from-file current-output-port current-input-port call-with-output-file call-with-input-file gc-verbose gc error load write-char char-ready? read min max >= <= > < even? odd? negative? positive? zero? eof-object? null? eqv? environment? vector? output-port? input-port? port?  procedure? char-lower-case? char-upper-case? char-whitespace? char-numeric? char-alphabetic? char-ci>=? char-ci<=? char-ci>? char-ci<? char-ci=? char>=? char<=? char>? char<? char=? char? inexact? exact? real? integer? string? number? symbol->string boolean? vector-fill! list->vector vector->list vector-set! vector-ref vector-length make-vector vector string-fill! string-copy list->string string->list string-append substring string-ci>=? string-ci<=? string-ci>? string-ci<? string-ci=? string>=? string<=? string>? string<? string=? string-set! string-ref string-length string make-string char-downcase char-upcase integer->char char->integer string->number number->string inexact->exact exact->inexact expt sqrt atan acos asin tan cos sin log exp round truncate ceiling floor lcm gcd modulo remainder quotient abs / * - + last-pair list-ref list-tail list cddddr cdddar cddadr cddaar cdaddr cdadar cdaadr cdaaar cadddr caddar cadadr cadaar caaddr caadar caaadr caaaar cdddr cddar cdadr cdaar cadar caadr caaar cdar caar set-cdr! set-car! force eager dynamic-wind call-with-values values call-with-current-continuation  receive letrec-syntax let-syntax define-syntax syntax-rules unless when case macro cons-stream or lazy delay => do letrec* let* cons not cdr car map append equal? pair? else cond quasiquote apply close-output-port get-output-string open-output-string string->symbol ))


(environment-update! (current-environment) (string->symbol "(auto scheme)") (apply environment-only (cons (current-environment)
													  (append auto_scheme-identifiers
														  '(
														    begin
														     ) 
														  )
													  )))


(define auto_scheme_environment-identifiers  (environment-defined-symbols (environment-ref (current-environment) (string->symbol "(auto scheme environment)"))))

(define auto_scheme_library-identifiers '(make-library environment-import-sets!))

(define all-identifiers (append auto_scheme_environment-identifiers (append auto_scheme_library-identifiers auto_scheme-identifiers)))

(apply environment-undefine! (cons (current-environment) (append all-identifiers '(auto_scheme_environment-identifiers
										   auto_scheme_library-identifiers
										   auto_scheme-identifiers
										   all-identifiers))))


;; (import (only (auto scheme) 
;; 	      display 
;; 	      newline 
;; 	      quit
;; 	      )
;; 	(only (auto scheme environment)
;; 	      current-environment
;; 	      environment-defined-symbols
;; 	      )
;; 	)

;; (display (environment-defined-symbols (current-environment)))(newline)


