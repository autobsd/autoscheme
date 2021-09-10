;;  This file is part of the 'AutoScheme' project.
;;  Copyright 2021 Steven Wiley <s.wiley@katchitek.com> 
;;  SPDX-License-Identifier: BSD-2-Clause

(set! (*s7* 'print-length) 1024)


(define-library (s7)

  (export hook-functions make-hook reader-cond cond-expand multiple-value-bind call-with-values make-polar make-rectangular require profile-in quasiquote tree-cyclic? tree-count tree-set-memq tree-memq tree-leaves s7-optimize abort exit emergency-exit gc type-of equivalent? equal? eqv? eq? aritable? arity setter dilambda funclet procedure-source help signature documentation values stacktrace error throw catch dynamic-unwind dynamic-wind map for-each apply eval-string eval autoload load call-with-exit call-with-current-continuation call/cc cyclic-sequences hash-code hash-table-entries hash-table-set! hash-table-ref weak-hash-table make-weak-hash-table make-hash-table hash-table byte-vector->string string->byte-vector byte-vector-set! byte-vector-ref make-byte-vector byte-vector int-vector-ref int-vector-set! make-int-vector int-vector float-vector-ref float-vector-set! make-float-vector float-vector subvector-vector subvector-position subvector vector make-vector vector-rank vector-dimensions vector-dimension vector-set! vector-ref vector->list vector-length vector-fill! list->vector vector-append append sort! reverse! reverse fill! copy length make-list list-tail list-set! list-ref member memv memq assoc assv assq cdddar cddadr cddddr cdaddr cddaar cdadar cdaadr cdaaar caddar cadadr cadddr caaddr cadaar caadar caaadr caaaar cddar cdadr cdddr caddr cdaar cadar caadr caaar cddr cdar cadr caar set-cdr! set-car! cdr car cons object->let format object->string string substring string-append string-upcase string-downcase string-copy string->list string-length list->string string-fill! string-ci>=? string-ci<=? string-ci>? string-ci<? string-ci=? char-ci>=? char-ci<=? char-ci>? char-ci<? char-ci=? string>=? string<=? string>? string<? string=? string-set! string-ref make-string string-position char-position char>=? char<=? char>? char<? char=? char-whitespace? char-numeric? char-alphabetic? char-lower-case? char-upper-case? integer->char char->integer char-downcase char-upcase string->number number->string random-state->list inexact? exact? exact->inexact inexact->exact integer-length integer-decode-float logbit? lognot logxor logior logand round truncate ceiling floor sqrt atanh acosh asinh atan acos asin tanh cosh sinh tan cos sin angle magnitude abs exp ash log expt random-state random rationalize lcm gcd >= <= > < = modulo remainder quotient max min / * - + complex nan? infinite? negative? positive? zero? odd? even? denominator numerator imag-part real-part file-mtime directory->list system getenv delete-file file-exists? directory? with-output-to-file with-output-to-string call-with-output-file call-with-output-string with-input-from-file with-input-from-string call-with-input-file call-with-input-string read read-string read-line write-byte read-byte write-string write-char peek-char read-char display write newline open-output-function open-input-function get-output-string open-output-string open-input-string open-output-file open-input-file flush-output-port close-output-port close-input-port char-ready? set-current-output-port set-current-input-port let->list set-current-error-port current-error-port current-output-port current-input-port port-closed? pair-filename pair-line-number port-filename port-line-number port-position port-file c-pointer->list c-pointer-weak2 c-pointer-weak1 c-pointer-type c-pointer-info c-pointer c-object-type defined? provide provided? iterator-at-end? iterator-sequence iterate make-iterator openlet coverlet owlet inlet cutlet varlet sublet funclet? curlet rootlet outlet keyword->symbol symbol->keyword string->keyword constant? immutable? immutable! symbol->dynamic-value symbol->value symbol string->symbol symbol->string symbol-table gensym bignum bignum? not goto? weak-hash-table? subvector? c-object? unspecified? undefined? null? sequence? proper-list? boolean? dilambda? procedure? continuation? hash-table? byte-vector? int-vector? float-vector? vector? pair? list? string? char? random-state? rational? complex? float? real? number? byte? integer? eof-object? output-port? input-port? c-pointer? macro? iterator? openlet? let? keyword? gensym? syntax? symbol? 

	  ;; *s7* *unbound-variable-hook* *missing-close-paren-hook* *load-hook* *autoload-hook* *error-hook* *read-error-hook* *rootlet-redefinition-hook* 
	  ;; unlet let-ref let-set! apply-values list-values pi

	  _begin
	  _list
	  _if
	  _cond
	  _else
	  _and
	  )

  (begin 

    (define _begin begin)
    (define _list list)
    (define _if if)
    (define _cond cond)
    (define _else else)
    (define _and and)

    (define s7_object->string object->string)

    (define object->string
      (lambda (obj)
	(let ((str (s7_object->string obj)))
	  (if (string? obj) 
	      (with-output-to-string (lambda ()
				       (do ((i 0 (+ i 1)))
					   ((= i (string-length str)) )      
					 (cond ((member (str i) `(,("\n" 0) #\newline))(display "\\n"))
					       (else (display (str i)))))))
	      str
	      ))))
    )
  )





(define root-identifiers '(
			   ;; *s7* *rootlet-redefinition-hook* *read-error-hook* *error-hook* *autoload-hook* *load-hook* *missing-close-paren-hook* *unbound-variable-hook* *#readers* *libraries* *autoload* *cload-directory* *load-path* *features* *function* *stderr* *stdout* *stdin* 

			   require hook-functions make-hook reader-cond cond-expand multiple-value-bind call-with-values make-polar make-rectangular pi profile-in quasiquote tree-cyclic? tree-count tree-set-memq tree-memq tree-leaves s7-optimize abort exit emergency-exit gc type-of equivalent? equal? eqv? eq? aritable? arity setter dilambda funclet procedure-source help signature documentation list-values apply-values values stacktrace error throw catch dynamic-unwind dynamic-wind map for-each eval-string eval autoload load call-with-exit call-with-current-continuation call/cc cyclic-sequences hash-code hash-table-entries hash-table-set! hash-table-ref weak-hash-table make-weak-hash-table make-hash-table hash-table byte-vector->string string->byte-vector byte-vector-set! byte-vector-ref make-byte-vector byte-vector int-vector-ref int-vector-set! make-int-vector int-vector float-vector-ref float-vector-set! make-float-vector float-vector subvector-vector subvector-position subvector vector make-vector vector-rank vector-dimensions vector-dimension vector-set! vector-ref vector->list vector-length vector-fill! list->vector vector-append append sort! reverse! reverse fill! copy length make-list list-tail list-set! list-ref member memv memq assoc assv assq cdddar cddadr cddddr cdaddr cddaar cdadar cdaadr cdaaar caddar cadadr cadddr caaddr cadaar caadar caaadr caaaar cddar cdadr cdddr caddr cdaar cadar caadr caaar cddr cdar cadr caar set-cdr! set-car! cdr car cons object->let format object->string string substring string-append string-upcase string-downcase string-copy string->list string-length list->string string-fill! string-ci>=? string-ci<=? string-ci>? string-ci<? string-ci=? char-ci>=? char-ci<=? char-ci>? char-ci<? char-ci=? string>=? string<=? string>? string<? string=? string-set! string-ref make-string string-position char-position char>=? char<=? char>? char<? char=? char-whitespace? char-numeric? char-alphabetic? char-lower-case? char-upper-case? integer->char char->integer char-downcase char-upcase string->number number->string random-state->list inexact? exact? exact->inexact inexact->exact integer-length integer-decode-float logbit? lognot logxor logior logand round truncate ceiling floor sqrt atanh acosh asinh atan acos asin tanh cosh sinh tan cos sin angle magnitude abs exp ash log expt random-state random rationalize lcm gcd >= <= > < = modulo remainder quotient max min / * - + complex nan? infinite? negative? positive? zero? odd? even? denominator numerator imag-part real-part file-mtime directory->list system getenv delete-file file-exists? directory? with-output-to-file with-output-to-string call-with-output-file call-with-output-string with-input-from-file with-input-from-string call-with-input-file call-with-input-string read read-string read-line write-byte read-byte write-string write-char peek-char read-char display write newline open-output-function open-input-function get-output-string open-output-string open-input-string open-output-file open-input-file flush-output-port close-output-port close-input-port char-ready? set-current-output-port set-current-input-port let->list set-current-error-port current-error-port current-output-port current-input-port port-closed? pair-filename pair-line-number port-filename port-line-number port-position port-file c-pointer->list c-pointer-weak2 c-pointer-weak1 c-pointer-type c-pointer-info c-pointer c-object-type defined? provide provided? iterator-at-end? iterator-sequence iterate make-iterator let-set! let-ref openlet coverlet owlet inlet cutlet varlet sublet funclet? unlet curlet rootlet outlet keyword->symbol symbol->keyword string->keyword constant? immutable? immutable! symbol->dynamic-value symbol->value symbol string->symbol symbol->string symbol-table gensym bignum bignum? not goto? weak-hash-table? subvector? c-object? unspecified? undefined? null? sequence? proper-list? boolean? dilambda? procedure? continuation? hash-table? byte-vector? int-vector? float-vector? vector? pair? list? string? char? random-state? rational? complex? float? real? number? byte? integer? eof-object? output-port? input-port? c-pointer? macro? iterator? openlet? let? keyword? gensym? syntax? symbol? 

				   ;; begin 
				   list if cond else and apply

				   ))

(define prime-identifiers '(list-values 
			    apply-values
rootlet
outlet
			    symbol
			    ;; begin
			    ))


;; (newline)
;; (write (environment-defined (current-environment)))(newline)
;; (newline)

(let ((cutlet cutlet)
      (member member)
      (rootlet rootlet)
      (write write)
      (newline newline)
      (curlet curlet)
      (outlet outlet)
      (for-each for-each)
      )
  (for-each (lambda (id)
	      (if (not (member id prime-identifiers))
		  (cutlet (rootlet) id)
		  ))
	    root-identifiers)

  (for-each (lambda (id)
	      (cutlet (outlet (outlet (current-environment))) id)
	      )
	    '(root-identifiers prime-identifiers))

  ;; (write (environment-defined (rootlet)))(newline) 
  ;; (newline)
  )

;; (import (only (s7) newline write))
;; (newline)
;; (write (environment-defined (current-environment)))(newline)
;; (newline)