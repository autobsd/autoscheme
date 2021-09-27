;;  This file is part of the 'AutoScheme' project.
;;  Copyright 2021 Steven Wiley <s.wiley@katchitek.com> 
;;  SPDX-License-Identifier: BSD-2-Clause

(define identifiers '( 

receive

atom?
environment?
defined?

define-macro
macro
macro-expand
macro?
gensym

make-closure
closure?
get-closure-code

set-output-port
set-input-port

eager
lazy
cons-stream
last-pair

quit

gc-verbose
gc
	
;; (auto scheme environment)
global-environment
current-environment
make-environment
environment-defined-symbols
environment-import!
environment-delete!
environment-rename
environment-prefix
environment-except
environment-only
environment-update!
environment-ref
environment-assoc
environment-undefine!
environment-define!

;; (auto scheme library)
environment-import-sets!
library-eval
import
define-library
	
;; (auto scheme base)
* + / < <= = => > >= - abs and append apply assoc assq assv begin boolean? caar cadr call-with-current-continuation call-with-values car case cdar cddr cdr ceiling char->integer char-ready? char<=? char<? char=? char>=? char>? char? close-input-port close-output-port close-port cond cons current-input-port current-output-port define define-syntax do dynamic-wind else eof-object? eq? equal? eqv? error even? exact? expt exact->inexact floor for-each gcd get-output-string if inexact->exact inexact? input-port? integer->char integer? lambda lcm length let let* let-syntax letrec letrec* letrec-syntax list list->string list->vector list-ref list-tail list? make-string make-vector map max member memq memv min modulo negative? newline not null? number->string number? odd? open-input-output-string open-input-string open-output-string or output-port? pair? peek-char port? positive? procedure? quasiquote quote quotient read-char real? remainder reverse round set! set-car! set-cdr! string string->list string->number string->symbol string-append string-copy string-fill! string-length string-ref string-set! string<=? string<? string=? string>=? string>? string? substring symbol->string symbol? syntax-rules truncate unless values vector vector->list vector-fill! vector-length vector-ref vector-set! vector? when write-char zero?

;; (auto scheme char)
char-alphabetic?
char-ci<=?
char-ci<?
char-ci=? 
char-ci>=?
char-ci>?
char-downcase
char-lower-case?
char-numeric?
char-upcase
char-upper-case?
char-whitespace?
string-ci<=?
string-ci<?
string-ci=?
string-ci>=?
string-ci>?

;; (auto scheme cxr)
caaaar
caaadr
caaar
caadar
caaddr
caadr
cadaar
cadadr
cadar
caddar
cadddr
caddr
cdaaar
cdaadr
cdaar
cdadar
cdaddr
cdadr
cddaar
cddadr
cddar
cdddar
cddddr
cdddr

;; (auto scheme eval)
eval

;; (auto scheme file)
call-with-output-file
call-with-input-file
open-input-file
open-input-output-file
open-output-file
with-input-from-file
with-output-to-file

;; (auto scheme inexact)
acos
asin
atan
cos
exp
log
sin
sqrt
tan

;; (auto scheme lazy)
delay
force

;; (auto scheme load)
load

;; (auto scheme read)
read

;; (auto scheme repl)
interaction-environment

;; (auto scheme write)
display
write

))
