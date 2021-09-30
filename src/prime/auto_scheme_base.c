/*  This file is part of the 'AutoScheme' project.
 *  Copyright 2021 Steven Wiley <s.wiley@katchitek.com> 
 *  SPDX-License-Identifier: BSD-2-Clause
 */
#include "autoscheme.h"
int LOAD_MODULE__auto_scheme_base(void);
int LOAD_MODULE__auto_scheme_base()
{
scheme_eval(cons(mk_symbol("begin"),cons(cons(mk_symbol("begin"),cons(cons(mk_symbol("define-library"),cons(cons(mk_symbol("auto"),cons(mk_symbol("scheme"),cons(mk_symbol("base"),NIL))),cons(cons(mk_symbol("import"),cons(cons(mk_symbol("auto"),cons(mk_symbol("scheme"),NIL)),NIL)),cons(cons(mk_symbol("export"),cons(mk_symbol("*"),cons(mk_symbol("+"),cons(mk_symbol("/"),cons(mk_symbol("<"),cons(mk_symbol("<="),cons(mk_symbol("="),cons(mk_symbol("=>"),cons(mk_symbol(">"),cons(mk_symbol(">="),cons(mk_symbol("-"),cons(mk_symbol("abs"),cons(mk_symbol("and"),cons(mk_symbol("append"),cons(mk_symbol("apply"),cons(mk_symbol("assoc"),cons(mk_symbol("assq"),cons(mk_symbol("assv"),cons(mk_symbol("begin"),cons(mk_symbol("boolean?"),cons(mk_symbol("caar"),cons(mk_symbol("cadr"),cons(mk_symbol("call-with-current-continuation"),cons(mk_symbol("call-with-values"),cons(mk_symbol("car"),cons(mk_symbol("case"),cons(mk_symbol("cdar"),cons(mk_symbol("cddr"),cons(mk_symbol("cdr"),cons(mk_symbol("ceiling"),cons(mk_symbol("char->integer"),cons(mk_symbol("char-ready?"),cons(mk_symbol("char<=?"),cons(mk_symbol("char<?"),cons(mk_symbol("char=?"),cons(mk_symbol("char>=?"),cons(mk_symbol("char>?"),cons(mk_symbol("char?"),cons(mk_symbol("close-input-port"),cons(mk_symbol("close-output-port"),cons(mk_symbol("close-port"),cons(mk_symbol("cond"),cons(mk_symbol("cons"),cons(mk_symbol("current-input-port"),cons(mk_symbol("current-output-port"),cons(mk_symbol("define"),cons(mk_symbol("define-syntax"),cons(mk_symbol("do"),cons(mk_symbol("dynamic-wind"),cons(mk_symbol("else"),cons(mk_symbol("eof-object?"),cons(mk_symbol("eq?"),cons(mk_symbol("equal?"),cons(mk_symbol("eqv?"),cons(mk_symbol("error"),cons(mk_symbol("even?"),cons(mk_symbol("exact?"),cons(mk_symbol("expt"),cons(mk_symbol("exact->inexact"),cons(mk_symbol("floor"),cons(mk_symbol("for-each"),cons(mk_symbol("gcd"),cons(mk_symbol("get-output-string"),cons(mk_symbol("if"),cons(mk_symbol("inexact->exact"),cons(mk_symbol("inexact?"),cons(mk_symbol("input-port?"),cons(mk_symbol("integer->char"),cons(mk_symbol("integer?"),cons(mk_symbol("lambda"),cons(mk_symbol("lcm"),cons(mk_symbol("length"),cons(mk_symbol("let"),cons(mk_symbol("let*"),cons(mk_symbol("let-syntax"),cons(mk_symbol("letrec"),cons(mk_symbol("letrec*"),cons(mk_symbol("letrec-syntax"),cons(mk_symbol("list"),cons(mk_symbol("list->string"),cons(mk_symbol("list->vector"),cons(mk_symbol("list-ref"),cons(mk_symbol("list-tail"),cons(mk_symbol("list?"),cons(mk_symbol("make-string"),cons(mk_symbol("make-vector"),cons(mk_symbol("map"),cons(mk_symbol("max"),cons(mk_symbol("member"),cons(mk_symbol("memq"),cons(mk_symbol("memv"),cons(mk_symbol("min"),cons(mk_symbol("modulo"),cons(mk_symbol("negative?"),cons(mk_symbol("newline"),cons(mk_symbol("not"),cons(mk_symbol("null?"),cons(mk_symbol("number->string"),cons(mk_symbol("number?"),cons(mk_symbol("odd?"),cons(mk_symbol("open-input-output-string"),cons(mk_symbol("open-input-string"),cons(mk_symbol("open-output-string"),cons(mk_symbol("or"),cons(mk_symbol("output-port?"),cons(mk_symbol("pair?"),cons(mk_symbol("peek-char"),cons(mk_symbol("port?"),cons(mk_symbol("positive?"),cons(mk_symbol("procedure?"),cons( mk_symbol("quasiquote" ),cons(mk_symbol("quote"),cons(mk_symbol("quotient"),cons(mk_symbol("read-char"),cons(mk_symbol("real?"),cons(mk_symbol("remainder"),cons(mk_symbol("reverse"),cons(mk_symbol("round"),cons(mk_symbol("set!"),cons(mk_symbol("set-car!"),cons(mk_symbol("set-cdr!"),cons(mk_symbol("string"),cons(mk_symbol("string->list"),cons(mk_symbol("string->number"),cons(mk_symbol("string->symbol"),cons(mk_symbol("string-append"),cons(mk_symbol("string-copy"),cons(mk_symbol("string-fill!"),cons(mk_symbol("string-length"),cons(mk_symbol("string-ref"),cons(mk_symbol("string-set!"),cons(mk_symbol("string<=?"),cons(mk_symbol("string<?"),cons(mk_symbol("string=?"),cons(mk_symbol("string>=?"),cons(mk_symbol("string>?"),cons(mk_symbol("string?"),cons(mk_symbol("substring"),cons(mk_symbol("symbol->string"),cons(mk_symbol("symbol?"),cons(mk_symbol("syntax-rules"),cons(mk_symbol("truncate"),cons(mk_symbol("unless"),cons(mk_symbol("values"),cons(mk_symbol("vector"),cons(mk_symbol("vector->list"),cons(mk_symbol("vector-fill!"),cons(mk_symbol("vector-length"),cons(mk_symbol("vector-ref"),cons(mk_symbol("vector-set!"),cons(mk_symbol("vector?"),cons(mk_symbol("when"),cons(mk_symbol("write-char"),cons(mk_symbol("zero?"),NIL)))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))),cons(cons(mk_symbol("export"),cons(cons(mk_symbol("rename"),cons(mk_symbol("call-with-current-continuation"),cons(mk_symbol("call/cc"),NIL))),NIL)),cons(cons(mk_symbol("export"),cons(cons(mk_symbol("rename"),cons(mk_symbol("auto-read-string"),cons(mk_symbol("read-string"),NIL))),NIL)),cons(cons(mk_symbol("begin"),cons(cons(mk_symbol("define"),cons(mk_symbol("read-string"),cons(cons(mk_symbol("lambda"),cons(cons(mk_symbol("k"),mk_symbol("rest")),cons(cons(mk_symbol("if"),cons(cons(mk_symbol("="),cons(mk_symbol("k"),cons(mk_integer(0),NIL))),cons(mk_string(""),cons(cons(mk_symbol("let"),cons(mk_symbol("read-chars"),cons(cons(cons(mk_symbol("s"),cons(cons(mk_symbol("make-string"),cons(mk_symbol("k"),NIL)),NIL)),cons(cons(mk_symbol("i"),cons(mk_integer(0),NIL)),cons(cons(mk_symbol("c"),cons(cons(mk_symbol("apply"),cons(mk_symbol("read-char"),cons(mk_symbol("rest"),NIL))),NIL)),NIL))),cons(cons(mk_symbol("cond"),cons(cons(cons(mk_symbol("and"),cons(cons(mk_symbol("eof-object?"),cons(mk_symbol("c"),NIL)),cons(cons(mk_symbol("="),cons(mk_symbol("i"),cons(mk_integer(0),NIL))),NIL))),cons(mk_symbol("c"),NIL)),cons(cons(cons(mk_symbol("eof-object?"),cons(mk_symbol("c"),NIL)),cons(cons(mk_symbol("substring"),cons(mk_symbol("s"),cons(mk_integer(0),cons(cons(mk_symbol("+"),cons(mk_symbol("i"),cons(mk_integer(1),NIL))),NIL)))),NIL)),cons(cons(cons(mk_symbol("<"),cons(mk_symbol("i"),cons(cons(mk_symbol("-"),cons(mk_symbol("k"),cons(mk_integer(1),NIL))),NIL))),cons(cons(mk_symbol("string-set!"),cons(mk_symbol("s"),cons(mk_symbol("i"),cons(mk_symbol("c"),NIL)))),cons(cons(mk_symbol("read-chars"),cons(mk_symbol("s"),cons(cons(mk_symbol("+"),cons(mk_symbol("i"),cons(mk_integer(1),NIL))),cons(cons(mk_symbol("apply"),cons(mk_symbol("read-char"),cons(mk_symbol("rest"),NIL))),NIL)))),NIL))),cons(cons(mk_symbol("else"),cons(cons(mk_symbol("string-set!"),cons(mk_symbol("s"),cons(mk_symbol("i"),cons(mk_symbol("c"),NIL)))),cons(cons(mk_symbol("substring"),cons(mk_symbol("s"),cons(mk_integer(0),cons(cons(mk_symbol("+"),cons(mk_symbol("i"),cons(mk_integer(1),NIL))),NIL)))),NIL))),NIL))))),NIL)))),NIL)))),NIL))),NIL))),cons(cons(mk_symbol("define"),cons(mk_symbol("auto-read-string"),cons(cons(mk_symbol("lambda"),cons(mk_symbol("args"),cons(cons(mk_symbol("let"),cons(cons(cons(mk_symbol("k"),cons(cons(mk_symbol("if"),cons(cons(mk_symbol("null?"),cons(mk_symbol("args"),NIL)),cons(F,cons(cons(mk_symbol("car"),cons(mk_symbol("args"),NIL)),NIL)))),NIL)),cons(cons(mk_symbol("rest"),cons(cons(mk_symbol("if"),cons(cons(mk_symbol("pair?"),cons(mk_symbol("args"),NIL)),cons(cons(mk_symbol("cdr"),cons(mk_symbol("args"),NIL)),cons(cons( mk_symbol("quote" ),cons(NIL,NIL)),NIL)))),NIL)),NIL)),cons(cons(mk_symbol("if"),cons(cons(mk_symbol("not"),cons(mk_symbol("k"),NIL)),cons(cons(mk_symbol("let"),cons(cons(cons(mk_symbol("s"),cons(cons(mk_symbol("apply"),cons(mk_symbol("read-string"),cons(cons(mk_symbol("cons"),cons(mk_integer(64),cons(mk_symbol("rest"),NIL))),NIL))),NIL)),NIL),cons(cons(mk_symbol("if"),cons(cons(mk_symbol("or"),cons(cons(mk_symbol("eof-object?"),cons(mk_symbol("s"),NIL)),cons(cons(mk_symbol("zero?"),cons(cons(mk_symbol("string-length"),cons(mk_symbol("s"),NIL)),NIL)),NIL))),cons(mk_string(""),cons(cons(mk_symbol("string-append"),cons(mk_symbol("s"),cons(cons(mk_symbol("apply"),cons(mk_symbol("auto-read-string"),cons(cons(mk_symbol("cons"),cons(F,cons(mk_symbol("rest"),NIL))),NIL))),NIL))),NIL)))),NIL))),cons(cons(mk_symbol("apply"),cons(mk_symbol("read-string"),cons(mk_symbol("args"),NIL))),NIL)))),NIL))),NIL))),NIL))),NIL))),NIL))))))),NIL)),NIL)));
return 0;
}