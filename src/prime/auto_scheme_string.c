/*  This file is part of the 'AutoScheme' project.
 *  Copyright 2021 Steven Wiley <s.wiley@katchitek.com> 
 *  SPDX-License-Identifier: BSD-2-Clause
 */
#include "autoscheme.h"
pointer LOAD_MODULE__auto_scheme_string(pointer environment);
pointer LOAD_MODULE__auto_scheme_string(pointer environment)
{
pointer return_value = T;
return_value = autoscheme_eval(cons(mk_symbol("define-library"),cons(cons(mk_symbol("auto"),cons(mk_symbol("scheme"),cons(mk_symbol("string"),NIL))),cons(cons(mk_symbol("import"),cons(cons(mk_symbol("auto"),cons(mk_symbol("scheme"),cons(mk_symbol("base"),NIL))),NIL)),cons(cons(mk_symbol("export"),cons(mk_symbol("string-join"),cons(mk_symbol("string-prefix?"),cons(mk_symbol("string-map"),cons(mk_symbol("string-tokenize"),NIL))))),cons(cons(mk_symbol("begin"),cons(cons(mk_symbol("define"),cons(mk_symbol("string-join"),cons(cons(mk_symbol("lambda"),cons(cons(mk_symbol("string-list"),mk_symbol("rest")),cons(cons(mk_symbol("let"),cons(cons(cons(mk_symbol("delimeter"),cons(cons(mk_symbol("if"),cons(cons(mk_symbol("pair?"),cons(mk_symbol("rest"),NIL)),cons(cons(mk_symbol("car"),cons(mk_symbol("rest"),NIL)),cons(mk_string(" "),NIL)))),NIL)),cons(cons(mk_symbol("grammar"),cons(cons(mk_symbol("if"),cons(cons(mk_symbol("and"),cons(cons(mk_symbol("pair?"),cons(mk_symbol("rest"),NIL)),cons(cons(mk_symbol("pair?"),cons(cons(mk_symbol("cdr"),cons(mk_symbol("rest"),NIL)),NIL)),NIL))),cons(cons(mk_symbol("cadr"),cons(mk_symbol("rest"),NIL)),cons(cons( mk_symbol("quote" ),cons(mk_symbol("infix"),NIL)),NIL)))),NIL)),NIL)),cons(cons(mk_symbol("if"),cons(cons(mk_symbol("and"),cons(cons(mk_symbol("equal?"),cons(mk_symbol("grammar"),cons(cons( mk_symbol("quote" ),cons(mk_symbol("strict-infix"),NIL)),NIL))),cons(cons(mk_symbol("null?"),cons(mk_symbol("string-list"),NIL)),NIL))),cons(cons(mk_symbol("error"),cons(mk_string("cannot join with 'strict-infix"),cons(mk_symbol("string-list"),NIL))),NIL))),cons(cons(mk_symbol("let"),cons(mk_symbol("join-strings"),cons(cons(cons(mk_symbol("remainder"),cons(mk_symbol("string-list"),NIL)),NIL),cons(cons(mk_symbol("cond"),cons(cons(cons(mk_symbol("null?"),cons(mk_symbol("remainder"),NIL)),cons(mk_string(""),NIL)),cons(cons(cons(mk_symbol("equal?"),cons(mk_symbol("grammar"),cons(cons( mk_symbol("quote" ),cons(mk_symbol("suffix"),NIL)),NIL))),cons(cons(mk_symbol("string-append"),cons(cons(mk_symbol("car"),cons(mk_symbol("remainder"),NIL)),cons(mk_symbol("delimeter"),cons(cons(mk_symbol("join-strings"),cons(cons(mk_symbol("cdr"),cons(mk_symbol("remainder"),NIL)),NIL)),NIL)))),NIL)),cons(cons(cons(mk_symbol("equal?"),cons(mk_symbol("grammar"),cons(cons( mk_symbol("quote" ),cons(mk_symbol("prefix"),NIL)),NIL))),cons(cons(mk_symbol("string-append"),cons(mk_symbol("delimeter"),cons(cons(mk_symbol("car"),cons(mk_symbol("remainder"),NIL)),cons(cons(mk_symbol("join-strings"),cons(cons(mk_symbol("cdr"),cons(mk_symbol("remainder"),NIL)),NIL)),NIL)))),NIL)),cons(cons(cons(mk_symbol("pair?"),cons(cons(mk_symbol("cdr"),cons(mk_symbol("remainder"),NIL)),NIL)),cons(cons(mk_symbol("string-append"),cons(cons(mk_symbol("car"),cons(mk_symbol("remainder"),NIL)),cons(mk_symbol("delimeter"),cons(cons(mk_symbol("join-strings"),cons(cons(mk_symbol("cdr"),cons(mk_symbol("remainder"),NIL)),NIL)),NIL)))),NIL)),cons(cons(mk_symbol("else"),cons(cons(mk_symbol("car"),cons(mk_symbol("remainder"),NIL)),NIL)),NIL)))))),NIL)))),NIL)))),NIL))),NIL))),cons(cons(mk_symbol("define"),cons(mk_symbol("string-prefix?"),cons(cons(mk_symbol("lambda"),cons(cons(mk_symbol("s1"),cons(mk_symbol("s2"),mk_symbol("rest"))),cons(cons(mk_symbol("let"),cons(cons(cons(mk_symbol("start1"),cons(cons(mk_symbol("or"),cons(cons(mk_symbol("and"),cons(cons(mk_symbol("pair?"),cons(mk_symbol("rest"),NIL)),cons(cons(mk_symbol("car"),cons(mk_symbol("rest"),NIL)),NIL))),cons(mk_integer(0),NIL))),NIL)),cons(cons(mk_symbol("end1"),cons(cons(mk_symbol("or"),cons(cons(mk_symbol("and"),cons(cons(mk_symbol("pair?"),cons(mk_symbol("rest"),NIL)),cons(cons(mk_symbol("pair?"),cons(cons(mk_symbol("cdr"),cons(mk_symbol("rest"),NIL)),NIL)),cons(cons(mk_symbol("cadr"),cons(mk_symbol("rest"),NIL)),NIL)))),cons(cons(mk_symbol("string-length"),cons(mk_symbol("s1"),NIL)),NIL))),NIL)),cons(cons(mk_symbol("start2"),cons(cons(mk_symbol("or"),cons(cons(mk_symbol("and"),cons(cons(mk_symbol("pair?"),cons(mk_symbol("rest"),NIL)),cons(cons(mk_symbol("pair?"),cons(cons(mk_symbol("cdr"),cons(mk_symbol("rest"),NIL)),NIL)),cons(cons(mk_symbol("pair?"),cons(cons(mk_symbol("cddr"),cons(mk_symbol("rest"),NIL)),NIL)),cons(cons(mk_symbol("caddr"),cons(mk_symbol("rest"),NIL)),NIL))))),cons(mk_integer(0),NIL))),NIL)),cons(cons(mk_symbol("end2"),cons(cons(mk_symbol("or"),cons(cons(mk_symbol("and"),cons(cons(mk_symbol("pair?"),cons(mk_symbol("rest"),NIL)),cons(cons(mk_symbol("pair?"),cons(cons(mk_symbol("cdr"),cons(mk_symbol("rest"),NIL)),NIL)),cons(cons(mk_symbol("pair?"),cons(cons(mk_symbol("cddr"),cons(mk_symbol("rest"),NIL)),NIL)),cons(cons(mk_symbol("pair?"),cons(cons(mk_symbol("cdddr"),cons(mk_symbol("rest"),NIL)),NIL)),cons(cons(mk_symbol("cadddr"),cons(mk_symbol("rest"),NIL)),NIL)))))),cons(cons(mk_symbol("string-length"),cons(mk_symbol("s2"),NIL)),NIL))),NIL)),NIL)))),cons(cons(mk_symbol("equal?"),cons(cons(mk_symbol("substring"),cons(mk_symbol("s1"),cons(mk_symbol("start1"),cons(mk_symbol("end1"),NIL)))),cons(cons(mk_symbol("substring"),cons(mk_symbol("s2"),cons(mk_symbol("start2"),cons(mk_symbol("end2"),NIL)))),NIL))),NIL))),NIL))),NIL))),cons(cons(mk_symbol("define"),cons(mk_symbol("string-map"),cons(cons(mk_symbol("lambda"),cons(cons(mk_symbol("proc"),mk_symbol("strings")),cons(cons(mk_symbol("let"),cons(mk_symbol("sub-char"),cons(cons(cons(mk_symbol("pos"),cons(mk_integer(0),NIL)),NIL),cons(cons(mk_symbol("let*"),cons(cons(cons(mk_symbol("char-list"),cons(cons(mk_symbol("call/cc"),cons(cons(mk_symbol("lambda"),cons(cons(mk_symbol("return"),NIL),cons(cons(mk_symbol("map"),cons(cons(mk_symbol("lambda"),cons(cons(mk_symbol("s"),NIL),cons(cons(mk_symbol("if"),cons(cons(mk_symbol("<"),cons(mk_symbol("pos"),cons(cons(mk_symbol("string-length"),cons(mk_symbol("s"),NIL)),NIL))),cons(cons(mk_symbol("string-ref"),cons(mk_symbol("s"),cons(mk_symbol("pos"),NIL))),cons(cons(mk_symbol("return"),cons(F,NIL)),NIL)))),NIL))),cons(mk_symbol("strings"),NIL))),NIL))),NIL)),NIL)),cons(cons(mk_symbol("result"),cons(cons(mk_symbol("if"),cons(mk_symbol("char-list"),cons(cons(mk_symbol("apply"),cons(mk_symbol("proc"),cons(mk_symbol("char-list"),NIL))),cons(F,NIL)))),NIL)),cons(cons(mk_symbol("result-string"),cons(cons(mk_symbol("if"),cons(cons(mk_symbol("char?"),cons(mk_symbol("result"),NIL)),cons(cons(mk_symbol("string"),cons(mk_symbol("result"),NIL)),cons(mk_symbol("result"),NIL)))),NIL)),NIL))),cons(cons(mk_symbol("if"),cons(mk_symbol("char-list"),cons(cons(mk_symbol("string-append"),cons(mk_symbol("result-string"),cons(cons(mk_symbol("sub-char"),cons(cons(mk_symbol("+"),cons(mk_symbol("pos"),cons(mk_integer(1),NIL))),NIL)),NIL))),cons(mk_string(""),NIL)))),NIL))),NIL)))),NIL))),NIL))),cons(cons(mk_symbol("define"),cons(mk_symbol("string-tokenize"),cons(cons(mk_symbol("lambda"),cons(cons(mk_symbol("s"),NIL),cons(cons(mk_symbol("let"),cons(mk_symbol("get-tokens"),cons(cons(cons(mk_symbol("current"),cons(cons( mk_symbol("quote" ),cons(NIL,NIL)),NIL)),cons(cons(mk_symbol("remainder"),cons(cons(mk_symbol("string->list"),cons(mk_symbol("s"),NIL)),NIL)),NIL)),cons(cons(mk_symbol("cond"),cons(cons(cons(mk_symbol("null?"),cons(mk_symbol("remainder"),NIL)),cons(cons(mk_symbol("list"),cons(cons(mk_symbol("list->string"),cons(cons(mk_symbol("reverse"),cons(mk_symbol("current"),NIL)),NIL)),NIL)),NIL)),cons(cons(cons(mk_symbol("equal?"),cons(cons(mk_symbol("car"),cons(mk_symbol("remainder"),NIL)),cons(mk_character(32),NIL))),cons(cons(mk_symbol("cons"),cons(cons(mk_symbol("list->string"),cons(cons(mk_symbol("reverse"),cons(mk_symbol("current"),NIL)),NIL)),cons(cons(mk_symbol("get-tokens"),cons(cons( mk_symbol("quote" ),cons(NIL,NIL)),cons(cons(mk_symbol("cdr"),cons(mk_symbol("remainder"),NIL)),NIL))),NIL))),NIL)),cons(cons(mk_symbol("else"),cons(cons(mk_symbol("get-tokens"),cons(cons(mk_symbol("cons"),cons(cons(mk_symbol("car"),cons(mk_symbol("remainder"),NIL)),cons(mk_symbol("current"),NIL))),cons(cons(mk_symbol("cdr"),cons(mk_symbol("remainder"),NIL)),NIL))),NIL)),NIL)))),NIL)))),NIL))),NIL))),NIL))))),NIL))))), environment);
return return_value;
}
