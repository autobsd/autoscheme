/*  This file is part of the 'AutoScheme' project.
 *  Copyright 2021 Steven Wiley <s.wiley@katchitek.com> 
 *  SPDX-License-Identifier: BSD-2-Clause
 */
#include "autoscheme.h"
int LOAD_MODULE__auto_scheme_path(pointer environment);
int LOAD_MODULE__auto_scheme_path(pointer environment)
{
autoscheme_eval(cons(mk_symbol("define-library"),cons(cons(mk_symbol("auto"),cons(mk_symbol("scheme"),cons(mk_symbol("path"),NIL))),cons(cons(mk_symbol("import"),cons(cons(mk_symbol("auto"),cons(mk_symbol("scheme"),cons(mk_symbol("base"),NIL))),cons(cons(mk_symbol("auto"),cons(mk_symbol("scheme"),cons(mk_symbol("process"),cons(mk_symbol("context"),NIL)))),cons(cons(mk_symbol("auto"),cons(mk_symbol("scheme"),cons(mk_symbol("write"),NIL))),NIL)))),cons(cons(mk_symbol("export"),cons(mk_symbol("path-directory"),cons(mk_symbol("path-absolute?"),cons(mk_symbol("path-make-absolute"),NIL)))),cons(cons(mk_symbol("begin"),cons(cons(mk_symbol("define"),cons(mk_symbol("path-directory"),cons(cons(mk_symbol("lambda"),cons(cons(mk_symbol("path"),NIL),cons(cons(mk_symbol("let"),cons(cons(cons(mk_symbol("pos"),cons(mk_integer(-1),NIL)),NIL),cons(cons(mk_symbol("do"),cons(cons(cons(mk_symbol("len"),cons(cons(mk_symbol("string-length"),cons(mk_symbol("path"),NIL)),NIL)),cons(cons(mk_symbol("i"),cons(mk_integer(0),cons(cons(mk_symbol("+"),cons(mk_symbol("i"),cons(mk_integer(1),NIL))),NIL))),NIL)),cons(cons(cons(mk_symbol("="),cons(mk_symbol("i"),cons(mk_symbol("len"),NIL))),NIL),cons(cons(mk_symbol("if"),cons(cons(mk_symbol("char=?"),cons(cons(mk_symbol("string-ref"),cons(mk_symbol("path"),cons(mk_symbol("i"),NIL))),cons(mk_character(47),NIL))),cons(cons(mk_symbol("set!"),cons(mk_symbol("pos"),cons(mk_symbol("i"),NIL))),NIL))),NIL)))),cons(cons(mk_symbol("if"),cons(cons(mk_symbol("positive?"),cons(mk_symbol("pos"),NIL)),cons(cons(mk_symbol("substring"),cons(mk_symbol("path"),cons(mk_integer(0),cons(mk_symbol("pos"),NIL)))),NIL))),NIL)))),NIL))),NIL))),cons(cons(mk_symbol("define"),cons(mk_symbol("path-absolute?"),cons(cons(mk_symbol("lambda"),cons(cons(mk_symbol("path"),NIL),cons(cons(mk_symbol("char=?"),cons(cons(mk_symbol("string-ref"),cons(mk_symbol("path"),cons(mk_integer(0),NIL))),cons(mk_character(47),NIL))),NIL))),NIL))),cons(cons(mk_symbol("define"),cons(mk_symbol("path-make-absolute"),cons(cons(mk_symbol("lambda"),cons(cons(mk_symbol("path"),mk_symbol("rest")),cons(cons(mk_symbol("if"),cons(cons(mk_symbol("path-absolute?"),cons(mk_symbol("path"),NIL)),cons(mk_symbol("path"),cons(cons(mk_symbol("let"),cons(cons(cons(mk_symbol("parent-dir"),cons(cons(mk_symbol("if"),cons(cons(mk_symbol("pair?"),cons(mk_symbol("rest"),NIL)),cons(cons(mk_symbol("car"),cons(mk_symbol("rest"),NIL)),cons(cons(mk_symbol("current-directory"),NIL),NIL)))),NIL)),NIL),cons(cons(mk_symbol("string-append"),cons(mk_symbol("parent-dir"),cons(mk_string("/"),cons(mk_symbol("path"),NIL)))),NIL))),NIL)))),NIL))),NIL))),NIL)))),NIL))))), environment);
return 0;
}
