/*  This file is part of the 'AutoScheme' project.
 *  Copyright 2021 Steven Wiley <s.wiley@katchitek.com> 
 *  SPDX-License-Identifier: BSD-2-Clause
 */
#include "autoscheme.h"
int LOAD_MODULE__auto_scheme_read(void);
int LOAD_MODULE__auto_scheme_read()
{
scheme_eval(cons(mk_symbol("begin"),cons(cons(mk_symbol("begin"),cons(cons(mk_symbol("define-library"),cons(cons(mk_symbol("auto"),cons(mk_symbol("scheme"),cons(mk_symbol("read"),NIL))),cons(cons(mk_symbol("export"),cons(mk_symbol("read"),cons(mk_symbol("read-list"),NIL))),cons(cons(mk_symbol("import"),cons(cons(mk_symbol("only"),cons(cons(mk_symbol("auto"),cons(mk_symbol("scheme"),NIL)),cons(mk_symbol("read"),NIL))),cons(cons(mk_symbol("auto"),cons(mk_symbol("scheme"),cons(mk_symbol("base"),NIL))),NIL))),cons(cons(mk_symbol("begin"),cons(cons(mk_symbol("define"),cons(mk_symbol("read-list"),cons(cons(mk_symbol("lambda"),cons(mk_symbol("args"),cons(cons(mk_symbol("reverse"),cons(cons(mk_symbol("let"),cons(mk_symbol("read-expressions"),cons(cons(cons(mk_symbol("expressions"),cons(cons( mk_symbol("quote" ),cons(NIL,NIL)),NIL)),NIL),cons(cons(mk_symbol("let"),cons(cons(cons(mk_symbol("next-expression"),cons(cons(mk_symbol("apply"),cons(mk_symbol("read"),cons(mk_symbol("args"),NIL))),NIL)),NIL),cons(cons(mk_symbol("if"),cons(cons(mk_symbol("eof-object?"),cons(mk_symbol("next-expression"),NIL)),cons(mk_symbol("expressions"),cons(cons(mk_symbol("read-expressions"),cons(cons(mk_symbol("cons"),cons(mk_symbol("next-expression"),cons(mk_symbol("expressions"),NIL))),NIL)),NIL)))),NIL))),NIL)))),NIL)),NIL))),NIL))),NIL)),NIL))))),NIL)),NIL)));
return 0;
}
