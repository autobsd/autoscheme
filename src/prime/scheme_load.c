/*  This file is part of the 'AutoScheme' project.
 *  Copyright 2021 Steven Wiley <s.wiley@katchitek.com> 
 *  SPDX-License-Identifier: BSD-2-Clause
 */
#include "autoscheme.h"
pointer LOAD_MODULE__scheme_load(pointer environment);
pointer LOAD_MODULE__scheme_load(pointer environment)
{
pointer return_value = T;
return_value = autoscheme_eval(cons(mk_symbol("define-library"),cons(cons(mk_symbol("scheme"),cons(mk_symbol("load"),NIL)),cons(cons(mk_symbol("import"),cons(cons(mk_symbol("auto"),cons(mk_symbol("scheme"),cons(mk_symbol("base"),NIL))),cons(cons(mk_symbol("auto"),cons(mk_symbol("scheme"),cons(mk_symbol("file"),NIL))),cons(cons(mk_symbol("scheme"),cons(mk_symbol("eval"),NIL)),cons(cons(mk_symbol("scheme"),cons(mk_symbol("read"),NIL)),NIL))))),cons(cons(mk_symbol("export"),cons(mk_symbol("load"),NIL)),cons(cons(mk_symbol("begin"),cons(cons(mk_symbol("define"),cons(mk_symbol("load"),cons(cons(mk_symbol("lambda"),cons(cons(mk_symbol("filename"),mk_symbol("rest")),cons(cons(mk_symbol("define"),cons(mk_symbol("load-environment"),cons(cons(mk_symbol("if"),cons(cons(mk_symbol("null?"),cons(mk_symbol("rest"),NIL)),cons(cons(mk_symbol("interaction-environment"),NIL),cons(cons(mk_symbol("car"),cons(mk_symbol("rest"),NIL)),NIL)))),NIL))),cons(cons(mk_symbol("with-input-from-file"),cons(mk_symbol("filename"),cons(cons(mk_symbol("lambda"),cons(NIL,cons(cons(mk_symbol("let"),cons(mk_symbol("load-expression"),cons(cons(cons(mk_symbol("expression"),cons(cons(mk_symbol("read"),NIL),NIL)),NIL),cons(cons(mk_symbol("cond"),cons(cons(cons(mk_symbol("not"),cons(cons(mk_symbol("eof-object?"),cons(mk_symbol("expression"),NIL)),NIL)),cons(cons(mk_symbol("eval"),cons(mk_symbol("expression"),cons(mk_symbol("load-environment"),NIL))),cons(cons(mk_symbol("load-expression"),cons(cons(mk_symbol("read"),NIL),NIL)),NIL))),NIL)),NIL)))),NIL))),NIL))),NIL)))),NIL))),NIL)),NIL))))), environment);
return return_value;
}
