/*  This file is part of the 'AutoScheme' project.
 *  Copyright 2021 Steven Wiley <s.wiley@katchitek.com> 
 *  SPDX-License-Identifier: BSD-2-Clause
 */
#include "autoscheme.h"
pointer LOAD_MODULE__auto_scheme_interpret(pointer environment);
pointer LOAD_MODULE__auto_scheme_interpret(pointer environment)
{
pointer return_value = T;
return_value = autoscheme_eval(cons(mk_symbol("define-library"),cons(cons(mk_symbol("auto"),cons(mk_symbol("scheme"),cons(mk_symbol("interpret"),NIL))),cons(cons(mk_symbol("import"),cons(cons(mk_symbol("auto"),cons(mk_symbol("scheme"),cons(mk_symbol("base"),NIL))),cons(cons(mk_symbol("scheme"),cons(mk_symbol("eval"),NIL)),cons(cons(mk_symbol("scheme"),cons(mk_symbol("load"),NIL)),cons(cons(mk_symbol("scheme"),cons(mk_symbol("process-context"),NIL)),NIL))))),cons(cons(mk_symbol("export"),cons(mk_symbol("interpret"),NIL)),cons(cons(mk_symbol("begin"),cons(cons(mk_symbol("define"),cons(mk_symbol("interpret"),cons(cons(mk_symbol("lambda"),cons(cons(mk_symbol("sources"),cons(mk_symbol("arguments"),NIL)),cons(cons(mk_symbol("define"),cons(mk_symbol("int-env"),cons(cons(mk_symbol("environment"),NIL),NIL))),cons(cons(mk_symbol("for-each"),cons(cons(mk_symbol("lambda"),cons(cons(mk_symbol("source"),NIL),cons(cons(mk_symbol("parameterize"),cons(cons(cons(mk_symbol("command-line"),cons(cons(mk_symbol("cons"),cons(mk_symbol("source"),cons(mk_symbol("arguments"),NIL))),NIL)),NIL),cons(cons(mk_symbol("load"),cons(mk_symbol("source"),cons(mk_symbol("int-env"),NIL))),NIL))),NIL))),cons(mk_symbol("sources"),NIL))),NIL)))),NIL))),NIL)),NIL))))), environment);
return return_value;
}
