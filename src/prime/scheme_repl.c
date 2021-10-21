/*  This file is part of the 'AutoScheme' project.
 *  Copyright 2021 Steven Wiley <s.wiley@katchitek.com> 
 *  SPDX-License-Identifier: BSD-2-Clause
 */
#include "autoscheme.h"
pointer LOAD_MODULE__scheme_repl(pointer environment);
pointer LOAD_MODULE__scheme_repl(pointer environment)
{
pointer return_value = T;
return_value = autoscheme_eval(cons(mk_symbol("define-library"),cons(cons(mk_symbol("scheme"),cons(mk_symbol("repl"),NIL)),cons(cons(mk_symbol("import"),cons(cons(mk_symbol("scheme"),cons(mk_symbol("eval"),NIL)),NIL)),cons(cons(mk_symbol("export"),cons(mk_symbol("interaction-environment"),NIL)),cons(cons(mk_symbol("begin"),cons(cons(mk_syntax(LOC_DEF0,"define"),cons(mk_symbol("interaction-environment"),cons(cons(mk_syntax(LOC_LAMBDA,"lambda"),cons(NIL,cons(cons(mk_symbol("environment"),cons(cons(mk_symbol("auto"),cons(mk_symbol("scheme"),cons(mk_symbol("base"),NIL))),NIL)),NIL))),NIL))),NIL)),NIL))))), environment);
return return_value;
}
