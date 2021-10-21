/*  This file is part of the 'AutoScheme' project.
 *  Copyright 2021 Steven Wiley <s.wiley@katchitek.com> 
 *  SPDX-License-Identifier: BSD-2-Clause
 */
#include "autoscheme.h"
pointer LOAD_MODULE__auto_scheme_lazy(pointer environment);
pointer LOAD_MODULE__auto_scheme_lazy(pointer environment)
{
pointer return_value = T;
return_value = autoscheme_eval(cons(mk_symbol("define-library"),cons(cons(mk_symbol("auto"),cons(mk_symbol("scheme"),cons(mk_symbol("lazy"),NIL))),cons(cons(mk_symbol("export"),cons(mk_symbol("delay"),cons(mk_symbol("force"),NIL))),cons(cons(mk_symbol("begin"),cons(cons(mk_syntax(LOC_DEF0,"define"),cons(mk_symbol("delay"),cons(mk_proc(LOC_DELAY,&NIL),NIL))),cons(cons(mk_syntax(LOC_DEF0,"define"),cons(mk_symbol("force"),cons(mk_proc(LOC_FORCE,&NIL),NIL))),NIL))),NIL)))), environment);
return return_value;
}
