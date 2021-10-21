/*  This file is part of the 'AutoScheme' project.
 *  Copyright 2021 Steven Wiley <s.wiley@katchitek.com> 
 *  SPDX-License-Identifier: BSD-2-Clause
 */
#include "autoscheme.h"
pointer LOAD_MODULE__auto_scheme_closure(pointer environment);
pointer LOAD_MODULE__auto_scheme_closure(pointer environment)
{
pointer return_value = T;
return_value = autoscheme_eval(cons(mk_symbol("define-library"),cons(cons(mk_symbol("auto"),cons(mk_symbol("scheme"),cons(mk_symbol("closure"),NIL))),cons(cons(mk_symbol("export"),cons(mk_symbol("make-closure"),cons(mk_symbol("get-closure-code"),cons(mk_symbol("closure?"),NIL)))),cons(cons(mk_symbol("begin"),cons(cons(mk_syntax(LOC_DEF0,"define"),cons(mk_symbol("make-closure"),cons(mk_proc(LOC_MKCLOSURE,&NIL),NIL))),cons(cons(mk_syntax(LOC_DEF0,"define"),cons(mk_symbol("get-closure-code"),cons(mk_proc(LOC_GET_CLOSURE,&NIL),NIL))),cons(cons(mk_syntax(LOC_DEF0,"define"),cons(mk_symbol("closure?"),cons(mk_proc(LOC_CLOSUREP,&NIL),NIL))),NIL)))),NIL)))), environment);
return return_value;
}
