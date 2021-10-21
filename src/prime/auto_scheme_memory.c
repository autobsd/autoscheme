/*  This file is part of the 'AutoScheme' project.
 *  Copyright 2021 Steven Wiley <s.wiley@katchitek.com> 
 *  SPDX-License-Identifier: BSD-2-Clause
 */
#include "autoscheme.h"
pointer LOAD_MODULE__auto_scheme_memory(pointer environment);
pointer LOAD_MODULE__auto_scheme_memory(pointer environment)
{
pointer return_value = T;
return_value = autoscheme_eval(cons(mk_symbol("define-library"),cons(cons(mk_symbol("auto"),cons(mk_symbol("scheme"),cons(mk_symbol("memory"),NIL))),cons(cons(mk_symbol("export"),cons(cons(mk_symbol("rename"),cons(mk_symbol("gc"),cons(mk_symbol("collect-garbage"),NIL))),cons(mk_symbol("gc-verbose"),NIL))),cons(cons(mk_symbol("begin"),cons(cons(mk_syntax(LOC_DEF0,"define"),cons(mk_symbol("gc"),cons(mk_operation(LOC_GC,&NIL),NIL))),cons(cons(mk_syntax(LOC_DEF0,"define"),cons(mk_symbol("gc-verbose"),cons(mk_operation(LOC_GCVERB,&NIL),NIL))),NIL))),NIL)))), environment);
return return_value;
}
