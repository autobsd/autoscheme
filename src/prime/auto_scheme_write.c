/*  This file is part of the 'AutoScheme' project.
 *  Copyright 2021 Steven Wiley <s.wiley@katchitek.com> 
 *  SPDX-License-Identifier: BSD-2-Clause
 */
#include "autoscheme.h"
pointer LOAD_MODULE__auto_scheme_write(pointer environment);
pointer LOAD_MODULE__auto_scheme_write(pointer environment)
{
pointer return_value = T;
return_value = autoscheme_eval(cons(mk_symbol("define-library"),cons(cons(mk_symbol("auto"),cons(mk_symbol("scheme"),cons(mk_symbol("write"),NIL))),cons(cons(mk_symbol("export"),cons(mk_symbol("write-simple"),cons(cons(mk_symbol("rename"),cons(mk_symbol("write-simple"),cons(mk_symbol("write"),NIL))),cons(cons(mk_symbol("rename"),cons(mk_symbol("display-simple"),cons(mk_symbol("display"),NIL))),NIL)))),cons(cons(mk_symbol("begin"),cons(cons(mk_syntax(OP_DEF0,"define"),cons(mk_symbol("display-simple"),cons(mk_proc(OP_DISPLAY,&NIL),NIL))),cons(cons(mk_syntax(OP_DEF0,"define"),cons(mk_symbol("write-simple"),cons(mk_proc(OP_WRITE,&NIL),NIL))),NIL))),NIL)))), environment);
return return_value;
}
