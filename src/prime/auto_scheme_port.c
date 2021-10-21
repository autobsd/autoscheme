/*  This file is part of the 'AutoScheme' project.
 *  Copyright 2021 Steven Wiley <s.wiley@katchitek.com> 
 *  SPDX-License-Identifier: BSD-2-Clause
 */
#include "autoscheme.h"
pointer LOAD_MODULE__auto_scheme_port(pointer environment);
pointer LOAD_MODULE__auto_scheme_port(pointer environment)
{
pointer return_value = T;
return_value = autoscheme_eval(cons(mk_symbol("define-library"),cons(cons(mk_symbol("auto"),cons(mk_symbol("scheme"),cons(mk_symbol("port"),NIL))),cons(cons(mk_symbol("export"),cons(mk_symbol("open-input-output-file"),cons(mk_symbol("open-input-output-string"),NIL))),cons(cons(mk_symbol("begin"),cons(cons(mk_syntax(LOC_DEF0,"define"),cons(mk_symbol("open-input-output-file"),cons(mk_operation(LOC_OPEN_INOUTFILE,&NIL),NIL))),cons(cons(mk_syntax(LOC_DEF0,"define"),cons(mk_symbol("open-input-output-string"),cons(mk_operation(LOC_OPEN_INOUTSTRING,&NIL),NIL))),NIL))),NIL)))), environment);
return return_value;
}
