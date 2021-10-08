/*  This file is part of the 'AutoScheme' project.
 *  Copyright 2021 Steven Wiley <s.wiley@katchitek.com> 
 *  SPDX-License-Identifier: BSD-2-Clause
 */
#include "autoscheme.h"
int LOAD_MODULE__auto_scheme_write(pointer environment);
int LOAD_MODULE__auto_scheme_write(pointer environment)
{
autoscheme_eval(cons(mk_symbol("define-library"),cons(cons(mk_symbol("auto"),cons(mk_symbol("scheme"),cons(mk_symbol("write"),NIL))),cons(cons(mk_symbol("import"),cons(cons(mk_symbol("only"),cons(cons(mk_symbol("auto"),cons(mk_symbol("scheme"),NIL)),cons(mk_symbol("display"),cons(mk_symbol("write"),cons(mk_symbol("begin"),NIL))))),NIL)),cons(cons(mk_symbol("export"),cons(mk_symbol("display"),cons(mk_symbol("write"),NIL))),NIL)))), environment);
return 0;
}
