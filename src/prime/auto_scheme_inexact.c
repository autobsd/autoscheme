/*  This file is part of the 'AutoScheme' project.
 *  Copyright 2021 Steven Wiley <s.wiley@katchitek.com> 
 *  SPDX-License-Identifier: BSD-2-Clause
 */
#include "autoscheme.h"
pointer LOAD_MODULE__auto_scheme_inexact(pointer environment);
pointer LOAD_MODULE__auto_scheme_inexact(pointer environment)
{
pointer return_value = T;
return_value = autoscheme_eval(cons(mk_symbol("define-library"),cons(cons(mk_symbol("auto"),cons(mk_symbol("scheme"),cons(mk_symbol("inexact"),NIL))),cons(cons(mk_symbol("export"),cons(mk_symbol("acos"),cons(mk_symbol("asin"),cons(mk_symbol("atan"),cons(mk_symbol("cos"),cons(mk_symbol("exp"),cons(mk_symbol("log"),cons(mk_symbol("sin"),cons(mk_symbol("sqrt"),cons(mk_symbol("tan"),NIL)))))))))),cons(cons(mk_symbol("begin"),cons(cons(mk_syntax(LOC_DEF0,"define"),cons(mk_symbol("acos"),cons(mk_operation(LOC_ACOS,&NIL),NIL))),cons(cons(mk_syntax(LOC_DEF0,"define"),cons(mk_symbol("asin"),cons(mk_operation(LOC_ASIN,&NIL),NIL))),cons(cons(mk_syntax(LOC_DEF0,"define"),cons(mk_symbol("atan"),cons(mk_operation(LOC_ATAN,&NIL),NIL))),cons(cons(mk_syntax(LOC_DEF0,"define"),cons(mk_symbol("cos"),cons(mk_operation(LOC_COS,&NIL),NIL))),cons(cons(mk_syntax(LOC_DEF0,"define"),cons(mk_symbol("exp"),cons(mk_operation(LOC_EXP,&NIL),NIL))),cons(cons(mk_syntax(LOC_DEF0,"define"),cons(mk_symbol("log"),cons(mk_operation(LOC_LOG,&NIL),NIL))),cons(cons(mk_syntax(LOC_DEF0,"define"),cons(mk_symbol("sin"),cons(mk_operation(LOC_SIN,&NIL),NIL))),cons(cons(mk_syntax(LOC_DEF0,"define"),cons(mk_symbol("sqrt"),cons(mk_operation(LOC_SQRT,&NIL),NIL))),cons(cons(mk_syntax(LOC_DEF0,"define"),cons(mk_symbol("tan"),cons(mk_operation(LOC_TAN,&NIL),NIL))),NIL)))))))))),NIL)))), environment);
return return_value;
}
