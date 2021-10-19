/*  This file is part of the 'AutoScheme' project.
 *  Copyright 2021 Steven Wiley <s.wiley@katchitek.com> 
 *  SPDX-License-Identifier: BSD-2-Clause
 */
#include "autoscheme.h"
pointer LOAD_MODULE__auto_scheme_char(pointer environment);
pointer LOAD_MODULE__auto_scheme_char(pointer environment)
{
pointer return_value = T;
return_value = autoscheme_eval(cons(mk_symbol("define-library"),cons(cons(mk_symbol("auto"),cons(mk_symbol("scheme"),cons(mk_symbol("car"),NIL))),cons(cons(mk_symbol("export"),cons(mk_symbol("char-alphabetic?"),cons(mk_symbol("char-ci<=?"),cons(mk_symbol("char-ci<?"),cons(mk_symbol("char-ci=?"),cons(mk_symbol("char-ci>=?"),cons(mk_symbol("char-ci>?"),cons(mk_symbol("char-downcase"),cons(mk_symbol("char-lower-case?"),cons(mk_symbol("char-numeric?"),cons(mk_symbol("char-upcase"),cons(mk_symbol("char-upper-case?"),cons(mk_symbol("char-whitespace?"),cons(mk_symbol("string-ci<=?"),cons(mk_symbol("string-ci<?"),cons(mk_symbol("string-ci=?"),cons(mk_symbol("string-ci>=?"),cons(mk_symbol("string-ci>?"),NIL)))))))))))))))))),cons(cons(mk_symbol("begin"),cons(cons(mk_syntax(OP_DEF0,"define"),cons(mk_symbol("char-alphabetic?"),cons(mk_proc(OP_CHARAP,&NIL),NIL))),cons(cons(mk_syntax(OP_DEF0,"define"),cons(mk_symbol("char-ci<=?"),cons(mk_proc(OP_CHARCILEQ,&NIL),NIL))),cons(cons(mk_syntax(OP_DEF0,"define"),cons(mk_symbol("char-ci<?"),cons(mk_proc(OP_CHARCILSS,&NIL),NIL))),cons(cons(mk_syntax(OP_DEF0,"define"),cons(mk_symbol("char-ci=?"),cons(mk_proc(OP_STRCIGEQ,&NIL),NIL))),cons(cons(mk_syntax(OP_DEF0,"define"),cons(mk_symbol("char-ci>=?"),cons(mk_proc(OP_CHARCIGEQ,&NIL),NIL))),cons(cons(mk_syntax(OP_DEF0,"define"),cons(mk_symbol("char-ci>?"),cons(mk_proc(OP_CHARCIGTR,&NIL),NIL))),cons(cons(mk_syntax(OP_DEF0,"define"),cons(mk_symbol("char-downcase"),cons(mk_proc(OP_CHARDNCASE,&NIL),NIL))),cons(cons(mk_syntax(OP_DEF0,"define"),cons(mk_symbol("char-lower-case?"),cons(mk_proc(OP_CHARLP,&NIL),NIL))),cons(cons(mk_syntax(OP_DEF0,"define"),cons(mk_symbol("char-numeric?"),cons(mk_proc(OP_CHARNP,&NIL),NIL))),cons(cons(mk_syntax(OP_DEF0,"define"),cons(mk_symbol("char-upper-case?"),cons(mk_proc(OP_CHARUP,&NIL),NIL))),cons(cons(mk_syntax(OP_DEF0,"define"),cons(mk_symbol("char-upcase"),cons(mk_proc(OP_CHARUPCASE,&NIL),NIL))),cons(cons(mk_syntax(OP_DEF0,"define"),cons(mk_symbol("char-whitespace?"),cons(mk_proc(OP_CHARWP,&NIL),NIL))),cons(cons(mk_syntax(OP_DEF0,"define"),cons(mk_symbol("string-ci<=?"),cons(mk_proc(OP_STRCILEQ,&NIL),NIL))),cons(cons(mk_syntax(OP_DEF0,"define"),cons(mk_symbol("string-ci<?"),cons(mk_proc(OP_STRCILSS,&NIL),NIL))),cons(cons(mk_syntax(OP_DEF0,"define"),cons(mk_symbol("string-ci=?"),cons(mk_proc(OP_STRCIEQU,&NIL),NIL))),cons(cons(mk_syntax(OP_DEF0,"define"),cons(mk_symbol("string-ci>=?"),cons(mk_proc(OP_STRCIGEQ,&NIL),NIL))),cons(cons(mk_syntax(OP_DEF0,"define"),cons(mk_symbol("string-ci>?"),cons(mk_proc(OP_STRCIGTR,&NIL),NIL))),NIL)))))))))))))))))),NIL)))), environment);
return return_value;
}