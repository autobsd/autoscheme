/*  This file is part of the 'AutoScheme' project.
 *  Copyright 2021 Steven Wiley <s.wiley@katchitek.com> 
 *  SPDX-License-Identifier: BSD-2-Clause
 */
#include "autoscheme.h"
pointer LOAD_MODULE__auto_scheme_file(pointer environment);
pointer LOAD_MODULE__auto_scheme_file(pointer environment)
{
pointer return_value = T;
return_value = autoscheme_eval(cons(mk_symbol("define-library"),cons(cons(mk_symbol("auto"),cons(mk_symbol("scheme"),cons(mk_symbol("file"),NIL))),cons(cons(mk_symbol("export"),cons(mk_symbol("call-with-input-file"),cons(mk_symbol("call-with-output-file"),cons(mk_symbol("open-input-file"),cons(mk_symbol("open-output-file"),cons(mk_symbol("with-input-from-file"),cons(mk_symbol("with-output-to-file"),NIL))))))),cons(cons(mk_symbol("begin"),cons(cons(mk_syntax(LOC_DEF0,"define"),cons(mk_symbol("call-with-input-file"),cons(mk_operation(LOC_CALL_INFILE0,&NIL),NIL))),cons(cons(mk_syntax(LOC_DEF0,"define"),cons(mk_symbol("call-with-output-file"),cons(mk_operation(LOC_CALL_OUTFILE0,&NIL),NIL))),cons(cons(mk_syntax(LOC_DEF0,"define"),cons(mk_symbol("open-input-file"),cons(mk_operation(LOC_OPEN_INFILE,&NIL),NIL))),cons(cons(mk_syntax(LOC_DEF0,"define"),cons(mk_symbol("open-output-file"),cons(mk_operation(LOC_OPEN_OUTFILE,&NIL),NIL))),cons(cons(mk_syntax(LOC_DEF0,"define"),cons(mk_symbol("with-input-from-file"),cons(mk_operation(LOC_WITH_INFILE0,&NIL),NIL))),cons(cons(mk_syntax(LOC_DEF0,"define"),cons(mk_symbol("with-output-to-file"),cons(mk_operation(LOC_WITH_OUTFILE0,&NIL),NIL))),NIL))))))),NIL)))), environment);
return return_value;
}
