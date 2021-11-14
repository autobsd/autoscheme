/*  This file is part of the 'AutoScheme' project.
 *  Copyright 2021 Steven Wiley <s.wiley@katchitek.com> 
 *  SPDX-License-Identifier: BSD-2-Clause
 */
#include "autoscheme.h"
pointer LOAD_MODULE__auto_scheme_sort(pointer environment);
pointer LOAD_MODULE__auto_scheme_sort(pointer environment)
{
pointer return_value = T;
return_value = autoscheme_eval(cons(mk_symbol("define-library"),cons(cons(mk_symbol("auto"),cons(mk_symbol("scheme"),cons(mk_symbol("sort"),NIL))),cons(cons(mk_symbol("import"),cons(cons(mk_symbol("auto"),cons(mk_symbol("scheme"),cons(mk_symbol("base"),NIL))),NIL)),cons(cons(mk_symbol("export"),cons(mk_symbol("list-sort"),NIL)),cons(cons(mk_symbol("begin"),cons(cons(mk_symbol("define"),cons(mk_symbol("list-sort"),cons(cons(mk_symbol("lambda"),cons(cons(mk_symbol("<"),cons(mk_symbol("unsorted-list"),NIL)),cons(cons(mk_symbol("let"),cons(cons(cons(mk_symbol("sorted-list"),cons(cons(mk_symbol("quote"),cons(NIL,NIL)),NIL)),NIL),cons(cons(mk_symbol("define"),cons(mk_symbol("add-to-list"),cons(cons(mk_symbol("lambda"),cons(cons(mk_symbol("item"),cons(mk_symbol("sorted-list"),NIL)),cons(cons(mk_symbol("cond"),cons(cons(cons(mk_symbol("null?"),cons(mk_symbol("sorted-list"),NIL)),cons(cons(mk_symbol("cons"),cons(mk_symbol("item"),cons(mk_symbol("sorted-list"),NIL))),NIL)),cons(cons(cons(mk_symbol("<"),cons(mk_symbol("item"),cons(cons(mk_symbol("car"),cons(mk_symbol("sorted-list"),NIL)),NIL))),cons(cons(mk_symbol("cons"),cons(mk_symbol("item"),cons(mk_symbol("sorted-list"),NIL))),NIL)),cons(cons(mk_symbol("else"),cons(cons(mk_symbol("cons"),cons(cons(mk_symbol("car"),cons(mk_symbol("sorted-list"),NIL)),cons(cons(mk_symbol("add-to-list"),cons(mk_symbol("item"),cons(cons(mk_symbol("cdr"),cons(mk_symbol("sorted-list"),NIL)),NIL))),NIL))),NIL)),NIL)))),NIL))),NIL))),cons(cons(mk_symbol("for-each"),cons(cons(mk_symbol("lambda"),cons(cons(mk_symbol("item"),NIL),cons(cons(mk_symbol("set!"),cons(mk_symbol("sorted-list"),cons(cons(mk_symbol("add-to-list"),cons(mk_symbol("item"),cons(mk_symbol("sorted-list"),NIL))),NIL))),NIL))),cons(mk_symbol("unsorted-list"),NIL))),cons(mk_symbol("sorted-list"),NIL))))),NIL))),NIL))),NIL)),NIL))))), environment);
return return_value;
}
