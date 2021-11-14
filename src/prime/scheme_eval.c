/*  This file is part of the 'AutoScheme' project.
 *  Copyright 2021 Steven Wiley <s.wiley@katchitek.com> 
 *  SPDX-License-Identifier: BSD-2-Clause
 */
#include "autoscheme.h"
pointer LOAD_MODULE__scheme_eval(pointer environment);
foreign_function ff_load_modules;pointer LOAD_MODULE__scheme_eval(pointer environment)
{
pointer return_value = T;
autoscheme_eval(T, environment);
return_value = autoscheme_eval(cons(mk_symbol("define-library"),cons(cons(mk_symbol("scheme"),cons(mk_symbol("eval"),NIL)),cons(cons(mk_symbol("import"),cons(cons(mk_symbol("only"),cons(cons(mk_symbol("auto"),cons(mk_symbol("scheme"),cons(mk_symbol("base"),NIL))),cons(mk_symbol("define"),cons(mk_symbol("cons"),cons(mk_symbol("quasiquote"),cons(mk_symbol("quote"),cons(mk_symbol("append"),NIL))))))),NIL)),cons(cons(mk_symbol("export"),cons(mk_symbol("environment"),cons(mk_symbol("eval"),NIL))),cons(cons(mk_symbol("begin"),cons(cons(mk_symbol("define"),cons(mk_symbol("eval"),cons(mk_operation(LOC_PEVAL,&NIL),NIL))),cons(cons(mk_symbol("define"),cons(mk_symbol("define-macro"),cons(mk_syntax(LOC_DEFMACRO0,"define-macro"),NIL))),cons(cons(mk_symbol("define"),cons(mk_symbol("load-modules"),cons(mk_function(ff_load_modules,&NIL),NIL))),cons(cons(mk_symbol("define"),cons(mk_symbol("make-environment"),cons(mk_function(make_environment,&NIL),NIL))),cons(cons(mk_symbol("define-macro"),cons(cons(mk_symbol("environment"),mk_symbol("sets")),cons(cons(mk_symbol("define"),cons(mk_symbol("_environment"),cons(cons(mk_symbol("load-modules"),cons(cons(mk_symbol("make-environment"),NIL),NIL)),NIL))),cons(cons(mk_symbol("eval"),cons(cons(mk_symbol("quasiquote"),cons(cons(mk_symbol("import"),cons(cons(mk_symbol("unquote-splicing"),cons(mk_symbol("sets"),NIL)),NIL)),NIL)),cons(mk_symbol("_environment"),NIL))),cons(mk_symbol("_environment"),NIL))))),NIL)))))),NIL))))), environment);
return return_value;
}
