/*  This file is part of the 'AutoScheme' project.
 *  Copyright 2021 Steven Wiley <s.wiley@katchitek.com> 
 *  SPDX-License-Identifier: BSD-2-Clause
 */
#include "autoscheme.h"
int LOAD_MODULE__auto_scheme_eval(pointer environment);
int LOAD_MODULE__auto_scheme_eval(pointer environment)
{
autoscheme_eval(cons(mk_symbol("begin"),cons(cons(mk_symbol("begin"),cons(cons(mk_symbol("define-library"),cons(cons(mk_symbol("auto"),cons(mk_symbol("scheme"),cons(mk_symbol("eval"),NIL))),cons(cons(mk_symbol("import"),cons(cons(mk_symbol("only"),cons(cons(mk_symbol("auto"),cons(mk_symbol("scheme"),NIL)),cons(mk_symbol("begin"),cons(mk_symbol("define-macro"),cons(mk_symbol("lambda"),cons(mk_symbol("eval"),cons(mk_symbol("cons"),cons(mk_symbol("apply"),cons(mk_symbol("make-environment"),cons(mk_symbol("environment-import-sets!"),cons(mk_symbol("global-environment"),NIL))))))))))),NIL)),cons(cons(mk_symbol("export"),cons(mk_symbol("environment"),cons(mk_symbol("eval"),NIL))),cons(cons(mk_symbol("begin"),cons(cons(mk_symbol("define-macro"),cons(cons(mk_symbol("environment"),mk_symbol("sets")),cons(cons(mk_symbol("apply"),cons(mk_symbol("environment-import-sets!"),cons(cons(mk_symbol("cons"),cons(cons(mk_symbol("make-environment"),NIL),cons(cons(mk_symbol("cons"),cons(cons(mk_symbol("expansion-environment"),NIL),cons(mk_symbol("sets"),NIL))),NIL))),NIL))),NIL))),NIL)),NIL))))),NIL)),NIL)),environment);
return 0;
}
