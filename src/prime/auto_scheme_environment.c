/*  This file is part of the 'AutoScheme' project.
 *  Copyright 2021 Steven Wiley <s.wiley@katchitek.com> 
 *  SPDX-License-Identifier: BSD-2-Clause
 */
#include "autoscheme.h"
int LOAD_MODULE__auto_scheme_environment(void);
int LOAD_MODULE__auto_scheme_environment()
{
scheme_eval(cons(mk_symbol("begin"),cons(cons(mk_symbol("begin"),cons(cons(mk_symbol("define-library"),cons(cons(mk_symbol("auto"),cons(mk_symbol("scheme"),cons(mk_symbol("environment"),NIL))),cons(cons(mk_symbol("import"),cons(cons(mk_symbol("only"),cons(cons(mk_symbol("auto"),cons(mk_symbol("scheme"),NIL)),cons(mk_symbol("global-environment"),cons(mk_symbol("current-environment"),cons(mk_symbol("make-environment"),cons(mk_symbol("environment-defined-symbols"),cons(mk_symbol("environment-import!"),cons(mk_symbol("environment-delete!"),cons(mk_symbol("environment-rename"),cons(mk_symbol("environment-prefix"),cons(mk_symbol("environment-except"),cons(mk_symbol("environment-only"),cons(mk_symbol("environment-update!"),cons(mk_symbol("environment-ref"),cons(mk_symbol("environment-assoc"),cons(mk_symbol("environment-undefine!"),cons(mk_symbol("environment-define!"),NIL))))))))))))))))),cons(cons(mk_symbol("only"),cons(cons(mk_symbol("auto"),cons(mk_symbol("scheme"),cons(mk_symbol("base"),NIL))),cons(mk_symbol("begin"),NIL))),NIL))),cons(cons(mk_symbol("export"),cons(mk_symbol("global-environment"),cons(mk_symbol("current-environment"),cons(mk_symbol("make-environment"),cons(mk_symbol("environment-defined-symbols"),cons(mk_symbol("environment-import!"),cons(mk_symbol("environment-delete!"),cons(mk_symbol("environment-rename"),cons(mk_symbol("environment-prefix"),cons(mk_symbol("environment-except"),cons(mk_symbol("environment-only"),cons(mk_symbol("environment-update!"),cons(mk_symbol("environment-ref"),cons(mk_symbol("environment-assoc"),cons(mk_symbol("environment-undefine!"),cons(mk_symbol("environment-define!"),NIL)))))))))))))))),NIL)))),NIL)),NIL)));
return 0;
}
