/*  This file is part of the 'AutoScheme' project.
 *  Copyright 2021 Steven Wiley <s.wiley@katchitek.com> 
 *  SPDX-License-Identifier: BSD-2-Clause
 */
#include "autoscheme.h"
pointer LOAD_MODULE__auto_scheme(pointer environment);
pointer LOAD_MODULE__auto_scheme_library( pointer environment );
pointer INITIALIZE_LIBRARY__auto_scheme_environment( pointer environment );
pointer LOAD_MODULE__auto_scheme(pointer environment)
{
pointer return_value = T;
pointer module_environment = environment;
pointer lib = LOAD_MODULE__auto_scheme_library( environment );
INITIALIZE_LIBRARY__auto_scheme_environment( lib );
environment = lib;
return_value = autoscheme_eval(T, environment);
return_value = autoscheme_eval(T, environment);
return_value = autoscheme_eval(cons(mk_symbol("environment-import!"),cons(module_environment,cons(cons(mk_symbol("environment-only"),cons(cons(mk_symbol("current-environment"),NIL),cons(cons(mk_proc(OP_STR2SYM,&NIL),cons(mk_string("define-library"),NIL)),cons(cons(mk_proc(OP_STR2SYM,&NIL),cons(mk_string("import"),NIL)),NIL)))),NIL))), environment);
return_value = autoscheme_eval(cons(mk_symbol("environment-delete!"),cons(module_environment,cons(cons(mk_proc(OP_STR2SYM,&NIL),cons(mk_string("(auto scheme library)"),NIL)),NIL))), environment);
return return_value;
}
