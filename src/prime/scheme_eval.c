/*  This file is part of the 'AutoScheme' project.
 *  Copyright 2021 Steven Wiley <s.wiley@katchitek.com> 
 *  SPDX-License-Identifier: BSD-2-Clause
 */
#include "autoscheme.h"
pointer LOAD_MODULE__scheme_eval(pointer environment);
/*  This file is part of the 'AutoScheme' project.
 *  Copyright 2021 Steven Wiley <s.wiley@katchitek.com> 
 *  SPDX-License-Identifier: BSD-2-Clause
 */
pointer LOAD_MODULE__scheme( pointer environment );
pointer LOAD_MODULE__auto_scheme_macro( pointer environment );
pointer LOAD_MODULE__auto_scheme_base( pointer environment );
pointer LOAD_MODULE__auto_scheme_environment( pointer environment );
pointer LOAD_MODULE__scheme_eval( pointer environment );
pointer LOAD_MODULE__scheme_repl( pointer environment );
pointer LOAD_MODULE__scheme_load( pointer environment );
pointer LOAD_MODULE__auto_scheme_list( pointer environment );
pointer LOAD_MODULE__auto_scheme_write( pointer environment );
pointer LOAD_MODULE__scheme_cxr( pointer environment );
pointer LOAD_MODULE__auto_scheme_string( pointer environment );
pointer LOAD_MODULE__auto_scheme_args_fold( pointer environment );
pointer LOAD_MODULE__auto_scheme_args( pointer environment );
pointer LOAD_MODULE__scheme_process_context( pointer environment );
pointer LOAD_MODULE__auto_scheme_directory( pointer environment );
pointer LOAD_MODULE__auto_scheme_path( pointer environment );
pointer LOAD_MODULE__auto_scheme_char( pointer environment );
pointer LOAD_MODULE__auto_scheme_inexact( pointer environment );
pointer LOAD_MODULE__scheme_read( pointer environment );
pointer LOAD_MODULE__auto_scheme_file( pointer environment );
pointer LOAD_MODULE__auto_scheme_port( pointer environment );
pointer LOAD_MODULE__auto_scheme_compile( pointer environment );
pointer LOAD_MODULE__auto_scheme_interpret( pointer environment );

static pointer ff_load_modules( pointer args );
 /*  This file is part of the 'AutoScheme' project.
 *  Copyright 2021 Steven Wiley <s.wiley@katchitek.com> 
 *  SPDX-License-Identifier: BSD-2-Clause
 */
static pointer ff_load_modules( pointer args )
{
    pointer environment = car( args );

    LOAD_MODULE__scheme( environment );
    LOAD_MODULE__auto_scheme_file( environment );
    LOAD_MODULE__auto_scheme_port( environment );
    LOAD_MODULE__auto_scheme_macro( environment );
    LOAD_MODULE__auto_scheme_base( environment );
    LOAD_MODULE__scheme_process_context( environment );
    LOAD_MODULE__scheme_read( environment );
    LOAD_MODULE__auto_scheme_environment( environment );
    LOAD_MODULE__scheme_eval( environment );
    LOAD_MODULE__scheme_repl( environment );
    LOAD_MODULE__scheme_load( environment );
    LOAD_MODULE__auto_scheme_list( environment );
    LOAD_MODULE__auto_scheme_write( environment );
    LOAD_MODULE__scheme_cxr( environment );
    LOAD_MODULE__auto_scheme_string( environment );
    LOAD_MODULE__auto_scheme_args_fold( environment );
    LOAD_MODULE__auto_scheme_args( environment );
    LOAD_MODULE__auto_scheme_directory( environment );
    LOAD_MODULE__auto_scheme_path( environment );
    LOAD_MODULE__auto_scheme_char( environment );
    LOAD_MODULE__auto_scheme_inexact( environment );
    LOAD_MODULE__auto_scheme_compile( environment );
    LOAD_MODULE__auto_scheme_interpret( environment );

    return environment;
}
 pointer LOAD_MODULE__scheme_eval(pointer environment)
{
pointer return_value = T;
return_value = autoscheme_eval(T, environment);
return_value = autoscheme_eval(T, environment);
return_value = autoscheme_eval(cons(mk_symbol("define-library"),cons(cons(mk_symbol("scheme"),cons(mk_symbol("eval"),NIL)),cons(cons(mk_symbol("import"),cons(cons(mk_symbol("only"),cons(cons(mk_symbol("auto"),cons(mk_symbol("scheme"),cons(mk_symbol("base"),NIL))),cons(mk_symbol("define"),cons(mk_symbol("cons"),cons( mk_symbol("quasiquote" ),cons(mk_symbol("quote"),cons(mk_symbol("append"),NIL))))))),NIL)),cons(cons(mk_symbol("export"),cons(mk_symbol("environment"),cons(mk_symbol("eval"),NIL))),cons(cons(mk_symbol("begin"),cons(cons(mk_symbol("define"),cons(mk_symbol("eval"),cons(mk_proc(OP_PEVAL,&NIL),NIL))),cons(cons(mk_symbol("define"),cons(mk_symbol("define-macro"),cons(mk_syntax(OP_DEFMACRO0,"define-macro"),NIL))),cons(cons(mk_symbol("define"),cons(mk_symbol("load-modules"),cons(mk_foreign_func(ff_load_modules,&NIL),NIL))),cons(cons(mk_symbol("define"),cons(mk_symbol("make-environment"),cons(mk_foreign_func(make_environment,&NIL),NIL))),cons(cons(mk_symbol("define-macro"),cons(cons(mk_symbol("environment"),mk_symbol("sets")),cons(cons(mk_symbol("define"),cons(mk_symbol("_environment"),cons(cons(mk_symbol("load-modules"),cons(cons(mk_symbol("make-environment"),NIL),NIL)),NIL))),cons(cons(mk_symbol("eval"),cons(cons( mk_symbol("quasiquote" ),cons(cons(mk_symbol("import"),cons(cons( mk_symbol("unquote-splicing" ),cons(mk_symbol("sets"),NIL)),NIL)),NIL)),cons(mk_symbol("_environment"),NIL))),cons(mk_symbol("_environment"),NIL))))),NIL)))))),NIL))))), environment);
return return_value;
}
