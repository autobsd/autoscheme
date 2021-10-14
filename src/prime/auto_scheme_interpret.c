/*  This file is part of the 'AutoScheme' project.
 *  Copyright 2021 Steven Wiley <s.wiley@katchitek.com> 
 *  SPDX-License-Identifier: BSD-2-Clause
 */
#include "autoscheme.h"
pointer LOAD_MODULE__auto_scheme_interpret(pointer environment);
/*  This file is part of the 'AutoScheme' project.
 *  Copyright 2021 Steven Wiley <s.wiley@katchitek.com> 
 *  SPDX-License-Identifier: BSD-2-Clause
 */
pointer LOAD_MODULE__scheme( pointer environment );
pointer LOAD_MODULE__auto_scheme_base( pointer environment );
pointer LOAD_MODULE__auto_scheme_environment( pointer environment );
pointer LOAD_MODULE__auto_scheme_eval( pointer environment );
pointer LOAD_MODULE__auto_scheme_list( pointer environment );
pointer LOAD_MODULE__auto_scheme_write( pointer environment );
pointer LOAD_MODULE__scheme_cxr( pointer environment );
pointer LOAD_MODULE__auto_scheme_string( pointer environment );
pointer LOAD_MODULE__auto_scheme_args_fold( pointer environment );
pointer LOAD_MODULE__auto_scheme_args( pointer environment );
pointer LOAD_MODULE__auto_scheme_process_context( pointer environment );
pointer LOAD_MODULE__auto_scheme_path( pointer environment );
pointer LOAD_MODULE__auto_scheme_read( pointer environment );
pointer LOAD_MODULE__auto_scheme_file( pointer environment );
pointer LOAD_MODULE__auto_scheme_compile( pointer environment );
pointer LOAD_MODULE__auto_scheme_interpret( pointer environment );
pointer LOAD_MODULE__auto_scheme_library( pointer environment );

static pointer ff_load_modules( pointer args );
 /*  This file is part of the 'AutoScheme' project.
 *  Copyright 2021 Steven Wiley <s.wiley@katchitek.com> 
 *  SPDX-License-Identifier: BSD-2-Clause
 */
static pointer ff_load_modules( pointer args )
{
    pointer environment = car( args );

    LOAD_MODULE__scheme( environment );
    LOAD_MODULE__auto_scheme_base( environment );
    LOAD_MODULE__auto_scheme_environment( environment );
    LOAD_MODULE__auto_scheme_library( environment );
    LOAD_MODULE__auto_scheme_eval( environment );
    LOAD_MODULE__auto_scheme_list( environment );
    LOAD_MODULE__auto_scheme_write( environment );
    LOAD_MODULE__scheme_cxr( environment );
    LOAD_MODULE__auto_scheme_string( environment );
    LOAD_MODULE__auto_scheme_args_fold( environment );
    LOAD_MODULE__auto_scheme_args( environment );
    LOAD_MODULE__auto_scheme_process_context( environment );
    LOAD_MODULE__auto_scheme_path( environment );
    LOAD_MODULE__auto_scheme_read( environment );
    LOAD_MODULE__auto_scheme_file( environment );
    LOAD_MODULE__auto_scheme_compile( environment );
    LOAD_MODULE__auto_scheme_interpret( environment );

    return environment;
}
 pointer LOAD_MODULE__auto_scheme_interpret(pointer environment)
{
pointer return_value = T;
return_value = autoscheme_eval(T, environment);
return_value = autoscheme_eval(T, environment);
return_value = autoscheme_eval(cons(mk_symbol("define-library"),cons(cons(mk_symbol("auto"),cons(mk_symbol("scheme"),cons(mk_symbol("interpret"),NIL))),cons(cons(mk_symbol("import"),cons(cons(mk_symbol("auto"),cons(mk_symbol("scheme"),cons(mk_symbol("base"),NIL))),cons(cons(mk_symbol("auto"),cons(mk_symbol("scheme"),cons(mk_symbol("write"),NIL))),cons(cons(mk_symbol("auto"),cons(mk_symbol("scheme"),cons(mk_symbol("environment"),NIL))),cons(cons(mk_symbol("auto"),cons(mk_symbol("scheme"),cons(mk_symbol("eval"),NIL))),cons(cons(mk_symbol("auto"),cons(mk_symbol("scheme"),cons(mk_symbol("file"),NIL))),cons(cons(mk_symbol("auto"),cons(mk_symbol("scheme"),cons(mk_symbol("read"),NIL))),NIL))))))),cons(cons(mk_symbol("export"),cons(mk_symbol("interpret"),cons(mk_symbol("interpretation-environment"),NIL))),cons(cons(mk_symbol("begin"),cons(cons(mk_symbol("define"),cons(mk_symbol("interpretation-environment"),cons(cons(mk_symbol("lambda"),cons(NIL,cons(cons(mk_symbol("let"),cons(cons(cons(mk_symbol("load-modules"),cons(mk_foreign_func(ff_load_modules,&NIL),NIL)),cons(cons(mk_symbol("int-env"),cons(cons(mk_symbol("make-environment"),NIL),NIL)),NIL)),cons(cons(mk_symbol("load-modules"),cons(mk_symbol("int-env"),NIL)),NIL))),NIL))),NIL))),cons(cons(mk_symbol("define"),cons(mk_symbol("interpret"),cons(cons(mk_symbol("lambda"),cons(mk_symbol("sources"),cons(cons(mk_symbol("define"),cons(mk_symbol("int-env"),cons(cons(mk_symbol("interpretation-environment"),NIL),NIL))),cons(cons(mk_symbol("environment-delete!"),cons(mk_symbol("int-env"),cons(cons( mk_symbol("quote" ),cons(mk_symbol("let"),NIL)),NIL))),cons(cons(mk_symbol("environment-delete!"),cons(mk_symbol("int-env"),cons(cons( mk_symbol("quote" ),cons(mk_symbol("begin"),NIL)),NIL))),cons(cons(mk_symbol("for-each"),cons(cons(mk_symbol("lambda"),cons(cons(mk_symbol("source"),NIL),cons(cons(mk_symbol("with-input-from-file"),cons(mk_symbol("source"),cons(cons(mk_symbol("lambda"),cons(NIL,cons(cons(mk_symbol("let"),cons(mk_symbol("interpret-expression"),cons(cons(cons(mk_symbol("expression"),cons(cons(mk_symbol("read"),NIL),NIL)),NIL),cons(cons(mk_symbol("cond"),cons(cons(cons(mk_symbol("not"),cons(cons(mk_symbol("eof-object?"),cons(mk_symbol("expression"),NIL)),NIL)),cons(cons(mk_symbol("eval"),cons(mk_symbol("expression"),cons(mk_symbol("int-env"),NIL))),cons(cons(mk_symbol("interpret-expression"),cons(cons(mk_symbol("read"),NIL),NIL)),NIL))),NIL)),NIL)))),NIL))),NIL))),NIL))),cons(mk_symbol("sources"),NIL))),NIL)))))),NIL))),NIL))),NIL))))), environment);
return return_value;
}
