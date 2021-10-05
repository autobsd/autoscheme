/*  This file is part of the 'AutoScheme' project.
 *  Copyright 2021 Steven Wiley <s.wiley@katchitek.com> 
 *  SPDX-License-Identifier: BSD-2-Clause
 */
#include "autoscheme.h"
int LOAD_MODULE__auto_scheme_interpret(pointer environment);
/*  This file is part of the 'AutoScheme' project.
 *  Copyright 2021 Steven Wiley <s.wiley@katchitek.com> 
 *  SPDX-License-Identifier: BSD-2-Clause
 */
int LOAD_MODULE__auto_scheme( pointer environment );
int LOAD_MODULE__auto_scheme_base( pointer environment );
int LOAD_MODULE__auto_scheme_environment( pointer environment );
int LOAD_MODULE__auto_scheme_eval( pointer environment );
int LOAD_MODULE__auto_scheme_list( pointer environment );
int LOAD_MODULE__auto_scheme_write( pointer environment );
int LOAD_MODULE__auto_scheme_cxr( pointer environment );
int LOAD_MODULE__auto_scheme_string( pointer environment );
int LOAD_MODULE__auto_scheme_args_fold( pointer environment );
int LOAD_MODULE__auto_scheme_args( pointer environment );
int LOAD_MODULE__auto_scheme_process_context( pointer environment );
int LOAD_MODULE__auto_scheme_path( pointer environment );
int LOAD_MODULE__auto_scheme_read( pointer environment );
int LOAD_MODULE__auto_scheme_file( pointer environment );
int LOAD_MODULE__auto_scheme_compile( pointer environment );
int LOAD_MODULE__auto_scheme_interpret( pointer environment );

static pointer ff_load_modules( pointer args );
 /*  This file is part of the 'AutoScheme' project.
 *  Copyright 2021 Steven Wiley <s.wiley@katchitek.com> 
 *  SPDX-License-Identifier: BSD-2-Clause
 */
static pointer ff_load_modules( pointer args )
{
    pointer environment = car( args );

    LOAD_MODULE__auto_scheme( environment );
    LOAD_MODULE__auto_scheme_base( environment );
    LOAD_MODULE__auto_scheme_environment( environment );
    LOAD_MODULE__auto_scheme_eval( environment );
    LOAD_MODULE__auto_scheme_list( environment );
    LOAD_MODULE__auto_scheme_write( environment );
    LOAD_MODULE__auto_scheme_cxr( environment );
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
 int LOAD_MODULE__auto_scheme_interpret(pointer environment)
{
autoscheme_eval(cons(mk_symbol("begin"),cons(cons(mk_symbol("begin"),cons(T,cons(T,cons(cons(mk_symbol("define-library"),cons(cons(mk_symbol("auto"),cons(mk_symbol("scheme"),cons(mk_symbol("interpret"),NIL))),cons(cons(mk_symbol("import"),cons(cons(mk_symbol("auto"),cons(mk_symbol("scheme"),cons(mk_symbol("base"),NIL))),cons(cons(mk_symbol("auto"),cons(mk_symbol("scheme"),cons(mk_symbol("write"),NIL))),cons(cons(mk_symbol("auto"),cons(mk_symbol("scheme"),cons(mk_symbol("environment"),NIL))),cons(cons(mk_symbol("auto"),cons(mk_symbol("scheme"),cons(mk_symbol("eval"),NIL))),NIL))))),cons(cons(mk_symbol("export"),cons(mk_symbol("interpret"),cons(mk_symbol("interpretation-environment"),NIL))),cons(cons(mk_symbol("begin"),cons(cons(mk_symbol("define"),cons(mk_symbol("interpretation-environment"),cons(cons(mk_symbol("lambda"),cons(NIL,cons(cons(mk_symbol("let"),cons(cons(cons(mk_symbol("load-modules"),cons(mk_foreign_func(ff_load_modules,&NIL),NIL)),cons(cons(mk_symbol("int-env"),cons(cons(mk_symbol("make-environment"),NIL),NIL)),NIL)),cons(cons(mk_symbol("load-modules"),cons(mk_symbol("int-env"),NIL)),NIL))),NIL))),NIL))),cons(cons(mk_symbol("define"),cons(mk_symbol("interpret"),cons(cons(mk_symbol("lambda"),cons(mk_symbol("sources"),cons(cons(mk_symbol("display"),cons(mk_string("sources: "),NIL)),cons(cons(mk_symbol("write"),cons(mk_symbol("sources"),NIL)),cons(cons(mk_symbol("newline"),NIL),cons(cons(mk_symbol("define"),cons(mk_symbol("int-env"),cons(cons(mk_symbol("interpretation-environment"),NIL),NIL))),cons(cons(mk_symbol("display"),cons(mk_string("int-env: "),NIL)),cons(cons(mk_symbol("write"),cons(mk_symbol("int-env"),NIL)),cons(cons(mk_symbol("newline"),NIL),cons(cons(mk_symbol("display"),cons(mk_string("interpretation symbols: "),NIL)),cons(cons(mk_symbol("write"),cons(cons(mk_symbol("environment-defined-symbols"),cons(mk_symbol("int-env"),NIL)),NIL)),cons(cons(mk_symbol("newline"),NIL),cons(cons(mk_symbol("eval"),cons(cons( mk_symbol("quote" ),cons(cons(mk_symbol("begin"),cons(cons(mk_symbol("import"),cons(cons(mk_symbol("only"),cons(cons(mk_symbol("auto"),cons(mk_symbol("scheme"),cons(mk_symbol("base"),NIL))),cons(mk_symbol("newline"),NIL))),cons(cons(mk_symbol("auto"),cons(mk_symbol("scheme"),cons(mk_symbol("write"),NIL))),NIL))),cons(cons(mk_symbol("display"),cons(mk_string("hello world"),NIL)),cons(cons(mk_symbol("newline"),NIL),NIL)))),NIL)),cons(mk_symbol("int-env"),NIL))),NIL))))))))))))),NIL))),NIL))),NIL))))),NIL)))),NIL)),environment);
return 0;
}
