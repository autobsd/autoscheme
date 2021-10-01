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
static pointer ff_load_modules( pointer args );
 /*  This file is part of the 'AutoScheme' project.
 *  Copyright 2021 Steven Wiley <s.wiley@katchitek.com> 
 *  SPDX-License-Identifier: BSD-2-Clause
 */
static pointer ff_load_modules( pointer args )
{
    printf( "inside ff_load_modules( pointer args )...<----- !!!!\n" );

    return T;
    (void )args;
}
 int LOAD_MODULE__auto_scheme_interpret(pointer environment)
{
autoscheme_eval(cons(mk_symbol("begin"),cons(cons(mk_symbol("begin"),cons(T,cons(T,cons(cons(mk_symbol("define-library"),cons(cons(mk_symbol("auto"),cons(mk_symbol("scheme"),cons(mk_symbol("interpret"),NIL))),cons(cons(mk_symbol("import"),cons(cons(mk_symbol("auto"),cons(mk_symbol("scheme"),cons(mk_symbol("base"),NIL))),cons(cons(mk_symbol("auto"),cons(mk_symbol("scheme"),cons(mk_symbol("write"),NIL))),cons(cons(mk_symbol("auto"),cons(mk_symbol("scheme"),cons(mk_symbol("environment"),NIL))),cons(cons(mk_symbol("auto"),cons(mk_symbol("scheme"),cons(mk_symbol("eval"),NIL))),NIL))))),cons(cons(mk_symbol("export"),cons(mk_symbol("interpret"),NIL)),cons(cons(mk_symbol("begin"),cons(cons(mk_foreign_func(ff_load_modules,&NIL),NIL),cons(cons(mk_symbol("define"),cons(mk_symbol("interpretation-environment"),cons(cons(mk_symbol("lambda"),cons(NIL,cons(cons(mk_symbol("let"),cons(cons(cons(mk_symbol("env"),cons(cons(mk_symbol("environment"),cons(cons(mk_symbol("only"),cons(cons(mk_symbol("auto"),cons(mk_symbol("scheme"),NIL)),cons(mk_symbol("import"),cons(mk_symbol("define-library"),NIL)))),NIL)),NIL)),NIL),cons(cons(mk_symbol("for-each"),cons(cons(mk_symbol("lambda"),cons(cons(mk_symbol("sym"),NIL),cons(cons(mk_symbol("let"),cons(cons(cons(mk_symbol("binding"),cons(cons(mk_symbol("environment-assoc"),cons(cons(mk_symbol("global-environment"),NIL),cons(mk_symbol("sym"),NIL))),NIL)),NIL),cons(cons(mk_symbol("if"),cons(cons(mk_symbol("environment?"),cons(cons(mk_symbol("cdr"),cons(mk_symbol("binding"),NIL)),NIL)),cons(cons(mk_symbol("environment-define!"),cons(mk_symbol("env"),cons(cons(mk_symbol("car"),cons(mk_symbol("binding"),NIL)),cons(cons(mk_symbol("cdr"),cons(mk_symbol("binding"),NIL)),NIL)))),NIL))),NIL))),NIL))),cons(cons(mk_symbol("environment-defined-symbols"),cons(cons(mk_symbol("global-environment"),NIL),NIL)),NIL))),cons(mk_symbol("env"),NIL)))),NIL))),NIL))),cons(cons(mk_symbol("define"),cons(mk_symbol("interpret"),cons(cons(mk_symbol("lambda"),cons(mk_symbol("sources"),cons(cons(mk_symbol("display"),cons(mk_string("sources: "),NIL)),cons(cons(mk_symbol("write"),cons(mk_symbol("sources"),NIL)),cons(cons(mk_symbol("newline"),NIL),cons(cons(mk_symbol("display"),cons(mk_string("interpretation symbols: "),NIL)),cons(cons(mk_symbol("write"),cons(cons(mk_symbol("environment-defined-symbols"),cons(cons(mk_symbol("interpretation-environment"),NIL),NIL)),NIL)),cons(cons(mk_symbol("newline"),NIL),NIL)))))))),NIL))),NIL)))),NIL))))),NIL)))),NIL)),environment);
return 0;
}
