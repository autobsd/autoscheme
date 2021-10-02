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
autoscheme_eval(cons(mk_symbol("begin"),cons(cons(mk_symbol("begin"),cons(T,cons(T,cons(cons(mk_symbol("define-library"),cons(cons(mk_symbol("auto"),cons(mk_symbol("scheme"),cons(mk_symbol("interpret"),NIL))),cons(cons(mk_symbol("import"),cons(cons(mk_symbol("auto"),cons(mk_symbol("scheme"),cons(mk_symbol("base"),NIL))),cons(cons(mk_symbol("auto"),cons(mk_symbol("scheme"),cons(mk_symbol("write"),NIL))),cons(cons(mk_symbol("auto"),cons(mk_symbol("scheme"),cons(mk_symbol("environment"),NIL))),cons(cons(mk_symbol("auto"),cons(mk_symbol("scheme"),cons(mk_symbol("eval"),NIL))),NIL))))),cons(cons(mk_symbol("export"),cons(mk_symbol("interpret"),NIL)),cons(cons(mk_symbol("begin"),cons(cons(mk_symbol("define"),cons(mk_symbol("interpret"),cons(cons(mk_symbol("lambda"),cons(mk_symbol("sources"),cons(cons(mk_symbol("display"),cons(mk_string("sources: "),NIL)),cons(cons(mk_symbol("write"),cons(mk_symbol("sources"),NIL)),cons(cons(mk_symbol("newline"),NIL),NIL))))),NIL))),NIL)),NIL))))),NIL)))),NIL)),environment);
return 0;
}
