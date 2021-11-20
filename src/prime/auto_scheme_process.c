/*  This file is part of the 'AutoScheme' project.
 *  Copyright 2021 Steven Wiley <s.wiley@katchitek.com> 
 *  SPDX-License-Identifier: BSD-2-Clause
 */
#include "autoscheme.h"
pointer LOAD_MODULE__auto_scheme_process(pointer environment);
/*  This file is part of the 'AutoScheme' project.
 *  Copyright 2021 Steven Wiley <s.wiley@katchitek.com> 
 *  SPDX-License-Identifier: BSD-2-Clause
 */
#include <stdlib.h>

static pointer ff_process_command( pointer args );

 /*  This file is part of the 'AutoScheme' project.
 *  Copyright 2021 Steven Wiley <s.wiley@katchitek.com> 
 *  SPDX-License-Identifier: BSD-2-Clause
 */
static pointer ff_process_command( pointer args )
{
    char *command = strvalue( car( args ));

    return( mk_integer( system( command )));
}

 pointer LOAD_MODULE__auto_scheme_process(pointer environment)
{
pointer return_value = T;
autoscheme_eval(T, environment);
autoscheme_eval(T, environment);
return_value = autoscheme_eval(cons(mk_symbol("define-library"),cons(cons(mk_symbol("auto"),cons(mk_symbol("scheme"),cons(mk_symbol("process"),NIL))),cons(cons(mk_symbol("export"),cons(mk_symbol("process-command"),NIL)),cons(cons(mk_symbol("begin"),cons(cons(mk_syntax(LOC_DEF0,"define"),cons(mk_symbol("process-command"),cons(mk_function(ff_process_command,&NIL),NIL))),NIL)),NIL)))), environment);
return return_value;
}
