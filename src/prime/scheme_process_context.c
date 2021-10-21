/*  This file is part of the 'AutoScheme' project.
 *  Copyright 2021 Steven Wiley <s.wiley@katchitek.com> 
 *  SPDX-License-Identifier: BSD-2-Clause
 */
#include "autoscheme.h"
pointer LOAD_MODULE__scheme_process_context(pointer environment);
/*  This file is part of the 'AutoScheme' project.
 *  Copyright 2021 Steven Wiley <s.wiley@katchitek.com> 
 *  SPDX-License-Identifier: BSD-2-Clause
 */
#include <limits.h>
#include <unistd.h>
#include <string.h>

extern char **environ;

extern int auto_argc;
extern char **auto_argv;

static pointer ff_command_line( pointer args );
static pointer ff_get_environment_variables( pointer args );
 /*  This file is part of the 'AutoScheme' project.
 *  Copyright 2021 Steven Wiley <s.wiley@katchitek.com> 
 *  SPDX-License-Identifier: BSD-2-Clause
 */
static pointer ff_command_line( pointer args )
{
    static pointer arguments = NULL;
    if( arguments == NULL )
    {
	int i;
	arguments = NIL;

	for( i = auto_argc - 1; i >= 0; i-- )
	{
	    arguments = cons( mk_string( auto_argv[i] ), arguments );
	}
    }
    return arguments;
    (void)args;
}



static pointer ff_get_environment_variables( pointer args )
{
    static pointer variables = NULL;
    if( variables == NULL )
    {
	char *s;
	char *d;
	int i = 0;
	variables = NIL;

	while( environ[i] ) 
	{
	    s = environ[i];
	    d = strchr( s, '=' );
	    
	    {
		pointer name = mk_counted_string( s, d - s );
		pointer value = mk_counted_string( d + 1, s + strlen( s ) - d - 1 ); 
		pointer pair = cons( name, value );
		variables = cons( pair, variables );
	    }
	    i++;
	}
    }
    return variables;
    (void)args;
}
 pointer LOAD_MODULE__scheme_process_context(pointer environment)
{
pointer return_value = T;
return_value = autoscheme_eval(T, environment);
return_value = autoscheme_eval(T, environment);
return_value = autoscheme_eval(cons(mk_symbol("define-library"),cons(cons(mk_symbol("scheme"),cons(mk_symbol("process-context"),NIL)),cons(cons(mk_symbol("import"),cons(cons(mk_symbol("auto"),cons(mk_symbol("scheme"),cons(mk_symbol("base"),NIL))),NIL)),cons(cons(mk_symbol("export"),cons(mk_symbol("command-line"),cons(mk_symbol("emergency-exit"),cons(mk_symbol("exit"),cons(mk_symbol("get-environment-variables"),cons(mk_symbol("get-environment-variable"),NIL)))))),cons(cons(mk_symbol("begin"),cons(cons(mk_symbol("define"),cons(mk_symbol("command-line"),cons(mk_foreign_func(ff_command_line,&NIL),NIL))),cons(cons(mk_symbol("define"),cons(mk_symbol("get-environment-variables"),cons(mk_foreign_func(ff_get_environment_variables,&NIL),NIL))),cons(cons(mk_symbol("define"),cons(mk_symbol("get-environment-variable"),cons(cons(mk_symbol("lambda"),cons(cons(mk_symbol("name"),NIL),cons(cons(mk_symbol("let"),cons(cons(cons(mk_symbol("variable"),cons(cons(mk_symbol("assoc"),cons(mk_symbol("name"),cons(cons(mk_symbol("get-environment-variables"),NIL),NIL))),NIL)),NIL),cons(cons(mk_symbol("and"),cons(mk_symbol("variable"),cons(cons(mk_symbol("cdr"),cons(mk_symbol("variable"),NIL)),NIL))),NIL))),NIL))),NIL))),cons(cons(mk_symbol("define"),cons(mk_symbol("emergency-exit"),cons(mk_operation(LOC_EMERGENCY_EXIT,&NIL),NIL))),cons(cons(mk_symbol("define"),cons(mk_symbol("exit"),cons(F,NIL))),cons(cons(mk_symbol("call-with-current-continuation"),cons(cons(mk_symbol("lambda"),cons(cons(mk_symbol("return"),NIL),cons(cons(mk_symbol("let"),cons(cons(cons(mk_symbol("obj"),cons(cons(mk_symbol("call-with-current-continuation"),cons(cons(mk_symbol("lambda"),cons(cons(mk_symbol("_exit"),NIL),cons(cons(mk_symbol("cond"),cons(cons(cons(mk_symbol("not"),cons(mk_symbol("exit"),NIL)),cons(cons(mk_symbol("set!"),cons(mk_symbol("exit"),cons(mk_symbol("_exit"),NIL))),cons(cons(mk_symbol("return"),NIL),NIL))),NIL)),NIL))),NIL)),NIL)),NIL),cons(cons(mk_symbol("emergency-exit"),cons(cons(mk_symbol("cond"),cons(cons(cons(mk_symbol("integer?"),cons(mk_symbol("obj"),NIL)),cons(mk_symbol("obj"),NIL)),cons(cons(cons(mk_symbol("eq?"),cons(mk_symbol("obj"),cons(F,NIL))),cons(mk_integer(1),NIL)),cons(cons(mk_symbol("else"),cons(mk_integer(0),NIL)),NIL)))),NIL)),NIL))),NIL))),NIL)),NIL))))))),NIL))))), environment);
return return_value;
}
