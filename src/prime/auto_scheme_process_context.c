/*  This file is part of the 'AutoScheme' project.
 *  Copyright 2021 Steven Wiley <s.wiley@katchitek.com> 
 *  SPDX-License-Identifier: BSD-2-Clause
 */
#include "autoscheme.h"
int LOAD_MODULE__auto_scheme_process_context(pointer environment);
/*  This file is part of the 'AutoScheme' project.
 *  Copyright 2021 Steven Wiley <s.wiley@katchitek.com> 
 *  SPDX-License-Identifier: BSD-2-Clause
 */
#include <limits.h>
#include <unistd.h>

extern int auto_argc;
extern char **auto_argv;

static pointer ff_command_line( pointer args );
static pointer ff_current_directory( pointer args );



 /*  This file is part of the 'AutoScheme' project.
 *  Copyright 2021 Steven Wiley <s.wiley@katchitek.com> 
 *  SPDX-License-Identifier: BSD-2-Clause
 */
static pointer ff_command_line( pointer args )
{
    pointer arguments = NIL;
    int i;
    for( i = auto_argc - 1; i >= 0; i-- )
    {
	arguments = cons( mk_string( auto_argv[i] ), arguments );
    }

    return arguments;
    ( void )args;
}

static pointer ff_current_directory( pointer args )
{
    /* char buff[FILENAME_MAX]; */
    /* GetCurrentDir( buff, FILENAME_MAX ); */

    char cwd[PATH_MAX];
    if( getcwd( cwd, sizeof( cwd )) == NULL) 
    {
	/* perror("getcwd() error"); */
	/* exit( 1 ); */
    }

    return mk_string( cwd );
    ( void )args;
}



 int LOAD_MODULE__auto_scheme_process_context(pointer environment)
{
/*  This file is part of the 'AutoScheme' project.
 *  Copyright 2021 Steven Wiley <s.wiley@katchitek.com> 
 *  SPDX-License-Identifier: BSD-2-Clause
 */
scheme_register_foreign_func( "command-line",            ff_command_line            , global_env);
scheme_register_foreign_func( "current-directory",      ff_current_directory       , global_env);



 autoscheme_eval(T, environment);
autoscheme_eval(T, environment);
autoscheme_eval(T, environment);
autoscheme_eval(cons(mk_symbol("define-library"),cons(cons(mk_symbol("auto"),cons(mk_symbol("scheme"),cons(mk_symbol("process"),cons(mk_symbol("context"),NIL)))),cons(cons(mk_symbol("import"),cons(cons(mk_symbol("only"),cons(cons(mk_symbol("auto"),cons(mk_symbol("scheme"),NIL)),cons(mk_symbol("emergency-exit"),cons(mk_symbol("exit"),cons( mk_symbol("quote" ),cons(mk_symbol("begin"),NIL)))))),cons(cons(mk_symbol("auto"),cons(mk_symbol("scheme"),cons(mk_symbol("environment"),NIL))),NIL))),cons(cons(mk_symbol("export"),cons(cons(mk_symbol("rename"),cons(mk_symbol("_command-line"),cons(mk_symbol("command-line"),NIL))),cons(cons(mk_symbol("rename"),cons(mk_symbol("_current-directory"),cons(mk_symbol("current-directory"),NIL))),cons(mk_symbol("emergency-exit"),cons(mk_symbol("exit"),NIL))))),cons(cons(mk_symbol("begin"),cons(cons(mk_symbol("environment-update!"),cons(cons(mk_symbol("current-environment"),NIL),cons(cons( mk_symbol("quote" ),cons(mk_symbol("_command-line"),NIL)),cons(cons(mk_symbol("environment-ref"),cons(cons(mk_symbol("global-environment"),NIL),cons(cons( mk_symbol("quote" ),cons(mk_symbol("command-line"),NIL)),NIL))),NIL)))),cons(cons(mk_symbol("environment-delete!"),cons(cons(mk_symbol("global-environment"),NIL),cons(cons( mk_symbol("quote" ),cons(mk_symbol("command-line"),NIL)),NIL))),cons(cons(mk_symbol("environment-update!"),cons(cons(mk_symbol("current-environment"),NIL),cons(cons( mk_symbol("quote" ),cons(mk_symbol("_current-directory"),NIL)),cons(cons(mk_symbol("environment-ref"),cons(cons(mk_symbol("global-environment"),NIL),cons(cons( mk_symbol("quote" ),cons(mk_symbol("current-directory"),NIL)),NIL))),NIL)))),cons(cons(mk_symbol("environment-delete!"),cons(cons(mk_symbol("global-environment"),NIL),cons(cons( mk_symbol("quote" ),cons(mk_symbol("current-directory"),NIL)),NIL))),NIL))))),NIL))))), environment);
return 0;
}
