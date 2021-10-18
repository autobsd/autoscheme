/*  This file is part of the 'AutoScheme' project.
 *  Copyright 2021 Steven Wiley <s.wiley@katchitek.com> 
 *  SPDX-License-Identifier: BSD-2-Clause
 */
#include "autoscheme.h"
pointer LOAD_MODULE__auto_scheme_directory(pointer environment);
/*  This file is part of the 'AutoScheme' project.
 *  Copyright 2021 Steven Wiley <s.wiley@katchitek.com> 
 *  SPDX-License-Identifier: BSD-2-Clause
 */
#include <limits.h>
#include <unistd.h>


static pointer ff_current_directory( pointer args );



 /*  This file is part of the 'AutoScheme' project.
 *  Copyright 2021 Steven Wiley <s.wiley@katchitek.com> 
 *  SPDX-License-Identifier: BSD-2-Clause
 */
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



 pointer LOAD_MODULE__auto_scheme_directory(pointer environment)
{
pointer return_value = T;
return_value = autoscheme_eval(T, environment);
return_value = autoscheme_eval(T, environment);
return_value = autoscheme_eval(cons(mk_symbol("define-library"),cons(cons(mk_symbol("auto"),cons(mk_symbol("scheme"),cons(mk_symbol("directory"),NIL))),cons(cons(mk_symbol("export"),cons(mk_symbol("current-directory"),NIL)),cons(cons(mk_symbol("begin"),cons(cons(mk_syntax(OP_DEF0,"define"),cons(mk_symbol("current-directory"),cons(mk_foreign_func(ff_current_directory,&NIL),NIL))),NIL)),NIL)))), environment);
return return_value;
}
