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



