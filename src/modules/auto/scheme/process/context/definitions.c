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
}

/* static s7_pointer current_directory( s7_scheme *sc, s7_pointer args ) */
/* { */
/*     if( !s7_is_null( sc, args )) */
/* 	return s7_wrong_type_arg_error( sc, "current-directory", 0, args, "null" ); */
/*     { */
/*        char buff[FILENAME_MAX]; */
/*        GetCurrentDir( buff, FILENAME_MAX ); */
/*        return s7_make_string( sc, buff ); */
/*     } */
/* } */



