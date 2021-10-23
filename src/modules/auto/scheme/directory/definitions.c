/*  This file is part of the 'AutoScheme' project.
 *  Copyright 2021 Steven Wiley <s.wiley@katchitek.com> 
 *  SPDX-License-Identifier: BSD-2-Clause
 */
pointer ff_current_directory( pointer args )
{
    if( is_pair( args ) && ( chdir( strvalue( car( args ))) == -1 ))
    {
	    /* throw exception */
    }

    {
	char cwd[ PATH_MAX ];
	size_t cwd_length;

	if( getcwd( cwd, sizeof( cwd )) == NULL) 
	{
	    /* perror("getcwd() error"); */
	    /* exit( 1 ); */
	}

	cwd_length = strlen( cwd );
	cwd[ cwd_length ] = '/';
	cwd[ cwd_length + 1 ] = '\0';
    
	return mk_string( cwd );
    }
}



