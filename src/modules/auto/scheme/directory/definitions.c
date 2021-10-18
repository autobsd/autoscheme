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



