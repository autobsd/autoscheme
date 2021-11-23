/*  This file is part of the 'AutoScheme' project.
 *  Copyright 2021 Steven Wiley <s.wiley@katchitek.com> 
 *  SPDX-License-Identifier: BSD-2-Clause
 */
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
