/*  This file is part of the 'AutoScheme' project.
 *  Copyright 2021 Steven Wiley <s.wiley@katchitek.com> 
 *  SPDX-License-Identifier: BSD-2-Clause
 */
pointer mk_environment( pointer args )
{
    pointer env = cons( args, NIL );
    setenvironment( env );

    return env;
}


pointer environment_undefine_d( pointer args )
{
    pointer env = car( args );
    pointer sym = cadr( args );

    pointer x;

    for( x = car( env ); x != NIL; x = cdr( x )) 
    {
	if( caar( x ) == sym )
	{
	    cdar( x ) = UNDEF;
		
	    return env;
	}
    }

    car( env ) = cons( cons( sym, UNDEF ), car( env ));

    return env;
}

int member( pointer obj, pointer l )
{
    pointer x;

    for( x = l; x != NIL; x = cdr( x )) 

	if( equal( car( x ),  obj )) return 1;
    
    return 0;
}
	

pointer environment_defined_symbols( pointer args )
{
    pointer env = car( args );
    pointer defined_symbols = NIL;
    pointer undefined_symbols = NIL;
    pointer x, y;

    for( x = env; x != NIL; x = cdr( x )) 
    {
	for( y = car( x ); y != NIL; y = cdr( y )) 
	{
	    if( ! member( caar( y ), defined_symbols ) && ! member( caar( y ), undefined_symbols ))
	    {
		if( cdar( y ) == UNDEF )
		    undefined_symbols = cons( caar( y ), undefined_symbols );
		else
		    defined_symbols = cons( caar( y ), defined_symbols );
	    }
	}
    }

    return reverse( defined_symbols );
}
