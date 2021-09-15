/*  This file is part of the 'AutoScheme' project.
 *  Copyright 2021 Steven Wiley <s.wiley@katchitek.com> 
 *  SPDX-License-Identifier: BSD-2-Clause
 */
static pointer environment_define_d( pointer environment, pointer symbol, pointer value )
{
    pointer x;

    for( x = car( environment ); x != NIL; x = cdr( x )) 
    {
	if( caar( x ) == symbol )
	{
	    cdar( x ) = value;
		
	    return environment;
	}
    }

    car( environment ) = cons( cons( symbol, value ), car( environment ));

    return environment;
}

static pointer ff_environment_define_d( pointer args )
{
    pointer environment = car( args );
    pointer symbol = cadr( args );
    pointer value = caddr( args );


    return environment_define_d( environment, symbol, value );
}
 



static pointer ff_environment_undefine_d( pointer args )
{
    pointer environment = car( args );
    pointer symbols = cdr( args );

    pointer x;

    for( x = symbols; x != NIL; x = cdr( x ))
    {
	environment_define_d( environment, car( x ), UNDEF );
    }
    return environment;
}

	

static pointer environment_defined_symbols( pointer environment )
{
    pointer defined_symbols = NIL;
    pointer undefined_symbols = NIL;
    pointer symbol;

    pointer x, y;

    for( x = environment; x != NIL; x = cdr( x )) 
    {
	for( y = car( x ); y != NIL; y = cdr( y )) 
	{
	    symbol = caar( y );

	    if( ! member( symbol, defined_symbols ) && ! member( symbol, undefined_symbols ))
	    {
		if( cdar( y ) == UNDEF )
		    undefined_symbols = cons( symbol, undefined_symbols );
		else
		    defined_symbols = cons( symbol, defined_symbols );
	    }
	}
    }

    return reverse( defined_symbols );
}

static pointer ff_environment_defined_symbols( pointer args )
{
    pointer environment = car( args );

    return environment_defined_symbols( environment );
}


static pointer environment_assoc( pointer environment, pointer symbol )
{
    pointer x, y;

    for( x = environment; x != NIL; x = cdr( x )) 
    {
	for( y = car( x ); y != NIL; y = cdr( y )) 
	{
	    if( caar( y ) == symbol )
	    {
		if( cdar( y ) == UNDEF )
		    return F;
		else
		    return car( y );
	    }
	}
    }
    return F;
}

static pointer ff_environment_assoc( pointer args )
{
    pointer environment = car( args );
    pointer symbol = cadr( args );
   
    return environment_assoc( environment, symbol );
}

static pointer environment_ref( pointer environment, pointer symbol )
{
    return cdr( environment_assoc( environment, symbol ));    
}
static pointer ff_environment_ref( pointer args )
{
    return cdr( ff_environment_assoc( args ));
}



static pointer ff_environment_update_d( pointer args )
{
    pointer environment = car( args );
    pointer symbol = cadr( args );
    pointer value = caddr( args );

    pointer assoc = environment_assoc( environment, symbol );

    if (istrue( assoc ))
	cdr( assoc ) = value;
    else 
	car( environment ) = cons( cons( symbol, value ), car( environment ));

    return environment;
}






static pointer ff_environment_import_d( pointer args )
{
    pointer target = car( args );
    pointer environments = cdr( args );
    pointer symbols;
    pointer value;
    pointer x,y;

    for( x = environments; x != NIL; x = cdr( x )) 
    {
	symbols = environment_defined_symbols( car( x ));

	for( y = symbols; y != NIL; y = cdr( y ))
	{
	    value = environment_ref( car( x ), car( y ));
	    environment_define_d( target, car( y ), value);
	}
    }
    return target;
}

static pointer ff_environment_only( pointer args )
{
    pointer environment = car( args );
    pointer symbols = cdr( args );
    pointer target = make_environment( NIL );
    pointer value;
    pointer x;

    for( x = symbols; x != NIL; x = cdr( x ))
    {
	value = environment_ref( environment, car( x ));
	environment_define_d( target, car( x ), value );
    }

    return target;
}

static pointer ff_environment_except( pointer args )
{
    pointer environment = car( args );
    pointer exceptions = cdr( args );
    pointer target = make_environment( NIL );

    pointer symbols = environment_defined_symbols( environment );
    pointer symbol;

    pointer value;
    pointer x;

    for( x = symbols; x != NIL; x = cdr( x ))
    {
	symbol = car( x );

	if( ! member( symbol, exceptions ))
	{
	    value = environment_ref( environment, symbol );
	    environment_define_d( target, symbol, value );
	}
    }

    return target;
}




static pointer ff_environment_prefix( pointer args )
{
    pointer environment = car( args );
    pointer prefix = cadr( args );
    pointer symbols = environment_defined_symbols( environment );
    pointer target = make_environment( NIL );

    pointer symbol;
    pointer value;

    size_t prefixed_string_length;
    pointer prefixed_string;

    pointer prefixed_symbol;

    pointer x;

    for( x = symbols; x != NIL; x = cdr( x ))
    {
	symbol = car( x );
	value = environment_ref( environment, symbol );

	prefixed_string_length = strlength( prefix ) + strlength( symbol );
	prefixed_string = mk_counted_string( strvalue( prefix ), prefixed_string_length );
	memcpy( strvalue( prefixed_string ) + strlength( prefix ), strvalue( symbol ), strlength( symbol ));
	strvalue( prefixed_string )[prefixed_string_length] = '\0';
	prefixed_symbol = mk_symbol( strvalue( prefixed_string ));

	environment_define_d( target, prefixed_symbol, value );
    }


    return target;
}






static pointer ff_environment_rename( pointer args )
{
    pointer environment = car( args );
    pointer pairs = cdr( args );
    pointer symbols = environment_defined_symbols( environment );
    pointer target = make_environment( NIL );

    pointer pair;
    pointer symbol;
    pointer value;

    pointer x;

    for( x = symbols; x != NIL; x = cdr( x ))
    {
	symbol = car( x );
	value = environment_ref( environment, symbol );

	pair = assoc( symbol, pairs );
	
	if( pair == F )
	    environment_define_d( target, symbol, value );
	else
	    environment_define_d( target, cdr( pair ), value );

    }

    return target;
}
