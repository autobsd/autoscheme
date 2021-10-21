/*  This file is part of the 'AutoScheme' project.
 *  Copyright 2021 Steven Wiley <s.wiley@katchitek.com> 
 *  SPDX-License-Identifier: BSD-2-Clause
 */
#include "autoscheme.h"
pointer LOAD_MODULE__auto_scheme_environment(pointer environment);
/*  This file is part of the 'AutoScheme' project.
 *  Copyright 2021 Steven Wiley <s.wiley@katchitek.com> 
 *  SPDX-License-Identifier: BSD-2-Clause
 */
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

static pointer ff_environment_define_d( pointer args );
static pointer ff_environment_undefine_d( pointer args );
static pointer ff_environment_defined_symbols( pointer args );
static pointer ff_environment_assoc( pointer args );
static pointer ff_environment_ref( pointer args );
static pointer ff_environment_update_d( pointer args );
static pointer ff_environment_import_d( pointer args );
static pointer ff_environment_only( pointer args );
static pointer ff_environment_except( pointer args );
static pointer ff_environment_prefix( pointer args );
static pointer ff_environment_rename( pointer args );

static pointer ff_environment_delete_d( pointer args );

pointer INITIALIZE_LIBRARY__auto_scheme_environment( pointer environment );
 /*  This file is part of the 'AutoScheme' project.
 *  Copyright 2021 Steven Wiley <s.wiley@katchitek.com> 
 *  SPDX-License-Identifier: BSD-2-Clause
 */
pointer INITIALIZE_LIBRARY__auto_scheme_environment( pointer environment )
{
    scheme_register_proc(LOC_CURR_ENV, "current-environment", environment);
    scheme_register_proc(LOC_GLOB_ENV, "global-environment", environment);

    scheme_register_foreign_func( "make-environment",            make_environment               , environment);
    scheme_register_proc(LOC_ENVP, "environment?"                                                , environment);
    scheme_register_foreign_func( "environment-define!"  ,       ff_environment_define_d        , environment);
    scheme_register_foreign_func( "environment-undefine!",       ff_environment_undefine_d      , environment);
    scheme_register_foreign_func( "environment-defined-symbols", ff_environment_defined_symbols , environment);
    scheme_register_foreign_func( "environment-assoc",           ff_environment_assoc           , environment);
    scheme_register_foreign_func( "environment-ref",             ff_environment_ref             , environment);
    scheme_register_foreign_func( "environment-update!",         ff_environment_update_d        , environment);
    scheme_register_foreign_func( "environment-import!",         ff_environment_import_d        , environment);
    scheme_register_foreign_func( "environment-only",            ff_environment_only            , environment);
    scheme_register_foreign_func( "environment-except",          ff_environment_except          , environment);
    scheme_register_foreign_func( "environment-prefix",          ff_environment_prefix          , environment);
    scheme_register_foreign_func( "environment-rename",          ff_environment_rename          , environment);
    scheme_register_foreign_func( "environment-delete!",         ff_environment_delete_d        , environment);
    return environment;
}

static pointer environment_define_d( pointer environment, pointer symbol, pointer value )
{
    pointer x;

    for( x = car( environment ); is_pair( x ); x = cdr( x )) 
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

    for( x = symbols; is_pair( x ); x = cdr( x ))
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

    for( x = environment; is_pair( x ); x = cdr( x )) 
    {
	for( y = car( x ); is_pair( y ); y = cdr( y )) 
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

    for( x = environment; is_pair( x ); x = cdr( x )) 
    {
	for( y = car( x ); is_pair( y ); y = cdr( y )) 
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
    pointer binding = environment_assoc( environment, symbol );

    if( isfalse( binding ))
    {
	char *format_string = "Unbound variable %s";
	char *symbol_name = symname( symbol );
	char *message;
	size_t message_length;

	message_length = ( size_t )snprintf( NULL, 0, format_string, symbol_name );
	message = malloc( message_length + 1);
	snprintf( message, message_length + 1, format_string, symbol_name );
   
	FatalForeignError( message );
	free( message );
    }
    return cdr( binding );    
}
static pointer ff_environment_ref( pointer args )
{
    pointer environment = car( args );
    pointer symbol = cadr( args );

    return environment_ref( environment, symbol );
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

    for( x = environments; is_pair( x ); x = cdr( x )) 
    {
	symbols = environment_defined_symbols( car( x ));

	for( y = symbols; is_pair( y ); y = cdr( y ))
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

    for( x = symbols; is_pair( x ); x = cdr( x ))
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

    for( x = symbols; is_pair( x ); x = cdr( x ))
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

    for( x = symbols; is_pair( x ); x = cdr( x ))
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

    for( x = symbols; is_pair( x ); x = cdr( x ))
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



static pointer ff_environment_delete_d( pointer args )
{
    pointer environment = car( args );
    pointer symbol = cadr( args );

    pointer x, y;

    pointer *parent;
    
    for( x = environment; is_pair( x ); x = cdr( x )) 
    {
	parent = &(car( x ));

	for( y = car( x ); is_pair( y ); y = cdr( y )) 
	{
	    if( caar( y ) == symbol )
		*parent = cdr( y );
	    else
		parent = &(cdr( y));
	}
    }
    
    return environment;
}
 pointer LOAD_MODULE__auto_scheme_environment(pointer environment)
{
pointer return_value = T;
/*  This file is part of the 'AutoScheme' project.
 *  Copyright 2021 Steven Wiley <s.wiley@katchitek.com> 
 *  SPDX-License-Identifier: BSD-2-Clause
 */
pointer library = make_environment( NIL );
pointer name = mk_symbol( "(auto scheme environment)" );

environment_define_d( environment, name, library );

INITIALIZE_LIBRARY__auto_scheme_environment( library );

 return_value = autoscheme_eval(T, environment);
return_value = autoscheme_eval(T, environment);
return_value = autoscheme_eval(T, environment);
return return_value;
}
