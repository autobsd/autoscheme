#ifdef _WIN32
#define _CRT_SECURE_NO_DEPRECATE
#endif

#include <stdio.h>
#include <string.h>
#include <stdlib.h>

#define _Bool int
#include "s7.h"

#include "version.h"

int interpret( s7_scheme *s7, char *file_name );
int scheme( s7_scheme *s7 );
int repl( s7_scheme *s7 );
int version( s7_scheme *s7 );

s7_scheme *auto_init( void );


int interpret( s7_scheme *s7, char *file_name )
{
    char *buffer;
    size_t buffer_size;
    buffer_size = snprintf( NULL, 0, "(load \"%s\")", file_name ) + 1;
    buffer = malloc( buffer_size );
    sprintf( buffer, "(load \"%s\")", file_name );

    s7_eval_c_string( s7, buffer );

    free( buffer );
    return 0;
}

int scheme( s7_scheme *s7 )
{
    char response[1024];

    char *buffer;
    size_t buffer_size;

    while( 1 )                    
    {
	fprintf( stdout, "scheme> " );    
	fgets( response, 1024, stdin );
	if(( response[0] != '\n' ) || ( strlen(response) > 1 ))
	{
	    buffer_size = snprintf( NULL, 0, "(begin %s)", response ) + 1;
	    buffer = malloc( buffer_size );
	    sprintf( buffer, "(begin %s)", response );
	    snprintf( buffer, 1024, "(begin %s)", response );

	    s7_eval_c_string( s7, buffer ); 
	    free( buffer );
	}
    }
    return 0;
}

int repl( s7_scheme *s7 )
{
    char response[1024];

    char *buffer;
    size_t buffer_size;

    while( 1 )                    
    {
	fprintf( stdout, "scheme-REPL> " );    
	fgets( response, 1024, stdin );
	if(( response[0] != '\n' ) || ( strlen(response) > 1 ))
	{
	    buffer_size = snprintf( NULL, 0, "(begin %s)", response ) + 1;
	    buffer = malloc( buffer_size );
	    sprintf( buffer, "(write %s)", response );
	    snprintf( buffer, 1024, "(write %s)", response );

	    s7_eval_c_string( s7, buffer ); 
	    free( buffer );
	}
	fprintf( stdout, "\n" );    
    }
    return 0;
}

int version( s7_scheme *s7 )
{
    /* printf( "AutoScheme version %s\n", VERSION ); */

    char buffer[512];
    snprintf( buffer, 512, "(begin (display \"AutoScheme version %s\")(newline))", VERSION );

    s7_eval_c_string( s7, buffer ); 

    return 0;
}

int auto_argc; char **auto_argv;

static s7_pointer command_line( s7_scheme *sc, s7_pointer args )
{
    if( !s7_is_null( sc, args ))
	return s7_wrong_type_arg_error( sc, "command-line", 0, args, "null");
    else
    {
	s7_pointer arguments = s7_nil( sc );

	int i;

	for( i = auto_argc - 1; i >= 0; i--)
	{
	    arguments = s7_cons( sc, s7_make_string( sc, auto_argv[i] ), arguments );	    
	}
	return arguments;
    }
}



s7_scheme *auto_init( )
{
    s7_scheme *s7 = s7_init();

    s7_define_function( s7, "command-line", command_line, 0, 0, false, "(command-line) returns a list of command-line arguments" );

    return s7;
}
int main( int argc, char **argv )
{
    s7_scheme *s7 = auto_init();
    int i;

    for( i = 1; i < argc; i++)
    {
	if( !strcmp( argv[1], "-V" ))
	{
	    return version( s7 );
	}
	if( !strcmp( argv[1], "-r" ))
	{
	    return repl( s7 );
	}
	if( !strcmp( argv[i], "-i" ) && ((i + 1) < argc)  )
	{
	    auto_argc = argc - i - 1; auto_argv = &(argv[i + 1]);

	    return interpret( s7, argv[i + 1] );
	}
    }

    return scheme( s7 );
}
