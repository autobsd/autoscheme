#ifdef _WIN32
#define _CRT_SECURE_NO_DEPRECATE
#endif

#include <stdio.h>
#include <string.h>
#include <stdlib.h>

#include "version.h"
#include "s7.h"


int interpret( char *file_name )
{
    char *buffer;
    size_t buffer_size;
    buffer_size = snprintf( NULL, 0, "(load \"%s\")", file_name ) + 1;
    buffer = malloc( buffer_size );
    sprintf( buffer, "(load \"%s\")", file_name );

    s7_scheme *s7;
    s7 = s7_init();
    s7_eval_c_string( s7, buffer );

    free( buffer );
    return 0;
}

int scheme()
{
    s7_scheme *s7;
    s7 = s7_init();              

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

int repl()
{
    s7_scheme *s7;
    s7 = s7_init();              

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

int version()
{
    /* printf( "AutoScheme version %s\n", VERSION ); */

    char buffer[512];
    snprintf( buffer, 512, "(begin (display \"AutoScheme version %s\")(newline))", VERSION );

    s7_scheme *s7;
    s7 = s7_init();              
    s7_eval_c_string( s7, buffer ); 
 
    return 0;
}

int main( int argc, char **argv )
{
    int i;
    for( i = 1; i < argc; i++)
    {
	if( !strcmp( argv[1], "-V" ))
	{
	    return version();
	}
	if( !strcmp( argv[1], "-r" ))
	{
	    return repl();
	}
	if( !strcmp( argv[i], "-i" ) && ((i + 1) < argc)  )
	{
	    return interpret( argv[i + 1] );
	}
    }

    return scheme();
}
