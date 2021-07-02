#include <stdio.h>
#include <string.h>

#include "version.h"
#include "s7.h"


int scheme()
{
    s7_scheme *s7;
    char buffer[512];
    char response[1024];

    s7 = s7_init();              

    while( 1 )                    
    {
	fprintf( stdout, "scheme> " );    
	fgets( buffer, 512, stdin );
	if(( buffer[0] != '\n' ) || ( strlen(buffer) > 1 ))
	{
	    snprintf( response, 1024, "(begin %s)", buffer );
	    s7_eval_c_string( s7, response ); 
	}
    }
    return 0;
}

int repl()
{
    s7_scheme *s7;
    char buffer[512];
    char response[1024];

    s7 = s7_init();              
    while( 1 )                    
    {
	fprintf( stdout, "scheme-REPL> " );    
	fgets( buffer, 512, stdin );
	if(( buffer[0] != '\n' ) || ( strlen(buffer) > 1 ))
	{
	    snprintf( response, 1024, "(write %s)", buffer );
	    s7_eval_c_string( s7, response ); 
	}
	fprintf( stdout, "\n" );    
    }
    return 0;
}

int version()
{
    /* printf( "AutoScheme version %s\n", VERSION ); */

    s7_scheme *s7;
    char buffer[512];

    snprintf( buffer, 512, "(begin (display \"AutoScheme version %s\")(newline))", VERSION );

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
    }

    return scheme();
}
