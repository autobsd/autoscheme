#include <stdio.h>
#include <string.h>

#include "version.h"
#include "s7.h"

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
    printf( "AutoScheme version %s\n", VERSION );
    return 0;
}

int main( int argc, char **argv )
{
    /* return repl(); */
    return version();

    return 0;
}
