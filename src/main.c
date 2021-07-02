#ifdef _WIN32
#define _CRT_SECURE_NO_DEPRECATE
#endif

#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <sys/stat.h>

#ifndef _WIN32
#include <errno.h>
#else
#endif


#include "version.h"
#include "s7.h"


int interpret( char *file_name )
{
    struct stat sb;
    off_t file_size;
    FILE *fp;

    char *program_str;
    s7_scheme *s7;

    stat( file_name, &sb );
    file_size = sb.st_size;

    fp = fopen( file_name, "r" ); 

    if( fp == NULL )
    {
    	printf( "File read error" );
    	if( errno == 2 ) printf( " - No such file or directory: %s\n", file_name );
    	exit( 1 );
    }

    program_str = malloc( file_size + 9 );
    snprintf( program_str, 8, "(begin " );
    fread( program_str + 7, 1, file_size, fp );
    snprintf( program_str + 7 + file_size, 2,  ")" );

    fclose( fp );

    s7 = s7_init();
    s7_eval_c_string( s7, program_str );

    free( program_str );
    return 0;
}

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
	if( !strcmp( argv[i], "-i" ) && ((i + 1) < argc)  )
	{
	    return interpret( argv[i + 1] );
	}
    }

    return scheme();
}
