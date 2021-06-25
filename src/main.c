#include <stdio.h>
#include <string.h>

int main( int argc, char **argv )
{
    printf( "argc: %i\n", argc );
    int i;
    for( i = 0; i < argc; i++)
    {
	printf( "argv[%i]: %s\n", i, argv[i] );
    }

    if( argc == 2 && !strcmp( argv[1], "-V" ))
    {
	printf( "\nAutoScheme version %s\n", VERSION );
    }

    return 0;
}
