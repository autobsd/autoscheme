#include "../../../src/version.h"
#include <stdio.h>

int version()
{
    printf( "AutoScheme version %s\n", VERSION );
    return 0;
}

int main( int argc, char **argv )
{
    return version();

    return 0;
}
