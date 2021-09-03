/*  This file is part of the 'AutoScheme' project.
 *  Copyright 2021 Steven Wiley <s.wiley@katchitek.com> 
 *  SPDX-License-Identifier: BSD-2-Clause
 */
#include "autoscheme.h"

int main( int argc, char **argv )
{
    scheme_init();

    /* scheme_load_string( "(define x 5)(display \"x:\")(write x)(newline)" ); */
    /* scheme_load_string( "(set! x 3)" ); */
    /* scheme_load_string( "(display \"x:\")(write x)(newline)" ); */
    
    pointer sym_display = mk_symbol( "display" );
    pointer str_message = mk_string( "message!!!!??\n" );

    scheme_eval( cons( sym_display, (cons( str_message, NIL ))));
    scheme_deinit();

    return 0;
}


