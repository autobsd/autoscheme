/*  This file is part of the 'AutoScheme' project.
 *  Copyright 2021 Steven Wiley <s.wiley@katchitek.com> 
 *  SPDX-License-Identifier: BSD-2-Clause
 */
#include "autoscheme.h"

pointer ff_load_modules( pointer args );
pointer ff_load_modules( pointer args )
{
    pointer environment = car( args );

    foreign_function LOAD_MODULE__scheme;
    foreign_function LOAD_MODULE__auto_scheme_base;
    foreign_function LOAD_MODULE__auto_scheme_write;
    foreign_function LOAD_MODULE__auto_scheme_environment;
    foreign_function LOAD_MODULE__auto_scheme_args_fold;
    foreign_function LOAD_MODULE__auto_scheme_char_set;
    foreign_function LOAD_MODULE__auto_scheme_string;
    foreign_function LOAD_MODULE__scheme_cxr;
    foreign_function LOAD_MODULE__auto_scheme_args;
    foreign_function LOAD_MODULE__auto_scheme_char;
    foreign_function LOAD_MODULE__auto_scheme_closure;
    foreign_function LOAD_MODULE__auto_scheme_path;
    foreign_function LOAD_MODULE__auto_scheme_file;
    foreign_function LOAD_MODULE__scheme_read;
    foreign_function LOAD_MODULE__auto_scheme_compile;
    foreign_function LOAD_MODULE__auto_scheme_directory;
    foreign_function LOAD_MODULE__auto_scheme_inexact;
    foreign_function LOAD_MODULE__scheme_eval;
    foreign_function LOAD_MODULE__scheme_load;
    foreign_function LOAD_MODULE__auto_scheme_interpret;
    foreign_function LOAD_MODULE__auto_scheme_lazy;
    foreign_function LOAD_MODULE__auto_scheme_list;
    foreign_function LOAD_MODULE__auto_scheme_macro;
    foreign_function LOAD_MODULE__auto_scheme_memory;
    foreign_function LOAD_MODULE__auto_scheme_port;
    foreign_function LOAD_MODULE__auto_scheme_process;
    foreign_function LOAD_MODULE__auto_scheme_sort;
    foreign_function LOAD_MODULE__scheme_process_context;
    foreign_function LOAD_MODULE__scheme_repl;

    LOAD_MODULE__scheme( environment );
    LOAD_MODULE__auto_scheme_base( environment );
    LOAD_MODULE__auto_scheme_write( environment );
    LOAD_MODULE__auto_scheme_environment( environment );
    LOAD_MODULE__auto_scheme_args_fold( environment );
    LOAD_MODULE__auto_scheme_char_set( environment );
    LOAD_MODULE__auto_scheme_string( environment );
    LOAD_MODULE__scheme_cxr( environment );
    LOAD_MODULE__auto_scheme_args( environment );
    LOAD_MODULE__auto_scheme_char( environment );
    LOAD_MODULE__auto_scheme_closure( environment );
    LOAD_MODULE__auto_scheme_path( environment );
    LOAD_MODULE__auto_scheme_file( environment );
    LOAD_MODULE__scheme_read( environment );
    LOAD_MODULE__auto_scheme_compile( environment );
    LOAD_MODULE__auto_scheme_directory( environment );
    LOAD_MODULE__auto_scheme_inexact( environment );
    LOAD_MODULE__scheme_eval( environment );
    LOAD_MODULE__scheme_load( environment );
    LOAD_MODULE__auto_scheme_interpret( environment );
    LOAD_MODULE__auto_scheme_lazy( environment );
    LOAD_MODULE__auto_scheme_list( environment );
    LOAD_MODULE__auto_scheme_macro( environment );
    LOAD_MODULE__auto_scheme_memory( environment );
    LOAD_MODULE__auto_scheme_port( environment );
    LOAD_MODULE__auto_scheme_process( environment );
    LOAD_MODULE__auto_scheme_sort( environment );
    LOAD_MODULE__scheme_process_context( environment );
    LOAD_MODULE__scheme_repl( environment );

    return environment;
}
