/*  This file is part of the 'AutoScheme' project.
 *  Copyright 2021 Steven Wiley <s.wiley@katchitek.com> 
 *  SPDX-License-Identifier: BSD-2-Clause
 */
static pointer ff_load_modules( pointer args )
{
    pointer environment = car( args );

    LOAD_MODULE__scheme( environment );
    LOAD_MODULE__auto_scheme_base( environment );
    LOAD_MODULE__auto_scheme_environment( environment );
    LOAD_MODULE__auto_scheme_library( environment );
    LOAD_MODULE__auto_scheme_eval( environment );
    LOAD_MODULE__auto_scheme_list( environment );
    LOAD_MODULE__auto_scheme_write( environment );
    LOAD_MODULE__auto_scheme_cxr( environment );
    LOAD_MODULE__auto_scheme_string( environment );
    LOAD_MODULE__auto_scheme_args_fold( environment );
    LOAD_MODULE__auto_scheme_args( environment );
    LOAD_MODULE__auto_scheme_process_context( environment );
    LOAD_MODULE__auto_scheme_path( environment );
    LOAD_MODULE__auto_scheme_read( environment );
    LOAD_MODULE__auto_scheme_file( environment );
    LOAD_MODULE__auto_scheme_compile( environment );
    LOAD_MODULE__auto_scheme_interpret( environment );

    return environment;
}
