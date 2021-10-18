/*  This file is part of the 'AutoScheme' project.
 *  Copyright 2021 Steven Wiley <s.wiley@katchitek.com> 
 *  SPDX-License-Identifier: BSD-2-Clause
 */
pointer LOAD_MODULE__scheme( pointer environment );
pointer LOAD_MODULE__auto_scheme_macro( pointer environment );
pointer LOAD_MODULE__auto_scheme_base( pointer environment );
pointer LOAD_MODULE__auto_scheme_environment( pointer environment );
pointer LOAD_MODULE__scheme_eval( pointer environment );
pointer LOAD_MODULE__scheme_repl( pointer environment );
pointer LOAD_MODULE__scheme_load( pointer environment );
pointer LOAD_MODULE__auto_scheme_list( pointer environment );
pointer LOAD_MODULE__auto_scheme_write( pointer environment );
pointer LOAD_MODULE__scheme_cxr( pointer environment );
pointer LOAD_MODULE__auto_scheme_string( pointer environment );
pointer LOAD_MODULE__auto_scheme_args_fold( pointer environment );
pointer LOAD_MODULE__auto_scheme_args( pointer environment );
pointer LOAD_MODULE__scheme_process_context( pointer environment );
pointer LOAD_MODULE__auto_scheme_directory( pointer environment );
pointer LOAD_MODULE__auto_scheme_path( pointer environment );
pointer LOAD_MODULE__scheme_read( pointer environment );
pointer LOAD_MODULE__auto_scheme_file( pointer environment );
pointer LOAD_MODULE__auto_scheme_compile( pointer environment );
pointer LOAD_MODULE__auto_scheme_interpret( pointer environment );

static pointer ff_load_modules( pointer args );
