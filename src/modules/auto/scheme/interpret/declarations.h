/*  This file is part of the 'AutoScheme' project.
 *  Copyright 2021 Steven Wiley <s.wiley@katchitek.com> 
 *  SPDX-License-Identifier: BSD-2-Clause
 */
int LOAD_MODULE__auto_scheme( pointer environment );
int LOAD_MODULE__auto_scheme_base( pointer environment );
int LOAD_MODULE__auto_scheme_environment( pointer environment );
int LOAD_MODULE__auto_scheme_eval( pointer environment );
int LOAD_MODULE__auto_scheme_list( pointer environment );
int LOAD_MODULE__auto_scheme_write( pointer environment );
int LOAD_MODULE__auto_scheme_cxr( pointer environment );
int LOAD_MODULE__auto_scheme_string( pointer environment );
int LOAD_MODULE__auto_scheme_args_fold( pointer environment );
int LOAD_MODULE__auto_scheme_args( pointer environment );
int LOAD_MODULE__auto_scheme_process_context( pointer environment );
int LOAD_MODULE__auto_scheme_path( pointer environment );
int LOAD_MODULE__auto_scheme_read( pointer environment );
int LOAD_MODULE__auto_scheme_file( pointer environment );
int LOAD_MODULE__auto_scheme_compile( pointer environment );
int LOAD_MODULE__auto_scheme_interpret( pointer environment );

static pointer ff_load_modules( pointer args );
