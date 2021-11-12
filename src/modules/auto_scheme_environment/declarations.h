/*  This file is part of the 'AutoScheme' project.
 *  Copyright 2021 Steven Wiley <s.wiley@katchitek.com> 
 *  SPDX-License-Identifier: BSD-2-Clause
 */
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

pointer ff_environment_define_d( pointer args );
static pointer ff_environment_undefine_d( pointer args );
static pointer ff_environment_defined_symbols( pointer args );
static pointer ff_environment_assoc( pointer args );
static pointer ff_environment_ref( pointer args );
static pointer ff_environment_update_d( pointer args );
static pointer ff_environment_import_d( pointer args );
static pointer ff_environment_only( pointer args );
static pointer ff_environment_except( pointer args );
static pointer ff_environment_prefix( pointer args );
static pointer ff_environment_rename( pointer args );

static pointer ff_environment_delete_d( pointer args );

pointer INITIALIZE_LIBRARY__auto_scheme_environment( pointer environment );
