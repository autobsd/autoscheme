/*  This file is part of the 'AutoScheme' project.
 *  Copyright (c) 2026 AutoBSD Contributors.
 *  SPDX-License-Identifier: BSD-2-Clause
 */
static pointer ff_process_command( pointer args )
{
    char *command = strvalue( car( args ));

    return( mk_integer( system( command )));
}

