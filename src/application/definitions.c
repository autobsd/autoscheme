/*  This file is part of the 'AutoScheme' project.
 *  Copyright 2021 Steven Wiley <s.wiley@katchitek.com> 
 *  SPDX-License-Identifier: BSD-2-Clause
 */
pointer mk_environment( pointer args )
{
    pointer env = cons( args, NIL );
    setenvironment( env );

    return env;
}
