/*  This file is part of the 'AutoScheme' project.
 *  Copyright 2021 Steven Wiley <s.wiley@katchitek.com> 
 *  SPDX-License-Identifier: BSD-2-Clause
 */
pointer mk_environment( pointer p )
{
    pointer env = cons( p, NIL );
    setenvironment( env );

    return env;
}
