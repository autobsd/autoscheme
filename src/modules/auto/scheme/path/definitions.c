/*  This file is part of the 'AutoScheme' project.
 *  Copyright 2021 Steven Wiley <s.wiley@katchitek.com> 
 *  SPDX-License-Identifier: BSD-2-Clause
 */
static pointer ff_path_absolute_p( pointer args )
{
    pointer path = car( args );

    if( strvalue( path )[0] == '/' )
	return T;

    return F;
}
