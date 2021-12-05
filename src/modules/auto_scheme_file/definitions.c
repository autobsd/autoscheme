/*  This file is part of the 'AutoScheme' project.
 *  Copyright 2021 Steven Wiley <s.wiley@katchitek.com> 
 *  SPDX-License-Identifier: BSD-2-Clause
 */
static pointer ff_file_exists_p( pointer args )
{
    pointer file_name = car( args );

    if( access( strvalue( file_name ), F_OK ) == 0 ) 
	return T;

    return F;
}


static pointer ff_delete_file( pointer args )
{
    pointer path = car( args );
    int force = is_pair( cdr( args )) && istrue( cadr( args ));

    if( !remove( strvalue( path )) || ( force && errno == ENOENT ))
    {
	return T;
    }
    return tail_error( mk_string( "File error - " ), cons( path, NIL ), errno );
}
