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

    if( remove( strvalue( path )))
    {
	char *error_string;
	switch( errno ) 
	{
	case EACCES :
	    error_string = "File error - permission denied, unable to delete";
	    break; 
	
	case EBUSY :
	    error_string = "File error - file currently in use, unable to delete";
	    break;

	case ENOENT :
	    error_string = "File error - file does not exist, unable to delete";
	    break;

	case EROFS :
	    error_string = "File error - file system is read-only, unable to delete";
	    break;

	default : 
	    error_string = "File error - unable to delete file";
	}

	return tail_error( mk_string( error_string ), cons( path, NIL ));
    }
    return T;
}
