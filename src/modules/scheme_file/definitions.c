/*  This file is part of the 'AutoScheme' project.
 *  Copyright 2021 Steven Wiley <s.wiley@katchitek.com> 
 *  SPDX-License-Identifier: BSD-2-Clause
 */
pointer ff_file_exists_p( pointer args )
{
    pointer file_name = car( args );

    if( access( strvalue( file_name ), F_OK ) == 0 ) 
	return T;

    return F;
}

pointer ff_delete_file( pointer args )
{
    pointer path = car( args );
    int recurse = is_pair( cdr( args )) && istrue( cadr( args ));
    int force = is_pair( cdr( args )) && is_pair( cddr( args )) && istrue( caddr( args ));

    if( !remove( strvalue( path )) || ( force && errno == ENOENT ))
    {
	return T;
    }
    else if( recurse && ( errno == ENOTEMPTY || errno == EEXIST ))
    {
	DIR *d;
	struct dirent *dir;
	char child[ PATH_MAX ];
	pointer result;

	strcpy( child, strvalue( path ));
	d = opendir( strvalue( path ));

	if( d )
	{
	    while(( dir = readdir( d )) != NULL )
	    {
		if( strcmp( dir->d_name, ".") && strcmp( dir->d_name, ".." ))
		{
		    child[ strlength( path ) ] = '/';
		    strcpy( child + strlength( path ) + 1, dir->d_name );
		    child[ strlength( path ) + 1 + strlen( dir->d_name ) ] = '\0';
		    result = ff_delete_file( cons( mk_string( child ), cons( T, cons( T, NIL ))));
		    if( result != T )
		    {
			closedir(d);
			return result;
		    }
		}
	    }
	    closedir(d);
	    if( !remove( strvalue( path )))
	    {
		return T;
	    }
	}
    }
    return tail_error( mk_string( "File error - " ), cons( path, NIL ), errno );
}


