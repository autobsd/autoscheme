/*  This file is part of the 'AutoScheme' project.
 *  Copyright 2021 Steven Wiley <s.wiley@katchitek.com> 
 *  SPDX-License-Identifier: BSD-2-Clause
 */
pointer ff_path_absolute_p( pointer args )
{
    pointer path = car( args );

    if( strvalue( path )[0] == '/' )
	return T;

    return F;
}


pointer ff_path_make_absolute( pointer args )
{
    if( !is_pair( cdr( args )) && istrue( ff_path_absolute_p( args ))) 
	return car( args );

    {
	pointer relative_path = car( args );
	const char *relative_str = strvalue( relative_path );
	size_t relative_len = strlength( relative_path );

	pointer parent;
	size_t parent_len;

	pointer absolute_path;


	foreign_function ff_current_directory;

	if( is_pair( cdr( args ))) 
	    parent = ff_path_make_absolute( cons( cadr( args ), NIL ));
	else
	    parent = ff_current_directory( NIL );

	parent_len = strlength( parent );

	if( !parent_len || strvalue( parent )[ parent_len - 1 ] != '/' )
	    parent_len++;

	if( relative_str[0] == '/' ) 
	{
	    relative_str++;
	    relative_len--;
	}

	absolute_path = mk_counted_string( "", parent_len + relative_len);
	memcpy( strvalue( absolute_path ), strvalue( parent ), strlength( parent ));
	strvalue( absolute_path )[ parent_len - 1 ] = '/';
	memcpy( strvalue( absolute_path ) + parent_len, relative_str, relative_len + 1 );
	
	return absolute_path;
    }
}




pointer ff_path_directory( pointer args )
{
    foreign_function ff_current_directory;
    pointer path = car( args );
    pointer directory;

    char *s = strvalue( path );
    size_t length = strlength( path );
    
    size_t i = length;

    while( i )
    {
	if( s[i - 1] == '/' ) break;
	i--;
    }
    
    if( i )
    {
    	directory = mk_counted_string( "", i );
	memcpy( strvalue( directory ), strvalue( path ), i);
	strvalue( path )[ i ] = '\0';
    }
    else
	directory = ff_current_directory( NIL );

    return directory;
}
