/*  This file is part of the 'AutoScheme' project.
 *  Copyright 2021 Steven Wiley <s.wiley@katchitek.com> 
 *  SPDX-License-Identifier: BSD-2-Clause
 */
static pointer ff_rename_file( pointer args )
{
    pointer old_name = car( args );
    pointer new_name = cadr( args );

    int replace = is_pair( cddr( args )) && istrue( caddr( args ));


    if( !replace && istrue( ff_file_exists_p( cons( new_name, NIL ))))
	return tail_error( mk_string( "File error - " ), cons( new_name, NIL ), EEXIST );

    if( !rename( strvalue( old_name ), strvalue( new_name )))
	return T;

    if( replace && errno == EEXIST )
    {
	pointer result = ff_delete_file( cons( new_name, cons( T, cons( T, NIL ))));

	if( result != T )
	    return result;

	if( !rename( strvalue( old_name ), strvalue( new_name )))
	    return T;
    }

    return tail_error( mk_string( "File error - " ), args, errno );
}

#define BUF_SIZE 65536 /* 2 bytes */
static pointer ff_copy_file( pointer args )
{
    pointer result = T;
    pointer src_path = car( args );
    pointer dest_path = cadr( args );

    int recurse = is_pair( cddr( args )) && istrue( caddr( args ));
    int replace = is_pair( cddr( args )) && is_pair( cdr( cddr( args ))) && istrue( cadr( cddr( args )));
    size_t buf_size = is_pair( cddr( args )) && is_pair( cdr( cddr( args ))) && is_pair( cddr( cddr( args ))) && is_integer( caddr( cddr( args ))) ? ivalue( caddr( cddr( args ))) : BUF_SIZE ;

    struct stat statbuf;
    FILE *input_file = NULL, *output_file = NULL;
    void *buf = NULL;

    if( stat( strvalue( src_path ), &statbuf )) goto SRC_ERR;

    if( S_ISDIR( statbuf.st_mode ))
    {
	mode_t mode = S_IRWXU | S_IRWXG | S_IRWXO;

	if( mkdir( strvalue( dest_path ), mode ))
	{
	    if( errno != EEXIST || !replace ) goto DEST_ERR;

	    result = ff_delete_file( cons( dest_path, cons( T, cons( T, NIL ))));

	    if( result != T ) goto DEST_ERR;

	    if( mkdir( strvalue( dest_path ), mode )) goto DEST_ERR;
	}

	if( recurse )
	{
	    DIR *d;
	    struct dirent *dir;
	    char src_str[PATH_MAX];
	    char dest_str[PATH_MAX];

	    errno = 0;
	    d = opendir( strvalue( src_path ));

	    if( d ) 
	    {
		while(( dir = readdir( d )) != NULL ) 
		{
		    if( strcmp( dir->d_name, "." ) &&
			strcmp( dir->d_name, ".." ))
		    {
			strcpy( src_str, strvalue( src_path ));
			strcpy( dest_str, strvalue( dest_path ));
			src_str[ strlength( src_path )] = '/';
			dest_str[ strlength( dest_path )] = '/';
			strcpy( src_str + strlength( src_path ) + 1, dir->d_name);
			strcpy( dest_str + strlength( dest_path ) + 1, dir->d_name);

			src_str[ strlength( src_path ) + 1 + strlen( dir->d_name ) ] = '\0';
			dest_str[ strlength( dest_path ) + 1 + strlen( dir->d_name ) ] = '\0';

			result = ff_copy_file( cons( mk_string( src_str ), cons( mk_string( dest_str ), cons( T, cons( T, NIL )))));

			if( result != T ) break;
		    }
		}
		closedir( d );
	    }
	    if( result != T ) goto END;

	    if( errno ) goto DEST_ERR;
	}
    }
    else
    {
	size_t n;
	buf = malloc( buf_size );

	if( !( input_file = fopen( strvalue( src_path ), "rb" ))) goto SRC_ERR;

	if( !( output_file = fopen( strvalue( dest_path ), "wb" ))) 
	{
	    if( errno == EEXIST && replace )
	    {
		result = ff_delete_file( cons( dest_path, cons( T, cons( T, NIL ))));

		if( result != T ) goto DEST_ERR;

		if( !( output_file = fopen( strvalue( dest_path ), "wb" ))) goto DEST_ERR;
	    }
	    else goto DEST_ERR;
	}

	do
	{
	    n = fread( buf, 1, buf_size, input_file );
	    if( ferror( input_file )) goto SRC_ERR;

	    if( n && fwrite( buf, 1, n, output_file ) != n )
		    goto DEST_ERR;
	} 
	while( !feof( input_file ));
    }

END:
    if( buf ) free( buf );

    if( input_file && fclose( input_file ) == EOF && result == T )
	result = tail_error( mk_string( "File error - " ), cons( src_path, NIL ), errno ); 

    if( output_file && fclose( output_file ) == EOF && result == T )
	result = tail_error( mk_string( "File error - " ), cons( dest_path, NIL ), errno );

    if( result == T && chmod( strvalue( dest_path ), statbuf.st_mode ) )
	result = tail_error( mk_string( "File error - " ), cons( dest_path, NIL ), errno );

    return result;

SRC_ERR:
    result = tail_error( mk_string( "File error - " ), cons( src_path, NIL ), errno ); 
    goto END; 

DEST_ERR:
    result = tail_error( mk_string( "File error - " ), cons( dest_path, NIL), errno ); 
    goto END;
}

