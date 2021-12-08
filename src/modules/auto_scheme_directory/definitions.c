/*  This file is part of the 'AutoScheme' project.
 *  Copyright 2021 Steven Wiley <s.wiley@katchitek.com> 
 *  SPDX-License-Identifier: BSD-2-Clause
 */
pointer ff_create_directory( pointer args )
{
    pointer path = car( args );
    mode_t mode = S_IRWXU | S_IRWXG | S_IRWXO;
    int make_parents = is_pair( cdr( args )) && istrue( cadr( args ));

    if( strlength( path ) >= PATH_MAX )
	return tail_error( mk_string( "File error - " ), cons( path, NIL ), ENAMETOOLONG);	

    if( make_parents )
    {
	char *ptr;
	char path_str[PATH_MAX];

	strcpy( path_str, strvalue( path ));

	for( ptr = path_str + 1; *ptr != '\0'; ptr++ ) 
	{
	    if( *ptr == '/') 
	    {
		*ptr = '\0';

		if( mkdir(path_str, mode ) && errno != EEXIST )
		    return tail_error( mk_string( "File error - " ), cons( mk_string( path_str ), NIL ), errno);
		*ptr = '/';
	    }
	}   
    }

    if( mkdir( strvalue( path ), mode ) == 0 || ( make_parents && errno == EEXIST ))
	return T;

    return tail_error( mk_string( "File error - " ), cons( path, NIL ), errno);
}

pointer ff_current_directory( pointer args )
{
    if( is_pair( args ) && ( chdir( strvalue( car( args ))) == -1 ))
    {
	    /* throw exception */
    }

    {
	char cwd[ PATH_MAX ];
	size_t cwd_length;

	if( getcwd( cwd, sizeof( cwd )) == NULL) 
	{
	    /* perror("getcwd() error"); */
	    /* exit( 1 ); */
	}

	cwd_length = strlen( cwd );
	cwd[ cwd_length ] = '/';
	cwd[ cwd_length + 1 ] = '\0';
    
	return mk_string( cwd );
    }
}

pointer ff_directory_files( pointer args )
{
    pointer result = NIL;

    DIR *d;
    struct dirent *dir;

    char *path = strvalue( car( args ));
    int show_hidden = is_pair( cdr( args )) && istrue( cadr( args ));

    errno = 0;

    d = opendir( path );
    if( d ) 
    {
	while(( dir = readdir( d )) != NULL ) 
	{
	    if( strcmp( dir->d_name, "." ) &&
		strcmp( dir->d_name, ".." ) &&
		( dir->d_name[0] != '.' || show_hidden ))
		result = cons( mk_string( dir->d_name ), result );
	}
	closedir( d );
    }
    if( errno ) 
	result = tail_error( mk_string( "File error - " ), cons( mk_string( path ), NIL ), errno);    
    
    return result;
}

pointer ff_directory_p( pointer args )
{
    char *path = strvalue( car( args ));

   struct stat statbuf;
   if( stat( path, &statbuf ) != 0 )
       return F;
   
   if( S_ISDIR( statbuf.st_mode ))
       return T;

   return F;
}


