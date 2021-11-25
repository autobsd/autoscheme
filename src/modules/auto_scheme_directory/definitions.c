/*  This file is part of the 'AutoScheme' project.
 *  Copyright 2021 Steven Wiley <s.wiley@katchitek.com> 
 *  SPDX-License-Identifier: BSD-2-Clause
 */
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

    char *path = "./";
    int show_hidden = 0;

    if( is_pair( args ))
    {
	if( is_string( car( args )))
	    path = strvalue( car( args ));
	else if( car( args ) == T )
	    show_hidden = 1;

	if( is_pair( cdr( args )) && istrue( cadr( args )))
	    show_hidden = 1;
    }

    d = opendir( path );

    if( d ) 
    {
	while(( dir = readdir( d )) != NULL ) 
	{
	    if( strcmp( dir->d_name, ".") &&
		strcmp( dir->d_name, "..") &&
		( dir->d_name[0] != '.' || show_hidden ))
		result = cons( mk_string( dir->d_name ), result );
	}
	closedir(d);
    }
    else
    {
	char *error_string;
	switch( errno ) 
	{
	case EACCES :
	    error_string = "Directory error - permission denied";
	    break; 
	
	case EBADF :
	    error_string = "Directory error - invalid read file descriptor for";
	    break; 

	case EMFILE :
	case ENFILE :
	    error_string = "Directory error - open file descriptor limit exceeded";
	    break;

	case ENOENT :
	    error_string = "Directory error - file does not exist";
	    break;

	case ENOMEM :
	    error_string = "Directory error - insufficient memory to open";
	    break;

	case ENOTDIR :
	    error_string = "Directory error - file is not a directory";
	    break;

	default : 
	    error_string = "Directory error - unable to open directory for reading";
	}

	return tail_error( mk_string( error_string ), cons( mk_string( path ), NIL ));
    }
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
