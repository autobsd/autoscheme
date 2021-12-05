/*  This file is part of the 'AutoScheme' project.
 *  Copyright 2021 Steven Wiley <s.wiley@katchitek.com> 
 *  SPDX-License-Identifier: BSD-2-Clause
 */
#include "autoscheme.h"
pointer LOAD_MODULE__auto_scheme_directory(pointer environment);
/*  This file is part of the 'AutoScheme' project.
 *  Copyright 2021 Steven Wiley <s.wiley@katchitek.com> 
 *  SPDX-License-Identifier: BSD-2-Clause
 */
#include <limits.h>
#include <unistd.h>
#include <string.h>

#include <dirent.h> 

#include <sys/types.h>
#include <sys/stat.h>
#include <unistd.h>

#include <errno.h>

pointer ff_create_directory( pointer args );
pointer ff_current_directory( pointer args );
pointer ff_directory_files( pointer args );
pointer ff_directory_p( pointer args );

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
	return tail_error( mk_string( "File error - " ), cons( mk_string( path ), NIL ), errno);
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


 pointer LOAD_MODULE__auto_scheme_directory(pointer environment)
{
pointer return_value = T;
autoscheme_eval(T, environment);
autoscheme_eval(T, environment);
return_value = autoscheme_eval(cons(mk_symbol("define-library"),cons(cons(mk_symbol("auto"),cons(mk_symbol("scheme"),cons(mk_symbol("directory"),NIL))),cons(cons(mk_symbol("export"),cons(mk_symbol("create-directory"),cons(mk_symbol("current-directory"),cons(mk_symbol("directory-files"),cons(mk_symbol("directory?"),NIL))))),cons(cons(mk_symbol("begin"),cons(cons(mk_syntax(LOC_DEF0,"define"),cons(mk_symbol("create-directory"),cons(mk_function(ff_create_directory,&NIL),NIL))),cons(cons(mk_syntax(LOC_DEF0,"define"),cons(mk_symbol("current-directory"),cons(mk_function(ff_current_directory,&NIL),NIL))),cons(cons(mk_syntax(LOC_DEF0,"define"),cons(mk_symbol("directory-files"),cons(mk_function(ff_directory_files,&NIL),NIL))),cons(cons(mk_syntax(LOC_DEF0,"define"),cons(mk_symbol("directory?"),cons(mk_function(ff_directory_p,&NIL),NIL))),NIL))))),NIL)))), environment);
return return_value;
}
