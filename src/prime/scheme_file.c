/*  This file is part of the 'AutoScheme' project.
 *  Copyright 2021 Steven Wiley <s.wiley@katchitek.com> 
 *  SPDX-License-Identifier: BSD-2-Clause
 */
#include "autoscheme.h"
pointer LOAD_MODULE__scheme_file(pointer environment);
/*  This file is part of the 'AutoScheme' project.
 *  Copyright 2021 Steven Wiley <s.wiley@katchitek.com> 
 *  SPDX-License-Identifier: BSD-2-Clause
 */
#include <unistd.h>
#include <errno.h>
#include <dirent.h> 
#include <limits.h>
#include <string.h>

pointer ff_file_exists_p( pointer args );

pointer ff_delete_file( pointer args );
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


 pointer LOAD_MODULE__scheme_file(pointer environment)
{
pointer return_value = T;
autoscheme_eval(T, environment);
autoscheme_eval(T, environment);
return_value = autoscheme_eval(cons(mk_symbol("define-library"),cons(cons(mk_symbol("scheme"),cons(mk_symbol("file"),NIL)),cons(cons(mk_symbol("export"),cons(mk_symbol("call-with-input-file"),cons(mk_symbol("call-with-output-file"),cons(mk_symbol("delete-file"),cons(mk_symbol("file-exists?"),cons(mk_symbol("open-binary-input-file"),cons(mk_symbol("open-binary-output-file"),cons(mk_symbol("open-input-file"),cons(mk_symbol("open-output-file"),cons(mk_symbol("with-input-from-file"),cons(mk_symbol("with-output-to-file"),NIL))))))))))),cons(cons(mk_symbol("begin"),cons(cons(mk_syntax(LOC_DEF0,"define"),cons(mk_symbol("call-with-input-file"),cons(mk_operation(LOC_CALL_INFILE0,&NIL),NIL))),cons(cons(mk_syntax(LOC_DEF0,"define"),cons(mk_symbol("call-with-output-file"),cons(mk_operation(LOC_CALL_OUTFILE0,&NIL),NIL))),cons(cons(mk_syntax(LOC_DEF0,"define"),cons(mk_symbol("delete-file"),cons(mk_function(ff_delete_file,&NIL),NIL))),cons(cons(mk_syntax(LOC_DEF0,"define"),cons(mk_symbol("file-exists?"),cons(mk_function(ff_file_exists_p,&NIL),NIL))),cons(cons(mk_syntax(LOC_DEF0,"define"),cons(mk_symbol("open-binary-input-file"),cons(mk_operation(LOC_OPEN_BINFILE,&NIL),NIL))),cons(cons(mk_syntax(LOC_DEF0,"define"),cons(mk_symbol("open-binary-output-file"),cons(mk_operation(LOC_OPEN_BOUTFILE,&NIL),NIL))),cons(cons(mk_syntax(LOC_DEF0,"define"),cons(mk_symbol("open-input-file"),cons(mk_operation(LOC_OPEN_INFILE,&NIL),NIL))),cons(cons(mk_syntax(LOC_DEF0,"define"),cons(mk_symbol("open-output-file"),cons(mk_operation(LOC_OPEN_OUTFILE,&NIL),NIL))),cons(cons(mk_syntax(LOC_DEF0,"define"),cons(mk_symbol("with-input-from-file"),cons(mk_operation(LOC_WITH_INFILE0,&NIL),NIL))),cons(cons(mk_syntax(LOC_DEF0,"define"),cons(mk_symbol("with-output-to-file"),cons(mk_operation(LOC_WITH_OUTFILE0,&NIL),NIL))),NIL))))))))))),NIL)))), environment);
return return_value;
}
