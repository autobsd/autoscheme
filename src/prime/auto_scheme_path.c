/*  This file is part of the 'AutoScheme' project.
 *  Copyright 2021 Steven Wiley <s.wiley@katchitek.com> 
 *  SPDX-License-Identifier: BSD-2-Clause
 */
#include "autoscheme.h"
pointer LOAD_MODULE__auto_scheme_path(pointer environment);
/*  This file is part of the 'AutoScheme' project.
 *  Copyright 2021 Steven Wiley <s.wiley@katchitek.com> 
 *  SPDX-License-Identifier: BSD-2-Clause
 */
#include <string.h>

pointer ff_path_absolute_p( pointer args );
pointer ff_path_make_absolute( pointer args );
pointer ff_path_directory( pointer args );
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
    if( istrue( ff_path_absolute_p( args ))) 
	return car( args );

    {
	pointer relative_path = car( args );
	pointer absolute_path;
	pointer parent_dir;

	size_t length;

	foreign_function ff_current_directory;

	if( is_pair( cdr( args ))) 
	    parent_dir = cadr( args );
	else
	    parent_dir = ff_current_directory( NIL );

	length = strlength( parent_dir ) + strlength( relative_path );

	absolute_path = mk_counted_string( "", length );
    
	memcpy( strvalue( absolute_path ), strvalue( parent_dir ), strlength( parent_dir ));
	memcpy( strvalue( absolute_path ) + strlength( parent_dir ), strvalue( relative_path ), strlength( relative_path ) + 1);
	
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
 pointer LOAD_MODULE__auto_scheme_path(pointer environment)
{
pointer return_value = T;
return_value = autoscheme_eval(T, environment);
return_value = autoscheme_eval(T, environment);
return_value = autoscheme_eval(cons(mk_symbol("define-library"),cons(cons(mk_symbol("auto"),cons(mk_symbol("scheme"),cons(mk_symbol("path"),NIL))),cons(cons(mk_symbol("export"),cons(mk_symbol("path-directory"),cons(mk_symbol("path-absolute?"),cons(mk_symbol("path-make-absolute"),NIL)))),cons(cons(mk_symbol("begin"),cons(cons(mk_syntax(LOC_DEF0,"define"),cons(mk_symbol("path-absolute?"),cons(mk_function(ff_path_absolute_p,&NIL),NIL))),cons(cons(mk_syntax(LOC_DEF0,"define"),cons(mk_symbol("path-make-absolute"),cons(mk_function(ff_path_make_absolute,&NIL),NIL))),cons(cons(mk_syntax(LOC_DEF0,"define"),cons(mk_symbol("path-directory"),cons(mk_function(ff_path_directory,&NIL),NIL))),NIL)))),NIL)))), environment);
return return_value;
}
