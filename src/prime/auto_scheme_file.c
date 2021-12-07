/*  This file is part of the 'AutoScheme' project.
 *  Copyright 2021 Steven Wiley <s.wiley@katchitek.com> 
 *  SPDX-License-Identifier: BSD-2-Clause
 */
#include "autoscheme.h"
pointer LOAD_MODULE__auto_scheme_file(pointer environment);
/*  This file is part of the 'AutoScheme' project.
 *  Copyright 2021 Steven Wiley <s.wiley@katchitek.com> 
 *  SPDX-License-Identifier: BSD-2-Clause
 */
#include <stdlib.h>
#include <sys/stat.h>

foreign_function ff_delete_file;
foreign_function ff_file_exists_p;

static pointer ff_rename_file( pointer args );
static pointer ff_copy_file( pointer args );
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

    if( !stat( strvalue( src_path ), &statbuf )) goto SRC_ERR;

    /* if( S_ISDIR( statbuf.st_mode )) */
    /* { */
    /* 	result = ff_copy_directory( args ); */
    /* 	if( result != T )  */
    /* 	    return result; */
    /* } */
    /* else */
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
	result = tail_error( mk_string( "File error - " ), src_path, errno ); 

    if( output_file && fclose( output_file ) == EOF && result == T )
	result = tail_error( mk_string( "File error - " ), dest_path, errno );

    if( result == T && !chmod( strvalue( dest_path ), statbuf.st_mode ) )
	result = tail_error( mk_string( "File error - " ), dest_path, errno );

    return result;

SRC_ERR:
    result = tail_error( mk_string( "File error - " ), src_path, errno ); 
    goto END; 

DEST_ERR:
    result = tail_error( mk_string( "File error - " ), dest_path, errno ); 
    goto END;
}

 pointer LOAD_MODULE__auto_scheme_file(pointer environment)
{
pointer return_value = T;
autoscheme_eval(T, environment);
autoscheme_eval(T, environment);
return_value = autoscheme_eval(cons(mk_symbol("define-library"),cons(cons(mk_symbol("auto"),cons(mk_symbol("scheme"),cons(mk_symbol("file"),NIL))),cons(cons(mk_symbol("export"),cons(mk_symbol("open-binary-input-output-file"),cons(mk_symbol("open-input-output-file"),cons(mk_symbol("rename-file"),NIL)))),cons(cons(mk_symbol("begin"),cons(cons(mk_syntax(LOC_DEF0,"define"),cons(mk_symbol("copy-file"),cons(mk_function(ff_copy_file,&NIL),NIL))),cons(cons(mk_syntax(LOC_DEF0,"define"),cons(mk_symbol("open-binary-input-output-file"),cons(mk_operation(LOC_OPEN_BINOUTFILE,&NIL),NIL))),cons(cons(mk_syntax(LOC_DEF0,"define"),cons(mk_symbol("open-input-output-file"),cons(mk_operation(LOC_OPEN_INOUTFILE,&NIL),NIL))),cons(cons(mk_syntax(LOC_DEF0,"define"),cons(mk_symbol("rename-file"),cons(mk_function(ff_rename_file,&NIL),NIL))),NIL))))),NIL)))), environment);
return return_value;
}
