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
