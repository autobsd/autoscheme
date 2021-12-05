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
#include <unistd.h>
#include <errno.h>

static pointer ff_file_exists_p( pointer args );

static pointer ff_delete_file( pointer args );
 /*  This file is part of the 'AutoScheme' project.
 *  Copyright 2021 Steven Wiley <s.wiley@katchitek.com> 
 *  SPDX-License-Identifier: BSD-2-Clause
 */
static pointer ff_file_exists_p( pointer args )
{
    pointer file_name = car( args );

    if( access( strvalue( file_name ), F_OK ) == 0 ) 
	return T;

    return F;
}


static pointer ff_delete_file( pointer args )
{
    pointer path = car( args );
    int force = is_pair( cdr( args )) && istrue( cadr( args ));

    if( !remove( strvalue( path )) || ( force && errno == ENOENT ))
    {
	return T;
    }
    return tail_error( mk_string( "File error - " ), cons( path, NIL ), errno );
}
 pointer LOAD_MODULE__auto_scheme_file(pointer environment)
{
pointer return_value = T;
autoscheme_eval(T, environment);
autoscheme_eval(T, environment);
return_value = autoscheme_eval(cons(mk_symbol("define-library"),cons(cons(mk_symbol("auto"),cons(mk_symbol("scheme"),cons(mk_symbol("file"),NIL))),cons(cons(mk_symbol("export"),NIL),cons(cons(mk_symbol("begin"),cons(cons(mk_syntax(LOC_DEF0,"define"),cons(mk_symbol("call-with-input-file"),cons(mk_operation(LOC_CALL_INFILE0,&NIL),NIL))),cons(cons(mk_syntax(LOC_DEF0,"define"),cons(mk_symbol("call-with-output-file"),cons(mk_operation(LOC_CALL_OUTFILE0,&NIL),NIL))),cons(cons(mk_syntax(LOC_DEF0,"define"),cons(mk_symbol("delete-file"),cons(mk_function(ff_delete_file,&NIL),NIL))),cons(cons(mk_syntax(LOC_DEF0,"define"),cons(mk_symbol("file-exists?"),cons(mk_function(ff_file_exists_p,&NIL),NIL))),cons(cons(mk_syntax(LOC_DEF0,"define"),cons(mk_symbol("open-binary-input-file"),cons(mk_operation(LOC_OPEN_BINFILE,&NIL),NIL))),cons(cons(mk_syntax(LOC_DEF0,"define"),cons(mk_symbol("open-binary-output-file"),cons(mk_operation(LOC_OPEN_BOUTFILE,&NIL),NIL))),cons(cons(mk_syntax(LOC_DEF0,"define"),cons(mk_symbol("open-input-file"),cons(mk_operation(LOC_OPEN_INFILE,&NIL),NIL))),cons(cons(mk_syntax(LOC_DEF0,"define"),cons(mk_symbol("open-output-file"),cons(mk_operation(LOC_OPEN_OUTFILE,&NIL),NIL))),cons(cons(mk_syntax(LOC_DEF0,"define"),cons(mk_symbol("with-input-from-file"),cons(mk_operation(LOC_WITH_INFILE0,&NIL),NIL))),cons(cons(mk_syntax(LOC_DEF0,"define"),cons(mk_symbol("with-output-to-file"),cons(mk_operation(LOC_WITH_OUTFILE0,&NIL),NIL))),NIL))))))))))),NIL)))), environment);
return return_value;
}
