/*  This file is part of the 'AutoScheme' project.
 *  Copyright 2021 Steven Wiley <s.wiley@katchitek.com> 
 *  SPDX-License-Identifier: BSD-2-Clause
 */
#include "autoscheme.h"
pointer LOAD_MODULE__scheme_cxr(pointer environment);
pointer LOAD_MODULE__scheme_cxr(pointer environment)
{
pointer return_value = T;
pointer library = make_environment( NIL );
pointer name = mk_symbol( "(scheme cxr)" );
car( environment ) = cons( cons( name, library ), car( environment ));
scheme_register_operation(LOC_CAAAR, "caaar", library);
scheme_register_operation(LOC_CAADR, "caadr", library);
scheme_register_operation(LOC_CADAR, "cadar", library);
scheme_register_operation(LOC_CADDR, "caddr", library);
scheme_register_operation(LOC_CDAAR, "cdaar", library);
scheme_register_operation(LOC_CDADR, "cdadr", library);
scheme_register_operation(LOC_CDDAR, "cddar", library);
scheme_register_operation(LOC_CDDDR, "cdddr", library);
scheme_register_operation(LOC_CAAAAR, "caaaar", library);
scheme_register_operation(LOC_CAAADR, "caaadr", library);
scheme_register_operation(LOC_CAADAR, "caadar", library);
scheme_register_operation(LOC_CAADDR, "caaddr", library);
scheme_register_operation(LOC_CADAAR, "cadaar", library);
scheme_register_operation(LOC_CADADR, "cadadr", library);
scheme_register_operation(LOC_CADDAR, "caddar", library);
scheme_register_operation(LOC_CADDDR, "cadddr", library);
scheme_register_operation(LOC_CDAAAR, "cdaaar", library);
scheme_register_operation(LOC_CDAADR, "cdaadr", library);
scheme_register_operation(LOC_CDADAR, "cdadar", library);
scheme_register_operation(LOC_CDADDR, "cdaddr", library);
scheme_register_operation(LOC_CDDAAR, "cddaar", library);
scheme_register_operation(LOC_CDDADR, "cddadr", library);
scheme_register_operation(LOC_CDDDAR, "cdddar", library);
scheme_register_operation(LOC_CDDDDR, "cddddr", library);
return_value = autoscheme_eval(T, environment);
return return_value;
}
