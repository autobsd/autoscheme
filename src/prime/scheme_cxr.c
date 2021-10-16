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
scheme_register_proc(OP_CAAAR, "caaar", library);
scheme_register_proc(OP_CAADR, "caadr", library);
scheme_register_proc(OP_CADAR, "cadar", library);
scheme_register_proc(OP_CADDR, "caddr", library);
scheme_register_proc(OP_CDAAR, "cdaar", library);
scheme_register_proc(OP_CDADR, "cdadr", library);
scheme_register_proc(OP_CDDAR, "cddar", library);
scheme_register_proc(OP_CDDDR, "cdddr", library);
scheme_register_proc(OP_CAAAAR, "caaaar", library);
scheme_register_proc(OP_CAAADR, "caaadr", library);
scheme_register_proc(OP_CAADAR, "caadar", library);
scheme_register_proc(OP_CAADDR, "caaddr", library);
scheme_register_proc(OP_CADAAR, "cadaar", library);
scheme_register_proc(OP_CADADR, "cadadr", library);
scheme_register_proc(OP_CADDAR, "caddar", library);
scheme_register_proc(OP_CADDDR, "cadddr", library);
scheme_register_proc(OP_CDAAAR, "cdaaar", library);
scheme_register_proc(OP_CDAADR, "cdaadr", library);
scheme_register_proc(OP_CDADAR, "cdadar", library);
scheme_register_proc(OP_CDADDR, "cdaddr", library);
scheme_register_proc(OP_CDDAAR, "cddaar", library);
scheme_register_proc(OP_CDDADR, "cddadr", library);
scheme_register_proc(OP_CDDDAR, "cdddar", library);
scheme_register_proc(OP_CDDDDR, "cddddr", library);
return_value = autoscheme_eval(T, environment);
return return_value;
}
