/*  This file is part of the 'AutoScheme' project.
 *  Copyright 2021 Steven Wiley <s.wiley@katchitek.com> 
 *  SPDX-License-Identifier: BSD-2-Clause
 */
#include "autoscheme.h"
pointer LOAD_MODULE__auto_scheme_cxr(pointer environment);
pointer LOAD_MODULE__auto_scheme_cxr(pointer environment)
{
pointer return_value = T;
return_value = autoscheme_eval(cons(mk_symbol("define-library"),cons(cons(mk_symbol("auto"),cons(mk_symbol("scheme"),cons(mk_symbol("cxr"),NIL))),cons(cons(mk_symbol("import"),cons(cons(mk_symbol("auto"),cons(mk_symbol("scheme"),cons(mk_symbol("base"),NIL))),cons(cons(mk_symbol("only"),cons(cons(mk_symbol("auto"),cons(mk_symbol("scheme"),cons(mk_symbol("base"),NIL))),cons(mk_symbol("caaaar"),cons(mk_symbol("caaar"),cons(mk_symbol("caaddr"),cons(mk_symbol("cadaar"),cons(mk_symbol("cadar"),cons(mk_symbol("cadddr"),cons(mk_symbol("cdaaar"),cons(mk_symbol("cdaar"),cons(mk_symbol("cdaddr"),cons(mk_symbol("cddaar"),cons(mk_symbol("cddar"),cons(mk_symbol("cddddr"),cons(mk_symbol("caaadr"),cons(mk_symbol("caadar"),cons(mk_symbol("caadr"),cons(mk_symbol("cadadr"),cons(mk_symbol("caddar"),cons(mk_symbol("caddr"),cons(mk_symbol("cdaadr"),cons(mk_symbol("cdadar"),cons(mk_symbol("cdadr"),cons(mk_symbol("cddadr"),cons(mk_symbol("cdddar"),cons(mk_symbol("cdddr"),NIL)))))))))))))))))))))))))),NIL))),cons(cons(mk_symbol("export"),cons(mk_symbol("caaaar"),cons(mk_symbol("caaar"),cons(mk_symbol("caaddr"),cons(mk_symbol("cadaar"),cons(mk_symbol("cadar"),cons(mk_symbol("cadddr"),cons(mk_symbol("cdaaar"),cons(mk_symbol("cdaar"),cons(mk_symbol("cdaddr"),cons(mk_symbol("cddaar"),cons(mk_symbol("cddar"),cons(mk_symbol("cddddr"),cons(mk_symbol("caaadr"),cons(mk_symbol("caadar"),cons(mk_symbol("caadr"),cons(mk_symbol("cadadr"),cons(mk_symbol("caddar"),cons(mk_symbol("caddr"),cons(mk_symbol("cdaadr"),cons(mk_symbol("cdadar"),cons(mk_symbol("cdadr"),cons(mk_symbol("cddadr"),cons(mk_symbol("cdddar"),cons(mk_symbol("cdddr"),NIL))))))))))))))))))))))))),NIL)))), environment);
return return_value;
}
