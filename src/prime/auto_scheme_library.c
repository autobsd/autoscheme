/*  This file is part of the 'AutoScheme' project.
 *  Copyright 2021 Steven Wiley <s.wiley@katchitek.com> 
 *  SPDX-License-Identifier: BSD-2-Clause
 */
#include "autoscheme.h"
int LOAD_MODULE__auto_scheme_library(void);
int LOAD_MODULE__auto_scheme_library()
{
scheme_eval(cons(mk_symbol("begin"),cons(cons(mk_symbol("begin"),NIL),NIL)));
return 0;
}