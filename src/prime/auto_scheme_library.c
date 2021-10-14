/*  This file is part of the 'AutoScheme' project.
 *  Copyright 2021 Steven Wiley <s.wiley@katchitek.com> 
 *  SPDX-License-Identifier: BSD-2-Clause
 */
#include "autoscheme.h"
pointer LOAD_MODULE__auto_scheme_library(pointer environment);
/*  This file is part of the 'AutoScheme' project.
 *  Copyright 2021 Steven Wiley <s.wiley@katchitek.com> 
 *  SPDX-License-Identifier: BSD-2-Clause
 */
pointer INITIALIZE_LIBRARY__auto_scheme_environment( pointer environment );
pointer INITIALIZE_LIBRARY__auto_scheme_base( pointer environment );
 pointer LOAD_MODULE__auto_scheme_library(pointer environment)
{
pointer return_value = T;
/*  This file is part of the 'AutoScheme' project.
 *  Copyright 2021 Steven Wiley <s.wiley@katchitek.com> 
 *  SPDX-License-Identifier: BSD-2-Clause
 */
pointer library = make_environment( NIL );
pointer name = mk_symbol( "(auto scheme library)" );

pointer binding = cons( name, library );

car( environment ) = cons( binding, car( environment ));  
INITIALIZE_LIBRARY__auto_scheme_environment( library );
INITIALIZE_LIBRARY__auto_scheme_base( library );
environment = library;




 return_value = autoscheme_eval(T, environment);
return_value = autoscheme_eval(T, environment);
return_value = autoscheme_eval(T, environment);
return_value = autoscheme_eval(cons(mk_symbol("begin"),cons(cons(mk_symbol("define-macro"),cons(cons(mk_symbol("define-library"),cons(mk_symbol("name"),mk_symbol("declarations"))),cons(cons(mk_symbol("let"),cons(cons(cons(mk_symbol("name-symbol"),cons(cons(mk_symbol("string->symbol"),cons(cons(mk_symbol("object->string"),cons(mk_symbol("name"),NIL)),NIL)),NIL)),cons(cons(mk_symbol("quoted-declarations"),cons(cons(mk_symbol("map"),cons(cons(mk_symbol("lambda"),cons(cons(mk_symbol("declaration"),NIL),cons(cons( mk_symbol("quasiquote" ),cons(cons(mk_symbol("quote"),cons(cons( mk_symbol("unquote" ),cons(mk_symbol("declaration"),NIL)),NIL)),NIL)),NIL))),cons(mk_symbol("declarations"),NIL))),NIL)),NIL)),cons(cons(mk_symbol("apply"),cons(mk_symbol("environment-define!"),cons(cons(mk_symbol("cons"),cons(cons(mk_symbol("expansion-environment"),NIL),cons(cons(mk_symbol("cons"),cons(mk_symbol("name-symbol"),cons(cons(mk_symbol("list"),cons(cons(mk_symbol("library-eval"),cons(mk_symbol("declarations"),cons(cons(mk_symbol("expansion-environment"),NIL),NIL))),NIL)),NIL))),NIL))),NIL))),NIL))),NIL))),cons(cons(mk_symbol("define-macro"),cons(cons(mk_symbol("import"),mk_symbol("sets")),cons(cons(mk_symbol("apply"),cons(mk_symbol("environment-import-sets!"),cons(cons(mk_symbol("cons"),cons(cons(mk_symbol("expansion-environment"),NIL),cons(cons(mk_symbol("cons"),cons(cons(mk_symbol("expansion-environment"),NIL),cons(mk_symbol("sets"),NIL))),NIL))),NIL))),NIL))),NIL))), environment);
return_value = autoscheme_eval(cons(mk_symbol("begin"),cons(cons(mk_symbol("define"),cons(mk_symbol("object->string"),cons(cons(mk_symbol("lambda"),cons(cons(mk_symbol("object"),NIL),cons(cons(mk_symbol("let"),cons(cons(cons(mk_symbol("string-port"),cons(cons(mk_symbol("open-output-string"),NIL),NIL)),NIL),cons(cons(mk_symbol("write"),cons(mk_symbol("object"),cons(mk_symbol("string-port"),NIL))),cons(cons(mk_symbol("let"),cons(cons(cons(mk_symbol("output-string"),cons(cons(mk_symbol("get-output-string"),cons(mk_symbol("string-port"),NIL)),NIL)),NIL),cons(cons(mk_symbol("close-output-port"),cons(mk_symbol("string-port"),NIL)),cons(mk_symbol("output-string"),NIL)))),NIL)))),NIL))),NIL))),cons(cons(mk_symbol("define"),cons(mk_symbol("environment-import-sets!"),cons(cons(mk_symbol("lambda"),cons(cons(mk_symbol("target"),cons(mk_symbol("source"),mk_symbol("sets"))),cons(cons(mk_symbol("letrec"),cons(cons(cons(mk_symbol("process-import-set"),cons(cons(mk_symbol("lambda"),cons(cons(mk_symbol("set"),NIL),cons(cons(mk_symbol("if"),cons(cons(mk_symbol("not"),cons(cons(mk_symbol("pair?"),cons(mk_symbol("set"),NIL)),NIL)),cons(cons(mk_symbol("error"),cons(mk_string("improper import-set:"),cons(mk_symbol("set"),NIL))),cons(cons(mk_symbol("cond"),cons(cons(cons(mk_symbol("equal?"),cons(cons(mk_symbol("car"),cons(mk_symbol("set"),NIL)),cons(cons( mk_symbol("quote" ),cons(mk_symbol("only"),NIL)),NIL))),cons(cons(mk_symbol("apply"),cons(mk_symbol("environment-only"),cons(cons(mk_symbol("cons"),cons(cons(mk_symbol("process-import-set"),cons(cons(mk_symbol("cadr"),cons(mk_symbol("set"),NIL)),NIL)),cons(cons(mk_symbol("cddr"),cons(mk_symbol("set"),NIL)),NIL))),NIL))),NIL)),cons(cons(cons(mk_symbol("equal?"),cons(cons(mk_symbol("car"),cons(mk_symbol("set"),NIL)),cons(cons( mk_symbol("quote" ),cons(mk_symbol("except"),NIL)),NIL))),cons(cons(mk_symbol("apply"),cons(mk_symbol("environment-except"),cons(cons(mk_symbol("cons"),cons(cons(mk_symbol("process-import-set"),cons(cons(mk_symbol("cadr"),cons(mk_symbol("set"),NIL)),NIL)),cons(cons(mk_symbol("cddr"),cons(mk_symbol("set"),NIL)),NIL))),NIL))),NIL)),cons(cons(cons(mk_symbol("equal?"),cons(cons(mk_symbol("car"),cons(mk_symbol("set"),NIL)),cons(cons( mk_symbol("quote" ),cons(mk_symbol("prefix"),NIL)),NIL))),cons(cons(mk_symbol("environment-prefix"),cons(cons(mk_symbol("process-import-set"),cons(cons(mk_symbol("cadr"),cons(mk_symbol("set"),NIL)),NIL)),cons(cons(mk_symbol("caddr"),cons(mk_symbol("set"),NIL)),NIL))),NIL)),cons(cons(cons(mk_symbol("equal?"),cons(cons(mk_symbol("car"),cons(mk_symbol("set"),NIL)),cons(cons( mk_symbol("quote" ),cons(mk_symbol("rename"),NIL)),NIL))),cons(cons(mk_symbol("apply"),cons(mk_symbol("environment-rename"),cons(cons(mk_symbol("cons"),cons(cons(mk_symbol("process-import-set"),cons(cons(mk_symbol("cadr"),cons(mk_symbol("set"),NIL)),NIL)),cons(cons(mk_symbol("cddr"),cons(mk_symbol("set"),NIL)),NIL))),NIL))),NIL)),cons(cons(mk_symbol("else"),cons(cons(mk_symbol("environment-ref"),cons(mk_symbol("source"),cons(cons(mk_symbol("string->symbol"),cons(cons(mk_symbol("object->string"),cons(mk_symbol("set"),NIL)),NIL)),NIL))),NIL)),NIL)))))),NIL)))),NIL))),NIL)),NIL),cons(cons(mk_symbol("for-each"),cons(cons(mk_symbol("lambda"),cons(cons(mk_symbol("set"),NIL),cons(cons(mk_symbol("environment-import!"),cons(mk_symbol("target"),cons(cons(mk_symbol("process-import-set"),cons(mk_symbol("set"),NIL)),NIL))),NIL))),cons(mk_symbol("sets"),NIL))),cons(mk_symbol("target"),NIL)))),NIL))),NIL))),cons(cons(mk_symbol("define"),cons(mk_symbol("library-eval"),cons(cons(mk_symbol("lambda"),cons(cons(mk_symbol("declarations"),cons(mk_symbol("environment"),NIL)),cons(cons(mk_symbol("letrec"),cons(cons(cons(mk_symbol("export-declarations"),cons(cons( mk_symbol("quote" ),cons(NIL,NIL)),NIL)),cons(cons(mk_symbol("import-declarations"),cons(cons( mk_symbol("quote" ),cons(NIL,NIL)),NIL)),cons(cons(mk_symbol("begin-declarations"),cons(cons( mk_symbol("quote" ),cons(NIL,NIL)),NIL)),cons(cons(mk_symbol("export-only"),cons(cons( mk_symbol("quote" ),cons(NIL,NIL)),NIL)),cons(cons(mk_symbol("export-rename"),cons(cons( mk_symbol("quote" ),cons(NIL,NIL)),NIL)),cons(cons(mk_symbol("library-environment"),cons(cons(mk_symbol("make-environment"),NIL),NIL)),NIL)))))),cons(cons(mk_symbol("for-each"),cons(cons(mk_symbol("lambda"),cons(cons(mk_symbol("declaration"),NIL),cons(cons(mk_symbol("cond"),cons(cons(cons(mk_symbol("eq?"),cons(cons(mk_symbol("car"),cons(mk_symbol("declaration"),NIL)),cons(cons( mk_symbol("quote" ),cons(mk_symbol("export"),NIL)),NIL))),cons(cons(mk_symbol("set!"),cons(mk_symbol("export-declarations"),cons(cons(mk_symbol("cons"),cons(mk_symbol("declaration"),cons(mk_symbol("export-declarations"),NIL))),NIL))),NIL)),cons(cons(cons(mk_symbol("eq?"),cons(cons(mk_symbol("car"),cons(mk_symbol("declaration"),NIL)),cons(cons( mk_symbol("quote" ),cons(mk_symbol("import"),NIL)),NIL))),cons(cons(mk_symbol("set!"),cons(mk_symbol("import-declarations"),cons(cons(mk_symbol("cons"),cons(mk_symbol("declaration"),cons(mk_symbol("import-declarations"),NIL))),NIL))),NIL)),cons(cons(cons(mk_symbol("eq?"),cons(cons(mk_symbol("car"),cons(mk_symbol("declaration"),NIL)),cons(cons( mk_symbol("quote" ),cons(mk_symbol("begin"),NIL)),NIL))),cons(cons(mk_symbol("set!"),cons(mk_symbol("begin-declarations"),cons(cons(mk_symbol("cons"),cons(mk_symbol("declaration"),cons(mk_symbol("begin-declarations"),NIL))),NIL))),NIL)),cons(cons(mk_symbol("else"),cons(cons(mk_symbol("error"),cons(cons( mk_symbol("quote" ),cons(mk_symbol("define-library"),NIL)),cons(mk_string("unknown declaration type:"),cons(cons(mk_symbol("car"),cons(mk_symbol("declaration"),NIL)),NIL)))),NIL)),NIL))))),NIL))),cons(cons(mk_symbol("reverse"),cons(mk_symbol("declarations"),NIL)),NIL))),cons(cons(mk_symbol("for-each"),cons(cons(mk_symbol("lambda"),cons(cons(mk_symbol("declaration"),NIL),cons(cons(mk_symbol("for-each"),cons(cons(mk_symbol("lambda"),cons(cons(mk_symbol("spec"),NIL),cons(cons(mk_symbol("cond"),cons(cons(cons(mk_symbol("symbol?"),cons(mk_symbol("spec"),NIL)),cons(cons(mk_symbol("set!"),cons(mk_symbol("export-only"),cons(cons(mk_symbol("cons"),cons(mk_symbol("spec"),cons(mk_symbol("export-only"),NIL))),NIL))),NIL)),cons(cons(cons(mk_symbol("and"),cons(cons(mk_symbol("list?"),cons(mk_symbol("spec"),NIL)),cons(cons(mk_symbol("="),cons(cons(mk_symbol("length"),cons(mk_symbol("spec"),NIL)),cons(mk_integer(3),NIL))),cons(cons(mk_symbol("equal?"),cons(cons(mk_symbol("car"),cons(mk_symbol("spec"),NIL)),cons(cons( mk_symbol("quote" ),cons(mk_symbol("rename"),NIL)),NIL))),NIL)))),cons(cons(mk_symbol("set!"),cons(mk_symbol("export-only"),cons(cons(mk_symbol("cons"),cons(cons(mk_symbol("cadr"),cons(mk_symbol("spec"),NIL)),cons(mk_symbol("export-only"),NIL))),NIL))),cons(cons(mk_symbol("set!"),cons(mk_symbol("export-rename"),cons(cons(mk_symbol("cons"),cons(cons(mk_symbol("cons"),cons(cons(mk_symbol("cadr"),cons(mk_symbol("spec"),NIL)),cons(cons(mk_symbol("car"),cons(cons(mk_symbol("cddr"),cons(mk_symbol("spec"),NIL)),NIL)),NIL))),cons(mk_symbol("export-rename"),NIL))),NIL))),NIL))),cons(cons(mk_symbol("else"),cons(cons(mk_symbol("error"),cons(cons( mk_symbol("quote" ),cons(mk_symbol("define-library"),NIL)),cons(mk_string("unknown export spec:"),cons(mk_symbol("spec"),NIL)))),NIL)),NIL)))),NIL))),cons(cons(mk_symbol("cdr"),cons(mk_symbol("declaration"),NIL)),NIL))),NIL))),cons(mk_symbol("export-declarations"),NIL))),cons(cons(mk_symbol("for-each"),cons(cons(mk_symbol("lambda"),cons(cons(mk_symbol("declaration"),NIL),cons(cons(mk_symbol("apply"),cons(mk_symbol("environment-import-sets!"),cons(cons(mk_symbol("cons"),cons(mk_symbol("library-environment"),cons(cons(mk_symbol("cons"),cons(mk_symbol("environment"),cons(cons(mk_symbol("cdr"),cons(mk_symbol("declaration"),NIL)),NIL))),NIL))),NIL))),NIL))),cons(mk_symbol("import-declarations"),NIL))),cons(cons(mk_symbol("eval"),cons(cons( mk_symbol("quasiquote" ),cons(cons(mk_symbol("begin"),cons(cons( mk_symbol("unquote-splicing" ),cons(mk_symbol("begin-declarations"),NIL)),NIL)),NIL)),cons(mk_symbol("library-environment"),NIL))),cons(cons(mk_symbol("apply"),cons(mk_symbol("environment-rename"),cons(cons(mk_symbol("cons"),cons(cons(mk_symbol("apply"),cons(mk_symbol("environment-only"),cons(cons(mk_symbol("cons"),cons(mk_symbol("library-environment"),cons(mk_symbol("export-only"),NIL))),NIL))),cons(mk_symbol("export-rename"),NIL))),NIL))),NIL))))))),NIL))),NIL))),NIL)))), environment);
return_value = autoscheme_eval(cons(mk_symbol("environment-only"),cons(cons(mk_symbol("current-environment"),NIL),cons(cons( mk_symbol("quote" ),cons(mk_symbol("define-library"),NIL)),cons(cons( mk_symbol("quote" ),cons(mk_symbol("import"),NIL)),cons(cons( mk_symbol("quote" ),cons(mk_symbol("environment-import-sets!"),NIL)),cons(cons( mk_symbol("quote" ),cons(mk_symbol("library-eval"),NIL)),NIL)))))), environment);
/*  This file is part of the 'AutoScheme' project.
 *  Copyright 2021 Steven Wiley <s.wiley@katchitek.com> 
 *  SPDX-License-Identifier: BSD-2-Clause
 */
cdr( binding ) = return_value;
 return return_value;
}
