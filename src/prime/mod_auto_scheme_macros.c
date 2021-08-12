#define _Bool int
#include "s7/s7.h"
s7_pointer autoscheme_module__mod_auto_scheme_macros( s7_scheme *s7 );
s7_pointer autoscheme_module__mod_auto_scheme_macros( s7_scheme *s7 )
{
return(s7_cons(s7,s7_make_symbol(s7,"begin"),s7_cons(s7,s7_cons(s7,s7_make_symbol(s7,"begin"),s7_cons(s7,s7_cons(s7,s7_make_symbol(s7,"define-macro"),s7_cons(s7,s7_cons(s7,s7_make_symbol(s7,"define-library"),s7_cons(s7,s7_make_symbol(s7,"name"),s7_make_symbol(s7,"declarations"))),s7_cons(s7,s7_cons(s7,s7_make_symbol(s7,"let"),s7_cons(s7,s7_cons(s7,s7_cons(s7,s7_make_symbol(s7,"export-declarations"),s7_cons(s7,s7_cons(s7,s7_make_symbol(s7,"quote"),s7_cons(s7,s7_nil(s7),s7_nil(s7))),s7_nil(s7))),s7_cons(s7,s7_cons(s7,s7_make_symbol(s7,"import-declarations"),s7_cons(s7,s7_cons(s7,s7_make_symbol(s7,"quote"),s7_cons(s7,s7_nil(s7),s7_nil(s7))),s7_nil(s7))),s7_cons(s7,s7_cons(s7,s7_make_symbol(s7,"begin-declarations"),s7_cons(s7,s7_cons(s7,s7_make_symbol(s7,"quote"),s7_cons(s7,s7_nil(s7),s7_nil(s7))),s7_nil(s7))),s7_cons(s7,s7_cons(s7,s7_make_symbol(s7,"export-only"),s7_cons(s7,s7_cons(s7,s7_make_symbol(s7,"quote"),s7_cons(s7,s7_nil(s7),s7_nil(s7))),s7_nil(s7))),s7_cons(s7,s7_cons(s7,s7_make_symbol(s7,"export-rename"),s7_cons(s7,s7_cons(s7,s7_make_symbol(s7,"quote"),s7_cons(s7,s7_nil(s7),s7_nil(s7))),s7_nil(s7))),s7_nil(s7)))))),s7_cons(s7,s7_cons(s7,s7_make_symbol(s7,"for-each"),s7_cons(s7,s7_cons(s7,s7_make_symbol(s7,"lambda"),s7_cons(s7,s7_cons(s7,s7_make_symbol(s7,"declaration"),s7_nil(s7)),s7_cons(s7,s7_cons(s7,s7_make_symbol(s7,"cond"),s7_cons(s7,s7_cons(s7,s7_cons(s7,s7_make_symbol(s7,"eq?"),s7_cons(s7,s7_cons(s7,s7_make_symbol(s7,"car"),s7_cons(s7,s7_make_symbol(s7,"declaration"),s7_nil(s7))),s7_cons(s7,s7_cons(s7,s7_make_symbol(s7,"quote"),s7_cons(s7,s7_make_symbol(s7,"export"),s7_nil(s7))),s7_nil(s7)))),s7_cons(s7,s7_cons(s7,s7_make_symbol(s7,"set!"),s7_cons(s7,s7_make_symbol(s7,"export-declarations"),s7_cons(s7,s7_cons(s7,s7_make_symbol(s7,"cons"),s7_cons(s7,s7_make_symbol(s7,"declaration"),s7_cons(s7,s7_make_symbol(s7,"export-declarations"),s7_nil(s7)))),s7_nil(s7)))),s7_nil(s7))),s7_cons(s7,s7_cons(s7,s7_cons(s7,s7_make_symbol(s7,"eq?"),s7_cons(s7,s7_cons(s7,s7_make_symbol(s7,"car"),s7_cons(s7,s7_make_symbol(s7,"declaration"),s7_nil(s7))),s7_cons(s7,s7_cons(s7,s7_make_symbol(s7,"quote"),s7_cons(s7,s7_make_symbol(s7,"import"),s7_nil(s7))),s7_nil(s7)))),s7_cons(s7,s7_cons(s7,s7_make_symbol(s7,"set!"),s7_cons(s7,s7_make_symbol(s7,"import-declarations"),s7_cons(s7,s7_cons(s7,s7_make_symbol(s7,"cons"),s7_cons(s7,s7_make_symbol(s7,"declaration"),s7_cons(s7,s7_make_symbol(s7,"import-declarations"),s7_nil(s7)))),s7_nil(s7)))),s7_nil(s7))),s7_cons(s7,s7_cons(s7,s7_cons(s7,s7_make_symbol(s7,"eq?"),s7_cons(s7,s7_cons(s7,s7_make_symbol(s7,"car"),s7_cons(s7,s7_make_symbol(s7,"declaration"),s7_nil(s7))),s7_cons(s7,s7_cons(s7,s7_make_symbol(s7,"quote"),s7_cons(s7,s7_make_symbol(s7,"begin"),s7_nil(s7))),s7_nil(s7)))),s7_cons(s7,s7_cons(s7,s7_make_symbol(s7,"set!"),s7_cons(s7,s7_make_symbol(s7,"begin-declarations"),s7_cons(s7,s7_cons(s7,s7_make_symbol(s7,"cons"),s7_cons(s7,s7_make_symbol(s7,"declaration"),s7_cons(s7,s7_make_symbol(s7,"begin-declarations"),s7_nil(s7)))),s7_nil(s7)))),s7_nil(s7))),s7_cons(s7,s7_cons(s7,s7_make_symbol(s7,"else"),s7_cons(s7,s7_cons(s7,s7_make_symbol(s7,"error"),s7_cons(s7,s7_cons(s7,s7_make_symbol(s7,"quote"),s7_cons(s7,s7_make_symbol(s7,"define-library"),s7_nil(s7))),s7_cons(s7,s7_make_string(s7,"unknown declaration type:"),s7_cons(s7,s7_cons(s7,s7_make_symbol(s7,"car"),s7_cons(s7,s7_make_symbol(s7,"declaration"),s7_nil(s7))),s7_nil(s7))))),s7_nil(s7))),s7_nil(s7)))))),s7_nil(s7)))),s7_cons(s7,s7_cons(s7,s7_make_symbol(s7,"reverse"),s7_cons(s7,s7_make_symbol(s7,"declarations"),s7_nil(s7))),s7_nil(s7)))),s7_cons(s7,s7_cons(s7,s7_make_symbol(s7,"for-each"),s7_cons(s7,s7_cons(s7,s7_make_symbol(s7,"lambda"),s7_cons(s7,s7_cons(s7,s7_make_symbol(s7,"declaration"),s7_nil(s7)),s7_cons(s7,s7_cons(s7,s7_make_symbol(s7,"for-each"),s7_cons(s7,s7_cons(s7,s7_make_symbol(s7,"lambda"),s7_cons(s7,s7_cons(s7,s7_make_symbol(s7,"spec"),s7_nil(s7)),s7_cons(s7,s7_cons(s7,s7_make_symbol(s7,"cond"),s7_cons(s7,s7_cons(s7,s7_cons(s7,s7_make_symbol(s7,"symbol?"),s7_cons(s7,s7_make_symbol(s7,"spec"),s7_nil(s7))),s7_cons(s7,s7_cons(s7,s7_make_symbol(s7,"set!"),s7_cons(s7,s7_make_symbol(s7,"export-only"),s7_cons(s7,s7_cons(s7,s7_make_symbol(s7,"cons"),s7_cons(s7,s7_make_symbol(s7,"spec"),s7_cons(s7,s7_make_symbol(s7,"export-only"),s7_nil(s7)))),s7_nil(s7)))),s7_nil(s7))),s7_cons(s7,s7_cons(s7,s7_cons(s7,s7_make_symbol(s7,"and"),s7_cons(s7,s7_cons(s7,s7_make_symbol(s7,"list?"),s7_cons(s7,s7_make_symbol(s7,"spec"),s7_nil(s7))),s7_cons(s7,s7_cons(s7,s7_make_symbol(s7,"="),s7_cons(s7,s7_cons(s7,s7_make_symbol(s7,"length"),s7_cons(s7,s7_make_symbol(s7,"spec"),s7_nil(s7))),s7_cons(s7,s7_make_integer(s7,3),s7_nil(s7)))),s7_cons(s7,s7_cons(s7,s7_make_symbol(s7,"equal?"),s7_cons(s7,s7_cons(s7,s7_make_symbol(s7,"car"),s7_cons(s7,s7_make_symbol(s7,"spec"),s7_nil(s7))),s7_cons(s7,s7_cons(s7,s7_make_symbol(s7,"quote"),s7_cons(s7,s7_make_symbol(s7,"rename"),s7_nil(s7))),s7_nil(s7)))),s7_nil(s7))))),s7_cons(s7,s7_cons(s7,s7_make_symbol(s7,"set!"),s7_cons(s7,s7_make_symbol(s7,"export-only"),s7_cons(s7,s7_cons(s7,s7_make_symbol(s7,"cons"),s7_cons(s7,s7_cons(s7,s7_make_symbol(s7,"cadr"),s7_cons(s7,s7_make_symbol(s7,"spec"),s7_nil(s7))),s7_cons(s7,s7_make_symbol(s7,"export-only"),s7_nil(s7)))),s7_nil(s7)))),s7_cons(s7,s7_cons(s7,s7_make_symbol(s7,"set!"),s7_cons(s7,s7_make_symbol(s7,"export-rename"),s7_cons(s7,s7_cons(s7,s7_make_symbol(s7,"cons"),s7_cons(s7,s7_cons(s7,s7_make_symbol(s7,"cons"),s7_cons(s7,s7_cons(s7,s7_make_symbol(s7,"cadr"),s7_cons(s7,s7_make_symbol(s7,"spec"),s7_nil(s7))),s7_cons(s7,s7_cons(s7,s7_make_symbol(s7,"caddr"),s7_cons(s7,s7_make_symbol(s7,"spec"),s7_nil(s7))),s7_nil(s7)))),s7_cons(s7,s7_make_symbol(s7,"export-rename"),s7_nil(s7)))),s7_nil(s7)))),s7_nil(s7)))),s7_cons(s7,s7_cons(s7,s7_make_symbol(s7,"else"),s7_cons(s7,s7_cons(s7,s7_make_symbol(s7,"error"),s7_cons(s7,s7_cons(s7,s7_make_symbol(s7,"quote"),s7_cons(s7,s7_make_symbol(s7,"define-library"),s7_nil(s7))),s7_cons(s7,s7_make_string(s7,"unknown export spec:"),s7_cons(s7,s7_make_symbol(s7,"spec"),s7_nil(s7))))),s7_nil(s7))),s7_nil(s7))))),s7_nil(s7)))),s7_cons(s7,s7_cons(s7,s7_make_symbol(s7,"cdr"),s7_cons(s7,s7_make_symbol(s7,"declaration"),s7_nil(s7))),s7_nil(s7)))),s7_nil(s7)))),s7_cons(s7,s7_make_symbol(s7,"export-declarations"),s7_nil(s7)))),s7_cons(s7,s7_cons(s7,s7_make_symbol(s7,"list-values"),s7_cons(s7,s7_cons(s7,s7_make_symbol(s7,"quote"),s7_cons(s7,s7_make_symbol(s7,"environment-update!"),s7_nil(s7))),s7_cons(s7,s7_cons(s7,s7_make_symbol(s7,"quote"),s7_cons(s7,s7_cons(s7,s7_make_symbol(s7,"current-environment"),s7_nil(s7)),s7_nil(s7))),s7_cons(s7,s7_cons(s7,s7_make_symbol(s7,"list-values"),s7_cons(s7,s7_cons(s7,s7_make_symbol(s7,"quote"),s7_cons(s7,s7_make_symbol(s7,"quote"),s7_nil(s7))),s7_cons(s7,s7_cons(s7,s7_make_symbol(s7,"symbol"),s7_cons(s7,s7_cons(s7,s7_make_symbol(s7,"object->string"),s7_cons(s7,s7_make_symbol(s7,"name"),s7_nil(s7))),s7_nil(s7))),s7_nil(s7)))),s7_cons(s7,s7_cons(s7,s7_make_symbol(s7,"list-values"),s7_cons(s7,s7_cons(s7,s7_make_symbol(s7,"quote"),s7_cons(s7,s7_make_symbol(s7,"let"),s7_nil(s7))),s7_cons(s7,s7_nil(s7),s7_cons(s7,s7_cons(s7,s7_make_symbol(s7,"apply-values"),s7_cons(s7,s7_make_symbol(s7,"import-declarations"),s7_nil(s7))),s7_cons(s7,s7_cons(s7,s7_make_symbol(s7,"apply-values"),s7_cons(s7,s7_make_symbol(s7,"begin-declarations"),s7_nil(s7))),s7_cons(s7,s7_cons(s7,s7_make_symbol(s7,"list-values"),s7_cons(s7,s7_cons(s7,s7_make_symbol(s7,"quote"),s7_cons(s7,s7_make_symbol(s7,"environment-rename"),s7_nil(s7))),s7_cons(s7,s7_cons(s7,s7_make_symbol(s7,"list-values"),s7_cons(s7,s7_cons(s7,s7_make_symbol(s7,"quote"),s7_cons(s7,s7_make_symbol(s7,"environment-only"),s7_nil(s7))),s7_cons(s7,s7_cons(s7,s7_make_symbol(s7,"quote"),s7_cons(s7,s7_cons(s7,s7_make_symbol(s7,"current-environment"),s7_nil(s7)),s7_nil(s7))),s7_cons(s7,s7_cons(s7,s7_make_symbol(s7,"list-values"),s7_cons(s7,s7_cons(s7,s7_make_symbol(s7,"quote"),s7_cons(s7,s7_make_symbol(s7,"quote"),s7_nil(s7))),s7_cons(s7,s7_make_symbol(s7,"export-only"),s7_nil(s7)))),s7_nil(s7))))),s7_cons(s7,s7_cons(s7,s7_make_symbol(s7,"list-values"),s7_cons(s7,s7_cons(s7,s7_make_symbol(s7,"quote"),s7_cons(s7,s7_make_symbol(s7,"quote"),s7_nil(s7))),s7_cons(s7,s7_make_symbol(s7,"export-rename"),s7_nil(s7)))),s7_nil(s7))))),s7_nil(s7))))))),s7_nil(s7)))))),s7_nil(s7)))))),s7_nil(s7)))),s7_cons(s7,s7_cons(s7,s7_make_symbol(s7,"define-macro"),s7_cons(s7,s7_cons(s7,s7_make_symbol(s7,"import"),s7_make_symbol(s7,"sets")),s7_cons(s7,s7_cons(s7,s7_make_symbol(s7,"letrec"),s7_cons(s7,s7_cons(s7,s7_cons(s7,s7_make_symbol(s7,"import-bindings"),s7_cons(s7,s7_cons(s7,s7_make_symbol(s7,"lambda"),s7_cons(s7,s7_cons(s7,s7_make_symbol(s7,"set"),s7_nil(s7)),s7_cons(s7,s7_cons(s7,s7_make_symbol(s7,"cond"),s7_cons(s7,s7_cons(s7,s7_cons(s7,s7_make_symbol(s7,"not"),s7_cons(s7,s7_cons(s7,s7_make_symbol(s7,"pair?"),s7_cons(s7,s7_make_symbol(s7,"set"),s7_nil(s7))),s7_nil(s7))),s7_cons(s7,s7_cons(s7,s7_make_symbol(s7,"error"),s7_cons(s7,s7_make_string(s7,"improper import-set:"),s7_cons(s7,s7_make_symbol(s7,"set"),s7_nil(s7)))),s7_nil(s7))),s7_cons(s7,s7_cons(s7,s7_cons(s7,s7_make_symbol(s7,"equal?"),s7_cons(s7,s7_cons(s7,s7_make_symbol(s7,"car"),s7_cons(s7,s7_make_symbol(s7,"set"),s7_nil(s7))),s7_cons(s7,s7_cons(s7,s7_make_symbol(s7,"quote"),s7_cons(s7,s7_make_symbol(s7,"only"),s7_nil(s7))),s7_nil(s7)))),s7_cons(s7,s7_cons(s7,s7_make_symbol(s7,"apply"),s7_cons(s7,s7_make_symbol(s7,"import-only"),s7_cons(s7,s7_cons(s7,s7_make_symbol(s7,"cdr"),s7_cons(s7,s7_make_symbol(s7,"set"),s7_nil(s7))),s7_nil(s7)))),s7_nil(s7))),s7_cons(s7,s7_cons(s7,s7_cons(s7,s7_make_symbol(s7,"equal?"),s7_cons(s7,s7_cons(s7,s7_make_symbol(s7,"car"),s7_cons(s7,s7_make_symbol(s7,"set"),s7_nil(s7))),s7_cons(s7,s7_cons(s7,s7_make_symbol(s7,"quote"),s7_cons(s7,s7_make_symbol(s7,"except"),s7_nil(s7))),s7_nil(s7)))),s7_cons(s7,s7_cons(s7,s7_make_symbol(s7,"apply"),s7_cons(s7,s7_make_symbol(s7,"import-except"),s7_cons(s7,s7_cons(s7,s7_make_symbol(s7,"cdr"),s7_cons(s7,s7_make_symbol(s7,"set"),s7_nil(s7))),s7_nil(s7)))),s7_nil(s7))),s7_cons(s7,s7_cons(s7,s7_cons(s7,s7_make_symbol(s7,"equal?"),s7_cons(s7,s7_cons(s7,s7_make_symbol(s7,"car"),s7_cons(s7,s7_make_symbol(s7,"set"),s7_nil(s7))),s7_cons(s7,s7_cons(s7,s7_make_symbol(s7,"quote"),s7_cons(s7,s7_make_symbol(s7,"prefix"),s7_nil(s7))),s7_nil(s7)))),s7_cons(s7,s7_cons(s7,s7_make_symbol(s7,"apply"),s7_cons(s7,s7_make_symbol(s7,"import-prefix"),s7_cons(s7,s7_cons(s7,s7_make_symbol(s7,"cdr"),s7_cons(s7,s7_make_symbol(s7,"set"),s7_nil(s7))),s7_nil(s7)))),s7_nil(s7))),s7_cons(s7,s7_cons(s7,s7_cons(s7,s7_make_symbol(s7,"equal?"),s7_cons(s7,s7_cons(s7,s7_make_symbol(s7,"car"),s7_cons(s7,s7_make_symbol(s7,"set"),s7_nil(s7))),s7_cons(s7,s7_cons(s7,s7_make_symbol(s7,"quote"),s7_cons(s7,s7_make_symbol(s7,"rename"),s7_nil(s7))),s7_nil(s7)))),s7_cons(s7,s7_cons(s7,s7_make_symbol(s7,"apply"),s7_cons(s7,s7_make_symbol(s7,"import-rename"),s7_cons(s7,s7_cons(s7,s7_make_symbol(s7,"cdr"),s7_cons(s7,s7_make_symbol(s7,"set"),s7_nil(s7))),s7_nil(s7)))),s7_nil(s7))),s7_cons(s7,s7_cons(s7,s7_make_symbol(s7,"else"),s7_cons(s7,s7_cons(s7,s7_make_symbol(s7,"import-library"),s7_cons(s7,s7_make_symbol(s7,"set"),s7_nil(s7))),s7_nil(s7))),s7_nil(s7)))))))),s7_nil(s7)))),s7_nil(s7))),s7_cons(s7,s7_cons(s7,s7_make_symbol(s7,"import-library"),s7_cons(s7,s7_cons(s7,s7_make_symbol(s7,"lambda"),s7_cons(s7,s7_cons(s7,s7_make_symbol(s7,"set"),s7_nil(s7)),s7_cons(s7,s7_cons(s7,s7_make_symbol(s7,"list-values"),s7_cons(s7,s7_cons(s7,s7_make_symbol(s7,"quote"),s7_cons(s7,s7_cons(s7,s7_make_symbol(s7,"current-environment"),s7_nil(s7)),s7_nil(s7))),s7_cons(s7,s7_cons(s7,s7_make_symbol(s7,"list-values"),s7_cons(s7,s7_cons(s7,s7_make_symbol(s7,"quote"),s7_cons(s7,s7_make_symbol(s7,"symbol"),s7_nil(s7))),s7_cons(s7,s7_cons(s7,s7_make_symbol(s7,"object->string"),s7_cons(s7,s7_make_symbol(s7,"set"),s7_nil(s7))),s7_nil(s7)))),s7_nil(s7)))),s7_nil(s7)))),s7_nil(s7))),s7_cons(s7,s7_cons(s7,s7_make_symbol(s7,"import-only"),s7_cons(s7,s7_cons(s7,s7_make_symbol(s7,"lambda"),s7_cons(s7,s7_cons(s7,s7_make_symbol(s7,"set"),s7_make_symbol(s7,"identifiers")),s7_cons(s7,s7_cons(s7,s7_make_symbol(s7,"list-values"),s7_cons(s7,s7_cons(s7,s7_make_symbol(s7,"quote"),s7_cons(s7,s7_make_symbol(s7,"environment-only"),s7_nil(s7))),s7_cons(s7,s7_cons(s7,s7_make_symbol(s7,"import-bindings"),s7_cons(s7,s7_cons(s7,s7_make_symbol(s7,"list"),s7_cons(s7,s7_cons(s7,s7_make_symbol(s7,"car"),s7_cons(s7,s7_make_symbol(s7,"set"),s7_nil(s7))),s7_nil(s7))),s7_nil(s7))),s7_cons(s7,s7_cons(s7,s7_make_symbol(s7,"list-values"),s7_cons(s7,s7_cons(s7,s7_make_symbol(s7,"quote"),s7_cons(s7,s7_make_symbol(s7,"quote"),s7_nil(s7))),s7_cons(s7,s7_cons(s7,s7_make_symbol(s7,"list-values"),s7_cons(s7,s7_cons(s7,s7_make_symbol(s7,"apply-values"),s7_cons(s7,s7_make_symbol(s7,"identifiers"),s7_nil(s7))),s7_nil(s7))),s7_nil(s7)))),s7_nil(s7))))),s7_nil(s7)))),s7_nil(s7))),s7_cons(s7,s7_cons(s7,s7_make_symbol(s7,"import-except"),s7_cons(s7,s7_cons(s7,s7_make_symbol(s7,"lambda"),s7_cons(s7,s7_cons(s7,s7_make_symbol(s7,"set"),s7_make_symbol(s7,"identifiers")),s7_cons(s7,s7_cons(s7,s7_make_symbol(s7,"list-values"),s7_cons(s7,s7_cons(s7,s7_make_symbol(s7,"quote"),s7_cons(s7,s7_make_symbol(s7,"environment-except"),s7_nil(s7))),s7_cons(s7,s7_cons(s7,s7_make_symbol(s7,"import-bindings"),s7_cons(s7,s7_cons(s7,s7_make_symbol(s7,"list"),s7_cons(s7,s7_cons(s7,s7_make_symbol(s7,"car"),s7_cons(s7,s7_make_symbol(s7,"set"),s7_nil(s7))),s7_nil(s7))),s7_nil(s7))),s7_cons(s7,s7_cons(s7,s7_make_symbol(s7,"list-values"),s7_cons(s7,s7_cons(s7,s7_make_symbol(s7,"quote"),s7_cons(s7,s7_make_symbol(s7,"quote"),s7_nil(s7))),s7_cons(s7,s7_cons(s7,s7_make_symbol(s7,"list-values"),s7_cons(s7,s7_cons(s7,s7_make_symbol(s7,"apply-values"),s7_cons(s7,s7_make_symbol(s7,"identifiers"),s7_nil(s7))),s7_nil(s7))),s7_nil(s7)))),s7_nil(s7))))),s7_nil(s7)))),s7_nil(s7))),s7_cons(s7,s7_cons(s7,s7_make_symbol(s7,"import-prefix"),s7_cons(s7,s7_cons(s7,s7_make_symbol(s7,"lambda"),s7_cons(s7,s7_cons(s7,s7_make_symbol(s7,"set"),s7_cons(s7,s7_make_symbol(s7,"identifier"),s7_nil(s7))),s7_cons(s7,s7_cons(s7,s7_make_symbol(s7,"list-values"),s7_cons(s7,s7_cons(s7,s7_make_symbol(s7,"quote"),s7_cons(s7,s7_make_symbol(s7,"environment-prefix"),s7_nil(s7))),s7_cons(s7,s7_cons(s7,s7_make_symbol(s7,"import-bindings"),s7_cons(s7,s7_cons(s7,s7_make_symbol(s7,"list"),s7_cons(s7,s7_cons(s7,s7_make_symbol(s7,"car"),s7_cons(s7,s7_make_symbol(s7,"set"),s7_nil(s7))),s7_nil(s7))),s7_nil(s7))),s7_cons(s7,s7_cons(s7,s7_make_symbol(s7,"list-values"),s7_cons(s7,s7_cons(s7,s7_make_symbol(s7,"quote"),s7_cons(s7,s7_make_symbol(s7,"quote"),s7_nil(s7))),s7_cons(s7,s7_make_symbol(s7,"identifier"),s7_nil(s7)))),s7_nil(s7))))),s7_nil(s7)))),s7_nil(s7))),s7_cons(s7,s7_cons(s7,s7_make_symbol(s7,"import-rename"),s7_cons(s7,s7_cons(s7,s7_make_symbol(s7,"lambda"),s7_cons(s7,s7_cons(s7,s7_make_symbol(s7,"set"),s7_make_symbol(s7,"rename-list")),s7_cons(s7,s7_cons(s7,s7_make_symbol(s7,"list-values"),s7_cons(s7,s7_cons(s7,s7_make_symbol(s7,"quote"),s7_cons(s7,s7_make_symbol(s7,"environment-rename"),s7_nil(s7))),s7_cons(s7,s7_cons(s7,s7_make_symbol(s7,"import-bindings"),s7_cons(s7,s7_cons(s7,s7_make_symbol(s7,"list"),s7_cons(s7,s7_cons(s7,s7_make_symbol(s7,"car"),s7_cons(s7,s7_make_symbol(s7,"set"),s7_nil(s7))),s7_nil(s7))),s7_nil(s7))),s7_cons(s7,s7_cons(s7,s7_make_symbol(s7,"list-values"),s7_cons(s7,s7_cons(s7,s7_make_symbol(s7,"quote"),s7_cons(s7,s7_make_symbol(s7,"quote"),s7_nil(s7))),s7_cons(s7,s7_cons(s7,s7_make_symbol(s7,"list-values"),s7_cons(s7,s7_cons(s7,s7_make_symbol(s7,"apply-values"),s7_cons(s7,s7_cons(s7,s7_make_symbol(s7,"map"),s7_cons(s7,s7_cons(s7,s7_make_symbol(s7,"lambda"),s7_cons(s7,s7_cons(s7,s7_make_symbol(s7,"rename"),s7_nil(s7)),s7_cons(s7,s7_cons(s7,s7_make_symbol(s7,"cons"),s7_cons(s7,s7_cons(s7,s7_make_symbol(s7,"car"),s7_cons(s7,s7_make_symbol(s7,"rename"),s7_nil(s7))),s7_cons(s7,s7_cons(s7,s7_make_symbol(s7,"cadr"),s7_cons(s7,s7_make_symbol(s7,"rename"),s7_nil(s7))),s7_nil(s7)))),s7_nil(s7)))),s7_cons(s7,s7_make_symbol(s7,"rename-list"),s7_nil(s7)))),s7_nil(s7))),s7_nil(s7))),s7_nil(s7)))),s7_nil(s7))))),s7_nil(s7)))),s7_nil(s7))),s7_nil(s7))))))),s7_cons(s7,s7_cons(s7,s7_make_symbol(s7,"list-values"),s7_cons(s7,s7_cons(s7,s7_make_symbol(s7,"quote"),s7_cons(s7,s7_make_symbol(s7,"environment-import!"),s7_nil(s7))),s7_cons(s7,s7_cons(s7,s7_make_symbol(s7,"quote"),s7_cons(s7,s7_cons(s7,s7_make_symbol(s7,"current-environment"),s7_nil(s7)),s7_nil(s7))),s7_cons(s7,s7_cons(s7,s7_make_symbol(s7,"cons"),s7_cons(s7,s7_cons(s7,s7_make_symbol(s7,"quote"),s7_cons(s7,s7_make_symbol(s7,"list"),s7_nil(s7))),s7_cons(s7,s7_cons(s7,s7_make_symbol(s7,"map"),s7_cons(s7,s7_cons(s7,s7_make_symbol(s7,"lambda"),s7_cons(s7,s7_cons(s7,s7_make_symbol(s7,"set"),s7_nil(s7)),s7_cons(s7,s7_cons(s7,s7_make_symbol(s7,"import-bindings"),s7_cons(s7,s7_make_symbol(s7,"set"),s7_nil(s7))),s7_nil(s7)))),s7_cons(s7,s7_make_symbol(s7,"sets"),s7_nil(s7)))),s7_nil(s7)))),s7_nil(s7))))),s7_nil(s7)))),s7_nil(s7)))),s7_nil(s7)))),s7_nil(s7))));
}
