#define _Bool int
#include "s7/s7.h"
s7_pointer autoscheme_module__mod_scheme_macros( s7_scheme *s7 );
s7_pointer autoscheme_module__mod_scheme_macros( s7_scheme *s7 )
{
return(s7_cons(s7,s7_make_symbol(s7,"begin"),s7_cons(s7,s7_cons(s7,s7_make_symbol(s7,"define-macro"),s7_cons(s7,s7_cons(s7,s7_make_symbol(s7,"define-library"),s7_cons(s7,s7_make_symbol(s7,"name"),s7_make_symbol(s7,"declarations"))),s7_cons(s7,s7_cons(s7,s7_make_symbol(s7,"let"),s7_cons(s7,s7_cons(s7,s7_cons(s7,s7_make_symbol(s7,"export-declarations"),s7_cons(s7,s7_cons(s7,s7_make_symbol(s7,"quote"),s7_cons(s7,s7_nil(s7),s7_nil(s7))),s7_nil(s7))),s7_cons(s7,s7_cons(s7,s7_make_symbol(s7,"import-declarations"),s7_cons(s7,s7_cons(s7,s7_make_symbol(s7,"quote"),s7_cons(s7,s7_nil(s7),s7_nil(s7))),s7_nil(s7))),s7_cons(s7,s7_cons(s7,s7_make_symbol(s7,"begin-declarations"),s7_cons(s7,s7_cons(s7,s7_make_symbol(s7,"quote"),s7_cons(s7,s7_nil(s7),s7_nil(s7))),s7_nil(s7))),s7_cons(s7,s7_cons(s7,s7_make_symbol(s7,"export-symbols"),s7_cons(s7,s7_cons(s7,s7_make_symbol(s7,"quote"),s7_cons(s7,s7_nil(s7),s7_nil(s7))),s7_nil(s7))),s7_nil(s7))))),s7_cons(s7,s7_cons(s7,s7_make_symbol(s7,"map"),s7_cons(s7,s7_cons(s7,s7_make_symbol(s7,"lambda"),s7_cons(s7,s7_cons(s7,s7_make_symbol(s7,"declaration"),s7_nil(s7)),s7_cons(s7,s7_cons(s7,s7_make_symbol(s7,"cond"),s7_cons(s7,s7_cons(s7,s7_cons(s7,s7_make_symbol(s7,"eq?"),s7_cons(s7,s7_cons(s7,s7_make_symbol(s7,"car"),s7_cons(s7,s7_make_symbol(s7,"declaration"),s7_nil(s7))),s7_cons(s7,s7_cons(s7,s7_make_symbol(s7,"quote"),s7_cons(s7,s7_make_symbol(s7,"export"),s7_nil(s7))),s7_nil(s7)))),s7_cons(s7,s7_cons(s7,s7_make_symbol(s7,"set!"),s7_cons(s7,s7_make_symbol(s7,"export-declarations"),s7_cons(s7,s7_cons(s7,s7_make_symbol(s7,"cons"),s7_cons(s7,s7_make_symbol(s7,"declaration"),s7_cons(s7,s7_make_symbol(s7,"export-declarations"),s7_nil(s7)))),s7_nil(s7)))),s7_nil(s7))),s7_cons(s7,s7_cons(s7,s7_cons(s7,s7_make_symbol(s7,"eq?"),s7_cons(s7,s7_cons(s7,s7_make_symbol(s7,"car"),s7_cons(s7,s7_make_symbol(s7,"declaration"),s7_nil(s7))),s7_cons(s7,s7_cons(s7,s7_make_symbol(s7,"quote"),s7_cons(s7,s7_make_symbol(s7,"import"),s7_nil(s7))),s7_nil(s7)))),s7_cons(s7,s7_cons(s7,s7_make_symbol(s7,"set!"),s7_cons(s7,s7_make_symbol(s7,"import-declarations"),s7_cons(s7,s7_cons(s7,s7_make_symbol(s7,"cons"),s7_cons(s7,s7_make_symbol(s7,"declaration"),s7_cons(s7,s7_make_symbol(s7,"import-declarations"),s7_nil(s7)))),s7_nil(s7)))),s7_nil(s7))),s7_cons(s7,s7_cons(s7,s7_cons(s7,s7_make_symbol(s7,"eq?"),s7_cons(s7,s7_cons(s7,s7_make_symbol(s7,"car"),s7_cons(s7,s7_make_symbol(s7,"declaration"),s7_nil(s7))),s7_cons(s7,s7_cons(s7,s7_make_symbol(s7,"quote"),s7_cons(s7,s7_make_symbol(s7,"begin"),s7_nil(s7))),s7_nil(s7)))),s7_cons(s7,s7_cons(s7,s7_make_symbol(s7,"set!"),s7_cons(s7,s7_make_symbol(s7,"begin-declarations"),s7_cons(s7,s7_cons(s7,s7_make_symbol(s7,"cons"),s7_cons(s7,s7_make_symbol(s7,"declaration"),s7_cons(s7,s7_make_symbol(s7,"begin-declarations"),s7_nil(s7)))),s7_nil(s7)))),s7_nil(s7))),s7_cons(s7,s7_cons(s7,s7_make_symbol(s7,"else"),s7_cons(s7,s7_cons(s7,s7_make_symbol(s7,"error"),s7_cons(s7,s7_cons(s7,s7_make_symbol(s7,"quote"),s7_cons(s7,s7_make_symbol(s7,"define-library"),s7_nil(s7))),s7_cons(s7,s7_make_string(s7,"unknown declaration type:"),s7_cons(s7,s7_cons(s7,s7_make_symbol(s7,"car"),s7_cons(s7,s7_make_symbol(s7,"declaration"),s7_nil(s7))),s7_nil(s7))))),s7_nil(s7))),s7_nil(s7)))))),s7_nil(s7)))),s7_cons(s7,s7_cons(s7,s7_make_symbol(s7,"reverse"),s7_cons(s7,s7_make_symbol(s7,"declarations"),s7_nil(s7))),s7_nil(s7)))),s7_cons(s7,s7_cons(s7,s7_make_symbol(s7,"map"),s7_cons(s7,s7_cons(s7,s7_make_symbol(s7,"lambda"),s7_cons(s7,s7_cons(s7,s7_make_symbol(s7,"declaration"),s7_nil(s7)),s7_cons(s7,s7_cons(s7,s7_make_symbol(s7,"set!"),s7_cons(s7,s7_make_symbol(s7,"export-symbols"),s7_cons(s7,s7_cons(s7,s7_make_symbol(s7,"append"),s7_cons(s7,s7_make_symbol(s7,"export-symbols"),s7_cons(s7,s7_cons(s7,s7_make_symbol(s7,"cdr"),s7_cons(s7,s7_make_symbol(s7,"declaration"),s7_nil(s7))),s7_nil(s7)))),s7_nil(s7)))),s7_nil(s7)))),s7_cons(s7,s7_make_symbol(s7,"export-declarations"),s7_nil(s7)))),s7_cons(s7,s7_cons(s7,s7_make_symbol(s7,"let*"),s7_cons(s7,s7_cons(s7,s7_cons(s7,s7_make_symbol(s7,"exported-env-expression"),s7_cons(s7,s7_cons(s7,s7_make_symbol(s7,"cons"),s7_cons(s7,s7_cons(s7,s7_make_symbol(s7,"quote"),s7_cons(s7,s7_make_symbol(s7,"inlet"),s7_nil(s7))),s7_cons(s7,s7_cons(s7,s7_make_symbol(s7,"map"),s7_cons(s7,s7_cons(s7,s7_make_symbol(s7,"lambda"),s7_cons(s7,s7_cons(s7,s7_make_symbol(s7,"sym"),s7_nil(s7)),s7_cons(s7,s7_cons(s7,s7_make_symbol(s7,"list-values"),s7_cons(s7,s7_cons(s7,s7_make_symbol(s7,"quote"),s7_cons(s7,s7_make_symbol(s7,"append"),s7_nil(s7))),s7_cons(s7,s7_cons(s7,s7_make_symbol(s7,"list-values"),s7_cons(s7,s7_cons(s7,s7_make_symbol(s7,"quote"),s7_cons(s7,s7_make_symbol(s7,"list-values"),s7_nil(s7))),s7_cons(s7,s7_cons(s7,s7_make_symbol(s7,"list-values"),s7_cons(s7,s7_cons(s7,s7_make_symbol(s7,"quote"),s7_cons(s7,s7_make_symbol(s7,"quote"),s7_nil(s7))),s7_cons(s7,s7_make_symbol(s7,"sym"),s7_nil(s7)))),s7_nil(s7)))),s7_cons(s7,s7_make_symbol(s7,"sym"),s7_nil(s7))))),s7_nil(s7)))),s7_cons(s7,s7_make_symbol(s7,"export-symbols"),s7_nil(s7)))),s7_nil(s7)))),s7_nil(s7))),s7_cons(s7,s7_cons(s7,s7_make_symbol(s7,"block"),s7_cons(s7,s7_cons(s7,s7_make_symbol(s7,"list-values"),s7_cons(s7,s7_cons(s7,s7_make_symbol(s7,"quote"),s7_cons(s7,s7_make_symbol(s7,"let"),s7_nil(s7))),s7_cons(s7,s7_nil(s7),s7_cons(s7,s7_cons(s7,s7_make_symbol(s7,"apply-values"),s7_cons(s7,s7_make_symbol(s7,"import-declarations"),s7_nil(s7))),s7_cons(s7,s7_cons(s7,s7_make_symbol(s7,"apply-values"),s7_cons(s7,s7_make_symbol(s7,"begin-declarations"),s7_nil(s7))),s7_cons(s7,s7_make_symbol(s7,"exported-env-expression"),s7_nil(s7))))))),s7_nil(s7))),s7_nil(s7))),s7_cons(s7,s7_cons(s7,s7_make_symbol(s7,"list-values"),s7_cons(s7,s7_cons(s7,s7_make_symbol(s7,"quote"),s7_cons(s7,s7_make_symbol(s7,"varlet"),s7_nil(s7))),s7_cons(s7,s7_cons(s7,s7_make_symbol(s7,"quote"),s7_cons(s7,s7_cons(s7,s7_make_symbol(s7,"curlet"),s7_nil(s7)),s7_nil(s7))),s7_cons(s7,s7_cons(s7,s7_make_symbol(s7,"list-values"),s7_cons(s7,s7_cons(s7,s7_make_symbol(s7,"quote"),s7_cons(s7,s7_make_symbol(s7,"append"),s7_nil(s7))),s7_cons(s7,s7_cons(s7,s7_make_symbol(s7,"list-values"),s7_cons(s7,s7_cons(s7,s7_make_symbol(s7,"quote"),s7_cons(s7,s7_make_symbol(s7,"list-values"),s7_nil(s7))),s7_cons(s7,s7_cons(s7,s7_make_symbol(s7,"list-values"),s7_cons(s7,s7_cons(s7,s7_make_symbol(s7,"quote"),s7_cons(s7,s7_make_symbol(s7,"symbol"),s7_nil(s7))),s7_cons(s7,s7_cons(s7,s7_make_symbol(s7,"list-values"),s7_cons(s7,s7_cons(s7,s7_make_symbol(s7,"quote"),s7_cons(s7,s7_make_symbol(s7,"object->string"),s7_nil(s7))),s7_cons(s7,s7_cons(s7,s7_make_symbol(s7,"list-values"),s7_cons(s7,s7_cons(s7,s7_make_symbol(s7,"quote"),s7_cons(s7,s7_make_symbol(s7,"quote"),s7_nil(s7))),s7_cons(s7,s7_make_symbol(s7,"name"),s7_nil(s7)))),s7_nil(s7)))),s7_nil(s7)))),s7_nil(s7)))),s7_cons(s7,s7_make_symbol(s7,"block"),s7_nil(s7))))),s7_nil(s7))))),s7_nil(s7)))),s7_nil(s7)))))),s7_nil(s7)))),s7_cons(s7,s7_cons(s7,s7_make_symbol(s7,"define-macro"),s7_cons(s7,s7_cons(s7,s7_make_symbol(s7,"import"),s7_make_symbol(s7,"sets")),s7_cons(s7,s7_cons(s7,s7_make_symbol(s7,"cons"),s7_cons(s7,s7_cons(s7,s7_make_symbol(s7,"quote"),s7_cons(s7,s7_make_symbol(s7,"begin"),s7_nil(s7))),s7_cons(s7,s7_cons(s7,s7_make_symbol(s7,"map"),s7_cons(s7,s7_cons(s7,s7_make_symbol(s7,"lambda"),s7_cons(s7,s7_cons(s7,s7_make_symbol(s7,"set"),s7_nil(s7)),s7_cons(s7,s7_cons(s7,s7_make_symbol(s7,"list-values"),s7_cons(s7,s7_cons(s7,s7_make_symbol(s7,"quote"),s7_cons(s7,s7_make_symbol(s7,"apply"),s7_nil(s7))),s7_cons(s7,s7_cons(s7,s7_make_symbol(s7,"quote"),s7_cons(s7,s7_make_symbol(s7,"varlet"),s7_nil(s7))),s7_cons(s7,s7_cons(s7,s7_make_symbol(s7,"list-values"),s7_cons(s7,s7_cons(s7,s7_make_symbol(s7,"quote"),s7_cons(s7,s7_make_symbol(s7,"cons"),s7_nil(s7))),s7_cons(s7,s7_cons(s7,s7_make_symbol(s7,"quote"),s7_cons(s7,s7_cons(s7,s7_make_symbol(s7,"curlet"),s7_nil(s7)),s7_nil(s7))),s7_cons(s7,s7_cons(s7,s7_make_symbol(s7,"list-values"),s7_cons(s7,s7_cons(s7,s7_make_symbol(s7,"quote"),s7_cons(s7,s7_make_symbol(s7,"let->list"),s7_nil(s7))),s7_cons(s7,s7_cons(s7,s7_make_symbol(s7,"list-values"),s7_cons(s7,s7_cons(s7,s7_make_symbol(s7,"quote"),s7_cons(s7,s7_make_symbol(s7,"let-ref"),s7_nil(s7))),s7_cons(s7,s7_cons(s7,s7_make_symbol(s7,"quote"),s7_cons(s7,s7_cons(s7,s7_make_symbol(s7,"curlet"),s7_nil(s7)),s7_nil(s7))),s7_cons(s7,s7_cons(s7,s7_make_symbol(s7,"list-values"),s7_cons(s7,s7_cons(s7,s7_make_symbol(s7,"quote"),s7_cons(s7,s7_make_symbol(s7,"symbol"),s7_nil(s7))),s7_cons(s7,s7_cons(s7,s7_make_symbol(s7,"object->string"),s7_cons(s7,s7_make_symbol(s7,"set"),s7_nil(s7))),s7_nil(s7)))),s7_nil(s7))))),s7_nil(s7)))),s7_nil(s7))))),s7_nil(s7))))),s7_nil(s7)))),s7_cons(s7,s7_make_symbol(s7,"sets"),s7_nil(s7)))),s7_nil(s7)))),s7_nil(s7)))),s7_nil(s7)))));
}
