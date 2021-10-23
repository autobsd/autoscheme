/*  This file is part of the 'AutoScheme' project.
 *  Copyright 2021 Steven Wiley <s.wiley@katchitek.com> 
 *  SPDX-License-Identifier: BSD-2-Clause
 */
#include "autoscheme.h"
pointer LOAD_MODULE__auto_scheme_base(pointer environment);
/*  This file is part of the 'AutoScheme' project.
 *  Copyright 2021 Steven Wiley <s.wiley@katchitek.com> 
 *  SPDX-License-Identifier: BSD-2-Clause
 */
pointer INITIALIZE_LIBRARY__auto_scheme_base( pointer environment );

foreign_function ff_current_directory;
foreign_function ff_path_absolute_p;
foreign_function ff_path_make_absolute;
foreign_function ff_path_directory;
 /*  This file is part of the 'AutoScheme' project.
 *  Copyright 2021 Steven Wiley <s.wiley@katchitek.com> 
 *  SPDX-License-Identifier: BSD-2-Clause
 */
pointer INITIALIZE_LIBRARY__auto_scheme_base( pointer environment )
{
    scheme_register_operation(LOC_MUL, "*", environment);
    scheme_register_operation(LOC_ADD, "+", environment);
    scheme_register_operation(LOC_SUB, "-", environment);

/* ... */

    scheme_register_operation(LOC_DIV, "/", environment);
    scheme_register_operation(LOC_LESS, "<", environment);
    scheme_register_operation(LOC_LEQ, "<=", environment);
    scheme_register_operation(LOC_NEQ, "=", environment);
    scheme_register_syntax(LOC_FEEDTO, "=>", environment);
    scheme_register_operation(LOC_GRE, ">", environment);
    scheme_register_operation(LOC_GEQ, ">=", environment);
    scheme_register_operation(LOC_ABS, "abs", environment);
    scheme_register_syntax(LOC_AND0, "and", environment);
    scheme_register_operation(LOC_APPEND, "append", environment);
    scheme_register_operation(LOC_PAPPLY, "apply", environment);
    scheme_register_operation(LOC_ASSOC, "assoc", environment);
    scheme_register_operation(LOC_ASSQ, "assq", environment);	/* a.k */
    scheme_register_operation(LOC_ASSV, "assv", environment);
    scheme_register_syntax(LOC_BEGIN, "begin", environment);

/* binary-port? */
/* boolean=? */

    scheme_register_operation(LOC_BOOL, "boolean?", environment);

/* bytevector */
/* bytevector-append */
/* bytevector-copy */
/* bytevector-copy! */
/* bytevector-length */
/* bytevector-u8-ref */
/* bytevector-u8-set! */
/* bytevector? */

    scheme_register_operation(LOC_CAAR, "caar", environment);
    scheme_register_operation(LOC_CADR, "cadr", environment);
    scheme_register_operation(LOC_CONTINUATION, "call-with-current-continuation", environment);

/* call-with-port */

    scheme_register_operation(LOC_WITHVALUES0, "call-with-values", environment);
    scheme_register_operation(LOC_CONTINUATION, "call/cc", environment);
    scheme_register_operation(LOC_CAR, "car", environment);
    scheme_register_syntax(LOC_CASE0, "case", environment);
    scheme_register_operation(LOC_CDAR, "cdar", environment);
    scheme_register_operation(LOC_CDDR, "cddr", environment);
    scheme_register_operation(LOC_CDR, "cdr", environment);
    scheme_register_operation(LOC_CEILING, "ceiling", environment);
    scheme_register_operation(LOC_CHAR2INT, "char->integer", environment);
    scheme_register_operation(LOC_CHAR_READY, "char-ready?", environment);
    scheme_register_operation(LOC_CHARLEQ, "char<=?", environment);
    scheme_register_operation(LOC_CHARLSS, "char<?", environment);
    scheme_register_operation(LOC_CHAREQU, "char=?", environment);
    scheme_register_operation(LOC_CHARGEQ, "char>=?", environment);
    scheme_register_operation(LOC_CHARGTR, "char>?", environment);
    scheme_register_operation(LOC_CHAR, "char?", environment);
    scheme_register_operation(LOC_CLOSE_INPORT, "close-input-port", environment);
    scheme_register_operation(LOC_CLOSE_OUTPORT, "close-output-port", environment);
    scheme_register_operation(LOC_CLOSE_PORT, "close-port", environment);

/* complex? */

    scheme_register_syntax(LOC_COND0, "cond", environment);

/* cond-expand */

    scheme_register_operation(LOC_CONS, "cons", environment);

/* current-error-port */

    scheme_register_operation(LOC_CURR_INPORT, "current-input-port", environment);
    scheme_register_operation(LOC_CURR_OUTPORT, "current-output-port", environment);
    scheme_register_syntax(LOC_DEF0, "define", environment);

/* define-record-type */

    scheme_register_syntax(LOC_DEFSYNTAX0, "define-syntax", environment);

/* define-values */
/* denominator */

    scheme_register_syntax(LOC_DO0, "do", environment);
    scheme_register_operation(LOC_DYNAMICWIND0, "dynamic-wind", environment);
    scheme_register_syntax(LOC_ELSE, "else", environment);

/* eof-object */

    scheme_register_operation(LOC_EOFOBJP, "eof-object?", environment);

    scheme_register_operation(LOC_EQ, "eq?", environment);
    scheme_register_operation(LOC_EQUAL, "equal?", environment);
    scheme_register_operation(LOC_EQV, "eqv?", environment);
    scheme_register_operation(LOC_ERR0, "error", environment);

/* error-object-irritants */
/* error-object-message */
/* error-object? */

    scheme_register_operation(LOC_EVEN, "even?", environment);

/* exact */     scheme_register_operation(LOC_INEX2EX, "inexact->exact", environment);

    scheme_register_operation(LOC_EXACT, "exact?", environment);
    scheme_register_operation(LOC_EXPT, "expt", environment);

/* features */
/* file-error? */

    scheme_register_operation(LOC_FLOOR, "floor", environment);

/* floor-quotient */
/* floor-remainder */
/* floor/ */
/* flush-output-port */

    scheme_register_operation(LOC_FOREACH0, "for-each", environment);
    scheme_register_operation(LOC_GCD, "gcd", environment);

/* get-output-bytvector */

    scheme_register_operation(LOC_GET_OUTSTRING, "get-output-string", environment);

/* guard */

    scheme_register_syntax(LOC_IF0, "if", environment);

/* include */
/* include-ci */
/* inexact */     scheme_register_operation(LOC_EX2INEX, "exact->inexact", environment);

    scheme_register_operation(LOC_INEXACT, "inexact?", environment);

/* input-port-open? */

    scheme_register_operation(LOC_INPORTP, "input-port?", environment);
    scheme_register_operation(LOC_INT2CHAR, "integer->char", environment);
    scheme_register_operation(LOC_INTEGER, "integer?", environment);
    scheme_register_syntax(LOC_LAMBDA, "lambda", environment);
    scheme_register_operation(LOC_LCM, "lcm", environment);
    scheme_register_operation(LOC_LIST_LENGTH, "length", environment);	/* a.k */
    scheme_register_syntax(LOC_LET0, "let", environment);
    scheme_register_syntax(LOC_LET0AST, "let*", environment);

/* let*-values */

    scheme_register_syntax(LOC_LETSYNTAX0, "let-syntax", environment);

/* let-values */

    scheme_register_syntax(LOC_LET0REC, "letrec", environment);
    scheme_register_syntax(LOC_LETRECAST0, "letrec*", environment);
    scheme_register_syntax(LOC_LETRECSYNTAX0, "letrec-syntax", environment);
    scheme_register_operation(LOC_LIST, "list", environment);
    scheme_register_operation(LOC_LIST2STR, "list->string", environment);
    scheme_register_operation(LOC_LIST2VEC, "list->vector", environment);

/* list-copy */

    scheme_register_operation(LOC_LISTREF, "list-ref", environment);

/* list-set! */

    scheme_register_operation(LOC_LISTTAIL, "list-tail", environment);
    scheme_register_operation(LOC_LISTP, "list?", environment);

/* make-bytevector */
/* make-list */
/* make-parameter */

    scheme_register_operation(LOC_MKSTRING, "make-string", environment);
    scheme_register_operation(LOC_MKVECTOR, "make-vector", environment);
    scheme_register_operation(LOC_MAP0, "map", environment);
    scheme_register_operation(LOC_MAX, "max", environment);
    scheme_register_operation(LOC_MEMBER, "member", environment);
    scheme_register_operation(LOC_MEMQ, "memq", environment);
    scheme_register_operation(LOC_MEMV, "memv", environment);
    scheme_register_operation(LOC_MIN, "min", environment);
    scheme_register_operation(LOC_MOD, "modulo", environment);
    scheme_register_operation(LOC_NEGP, "negative?", environment);
    scheme_register_operation(LOC_NEWLINE, "newline", environment);
    scheme_register_operation(LOC_NOT, "not", environment);
    scheme_register_operation(LOC_NULL, "null?", environment);
    scheme_register_operation(LOC_NUM2STR, "number->string", environment);
    scheme_register_operation(LOC_NUMBER, "number?", environment);

/* numerator? */

    scheme_register_operation(LOC_ODD, "odd?", environment);

/* open-input-bytevector */

    scheme_register_operation(LOC_OPEN_INSTRING, "open-input-string", environment);

/* open-output-bytevector */

    scheme_register_operation(LOC_OPEN_OUTSTRING, "open-output-string", environment);
    scheme_register_syntax(LOC_OR0, "or", environment);

/* output-port-open? */

    scheme_register_operation(LOC_OUTPORTP, "output-port?", environment);
    scheme_register_operation(LOC_PAIR, "pair?", environment);

/* parameterize */

    scheme_register_operation(LOC_PEEK_CHAR, "peek-char", environment);

/* peek-u8 */

    scheme_register_operation(LOC_PORTP, "port?", environment);
    scheme_register_operation(LOC_POSP, "positive?", environment);
    scheme_register_operation(LOC_PROCP, "procedure?", environment);
    scheme_register_syntax(LOC_QQUOTE0, "quasiquote", environment);
    scheme_register_syntax(LOC_QUOTE, "quote", environment);
    scheme_register_operation(LOC_QUO, "quotient", environment);


/* raise */
/* raise-continuable */
/* rational? */
/* rationalize */
/* read-bytevector */
/* read-bytevector! */

    scheme_register_operation(LOC_READ_CHAR, "read-char", environment);

/* read-error? */
/* read-line */
/* read-string */
/* read-u8 */

    scheme_register_operation(LOC_REAL, "real?", environment);
    scheme_register_operation(LOC_REM, "remainder", environment);
    scheme_register_operation(LOC_REVERSE, "reverse", environment);
    scheme_register_operation(LOC_ROUND, "round", environment);
    scheme_register_syntax(LOC_SET0, "set!", environment);
    scheme_register_operation(LOC_SETCAR, "set-car!", environment);
    scheme_register_operation(LOC_SETCDR, "set-cdr!", environment);

/* square */

    scheme_register_operation(LOC_STRING, "string", environment);
    scheme_register_operation(LOC_STR2LIST, "string->list", environment);
    scheme_register_operation(LOC_STR2NUM, "string->number", environment);
    scheme_register_operation(LOC_STR2SYM, "string->symbol", environment);

/* string->utf8 */
/* string->vector */

    scheme_register_operation(LOC_STRAPPEND, "string-append", environment);
    scheme_register_operation(LOC_STRCOPY, "string-copy", environment);

/* string-copy! */

    scheme_register_operation(LOC_STRFILL, "string-fill!", environment);

/* string-for-each */

    scheme_register_operation(LOC_STRLEN, "string-length", environment);

/* string-map */

    scheme_register_operation(LOC_STRREF, "string-ref", environment);
    scheme_register_operation(LOC_STRSET, "string-set!", environment);
    scheme_register_operation(LOC_STRLEQ, "string<=?", environment);
    scheme_register_operation(LOC_STRLSS, "string<?", environment);
    scheme_register_operation(LOC_STREQU, "string=?", environment);
    scheme_register_operation(LOC_STRGEQ, "string>=?", environment);
    scheme_register_operation(LOC_STRGTR, "string>?", environment);
    scheme_register_operation(LOC_STRINGP, "string?", environment);
    scheme_register_operation(LOC_SUBSTR, "substring", environment);
    scheme_register_operation(LOC_SYM2STR, "symbol->string", environment);

/* symbol=? */

    scheme_register_operation(LOC_SYMBOL, "symbol?", environment);

/* syntax-error */

    scheme_register_syntax(LOC_SYNTAXRULES, "syntax-rules", environment);

/* textual-port? */

    scheme_register_operation(LOC_TRUNCATE, "truncate", environment);

/* truncate-quotient */
/* truncate-remainder */
/* truncate/ */
/* u8-ready? */

    scheme_register_syntax(LOC_UNLESS0, "unless", environment);

/* unquote */
/* unquote-splicing */
/* utf8->string */

    scheme_register_operation(LOC_VALUES, "values", environment);
    scheme_register_operation(LOC_VECTOR, "vector", environment);
    scheme_register_operation(LOC_VEC2LIST, "vector->list", environment);

/* vector->string */
/* vector-append */
/* vector-copy */
/* vector-copy! */

    scheme_register_operation(LOC_VECFILL, "vector-fill!", environment);

/* vector-for-each */

    scheme_register_operation(LOC_VECLEN, "vector-length", environment);

/* vector-map */

    scheme_register_operation(LOC_VECREF, "vector-ref", environment);
    scheme_register_operation(LOC_VECSET, "vector-set!", environment);
    scheme_register_operation(LOC_VECTORP, "vector?", environment);
    scheme_register_syntax(LOC_WHEN0, "when", environment);

/* with-exception-handler */
/* write-bytevector */

    scheme_register_operation(LOC_WRITE_CHAR, "write-char", environment);

/* write-string */
/* write-u8 */

    scheme_register_operation(LOC_ZEROP, "zero?", environment);


/* ******** unused ********** */

    /* scheme_register_syntax(LOC_RECEIVE0, "receive", environment); */
    /* scheme_register_operation(LOC_QUIT, "quit", environment); */
    /* scheme_register_operation(LOC_DEFP, "defined?", environment); */
    /* scheme_register_operation(LOC_ATOMP, "atom?", environment); */


    return environment;
}
 pointer LOAD_MODULE__auto_scheme_base(pointer environment)
{
pointer return_value = T;
/*  This file is part of the 'AutoScheme' project.
 *  Copyright 2021 Steven Wiley <s.wiley@katchitek.com> 
 *  SPDX-License-Identifier: BSD-2-Clause
 */
pointer library = make_environment( NIL );
pointer name = mk_symbol( "(auto scheme base)" );

car( environment ) = cons( cons( name, library ), car( environment ));  
INITIALIZE_LIBRARY__auto_scheme_base( library );
environment = library;
 return_value = autoscheme_eval(T, environment);
return_value = autoscheme_eval(T, environment);
return_value = autoscheme_eval(T, environment);
return_value = autoscheme_eval(cons(mk_symbol("define"),cons(mk_symbol("make-parameter"),cons(cons(mk_symbol("lambda"),cons(cons(mk_symbol("init"),mk_symbol("rest")),cons(cons(mk_symbol("let*"),cons(cons(cons(mk_symbol("converter"),cons(cons(mk_symbol("if"),cons(cons(mk_symbol("null?"),cons(mk_symbol("rest"),NIL)),cons(F,cons(cons(mk_symbol("car"),cons(mk_symbol("rest"),NIL)),NIL)))),NIL)),cons(cons(mk_symbol("value"),cons(cons(mk_symbol("if"),cons(mk_symbol("converter"),cons(cons(mk_symbol("converter"),cons(mk_symbol("init"),NIL)),cons(mk_symbol("init"),NIL)))),NIL)),NIL)),cons(cons(mk_symbol("lambda"),cons(mk_symbol("args"),cons(cons(mk_symbol("cond"),cons(cons(cons(mk_symbol("null?"),cons(mk_symbol("args"),NIL)),cons(mk_symbol("value"),NIL)),cons(cons(cons(mk_symbol("and"),cons(cons(mk_symbol("pair?"),cons(cons(mk_symbol("cdr"),cons(mk_symbol("args"),NIL)),NIL)),cons(cons(mk_symbol("not"),cons(cons(mk_symbol("cadr"),cons(mk_symbol("args"),NIL)),NIL)),NIL))),cons(cons(mk_symbol("set!"),cons(mk_symbol("value"),cons(cons(mk_symbol("car"),cons(mk_symbol("args"),NIL)),NIL))),NIL)),cons(cons(mk_symbol("converter"),cons(cons(mk_symbol("set!"),cons(mk_symbol("value"),cons(cons(mk_symbol("converter"),cons(cons(mk_symbol("car"),cons(mk_symbol("args"),NIL)),NIL)),NIL))),NIL)),cons(cons(mk_symbol("else"),cons(cons(mk_symbol("set!"),cons(mk_symbol("value"),cons(cons(mk_symbol("car"),cons(mk_symbol("args"),NIL)),NIL))),NIL)),NIL))))),NIL))),NIL))),NIL))),NIL))), environment);
return_value = autoscheme_eval(cons(mk_syntax(LOC_DEFMACRO0,"define-macro"),cons(cons(mk_symbol("parameterize"),cons(mk_symbol("associations"),mk_symbol("body"))),cons(cons(mk_symbol("let*"),cons(cons(cons(mk_symbol("eval"),cons(mk_operation(LOC_PEVAL,&NIL),NIL)),cons(cons(mk_symbol("params"),cons(cons(mk_symbol("map"),cons(cons(mk_symbol("lambda"),cons(cons(mk_symbol("association"),NIL),cons(cons(mk_symbol("eval"),cons(cons(mk_symbol("car"),cons(mk_symbol("association"),NIL)),cons(cons(mk_symbol("expansion-environment"),NIL),NIL))),NIL))),cons(mk_symbol("associations"),NIL))),NIL)),cons(cons(mk_symbol("tmp-values"),cons(cons(mk_symbol("map"),cons(cons(mk_symbol("lambda"),cons(cons(mk_symbol("association"),NIL),cons(cons(mk_symbol("eval"),cons(cons(mk_symbol("cadr"),cons(mk_symbol("association"),NIL)),cons(cons(mk_symbol("expansion-environment"),NIL),NIL))),NIL))),cons(mk_symbol("associations"),NIL))),NIL)),cons(cons(mk_symbol("prev-values"),cons(cons(mk_symbol("map"),cons(cons(mk_symbol("lambda"),cons(cons(mk_symbol("param"),NIL),cons(cons(mk_symbol("apply"),cons(mk_symbol("param"),cons(cons( mk_symbol("quote" ),cons(NIL,NIL)),NIL))),NIL))),cons(mk_symbol("params"),NIL))),NIL)),NIL)))),cons(cons(mk_symbol("dynamic-wind"),cons(cons(mk_symbol("lambda"),cons(NIL,cons(cons(mk_symbol("for-each"),cons(cons(mk_symbol("lambda"),cons(cons(mk_symbol("param"),cons(mk_symbol("value"),NIL)),cons(cons(mk_symbol("param"),cons(mk_symbol("value"),NIL)),NIL))),cons(mk_symbol("params"),cons(mk_symbol("tmp-values"),NIL)))),NIL))),cons(cons(mk_symbol("lambda"),cons(NIL,cons(cons(mk_symbol("for-each"),cons(cons(mk_symbol("lambda"),cons(cons(mk_symbol("statement"),NIL),cons(cons(mk_symbol("eval"),cons(mk_symbol("statement"),cons(cons(mk_symbol("expansion-environment"),NIL),NIL))),NIL))),cons(mk_symbol("body"),NIL))),NIL))),cons(cons(mk_symbol("lambda"),cons(NIL,cons(cons(mk_symbol("for-each"),cons(cons(mk_symbol("lambda"),cons(cons(mk_symbol("param"),cons(mk_symbol("value"),NIL)),cons(cons(mk_symbol("param"),cons(mk_symbol("value"),NIL)),NIL))),cons(mk_symbol("params"),cons(mk_symbol("prev-values"),NIL)))),NIL))),NIL)))),NIL))),NIL))), environment);
return_value = autoscheme_eval(cons(mk_symbol("define"),cons(mk_symbol("read-string"),cons(cons(mk_symbol("letrec"),cons(cons(cons(mk_symbol("r7-read-string"),cons(cons(mk_symbol("lambda"),cons(cons(mk_symbol("k"),mk_symbol("rest")),cons(cons(mk_symbol("if"),cons(cons(mk_symbol("="),cons(mk_symbol("k"),cons(mk_integer(0),NIL))),cons(mk_string(""),cons(cons(mk_symbol("let"),cons(mk_symbol("read-chars"),cons(cons(cons(mk_symbol("s"),cons(cons(mk_symbol("make-string"),cons(mk_symbol("k"),NIL)),NIL)),cons(cons(mk_symbol("i"),cons(mk_integer(0),NIL)),cons(cons(mk_symbol("c"),cons(cons(mk_symbol("apply"),cons(mk_symbol("read-char"),cons(mk_symbol("rest"),NIL))),NIL)),NIL))),cons(cons(mk_symbol("cond"),cons(cons(cons(mk_symbol("and"),cons(cons(mk_symbol("eof-object?"),cons(mk_symbol("c"),NIL)),cons(cons(mk_symbol("="),cons(mk_symbol("i"),cons(mk_integer(0),NIL))),NIL))),cons(mk_symbol("c"),NIL)),cons(cons(cons(mk_symbol("eof-object?"),cons(mk_symbol("c"),NIL)),cons(cons(mk_symbol("substring"),cons(mk_symbol("s"),cons(mk_integer(0),cons(cons(mk_symbol("+"),cons(mk_symbol("i"),cons(mk_integer(1),NIL))),NIL)))),NIL)),cons(cons(cons(mk_symbol("<"),cons(mk_symbol("i"),cons(cons(mk_symbol("-"),cons(mk_symbol("k"),cons(mk_integer(1),NIL))),NIL))),cons(cons(mk_symbol("string-set!"),cons(mk_symbol("s"),cons(mk_symbol("i"),cons(mk_symbol("c"),NIL)))),cons(cons(mk_symbol("read-chars"),cons(mk_symbol("s"),cons(cons(mk_symbol("+"),cons(mk_symbol("i"),cons(mk_integer(1),NIL))),cons(cons(mk_symbol("apply"),cons(mk_symbol("read-char"),cons(mk_symbol("rest"),NIL))),NIL)))),NIL))),cons(cons(mk_symbol("else"),cons(cons(mk_symbol("string-set!"),cons(mk_symbol("s"),cons(mk_symbol("i"),cons(mk_symbol("c"),NIL)))),cons(cons(mk_symbol("substring"),cons(mk_symbol("s"),cons(mk_integer(0),cons(cons(mk_symbol("+"),cons(mk_symbol("i"),cons(mk_integer(1),NIL))),NIL)))),NIL))),NIL))))),NIL)))),NIL)))),NIL))),NIL)),cons(cons(mk_symbol("auto-read-string"),cons(cons(mk_symbol("lambda"),cons(mk_symbol("args"),cons(cons(mk_symbol("let"),cons(cons(cons(mk_symbol("k"),cons(cons(mk_symbol("if"),cons(cons(mk_symbol("null?"),cons(mk_symbol("args"),NIL)),cons(F,cons(cons(mk_symbol("car"),cons(mk_symbol("args"),NIL)),NIL)))),NIL)),cons(cons(mk_symbol("rest"),cons(cons(mk_symbol("if"),cons(cons(mk_symbol("pair?"),cons(mk_symbol("args"),NIL)),cons(cons(mk_symbol("cdr"),cons(mk_symbol("args"),NIL)),cons(cons( mk_symbol("quote" ),cons(NIL,NIL)),NIL)))),NIL)),NIL)),cons(cons(mk_symbol("if"),cons(cons(mk_symbol("not"),cons(mk_symbol("k"),NIL)),cons(cons(mk_symbol("let"),cons(cons(cons(mk_symbol("s"),cons(cons(mk_symbol("apply"),cons(mk_symbol("r7-read-string"),cons(cons(mk_symbol("cons"),cons(mk_integer(64),cons(mk_symbol("rest"),NIL))),NIL))),NIL)),NIL),cons(cons(mk_symbol("if"),cons(cons(mk_symbol("or"),cons(cons(mk_symbol("eof-object?"),cons(mk_symbol("s"),NIL)),cons(cons(mk_symbol("zero?"),cons(cons(mk_symbol("string-length"),cons(mk_symbol("s"),NIL)),NIL)),NIL))),cons(mk_string(""),cons(cons(mk_symbol("string-append"),cons(mk_symbol("s"),cons(cons(mk_symbol("apply"),cons(mk_symbol("auto-read-string"),cons(cons(mk_symbol("cons"),cons(F,cons(mk_symbol("rest"),NIL))),NIL))),NIL))),NIL)))),NIL))),cons(cons(mk_symbol("apply"),cons(mk_symbol("r7-read-string"),cons(mk_symbol("args"),NIL))),NIL)))),NIL))),NIL))),NIL)),NIL)),cons(mk_symbol("auto-read-string"),NIL))),NIL))), environment);
return_value = autoscheme_eval(cons(mk_syntax(LOC_DEF0,"define"),cons(mk_symbol("write-simple"),cons(mk_operation(LOC_WRITE,&NIL),NIL))), environment);
return_value = autoscheme_eval(cons(mk_symbol("define"),cons(mk_symbol("include"),cons(cons(mk_symbol("let*"),cons(cons(cons(mk_symbol("current-directory"),cons(mk_function(ff_current_directory,&NIL),NIL)),cons(cons(mk_symbol("current-source"),cons(mk_operation(LOC_CURR_SOURCE,&NIL),NIL)),cons(cons(mk_symbol("macro"),cons(mk_syntax(LOC_MACRO,"macro"),NIL)),cons(cons(mk_symbol("path-make-absolute"),cons(mk_function(ff_path_make_absolute,&NIL),NIL)),cons(cons(mk_symbol("path-directory"),cons(mk_function(ff_path_directory,&NIL),NIL)),cons(cons(mk_symbol("with-input-from-file"),cons(mk_operation(LOC_WITH_INFILE0,&NIL),NIL)),cons(cons(mk_symbol("eval"),cons(mk_operation(LOC_PEVAL,&NIL),NIL)),cons(cons(mk_symbol("read"),cons(mk_operation(LOC_READ,&NIL),NIL)),NIL)))))))),cons(cons(mk_symbol("macro"),cons(mk_symbol("filenames"),cons(cons(mk_symbol("let"),cons(cons(cons(mk_symbol("result"),cons(F,NIL)),NIL),cons(cons(mk_symbol("for-each"),cons(cons(mk_symbol("lambda"),cons(cons(mk_symbol("filename"),NIL),cons(cons(mk_symbol("parameterize"),cons(cons(cons(mk_symbol("current-source"),cons(cons(mk_symbol("path-make-absolute"),cons(mk_symbol("filename"),cons(cons(mk_symbol("path-directory"),cons(cons(mk_symbol("current-source"),NIL),NIL)),NIL))),NIL)),NIL),cons(cons(mk_symbol("with-input-from-file"),cons(cons(mk_symbol("current-source"),NIL),cons(cons(mk_symbol("lambda"),cons(NIL,cons(cons(mk_symbol("let"),cons(mk_symbol("eval-statement"),cons(cons(cons(mk_symbol("statement"),cons(cons(mk_symbol("read"),NIL),NIL)),NIL),cons(cons(mk_symbol("cond"),cons(cons(cons(mk_symbol("not"),cons(cons(mk_symbol("eof-object?"),cons(mk_symbol("statement"),NIL)),NIL)),cons(cons(mk_symbol("set!"),cons(mk_symbol("result"),cons(cons(mk_symbol("eval"),cons(mk_symbol("statement"),cons(cons(mk_symbol("expansion-environment"),NIL),NIL))),NIL))),cons(cons(mk_symbol("eval-statement"),cons(cons(mk_symbol("read"),NIL),NIL)),NIL))),NIL)),NIL)))),NIL))),NIL))),NIL))),NIL))),cons(mk_symbol("filenames"),NIL))),cons(mk_symbol("result"),NIL)))),NIL))),NIL))),NIL))), environment);
return return_value;
}
