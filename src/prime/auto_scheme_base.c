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
 /*  This file is part of the 'AutoScheme' project.
 *  Copyright 2021 Steven Wiley <s.wiley@katchitek.com> 
 *  SPDX-License-Identifier: BSD-2-Clause
 */
pointer INITIALIZE_LIBRARY__auto_scheme_base( pointer environment )
{
    /* initialize syntax */
    scheme_register_syntax(OP_LAMBDA, "lambda", environment);
    scheme_register_syntax(OP_QUOTE, "quote", environment);
    scheme_register_syntax(OP_QQUOTE0, "quasiquote", environment);
    scheme_register_syntax(OP_DEF0, "define", environment);
    scheme_register_syntax(OP_IF0, "if", environment);
    scheme_register_syntax(OP_BEGIN, "begin", environment);
    scheme_register_syntax(OP_SET0, "set!", environment);
    scheme_register_syntax(OP_LET0, "let", environment);
    scheme_register_syntax(OP_LET0AST, "let*", environment);
    scheme_register_syntax(OP_LET0REC, "letrec", environment);
    scheme_register_syntax(OP_LETRECAST0, "letrec*", environment);
    scheme_register_syntax(OP_DO0, "do", environment);
    scheme_register_syntax(OP_COND0, "cond", environment);
    scheme_register_syntax(OP_ELSE, "else", environment);
    scheme_register_syntax(OP_FEEDTO, "=>", environment);
    scheme_register_syntax(OP_DELAY, "delay", environment);
    scheme_register_syntax(OP_LAZY, "lazy", environment);
    scheme_register_syntax(OP_AND0, "and", environment);
    scheme_register_syntax(OP_OR0, "or", environment);
    scheme_register_syntax(OP_C0STREAM, "cons-stream", environment);
    scheme_register_syntax(OP_0MACRO, "macro", environment);

    /* scheme_register_syntax(OP_DEFMACRO0, "define-macro", environment); */

    scheme_register_syntax(OP_CASE0, "case", environment);
    scheme_register_syntax(OP_WHEN0, "when", environment);
    scheme_register_syntax(OP_UNLESS0, "unless", environment);
    scheme_register_syntax(OP_SYNTAXRULES, "syntax-rules", environment);
    scheme_register_syntax(OP_DEFSYNTAX0, "define-syntax", environment);
    scheme_register_syntax(OP_LETSYNTAX0, "let-syntax", environment);
    scheme_register_syntax(OP_LETRECSYNTAX0, "letrec-syntax", environment);
    scheme_register_syntax(OP_RECEIVE0, "receive", environment);


    /* initialize procedures */
    scheme_register_proc(OP_PEVAL, "eval", environment);
    scheme_register_proc(OP_PAPPLY, "apply", environment);
    scheme_register_proc(OP_MAP0, "map", environment);
    scheme_register_proc(OP_FOREACH0, "for-each", environment);
    scheme_register_proc(OP_CONTINUATION, "call-with-current-continuation", environment);
    scheme_register_proc(OP_CONTINUATION, "call/cc", environment);
    scheme_register_proc(OP_VALUES, "values", environment);
    scheme_register_proc(OP_WITHVALUES0, "call-with-values", environment);
    scheme_register_proc(OP_DYNAMICWIND0, "dynamic-wind", environment);
    scheme_register_proc(OP_EAGER, "eager", environment);
    scheme_register_proc(OP_FORCE, "force", environment);
    scheme_register_proc(OP_CAR, "car", environment);
    scheme_register_proc(OP_CDR, "cdr", environment);
    scheme_register_proc(OP_CONS, "cons", environment);
    scheme_register_proc(OP_SETCAR, "set-car!", environment);
    scheme_register_proc(OP_SETCDR, "set-cdr!", environment);
    scheme_register_proc(OP_CAAR, "caar", environment);
    scheme_register_proc(OP_CADR, "cadr", environment);
    scheme_register_proc(OP_CDAR, "cdar", environment);
    scheme_register_proc(OP_CDDR, "cddr", environment);
  
    scheme_register_proc(OP_LIST, "list", environment);
    scheme_register_proc(OP_LISTTAIL, "list-tail", environment);
    scheme_register_proc(OP_LISTREF, "list-ref", environment);
    scheme_register_proc(OP_LASTPAIR, "last-pair", environment);
    scheme_register_proc(OP_ADD, "+", environment);
    scheme_register_proc(OP_SUB, "-", environment);
    scheme_register_proc(OP_MUL, "*", environment);
    scheme_register_proc(OP_DIV, "/", environment);
    scheme_register_proc(OP_ABS, "abs", environment);
    scheme_register_proc(OP_QUO, "quotient", environment);
    scheme_register_proc(OP_REM, "remainder", environment);
    scheme_register_proc(OP_MOD, "modulo", environment);
    scheme_register_proc(OP_GCD, "gcd", environment);
    scheme_register_proc(OP_LCM, "lcm", environment);
    scheme_register_proc(OP_FLOOR, "floor", environment);
    scheme_register_proc(OP_CEILING, "ceiling", environment);
    scheme_register_proc(OP_TRUNCATE, "truncate", environment);
    scheme_register_proc(OP_ROUND, "round", environment);
    scheme_register_proc(OP_EXP, "exp", environment);
    scheme_register_proc(OP_LOG, "log", environment);
    scheme_register_proc(OP_SIN, "sin", environment);
    scheme_register_proc(OP_COS, "cos", environment);
    scheme_register_proc(OP_TAN, "tan", environment);
    scheme_register_proc(OP_ASIN, "asin", environment);
    scheme_register_proc(OP_ACOS, "acos", environment);
    scheme_register_proc(OP_ATAN, "atan", environment);
    scheme_register_proc(OP_SQRT, "sqrt", environment);
    scheme_register_proc(OP_EXPT, "expt", environment);
    scheme_register_proc(OP_EX2INEX, "exact->inexact", environment);
    scheme_register_proc(OP_INEX2EX, "inexact->exact", environment);
    scheme_register_proc(OP_NUM2STR, "number->string", environment);
    scheme_register_proc(OP_STR2NUM, "string->number", environment);
    scheme_register_proc(OP_CHAR2INT, "char->integer", environment);
    scheme_register_proc(OP_INT2CHAR, "integer->char", environment);
    scheme_register_proc(OP_CHARUPCASE, "char-upcase", environment);
    scheme_register_proc(OP_CHARDNCASE, "char-downcase", environment);
    scheme_register_proc(OP_MKSTRING, "make-string", environment);
    scheme_register_proc(OP_STRING, "string", environment);
    scheme_register_proc(OP_STRLEN, "string-length", environment);
    scheme_register_proc(OP_STRREF, "string-ref", environment);
    scheme_register_proc(OP_STRSET, "string-set!", environment);
    scheme_register_proc(OP_STREQU, "string=?", environment);
    scheme_register_proc(OP_STRLSS, "string<?", environment);
    scheme_register_proc(OP_STRGTR, "string>?", environment);
    scheme_register_proc(OP_STRLEQ, "string<=?", environment);
    scheme_register_proc(OP_STRGEQ, "string>=?", environment);
    scheme_register_proc(OP_STRCIEQU, "string-ci=?", environment);
    scheme_register_proc(OP_STRCILSS, "string-ci<?", environment);
    scheme_register_proc(OP_STRCIGTR, "string-ci>?", environment);
    scheme_register_proc(OP_STRCILEQ, "string-ci<=?", environment);
    scheme_register_proc(OP_STRCIGEQ, "string-ci>=?", environment);
    scheme_register_proc(OP_SUBSTR, "substring", environment);
    scheme_register_proc(OP_STRAPPEND, "string-append", environment);
    scheme_register_proc(OP_STR2LIST, "string->list", environment);
    scheme_register_proc(OP_LIST2STR, "list->string", environment);
    scheme_register_proc(OP_STRCOPY, "string-copy", environment);
    scheme_register_proc(OP_STRFILL, "string-fill!", environment);
    scheme_register_proc(OP_VECTOR, "vector", environment);
    scheme_register_proc(OP_MKVECTOR, "make-vector", environment);
    scheme_register_proc(OP_VECLEN, "vector-length", environment);
    scheme_register_proc(OP_VECREF, "vector-ref", environment);
    scheme_register_proc(OP_VECSET, "vector-set!", environment);
    scheme_register_proc(OP_VEC2LIST, "vector->list", environment);
    scheme_register_proc(OP_LIST2VEC, "list->vector", environment);
    scheme_register_proc(OP_VECFILL, "vector-fill!", environment);
    scheme_register_proc(OP_NOT, "not", environment);
    scheme_register_proc(OP_BOOL, "boolean?", environment);
    scheme_register_proc(OP_SYMBOL, "symbol?", environment);
    scheme_register_proc(OP_SYM2STR, "symbol->string", environment);
    scheme_register_proc(OP_STR2SYM, "string->symbol", environment);
    scheme_register_proc(OP_NUMBER, "number?", environment);
    scheme_register_proc(OP_STRINGP, "string?", environment);
    scheme_register_proc(OP_INTEGER, "integer?", environment);
    scheme_register_proc(OP_REAL, "real?", environment);
    scheme_register_proc(OP_EXACT, "exact?", environment);
    scheme_register_proc(OP_INEXACT, "inexact?", environment);
    scheme_register_proc(OP_CHAR, "char?", environment);
    scheme_register_proc(OP_CHAREQU, "char=?", environment);
    scheme_register_proc(OP_CHARLSS, "char<?", environment);
    scheme_register_proc(OP_CHARGTR, "char>?", environment);
    scheme_register_proc(OP_CHARLEQ, "char<=?", environment);
    scheme_register_proc(OP_CHARGEQ, "char>=?", environment);
    scheme_register_proc(OP_CHARCIEQU, "char-ci=?", environment);
    scheme_register_proc(OP_CHARCILSS, "char-ci<?", environment);
    scheme_register_proc(OP_CHARCIGTR, "char-ci>?", environment);
    scheme_register_proc(OP_CHARCILEQ, "char-ci<=?", environment);
    scheme_register_proc(OP_CHARCIGEQ, "char-ci>=?", environment);
    scheme_register_proc(OP_CHARAP, "char-alphabetic?", environment);
    scheme_register_proc(OP_CHARNP, "char-numeric?", environment);
    scheme_register_proc(OP_CHARWP, "char-whitespace?", environment);
    scheme_register_proc(OP_CHARUP, "char-upper-case?", environment);
    scheme_register_proc(OP_CHARLP, "char-lower-case?", environment);
    scheme_register_proc(OP_PROC, "procedure?", environment);
    scheme_register_proc(OP_PAIR, "pair?", environment);
    scheme_register_proc(OP_LISTP, "list?", environment);
    scheme_register_proc(OP_PORTP, "port?", environment);
    scheme_register_proc(OP_INPORTP, "input-port?", environment);
    scheme_register_proc(OP_OUTPORTP, "output-port?", environment);
    scheme_register_proc(OP_VECTORP, "vector?", environment);
    scheme_register_proc(OP_ENVP, "environment?", environment);
    scheme_register_proc(OP_EQ, "eq?", environment);
    scheme_register_proc(OP_EQV, "eqv?", environment);
    scheme_register_proc(OP_EQUAL, "equal?", environment);
    scheme_register_proc(OP_NULL, "null?", environment);
    scheme_register_proc(OP_EOFOBJP, "eof-object?", environment);
    scheme_register_proc(OP_ZEROP, "zero?", environment);
    scheme_register_proc(OP_POSP, "positive?", environment);
    scheme_register_proc(OP_NEGP, "negative?", environment);
    scheme_register_proc(OP_ODD, "odd?", environment);
    scheme_register_proc(OP_EVEN, "even?", environment);
    scheme_register_proc(OP_NEQ, "=", environment);
    scheme_register_proc(OP_LESS, "<", environment);
    scheme_register_proc(OP_GRE, ">", environment);
    scheme_register_proc(OP_LEQ, "<=", environment);
    scheme_register_proc(OP_GEQ, ">=", environment);
    scheme_register_proc(OP_MAX, "max", environment);
    scheme_register_proc(OP_MIN, "min", environment);
    scheme_register_proc(OP_CHAR_READY, "char-ready?", environment);
    scheme_register_proc(OP_WRITE_CHAR, "write-char", environment);
    scheme_register_proc(OP_WRITE, "write", environment);
    scheme_register_proc(OP_ERR0, "error", environment);
    scheme_register_proc(OP_REVERSE, "reverse", environment);
    scheme_register_proc(OP_APPEND, "append", environment);
    scheme_register_proc(OP_GC, "gc", environment);
    scheme_register_proc(OP_GCVERB, "gc-verbose", environment);
    scheme_register_proc(OP_CALL_INFILE0, "call-with-input-file", environment);
    scheme_register_proc(OP_CALL_OUTFILE0, "call-with-output-file", environment);
    scheme_register_proc(OP_CURR_INPORT, "current-input-port", environment);
    scheme_register_proc(OP_CURR_OUTPORT, "current-output-port", environment);
    scheme_register_proc(OP_WITH_INFILE0, "with-input-from-file", environment);
    scheme_register_proc(OP_WITH_OUTFILE0, "with-output-to-file", environment);
    scheme_register_proc(OP_OPEN_INFILE, "open-input-file", environment);
    scheme_register_proc(OP_OPEN_OUTFILE, "open-output-file", environment);
    scheme_register_proc(OP_OPEN_INOUTFILE, "open-input-output-file", environment);
    scheme_register_proc(OP_OPEN_INSTRING, "open-input-string", environment);
    scheme_register_proc(OP_OPEN_OUTSTRING, "open-output-string", environment);
    scheme_register_proc(OP_OPEN_INOUTSTRING, "open-input-output-string", environment);
    scheme_register_proc(OP_GET_OUTSTRING, "get-output-string", environment);
    scheme_register_proc(OP_CLOSE_INPORT, "close-input-port", environment);
    scheme_register_proc(OP_CLOSE_OUTPORT, "close-output-port", environment);
    scheme_register_proc(OP_CLOSE_PORT, "close-port", environment);
    scheme_register_proc(OP_INT_ENV, "interaction-environment", environment);

    scheme_register_proc(OP_DISPLAY, "display", environment);
    scheme_register_proc(OP_NEWLINE, "newline", environment);


    scheme_register_proc(OP_READ_CHAR, "read-char", environment);
    scheme_register_proc(OP_PEEK_CHAR, "peek-char", environment);
    scheme_register_proc(OP_SET_INPORT, "set-input-port", environment);
    scheme_register_proc(OP_SET_OUTPORT, "set-output-port", environment);
    scheme_register_proc(OP_LIST_LENGTH, "length", environment);	/* a.k */
    scheme_register_proc(OP_MEMQ, "memq", environment);
    scheme_register_proc(OP_MEMV, "memv", environment);
    scheme_register_proc(OP_MEMBER, "member", environment);
    scheme_register_proc(OP_ASSQ, "assq", environment);	/* a.k */
    scheme_register_proc(OP_ASSV, "assv", environment);
    scheme_register_proc(OP_ASSOC, "assoc", environment);
    scheme_register_proc(OP_DEFP, "defined?", environment);
    scheme_register_proc(OP_MKCLOSURE, "make-closure", environment);
    scheme_register_proc(OP_GET_CLOSURE, "get-closure-code", environment);	/* a.k */
    scheme_register_proc(OP_CLOSUREP, "closure?", environment);	/* a.k */
    scheme_register_proc(OP_MACROP, "macro?", environment);	/* a.k */
    scheme_register_proc(OP_MACRO_EXPAND0, "macro-expand", environment);
    scheme_register_proc(OP_ATOMP, "atom?", environment);
    scheme_register_proc(OP_GENSYM, "gensym", environment);
    scheme_register_proc(OP_QUIT, "quit", environment);
    scheme_register_proc(OP_EMERGENCY_EXIT, "emergency-exit", environment);



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
return_value = autoscheme_eval(cons(mk_symbol("define"),cons(mk_symbol("exit"),cons(F,NIL))), environment);
return_value = autoscheme_eval(cons(mk_symbol("call-with-current-continuation"),cons(cons(mk_symbol("lambda"),cons(cons(mk_symbol("return"),NIL),cons(cons(mk_symbol("let"),cons(cons(cons(mk_symbol("obj"),cons(cons(mk_symbol("call-with-current-continuation"),cons(cons(mk_symbol("lambda"),cons(cons(mk_symbol("_exit"),NIL),cons(cons(mk_symbol("cond"),cons(cons(cons(mk_symbol("not"),cons(mk_symbol("exit"),NIL)),cons(cons(mk_symbol("set!"),cons(mk_symbol("exit"),cons(mk_symbol("_exit"),NIL))),cons(cons(mk_symbol("return"),NIL),NIL))),NIL)),NIL))),NIL)),NIL)),NIL),cons(cons(mk_symbol("emergency-exit"),cons(cons(mk_symbol("cond"),cons(cons(cons(mk_symbol("integer?"),cons(mk_symbol("obj"),NIL)),cons(mk_symbol("obj"),NIL)),cons(cons(cons(mk_symbol("eq?"),cons(mk_symbol("obj"),cons(F,NIL))),cons(mk_integer(1),NIL)),cons(cons(mk_symbol("else"),cons(mk_integer(0),NIL)),NIL)))),NIL)),NIL))),NIL))),NIL)), environment);
return_value = autoscheme_eval(cons(mk_symbol("define"),cons(mk_symbol("object->string"),cons(cons(mk_symbol("lambda"),cons(cons(mk_symbol("object"),NIL),cons(cons(mk_symbol("let"),cons(cons(cons(mk_symbol("string-port"),cons(cons(mk_symbol("open-output-string"),NIL),NIL)),NIL),cons(cons(mk_symbol("write"),cons(mk_symbol("object"),cons(mk_symbol("string-port"),NIL))),cons(cons(mk_symbol("let"),cons(cons(cons(mk_symbol("output-string"),cons(cons(mk_symbol("get-output-string"),cons(mk_symbol("string-port"),NIL)),NIL)),NIL),cons(cons(mk_symbol("close-output-port"),cons(mk_symbol("string-port"),NIL)),cons(mk_symbol("output-string"),NIL)))),NIL)))),NIL))),NIL))), environment);
return_value = autoscheme_eval(cons(mk_symbol("define"),cons(mk_symbol("r7-read-string"),cons(cons(mk_symbol("lambda"),cons(cons(mk_symbol("k"),mk_symbol("rest")),cons(cons(mk_symbol("if"),cons(cons(mk_symbol("="),cons(mk_symbol("k"),cons(mk_integer(0),NIL))),cons(mk_string(""),cons(cons(mk_symbol("let"),cons(mk_symbol("read-chars"),cons(cons(cons(mk_symbol("s"),cons(cons(mk_symbol("make-string"),cons(mk_symbol("k"),NIL)),NIL)),cons(cons(mk_symbol("i"),cons(mk_integer(0),NIL)),cons(cons(mk_symbol("c"),cons(cons(mk_symbol("apply"),cons(mk_symbol("read-char"),cons(mk_symbol("rest"),NIL))),NIL)),NIL))),cons(cons(mk_symbol("cond"),cons(cons(cons(mk_symbol("and"),cons(cons(mk_symbol("eof-object?"),cons(mk_symbol("c"),NIL)),cons(cons(mk_symbol("="),cons(mk_symbol("i"),cons(mk_integer(0),NIL))),NIL))),cons(mk_symbol("c"),NIL)),cons(cons(cons(mk_symbol("eof-object?"),cons(mk_symbol("c"),NIL)),cons(cons(mk_symbol("substring"),cons(mk_symbol("s"),cons(mk_integer(0),cons(cons(mk_symbol("+"),cons(mk_symbol("i"),cons(mk_integer(1),NIL))),NIL)))),NIL)),cons(cons(cons(mk_symbol("<"),cons(mk_symbol("i"),cons(cons(mk_symbol("-"),cons(mk_symbol("k"),cons(mk_integer(1),NIL))),NIL))),cons(cons(mk_symbol("string-set!"),cons(mk_symbol("s"),cons(mk_symbol("i"),cons(mk_symbol("c"),NIL)))),cons(cons(mk_symbol("read-chars"),cons(mk_symbol("s"),cons(cons(mk_symbol("+"),cons(mk_symbol("i"),cons(mk_integer(1),NIL))),cons(cons(mk_symbol("apply"),cons(mk_symbol("read-char"),cons(mk_symbol("rest"),NIL))),NIL)))),NIL))),cons(cons(mk_symbol("else"),cons(cons(mk_symbol("string-set!"),cons(mk_symbol("s"),cons(mk_symbol("i"),cons(mk_symbol("c"),NIL)))),cons(cons(mk_symbol("substring"),cons(mk_symbol("s"),cons(mk_integer(0),cons(cons(mk_symbol("+"),cons(mk_symbol("i"),cons(mk_integer(1),NIL))),NIL)))),NIL))),NIL))))),NIL)))),NIL)))),NIL))),NIL))), environment);
return_value = autoscheme_eval(cons(mk_symbol("define"),cons(mk_symbol("auto-read-string"),cons(cons(mk_symbol("lambda"),cons(mk_symbol("args"),cons(cons(mk_symbol("let"),cons(cons(cons(mk_symbol("k"),cons(cons(mk_symbol("if"),cons(cons(mk_symbol("null?"),cons(mk_symbol("args"),NIL)),cons(F,cons(cons(mk_symbol("car"),cons(mk_symbol("args"),NIL)),NIL)))),NIL)),cons(cons(mk_symbol("rest"),cons(cons(mk_symbol("if"),cons(cons(mk_symbol("pair?"),cons(mk_symbol("args"),NIL)),cons(cons(mk_symbol("cdr"),cons(mk_symbol("args"),NIL)),cons(cons( mk_symbol("quote" ),cons(NIL,NIL)),NIL)))),NIL)),NIL)),cons(cons(mk_symbol("if"),cons(cons(mk_symbol("not"),cons(mk_symbol("k"),NIL)),cons(cons(mk_symbol("let"),cons(cons(cons(mk_symbol("s"),cons(cons(mk_symbol("apply"),cons(mk_symbol("r7-read-string"),cons(cons(mk_symbol("cons"),cons(mk_integer(64),cons(mk_symbol("rest"),NIL))),NIL))),NIL)),NIL),cons(cons(mk_symbol("if"),cons(cons(mk_symbol("or"),cons(cons(mk_symbol("eof-object?"),cons(mk_symbol("s"),NIL)),cons(cons(mk_symbol("zero?"),cons(cons(mk_symbol("string-length"),cons(mk_symbol("s"),NIL)),NIL)),NIL))),cons(mk_string(""),cons(cons(mk_symbol("string-append"),cons(mk_symbol("s"),cons(cons(mk_symbol("apply"),cons(mk_symbol("auto-read-string"),cons(cons(mk_symbol("cons"),cons(F,cons(mk_symbol("rest"),NIL))),NIL))),NIL))),NIL)))),NIL))),cons(cons(mk_symbol("apply"),cons(mk_symbol("r7-read-string"),cons(mk_symbol("args"),NIL))),NIL)))),NIL))),NIL))),NIL))), environment);
return_value = autoscheme_eval(cons(mk_symbol("define"),cons(mk_symbol("read-string"),cons(mk_symbol("auto-read-string"),NIL))), environment);
return return_value;
}
