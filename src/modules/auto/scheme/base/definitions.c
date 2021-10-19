/*  This file is part of the 'AutoScheme' project.
 *  Copyright 2021 Steven Wiley <s.wiley@katchitek.com> 
 *  SPDX-License-Identifier: BSD-2-Clause
 */
pointer INITIALIZE_LIBRARY__auto_scheme_base( pointer environment )
{
    scheme_register_proc(OP_MUL, "*", environment);
    scheme_register_proc(OP_ADD, "+", environment);
    scheme_register_proc(OP_SUB, "-", environment);

/* ... */

    scheme_register_proc(OP_DIV, "/", environment);
    scheme_register_proc(OP_LESS, "<", environment);
    scheme_register_proc(OP_LEQ, "<=", environment);
    scheme_register_proc(OP_NEQ, "=", environment);
    scheme_register_syntax(OP_FEEDTO, "=>", environment);
    scheme_register_proc(OP_GRE, ">", environment);
    scheme_register_proc(OP_GEQ, ">=", environment);
    scheme_register_proc(OP_ABS, "abs", environment);
    scheme_register_syntax(OP_AND0, "and", environment);
    scheme_register_proc(OP_APPEND, "append", environment);
    scheme_register_proc(OP_PAPPLY, "apply", environment);
    scheme_register_proc(OP_ASSOC, "assoc", environment);
    scheme_register_proc(OP_ASSQ, "assq", environment);	/* a.k */
    scheme_register_proc(OP_ASSV, "assv", environment);
    scheme_register_syntax(OP_BEGIN, "begin", environment);

/* binary-port? */
/* boolean=? */

    scheme_register_proc(OP_BOOL, "boolean?", environment);

/* bytevector */
/* bytevector-append */
/* bytevector-copy */
/* bytevector-copy! */
/* bytevector-length */
/* bytevector-u8-ref */
/* bytevector-u8-set! */
/* bytevector? */

    scheme_register_proc(OP_CAAR, "caar", environment);
    scheme_register_proc(OP_CADR, "cadr", environment);
    scheme_register_proc(OP_CONTINUATION, "call-with-current-continuation", environment);

/* call-with-port */

    scheme_register_proc(OP_WITHVALUES0, "call-with-values", environment);
    scheme_register_proc(OP_CONTINUATION, "call/cc", environment);
    scheme_register_proc(OP_CAR, "car", environment);
    scheme_register_syntax(OP_CASE0, "case", environment);
    scheme_register_proc(OP_CDAR, "cdar", environment);
    scheme_register_proc(OP_CDDR, "cddr", environment);
    scheme_register_proc(OP_CDR, "cdr", environment);
    scheme_register_proc(OP_CEILING, "ceiling", environment);
    scheme_register_proc(OP_CHAR2INT, "char->integer", environment);
    scheme_register_proc(OP_CHAR_READY, "char-ready?", environment);
    scheme_register_proc(OP_CHARLEQ, "char<=?", environment);
    scheme_register_proc(OP_CHARLSS, "char<?", environment);
    scheme_register_proc(OP_CHAREQU, "char=?", environment);
    scheme_register_proc(OP_CHARGEQ, "char>=?", environment);
    scheme_register_proc(OP_CHARGTR, "char>?", environment);
    scheme_register_proc(OP_CHAR, "char?", environment);
    scheme_register_proc(OP_CLOSE_INPORT, "close-input-port", environment);
    scheme_register_proc(OP_CLOSE_OUTPORT, "close-output-port", environment);
    scheme_register_proc(OP_CLOSE_PORT, "close-port", environment);

/* complex? */

    scheme_register_syntax(OP_COND0, "cond", environment);

/* cond-expand */

    scheme_register_proc(OP_CONS, "cons", environment);

/* current-error-port */

    scheme_register_proc(OP_CURR_INPORT, "current-input-port", environment);
    scheme_register_proc(OP_CURR_OUTPORT, "current-output-port", environment);
    scheme_register_syntax(OP_DEF0, "define", environment);

/* define-record-type */

    scheme_register_syntax(OP_DEFSYNTAX0, "define-syntax", environment);

/* define-values */
/* denominator */

    scheme_register_syntax(OP_DO0, "do", environment);
    scheme_register_proc(OP_DYNAMICWIND0, "dynamic-wind", environment);
    scheme_register_syntax(OP_ELSE, "else", environment);

/* eof-object */

    scheme_register_proc(OP_EOFOBJP, "eof-object?", environment);

    scheme_register_proc(OP_EQ, "eq?", environment);
    scheme_register_proc(OP_EQUAL, "equal?", environment);
    scheme_register_proc(OP_EQV, "eqv?", environment);
    scheme_register_proc(OP_ERR0, "error", environment);

/* error-object-irritants */
/* error-object-message */
/* error-object? */

    scheme_register_proc(OP_EVEN, "even?", environment);

/* exact */     scheme_register_proc(OP_INEX2EX, "inexact->exact", environment);

    scheme_register_proc(OP_EXACT, "exact?", environment);
    scheme_register_proc(OP_EXPT, "expt", environment);

/* features */
/* file-error? */

    scheme_register_proc(OP_FLOOR, "floor", environment);

/* floor-quotient */
/* floor-remainder */
/* floor/ */
/* flush-output-port */

    scheme_register_proc(OP_FOREACH0, "for-each", environment);
    scheme_register_proc(OP_GCD, "gcd", environment);

/* get-output-bytvector */

    scheme_register_proc(OP_GET_OUTSTRING, "get-output-string", environment);

/* guard */

    scheme_register_syntax(OP_IF0, "if", environment);

/* include */
/* include-ci */
/* inexact */     scheme_register_proc(OP_EX2INEX, "exact->inexact", environment);

    scheme_register_proc(OP_INEXACT, "inexact?", environment);

/* input-port-open? */

    scheme_register_proc(OP_INPORTP, "input-port?", environment);
    scheme_register_proc(OP_INT2CHAR, "integer->char", environment);
    scheme_register_proc(OP_INTEGER, "integer?", environment);
    scheme_register_syntax(OP_LAMBDA, "lambda", environment);
    scheme_register_proc(OP_LCM, "lcm", environment);
    scheme_register_proc(OP_LIST_LENGTH, "length", environment);	/* a.k */
    scheme_register_syntax(OP_LET0, "let", environment);
    scheme_register_syntax(OP_LET0AST, "let*", environment);

/* let*-values */

    scheme_register_syntax(OP_LETSYNTAX0, "let-syntax", environment);

/* let-values */

    scheme_register_syntax(OP_LET0REC, "letrec", environment);
    scheme_register_syntax(OP_LETRECAST0, "letrec*", environment);
    scheme_register_syntax(OP_LETRECSYNTAX0, "letrec-syntax", environment);
    scheme_register_proc(OP_LIST, "list", environment);
    scheme_register_proc(OP_LIST2STR, "list->string", environment);
    scheme_register_proc(OP_LIST2VEC, "list->vector", environment);

/* list-copy */

    scheme_register_proc(OP_LISTREF, "list-ref", environment);

/* list-set! */

    scheme_register_proc(OP_LISTTAIL, "list-tail", environment);
    scheme_register_proc(OP_LISTP, "list?", environment);

/* make-bytevector */
/* make-list */
/* make-parameter */

    scheme_register_proc(OP_MKSTRING, "make-string", environment);
    scheme_register_proc(OP_MKVECTOR, "make-vector", environment);
    scheme_register_proc(OP_MAP0, "map", environment);
    scheme_register_proc(OP_MAX, "max", environment);
    scheme_register_proc(OP_MEMBER, "member", environment);
    scheme_register_proc(OP_MEMQ, "memq", environment);
    scheme_register_proc(OP_MEMV, "memv", environment);
    scheme_register_proc(OP_MIN, "min", environment);
    scheme_register_proc(OP_MOD, "modulo", environment);
    scheme_register_proc(OP_NEGP, "negative?", environment);
    scheme_register_proc(OP_NEWLINE, "newline", environment);
    scheme_register_proc(OP_NOT, "not", environment);
    scheme_register_proc(OP_NULL, "null?", environment);
    scheme_register_proc(OP_NUM2STR, "number->string", environment);
    scheme_register_proc(OP_NUMBER, "number?", environment);

/* numerator? */

    scheme_register_proc(OP_ODD, "odd?", environment);

/* open-input-bytevector */

    scheme_register_proc(OP_OPEN_INSTRING, "open-input-string", environment);

/* open-output-bytevector */

    scheme_register_proc(OP_OPEN_OUTSTRING, "open-output-string", environment);
    scheme_register_syntax(OP_OR0, "or", environment);

/* output-port-open? */

    scheme_register_proc(OP_OUTPORTP, "output-port?", environment);
    scheme_register_proc(OP_PAIR, "pair?", environment);

/* parameterize */

    scheme_register_proc(OP_PEEK_CHAR, "peek-char", environment);

/* peek-u8 */

    scheme_register_proc(OP_PORTP, "port?", environment);
    scheme_register_proc(OP_POSP, "positive?", environment);
    scheme_register_proc(OP_PROC, "procedure?", environment);
    scheme_register_syntax(OP_QQUOTE0, "quasiquote", environment);
    scheme_register_syntax(OP_QUOTE, "quote", environment);
    scheme_register_proc(OP_QUO, "quotient", environment);


/* raise */
/* raise-continuable */
/* rational? */
/* rationalize */
/* read-bytevector */
/* read-bytevector! */

    scheme_register_proc(OP_READ_CHAR, "read-char", environment);

/* read-error? */
/* read-line */
/* read-string */
/* read-u8 */

    scheme_register_proc(OP_REAL, "real?", environment);
    scheme_register_proc(OP_REM, "remainder", environment);
    scheme_register_proc(OP_REVERSE, "reverse", environment);
    scheme_register_proc(OP_ROUND, "round", environment);
    scheme_register_syntax(OP_SET0, "set!", environment);
    scheme_register_proc(OP_SETCAR, "set-car!", environment);
    scheme_register_proc(OP_SETCDR, "set-cdr!", environment);

/* square */

    scheme_register_proc(OP_STRING, "string", environment);
    scheme_register_proc(OP_STR2LIST, "string->list", environment);
    scheme_register_proc(OP_STR2NUM, "string->number", environment);
    scheme_register_proc(OP_STR2SYM, "string->symbol", environment);

/* string->utf8 */
/* string->vector */

    scheme_register_proc(OP_STRAPPEND, "string-append", environment);
    scheme_register_proc(OP_STRCOPY, "string-copy", environment);

/* string-copy! */

    scheme_register_proc(OP_STRFILL, "string-fill!", environment);

/* string-for-each */

    scheme_register_proc(OP_STRLEN, "string-length", environment);

/* string-map */

    scheme_register_proc(OP_STRREF, "string-ref", environment);
    scheme_register_proc(OP_STRSET, "string-set!", environment);
    scheme_register_proc(OP_STRLEQ, "string<=?", environment);
    scheme_register_proc(OP_STRLSS, "string<?", environment);
    scheme_register_proc(OP_STREQU, "string=?", environment);
    scheme_register_proc(OP_STRGEQ, "string>=?", environment);
    scheme_register_proc(OP_STRGTR, "string>?", environment);
    scheme_register_proc(OP_STRINGP, "string?", environment);
    scheme_register_proc(OP_SUBSTR, "substring", environment);
    scheme_register_proc(OP_SYM2STR, "symbol->string", environment);

/* symbol=? */

    scheme_register_proc(OP_SYMBOL, "symbol?", environment);

/* syntax-error */

    scheme_register_syntax(OP_SYNTAXRULES, "syntax-rules", environment);

/* textual-port? */

    scheme_register_proc(OP_TRUNCATE, "truncate", environment);

/* truncate-quotient */
/* truncate-remainder */
/* truncate/ */
/* u8-ready? */

    scheme_register_syntax(OP_UNLESS0, "unless", environment);

/* unquote */
/* unquote-splicing */
/* utf8->string */

    scheme_register_proc(OP_VALUES, "values", environment);
    scheme_register_proc(OP_VECTOR, "vector", environment);
    scheme_register_proc(OP_VEC2LIST, "vector->list", environment);

/* vector->string */
/* vector-append */
/* vector-copy */
/* vector-copy! */

    scheme_register_proc(OP_VECFILL, "vector-fill!", environment);

/* vector-for-each */

    scheme_register_proc(OP_VECLEN, "vector-length", environment);

/* vector-map */

    scheme_register_proc(OP_VECREF, "vector-ref", environment);
    scheme_register_proc(OP_VECSET, "vector-set!", environment);
    scheme_register_proc(OP_VECTORP, "vector?", environment);
    scheme_register_syntax(OP_WHEN0, "when", environment);

/* with-exception-handler */
/* write-bytevector */

    scheme_register_proc(OP_WRITE_CHAR, "write-char", environment);

/* write-string */
/* write-u8 */

    scheme_register_proc(OP_ZEROP, "zero?", environment);


/* ************************ */


    scheme_register_syntax(OP_DELAY, "delay", environment);
    scheme_register_proc(OP_FORCE, "force", environment);

    scheme_register_syntax(OP_LAZY, "lazy", environment);
    scheme_register_proc(OP_EAGER, "eager", environment);
    scheme_register_syntax(OP_C0STREAM, "cons-stream", environment);


  

    scheme_register_proc(OP_GC, "gc", environment);
    scheme_register_proc(OP_GCVERB, "gc-verbose", environment);




    scheme_register_proc(OP_MKCLOSURE, "make-closure", environment);
    scheme_register_proc(OP_GET_CLOSURE, "get-closure-code", environment);	/* a.k */
    scheme_register_proc(OP_CLOSUREP, "closure?", environment);	/* a.k */


    scheme_register_proc(OP_GENSYM, "gensym", environment);


    scheme_register_syntax(OP_RECEIVE0, "receive", environment);
    scheme_register_proc(OP_QUIT, "quit", environment);
    scheme_register_proc(OP_DEFP, "defined?", environment);
    scheme_register_proc(OP_ATOMP, "atom?", environment);



    return environment;
}
