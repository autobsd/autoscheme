/*  This file is part of the 'AutoScheme' project.
 *  Copyright 2021 Steven Wiley <s.wiley@katchitek.com> 
 *  SPDX-License-Identifier: BSD-2-Clause
 */
#include "autoscheme.h"
int LOAD_MODULE__auto_scheme(pointer environment);
/*  This file is part of the 'AutoScheme' project.
 *  Copyright 2021 Steven Wiley <s.wiley@katchitek.com> 
 *  SPDX-License-Identifier: BSD-2-Clause
 */
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
static pointer ff_environment_p( pointer args );
static pointer ff_environment_define_d( pointer args );
static pointer ff_environment_undefine_d( pointer args );
static pointer ff_environment_defined_symbols( pointer args );
static pointer ff_environment_assoc( pointer args );
static pointer ff_environment_ref( pointer args );
static pointer ff_environment_update_d( pointer args );
static pointer ff_environment_import_d( pointer args );
static pointer ff_environment_only( pointer args );
static pointer ff_environment_except( pointer args );
static pointer ff_environment_prefix( pointer args );
static pointer ff_environment_rename( pointer args );

static pointer ff_environment_delete_d( pointer args );
 /*  This file is part of the 'AutoScheme' project.
 *  Copyright 2021 Steven Wiley <s.wiley@katchitek.com> 
 *  SPDX-License-Identifier: BSD-2-Clause
 */
static pointer ff_environment_p( pointer args )
{
    if( is_environment( car( args )))
	return T;

    return F;
}

static pointer environment_define_d( pointer environment, pointer symbol, pointer value )
{
    pointer x;

    for( x = car( environment ); is_pair( x ); x = cdr( x )) 
    {
	if( caar( x ) == symbol )
	{
	    cdar( x ) = value;
		
	    return environment;
	}
    }

    car( environment ) = cons( cons( symbol, value ), car( environment ));

    return environment;
}

static pointer ff_environment_define_d( pointer args )
{
    pointer environment = car( args );
    pointer symbol = cadr( args );
    pointer value = caddr( args );


    return environment_define_d( environment, symbol, value );
}
 



static pointer ff_environment_undefine_d( pointer args )
{
    pointer environment = car( args );
    pointer symbols = cdr( args );

    pointer x;

    for( x = symbols; is_pair( x ); x = cdr( x ))
    {
	environment_define_d( environment, car( x ), UNDEF );
    }
    return environment;
}

	

static pointer environment_defined_symbols( pointer environment )
{
    pointer defined_symbols = NIL;
    pointer undefined_symbols = NIL;
    pointer symbol;

    pointer x, y;

    for( x = environment; is_pair( x ); x = cdr( x )) 
    {
	for( y = car( x ); is_pair( y ); y = cdr( y )) 
	{
	    symbol = caar( y );

	    if( ! member( symbol, defined_symbols ) && ! member( symbol, undefined_symbols ))
	    {
		if( cdar( y ) == UNDEF )
		    undefined_symbols = cons( symbol, undefined_symbols );
		else
		    defined_symbols = cons( symbol, defined_symbols );
	    }
	}
    }
    return reverse( defined_symbols );
}

static pointer ff_environment_defined_symbols( pointer args )
{
    pointer environment = car( args );

    return environment_defined_symbols( environment );
}


static pointer environment_assoc( pointer environment, pointer symbol )
{
    pointer x, y;

    for( x = environment; is_pair( x ); x = cdr( x )) 
    {
	for( y = car( x ); is_pair( y ); y = cdr( y )) 
	{
	    if( caar( y ) == symbol )
	    {
		if( cdar( y ) == UNDEF )
		    return F;
		else
		    return car( y );
	    }
	}
    }
    return F;
}

static pointer ff_environment_assoc( pointer args )
{
    pointer environment = car( args );
    pointer symbol = cadr( args );
   
    return environment_assoc( environment, symbol );
}

static pointer environment_ref( pointer environment, pointer symbol )
{
    pointer binding = environment_assoc( environment, symbol );

    if( isfalse( binding ))
    {
	char *format_string = "Unbound variable %s";
	char *symbol_name = symname( symbol );
	char *message;
	size_t message_length;

	message_length = ( size_t )snprintf( NULL, 0, format_string, symbol_name );
	message = malloc( message_length + 1);
	snprintf( message, message_length + 1, format_string, symbol_name );
   
	FatalForeignError( message );
	free( message );
    }
    return cdr( binding );    
}
static pointer ff_environment_ref( pointer args )
{
    pointer environment = car( args );
    pointer symbol = cadr( args );

    return environment_ref( environment, symbol );
}



static pointer ff_environment_update_d( pointer args )
{
    pointer environment = car( args );
    pointer symbol = cadr( args );
    pointer value = caddr( args );

    pointer assoc = environment_assoc( environment, symbol );

    if (istrue( assoc ))
	cdr( assoc ) = value;
    else 
	car( environment ) = cons( cons( symbol, value ), car( environment ));

    return environment;
}






static pointer ff_environment_import_d( pointer args )
{
    pointer target = car( args );
    pointer environments = cdr( args );
    pointer symbols;
    pointer value;
    pointer x,y;

    for( x = environments; is_pair( x ); x = cdr( x )) 
    {
	symbols = environment_defined_symbols( car( x ));

	for( y = symbols; is_pair( y ); y = cdr( y ))
	{
	    value = environment_ref( car( x ), car( y ));
	    environment_define_d( target, car( y ), value);
	}
    }
    return target;
}

static pointer ff_environment_only( pointer args )
{
    pointer environment = car( args );
    pointer symbols = cdr( args );
    pointer target = make_environment( NIL );
    pointer value;
    pointer x;

    for( x = symbols; is_pair( x ); x = cdr( x ))
    {
	value = environment_ref( environment, car( x ));
	environment_define_d( target, car( x ), value );
    }

    return target;
}

static pointer ff_environment_except( pointer args )
{
    pointer environment = car( args );
    pointer exceptions = cdr( args );
    pointer target = make_environment( NIL );

    pointer symbols = environment_defined_symbols( environment );
    pointer symbol;

    pointer value;
    pointer x;

    for( x = symbols; is_pair( x ); x = cdr( x ))
    {
	symbol = car( x );

	if( ! member( symbol, exceptions ))
	{
	    value = environment_ref( environment, symbol );
	    environment_define_d( target, symbol, value );
	}
    }

    return target;
}




static pointer ff_environment_prefix( pointer args )
{
    pointer environment = car( args );
    pointer prefix = cadr( args );
    pointer symbols = environment_defined_symbols( environment );
    pointer target = make_environment( NIL );

    pointer symbol;
    pointer value;

    size_t prefixed_string_length;
    pointer prefixed_string;

    pointer prefixed_symbol;

    pointer x;

    for( x = symbols; is_pair( x ); x = cdr( x ))
    {
	symbol = car( x );
	value = environment_ref( environment, symbol );

	prefixed_string_length = strlength( prefix ) + strlength( symbol );
	prefixed_string = mk_counted_string( strvalue( prefix ), prefixed_string_length );
	memcpy( strvalue( prefixed_string ) + strlength( prefix ), strvalue( symbol ), strlength( symbol ));
	strvalue( prefixed_string )[prefixed_string_length] = '\0';
	prefixed_symbol = mk_symbol( strvalue( prefixed_string ));

	environment_define_d( target, prefixed_symbol, value );
    }


    return target;
}






static pointer ff_environment_rename( pointer args )
{
    pointer environment = car( args );
    pointer pairs = cdr( args );
    pointer symbols = environment_defined_symbols( environment );
    pointer target = make_environment( NIL );

    pointer pair;
    pointer symbol;
    pointer value;

    pointer x;

    for( x = symbols; is_pair( x ); x = cdr( x ))
    {
	symbol = car( x );
	value = environment_ref( environment, symbol );

	pair = assoc( symbol, pairs );
	
	if( pair == F )
	    environment_define_d( target, symbol, value );
	else
	    environment_define_d( target, cdr( pair ), value );

    }

    return target;
}



static pointer ff_environment_delete_d( pointer args )
{
    pointer environment = car( args );
    pointer symbol = cadr( args );

    pointer x, y;

    pointer *parent;
    
    for( x = environment; is_pair( x ); x = cdr( x )) 
    {
	parent = &(car( x ));

	for( y = car( x ); is_pair( y ); y = cdr( y )) 
	{
	    if( caar( y ) == symbol )
		*parent = cdr( y );
	    else
		parent = &(cdr( y));
	}
    }
    
    return environment;
}
 int LOAD_MODULE__auto_scheme(pointer environment)
{
/*  This file is part of the 'AutoScheme' project.
 *  Copyright 2021 Steven Wiley <s.wiley@katchitek.com> 
 *  SPDX-License-Identifier: BSD-2-Clause
 */
scheme_register_foreign_func( "make-environment",            make_environment               , environment);
scheme_register_foreign_func( "environment?"    ,            ff_environment_p               , environment);
scheme_register_foreign_func( "environment-define!"  ,       ff_environment_define_d        , environment);
scheme_register_foreign_func( "environment-undefine!",       ff_environment_undefine_d      , environment);
scheme_register_foreign_func( "environment-defined-symbols", ff_environment_defined_symbols , environment);
scheme_register_foreign_func( "environment-assoc",           ff_environment_assoc           , environment);
scheme_register_foreign_func( "environment-ref",             ff_environment_ref             , environment);
scheme_register_foreign_func( "environment-update!",         ff_environment_update_d        , environment);
scheme_register_foreign_func( "environment-import!",         ff_environment_import_d        , environment);
scheme_register_foreign_func( "environment-only",            ff_environment_only            , environment);
scheme_register_foreign_func( "environment-except",          ff_environment_except          , environment);
scheme_register_foreign_func( "environment-prefix",          ff_environment_prefix          , environment);
scheme_register_foreign_func( "environment-rename",          ff_environment_rename          , environment);
scheme_register_foreign_func( "environment-delete!",         ff_environment_delete_d        , environment);
 /*  This file is part of the 'AutoScheme' project.
 *  Copyright 2021 Steven Wiley <s.wiley@katchitek.com> 
 *  SPDX-License-Identifier: BSD-2-Clause
 */
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
	scheme_register_syntax(OP_DEFMACRO0, "define-macro", environment);
	scheme_register_syntax(OP_CASE0, "case", environment);
	scheme_register_syntax(OP_WHEN0, "when", environment);
	scheme_register_syntax(OP_UNLESS0, "unless", environment);
	scheme_register_syntax(OP_SYNTAXRULES, "syntax-rules", environment);
	scheme_register_syntax(OP_DEFSYNTAX0, "define-syntax", environment);
	scheme_register_syntax(OP_LETSYNTAX0, "let-syntax", environment);
	scheme_register_syntax(OP_LETRECSYNTAX0, "letrec-syntax", environment);
	scheme_register_syntax(OP_RECEIVE0, "receive", environment);
 /*  This file is part of the 'AutoScheme' project.
 *  Copyright 2021 Steven Wiley <s.wiley@katchitek.com> 
 *  SPDX-License-Identifier: BSD-2-Clause
 */
	/* init procedure */
	scheme_register_proc(OP_PEVAL, "eval", environment);
	scheme_register_proc(OP_PAPPLY, "apply", environment);
	scheme_register_proc(OP_MAP0, "map", environment);
	scheme_register_proc(OP_FOREACH0, "for-each", environment);
	scheme_register_proc(OP_CONTINUATION, "call-with-current-continuation", environment);
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
	scheme_register_proc(OP_CAAAR, "caaar", environment);
	scheme_register_proc(OP_CAADR, "caadr", environment);
	scheme_register_proc(OP_CADAR, "cadar", environment);
	scheme_register_proc(OP_CADDR, "caddr", environment);
	scheme_register_proc(OP_CDAAR, "cdaar", environment);
	scheme_register_proc(OP_CDADR, "cdadr", environment);
	scheme_register_proc(OP_CDDAR, "cddar", environment);
	scheme_register_proc(OP_CDDDR, "cdddr", environment);
	scheme_register_proc(OP_CAAAAR, "caaaar", environment);
	scheme_register_proc(OP_CAAADR, "caaadr", environment);
	scheme_register_proc(OP_CAADAR, "caadar", environment);
	scheme_register_proc(OP_CAADDR, "caaddr", environment);
	scheme_register_proc(OP_CADAAR, "cadaar", environment);
	scheme_register_proc(OP_CADADR, "cadadr", environment);
	scheme_register_proc(OP_CADDAR, "caddar", environment);
	scheme_register_proc(OP_CADDDR, "cadddr", environment);
	scheme_register_proc(OP_CDAAAR, "cdaaar", environment);
	scheme_register_proc(OP_CDAADR, "cdaadr", environment);
	scheme_register_proc(OP_CDADAR, "cdadar", environment);
	scheme_register_proc(OP_CDADDR, "cdaddr", environment);
	scheme_register_proc(OP_CDDAAR, "cddaar", environment);
	scheme_register_proc(OP_CDDADR, "cddadr", environment);
	scheme_register_proc(OP_CDDDAR, "cdddar", environment);
	scheme_register_proc(OP_CDDDDR, "cddddr", environment);
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
	scheme_register_proc(OP_READ, "read", environment);
	scheme_register_proc(OP_CHAR_READY, "char-ready?", environment);
	scheme_register_proc(OP_WRITE_CHAR, "write-char", environment);
	scheme_register_proc(OP_WRITE, "write", environment);
	/* scheme_register_proc(OP_DISPLAY, "display", environment); */
	/* scheme_register_proc(OP_NEWLINE, "newline", environment); */
	scheme_register_proc(OP_LOAD, "load", environment);
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
	scheme_register_proc(OP_CURR_ENV, "current-environment", environment);

	scheme_register_proc(OP_GLOB_ENV, "global-environment", environment);

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
 autoscheme_eval(cons(mk_symbol("begin"),cons(cons(mk_symbol("begin"),cons(T,cons(T,cons(T,cons(cons(cons(mk_symbol("lambda"),cons(cons(mk_symbol("parent-environment"),NIL),cons(cons(mk_symbol("environment-import!"),cons(cons(mk_symbol("current-environment"),NIL),cons(mk_symbol("parent-environment"),NIL))),cons(cons(mk_symbol("environment-define!"),cons(cons(mk_symbol("current-environment"),NIL),cons(cons( mk_symbol("quote" ),cons(mk_symbol("display"),NIL)),cons(mk_proc(OP_DISPLAY,&NIL),NIL)))),cons(cons(mk_symbol("environment-define!"),cons(cons(mk_symbol("current-environment"),NIL),cons(cons( mk_symbol("quote" ),cons(mk_symbol("newline"),NIL)),cons(mk_proc(OP_NEWLINE,&NIL),NIL)))),cons(cons(mk_symbol("define"),cons(mk_symbol("exit"),cons(F,NIL))),cons(cons(mk_symbol("call-with-current-continuation"),cons(cons(mk_symbol("lambda"),cons(cons(mk_symbol("return"),NIL),cons(cons(mk_symbol("let"),cons(cons(cons(mk_symbol("obj"),cons(cons(mk_symbol("call-with-current-continuation"),cons(cons(mk_symbol("lambda"),cons(cons(mk_symbol("_exit"),NIL),cons(cons(mk_symbol("cond"),cons(cons(cons(mk_symbol("not"),cons(mk_symbol("exit"),NIL)),cons(cons(mk_symbol("set!"),cons(mk_symbol("exit"),cons(mk_symbol("_exit"),NIL))),cons(cons(mk_symbol("return"),NIL),NIL))),NIL)),NIL))),NIL)),NIL)),NIL),cons(cons(mk_symbol("emergency-exit"),cons(cons(mk_symbol("cond"),cons(cons(cons(mk_symbol("integer?"),cons(mk_symbol("obj"),NIL)),cons(mk_symbol("obj"),NIL)),cons(cons(cons(mk_symbol("eq?"),cons(mk_symbol("obj"),cons(F,NIL))),cons(mk_integer(1),NIL)),cons(cons(mk_symbol("else"),cons(mk_integer(0),NIL)),NIL)))),NIL)),NIL))),NIL))),NIL)),cons(cons(mk_symbol("define"),cons(mk_symbol("object->string"),cons(cons(mk_symbol("lambda"),cons(cons(mk_symbol("object"),NIL),cons(cons(mk_symbol("let"),cons(cons(cons(mk_symbol("string-port"),cons(cons(mk_symbol("open-output-string"),NIL),NIL)),NIL),cons(cons(mk_symbol("write"),cons(mk_symbol("object"),cons(mk_symbol("string-port"),NIL))),cons(cons(mk_symbol("let"),cons(cons(cons(mk_symbol("output-string"),cons(cons(mk_symbol("get-output-string"),cons(mk_symbol("string-port"),NIL)),NIL)),NIL),cons(cons(mk_symbol("close-output-port"),cons(mk_symbol("string-port"),NIL)),cons(mk_symbol("output-string"),NIL)))),NIL)))),NIL))),NIL))),cons(cons(mk_symbol("begin"),cons(cons(mk_symbol("define-macro"),cons(cons(mk_symbol("define-library"),cons(mk_symbol("name"),mk_symbol("declarations"))),cons(cons(mk_symbol("let"),cons(cons(cons(mk_symbol("name-symbol"),cons(cons(mk_symbol("string->symbol"),cons(cons(mk_symbol("object->string"),cons(mk_symbol("name"),NIL)),NIL)),NIL)),cons(cons(mk_symbol("quoted-declarations"),cons(cons(mk_symbol("map"),cons(cons(mk_symbol("lambda"),cons(cons(mk_symbol("declaration"),NIL),cons(cons( mk_symbol("quasiquote" ),cons(cons(mk_symbol("quote"),cons(cons( mk_symbol("unquote" ),cons(mk_symbol("declaration"),NIL)),NIL)),NIL)),NIL))),cons(mk_symbol("declarations"),NIL))),NIL)),NIL)),cons(cons(mk_symbol("apply"),cons(mk_symbol("environment-define!"),cons(cons(mk_symbol("cons"),cons(cons(mk_symbol("expansion-environment"),NIL),cons(cons(mk_symbol("cons"),cons(mk_symbol("name-symbol"),cons(cons(mk_symbol("list"),cons(cons(mk_symbol("library-eval"),cons(mk_symbol("declarations"),cons(cons(mk_symbol("expansion-environment"),NIL),NIL))),NIL)),NIL))),NIL))),NIL))),NIL))),NIL))),cons(cons(mk_symbol("define-macro"),cons(cons(mk_symbol("import"),mk_symbol("sets")),cons(cons(mk_symbol("apply"),cons(mk_symbol("environment-import-sets!"),cons(cons(mk_symbol("cons"),cons(cons(mk_symbol("expansion-environment"),NIL),cons(cons(mk_symbol("cons"),cons(cons(mk_symbol("expansion-environment"),NIL),cons(mk_symbol("sets"),NIL))),NIL))),NIL))),NIL))),NIL))),cons(cons(mk_symbol("begin"),cons(cons(mk_symbol("define"),cons(mk_symbol("environment-import-sets!"),cons(cons(mk_symbol("lambda"),cons(cons(mk_symbol("target"),cons(mk_symbol("source"),mk_symbol("sets"))),cons(cons(mk_symbol("letrec"),cons(cons(cons(mk_symbol("process-import-set"),cons(cons(mk_symbol("lambda"),cons(cons(mk_symbol("set"),NIL),cons(cons(mk_symbol("if"),cons(cons(mk_symbol("not"),cons(cons(mk_symbol("pair?"),cons(mk_symbol("set"),NIL)),NIL)),cons(cons(mk_symbol("error"),cons(mk_string("improper import-set:"),cons(mk_symbol("set"),NIL))),cons(cons(mk_symbol("cond"),cons(cons(cons(mk_symbol("equal?"),cons(cons(mk_symbol("car"),cons(mk_symbol("set"),NIL)),cons(cons( mk_symbol("quote" ),cons(mk_symbol("only"),NIL)),NIL))),cons(cons(mk_symbol("apply"),cons(mk_symbol("environment-only"),cons(cons(mk_symbol("cons"),cons(cons(mk_symbol("process-import-set"),cons(cons(mk_symbol("cadr"),cons(mk_symbol("set"),NIL)),NIL)),cons(cons(mk_symbol("cddr"),cons(mk_symbol("set"),NIL)),NIL))),NIL))),NIL)),cons(cons(cons(mk_symbol("equal?"),cons(cons(mk_symbol("car"),cons(mk_symbol("set"),NIL)),cons(cons( mk_symbol("quote" ),cons(mk_symbol("except"),NIL)),NIL))),cons(cons(mk_symbol("apply"),cons(mk_symbol("environment-except"),cons(cons(mk_symbol("cons"),cons(cons(mk_symbol("process-import-set"),cons(cons(mk_symbol("cadr"),cons(mk_symbol("set"),NIL)),NIL)),cons(cons(mk_symbol("cddr"),cons(mk_symbol("set"),NIL)),NIL))),NIL))),NIL)),cons(cons(cons(mk_symbol("equal?"),cons(cons(mk_symbol("car"),cons(mk_symbol("set"),NIL)),cons(cons( mk_symbol("quote" ),cons(mk_symbol("prefix"),NIL)),NIL))),cons(cons(mk_symbol("environment-prefix"),cons(cons(mk_symbol("process-import-set"),cons(cons(mk_symbol("cadr"),cons(mk_symbol("set"),NIL)),NIL)),cons(cons(mk_symbol("caddr"),cons(mk_symbol("set"),NIL)),NIL))),NIL)),cons(cons(cons(mk_symbol("equal?"),cons(cons(mk_symbol("car"),cons(mk_symbol("set"),NIL)),cons(cons( mk_symbol("quote" ),cons(mk_symbol("rename"),NIL)),NIL))),cons(cons(mk_symbol("apply"),cons(mk_symbol("environment-rename"),cons(cons(mk_symbol("cons"),cons(cons(mk_symbol("process-import-set"),cons(cons(mk_symbol("cadr"),cons(mk_symbol("set"),NIL)),NIL)),cons(cons(mk_symbol("cddr"),cons(mk_symbol("set"),NIL)),NIL))),NIL))),NIL)),cons(cons(mk_symbol("else"),cons(cons(mk_symbol("environment-ref"),cons(mk_symbol("source"),cons(cons(mk_symbol("string->symbol"),cons(cons(mk_symbol("object->string"),cons(mk_symbol("set"),NIL)),NIL)),NIL))),NIL)),NIL)))))),NIL)))),NIL))),NIL)),NIL),cons(cons(mk_symbol("for-each"),cons(cons(mk_symbol("lambda"),cons(cons(mk_symbol("set"),NIL),cons(cons(mk_symbol("environment-import!"),cons(mk_symbol("target"),cons(cons(mk_symbol("process-import-set"),cons(mk_symbol("set"),NIL)),NIL))),NIL))),cons(mk_symbol("sets"),NIL))),cons(mk_symbol("target"),NIL)))),NIL))),NIL))),cons(cons(mk_symbol("define"),cons(mk_symbol("library-eval"),cons(cons(mk_symbol("lambda"),cons(cons(mk_symbol("declarations"),cons(mk_symbol("environment"),NIL)),cons(cons(mk_symbol("letrec"),cons(cons(cons(mk_symbol("export-declarations"),cons(cons( mk_symbol("quote" ),cons(NIL,NIL)),NIL)),cons(cons(mk_symbol("import-declarations"),cons(cons( mk_symbol("quote" ),cons(NIL,NIL)),NIL)),cons(cons(mk_symbol("begin-declarations"),cons(cons( mk_symbol("quote" ),cons(NIL,NIL)),NIL)),cons(cons(mk_symbol("export-only"),cons(cons( mk_symbol("quote" ),cons(NIL,NIL)),NIL)),cons(cons(mk_symbol("export-rename"),cons(cons( mk_symbol("quote" ),cons(NIL,NIL)),NIL)),cons(cons(mk_symbol("library-environment"),cons(cons(mk_symbol("make-environment"),NIL),NIL)),NIL)))))),cons(cons(mk_symbol("for-each"),cons(cons(mk_symbol("lambda"),cons(cons(mk_symbol("declaration"),NIL),cons(cons(mk_symbol("cond"),cons(cons(cons(mk_symbol("eq?"),cons(cons(mk_symbol("car"),cons(mk_symbol("declaration"),NIL)),cons(cons( mk_symbol("quote" ),cons(mk_symbol("export"),NIL)),NIL))),cons(cons(mk_symbol("set!"),cons(mk_symbol("export-declarations"),cons(cons(mk_symbol("cons"),cons(mk_symbol("declaration"),cons(mk_symbol("export-declarations"),NIL))),NIL))),NIL)),cons(cons(cons(mk_symbol("eq?"),cons(cons(mk_symbol("car"),cons(mk_symbol("declaration"),NIL)),cons(cons( mk_symbol("quote" ),cons(mk_symbol("import"),NIL)),NIL))),cons(cons(mk_symbol("set!"),cons(mk_symbol("import-declarations"),cons(cons(mk_symbol("cons"),cons(mk_symbol("declaration"),cons(mk_symbol("import-declarations"),NIL))),NIL))),NIL)),cons(cons(cons(mk_symbol("eq?"),cons(cons(mk_symbol("car"),cons(mk_symbol("declaration"),NIL)),cons(cons( mk_symbol("quote" ),cons(mk_symbol("begin"),NIL)),NIL))),cons(cons(mk_symbol("set!"),cons(mk_symbol("begin-declarations"),cons(cons(mk_symbol("cons"),cons(mk_symbol("declaration"),cons(mk_symbol("begin-declarations"),NIL))),NIL))),NIL)),cons(cons(mk_symbol("else"),cons(cons(mk_symbol("error"),cons(cons( mk_symbol("quote" ),cons(mk_symbol("define-library"),NIL)),cons(mk_string("unknown declaration type:"),cons(cons(mk_symbol("car"),cons(mk_symbol("declaration"),NIL)),NIL)))),NIL)),NIL))))),NIL))),cons(cons(mk_symbol("reverse"),cons(mk_symbol("declarations"),NIL)),NIL))),cons(cons(mk_symbol("for-each"),cons(cons(mk_symbol("lambda"),cons(cons(mk_symbol("declaration"),NIL),cons(cons(mk_symbol("for-each"),cons(cons(mk_symbol("lambda"),cons(cons(mk_symbol("spec"),NIL),cons(cons(mk_symbol("cond"),cons(cons(cons(mk_symbol("symbol?"),cons(mk_symbol("spec"),NIL)),cons(cons(mk_symbol("set!"),cons(mk_symbol("export-only"),cons(cons(mk_symbol("cons"),cons(mk_symbol("spec"),cons(mk_symbol("export-only"),NIL))),NIL))),NIL)),cons(cons(cons(mk_symbol("and"),cons(cons(mk_symbol("list?"),cons(mk_symbol("spec"),NIL)),cons(cons(mk_symbol("="),cons(cons(mk_symbol("length"),cons(mk_symbol("spec"),NIL)),cons(mk_integer(3),NIL))),cons(cons(mk_symbol("equal?"),cons(cons(mk_symbol("car"),cons(mk_symbol("spec"),NIL)),cons(cons( mk_symbol("quote" ),cons(mk_symbol("rename"),NIL)),NIL))),NIL)))),cons(cons(mk_symbol("set!"),cons(mk_symbol("export-only"),cons(cons(mk_symbol("cons"),cons(cons(mk_symbol("cadr"),cons(mk_symbol("spec"),NIL)),cons(mk_symbol("export-only"),NIL))),NIL))),cons(cons(mk_symbol("set!"),cons(mk_symbol("export-rename"),cons(cons(mk_symbol("cons"),cons(cons(mk_symbol("cons"),cons(cons(mk_symbol("cadr"),cons(mk_symbol("spec"),NIL)),cons(cons(mk_symbol("caddr"),cons(mk_symbol("spec"),NIL)),NIL))),cons(mk_symbol("export-rename"),NIL))),NIL))),NIL))),cons(cons(mk_symbol("else"),cons(cons(mk_symbol("error"),cons(cons( mk_symbol("quote" ),cons(mk_symbol("define-library"),NIL)),cons(mk_string("unknown export spec:"),cons(mk_symbol("spec"),NIL)))),NIL)),NIL)))),NIL))),cons(cons(mk_symbol("cdr"),cons(mk_symbol("declaration"),NIL)),NIL))),NIL))),cons(mk_symbol("export-declarations"),NIL))),cons(cons(mk_symbol("for-each"),cons(cons(mk_symbol("lambda"),cons(cons(mk_symbol("declaration"),NIL),cons(cons(mk_symbol("apply"),cons(mk_symbol("environment-import-sets!"),cons(cons(mk_symbol("cons"),cons(mk_symbol("library-environment"),cons(cons(mk_symbol("cons"),cons(mk_symbol("environment"),cons(cons(mk_symbol("cdr"),cons(mk_symbol("declaration"),NIL)),NIL))),NIL))),NIL))),NIL))),cons(mk_symbol("import-declarations"),NIL))),cons(cons(mk_symbol("eval"),cons(cons( mk_symbol("quasiquote" ),cons(cons(mk_symbol("begin"),cons(cons( mk_symbol("unquote-splicing" ),cons(mk_symbol("begin-declarations"),NIL)),NIL)),NIL)),cons(mk_symbol("library-environment"),NIL))),cons(cons(mk_symbol("apply"),cons(mk_symbol("environment-rename"),cons(cons(mk_symbol("cons"),cons(cons(mk_symbol("apply"),cons(mk_symbol("environment-only"),cons(cons(mk_symbol("cons"),cons(mk_symbol("library-environment"),cons(mk_symbol("export-only"),NIL))),NIL))),cons(mk_symbol("export-rename"),NIL))),NIL))),NIL))))))),NIL))),NIL))),NIL))),cons(cons(mk_symbol("begin"),NIL),cons(cons(mk_symbol("begin"),cons(cons(mk_symbol("define"),cons(mk_symbol("identifiers"),cons(cons( mk_symbol("quote" ),cons(cons(mk_symbol("object->string"),cons(mk_symbol("receive"),cons(mk_symbol("atom?"),cons(mk_symbol("environment?"),cons(mk_symbol("defined?"),cons(mk_symbol("define-macro"),cons(mk_symbol("macro"),cons(mk_symbol("macro-expand"),cons(mk_symbol("macro?"),cons(mk_symbol("gensym"),cons(mk_symbol("make-closure"),cons(mk_symbol("closure?"),cons(mk_symbol("get-closure-code"),cons(mk_symbol("set-output-port"),cons(mk_symbol("set-input-port"),cons(mk_symbol("eager"),cons(mk_symbol("lazy"),cons(mk_symbol("cons-stream"),cons(mk_symbol("last-pair"),cons(mk_symbol("quit"),cons(mk_symbol("gc-verbose"),cons(mk_symbol("gc"),cons(mk_symbol("global-environment"),cons(mk_symbol("current-environment"),cons(mk_symbol("make-environment"),cons(mk_symbol("environment?"),cons(mk_symbol("environment-defined-symbols"),cons(mk_symbol("environment-import!"),cons(mk_symbol("environment-delete!"),cons(mk_symbol("environment-rename"),cons(mk_symbol("environment-prefix"),cons(mk_symbol("environment-except"),cons(mk_symbol("environment-only"),cons(mk_symbol("environment-update!"),cons(mk_symbol("environment-ref"),cons(mk_symbol("environment-assoc"),cons(mk_symbol("environment-undefine!"),cons(mk_symbol("environment-define!"),cons(mk_symbol("environment-import-sets!"),cons(mk_symbol("library-eval"),cons(mk_symbol("import"),cons(mk_symbol("define-library"),cons(mk_symbol("*"),cons(mk_symbol("+"),cons(mk_symbol("/"),cons(mk_symbol("<"),cons(mk_symbol("<="),cons(mk_symbol("="),cons(mk_symbol("=>"),cons(mk_symbol(">"),cons(mk_symbol(">="),cons(mk_symbol("-"),cons(mk_symbol("abs"),cons(mk_symbol("and"),cons(mk_symbol("append"),cons(mk_symbol("apply"),cons(mk_symbol("assoc"),cons(mk_symbol("assq"),cons(mk_symbol("assv"),cons(mk_symbol("begin"),cons(mk_symbol("boolean?"),cons(mk_symbol("caar"),cons(mk_symbol("cadr"),cons(mk_symbol("call-with-current-continuation"),cons(mk_symbol("call-with-values"),cons(mk_symbol("car"),cons(mk_symbol("case"),cons(mk_symbol("cdar"),cons(mk_symbol("cddr"),cons(mk_symbol("cdr"),cons(mk_symbol("ceiling"),cons(mk_symbol("char->integer"),cons(mk_symbol("char-ready?"),cons(mk_symbol("char<=?"),cons(mk_symbol("char<?"),cons(mk_symbol("char=?"),cons(mk_symbol("char>=?"),cons(mk_symbol("char>?"),cons(mk_symbol("char?"),cons(mk_symbol("close-input-port"),cons(mk_symbol("close-output-port"),cons(mk_symbol("close-port"),cons(mk_symbol("cond"),cons(mk_symbol("cons"),cons(mk_symbol("current-input-port"),cons(mk_symbol("current-output-port"),cons(mk_symbol("define"),cons(mk_symbol("define-syntax"),cons(mk_symbol("do"),cons(mk_symbol("dynamic-wind"),cons(mk_symbol("else"),cons(mk_symbol("eof-object?"),cons(mk_symbol("eq?"),cons(mk_symbol("equal?"),cons(mk_symbol("eqv?"),cons(mk_symbol("error"),cons(mk_symbol("even?"),cons(mk_symbol("exact?"),cons(mk_symbol("expt"),cons(mk_symbol("exact->inexact"),cons(mk_symbol("floor"),cons(mk_symbol("for-each"),cons(mk_symbol("gcd"),cons(mk_symbol("get-output-string"),cons(mk_symbol("if"),cons(mk_symbol("inexact->exact"),cons(mk_symbol("inexact?"),cons(mk_symbol("input-port?"),cons(mk_symbol("integer->char"),cons(mk_symbol("integer?"),cons(mk_symbol("lambda"),cons(mk_symbol("lcm"),cons(mk_symbol("length"),cons(mk_symbol("let"),cons(mk_symbol("let*"),cons(mk_symbol("let-syntax"),cons(mk_symbol("letrec"),cons(mk_symbol("letrec*"),cons(mk_symbol("letrec-syntax"),cons(mk_symbol("list"),cons(mk_symbol("list->string"),cons(mk_symbol("list->vector"),cons(mk_symbol("list-ref"),cons(mk_symbol("list-tail"),cons(mk_symbol("list?"),cons(mk_symbol("make-string"),cons(mk_symbol("make-vector"),cons(mk_symbol("map"),cons(mk_symbol("max"),cons(mk_symbol("member"),cons(mk_symbol("memq"),cons(mk_symbol("memv"),cons(mk_symbol("min"),cons(mk_symbol("modulo"),cons(mk_symbol("negative?"),cons(mk_symbol("newline"),cons(mk_symbol("not"),cons(mk_symbol("null?"),cons(mk_symbol("number->string"),cons(mk_symbol("number?"),cons(mk_symbol("odd?"),cons(mk_symbol("open-input-output-string"),cons(mk_symbol("open-input-string"),cons(mk_symbol("open-output-string"),cons(mk_symbol("or"),cons(mk_symbol("output-port?"),cons(mk_symbol("pair?"),cons(mk_symbol("peek-char"),cons(mk_symbol("port?"),cons(mk_symbol("positive?"),cons(mk_symbol("procedure?"),cons(mk_symbol("quasiquote"),cons(mk_symbol("quote"),cons(mk_symbol("quotient"),cons(mk_symbol("read-char"),cons(mk_symbol("real?"),cons(mk_symbol("remainder"),cons(mk_symbol("reverse"),cons(mk_symbol("round"),cons(mk_symbol("set!"),cons(mk_symbol("set-car!"),cons(mk_symbol("set-cdr!"),cons(mk_symbol("string"),cons(mk_symbol("string->list"),cons(mk_symbol("string->number"),cons(mk_symbol("string->symbol"),cons(mk_symbol("string-append"),cons(mk_symbol("string-copy"),cons(mk_symbol("string-fill!"),cons(mk_symbol("string-length"),cons(mk_symbol("string-ref"),cons(mk_symbol("string-set!"),cons(mk_symbol("string<=?"),cons(mk_symbol("string<?"),cons(mk_symbol("string=?"),cons(mk_symbol("string>=?"),cons(mk_symbol("string>?"),cons(mk_symbol("string?"),cons(mk_symbol("substring"),cons(mk_symbol("symbol->string"),cons(mk_symbol("symbol?"),cons(mk_symbol("syntax-rules"),cons(mk_symbol("truncate"),cons(mk_symbol("unless"),cons(mk_symbol("values"),cons(mk_symbol("vector"),cons(mk_symbol("vector->list"),cons(mk_symbol("vector-fill!"),cons(mk_symbol("vector-length"),cons(mk_symbol("vector-ref"),cons(mk_symbol("vector-set!"),cons(mk_symbol("vector?"),cons(mk_symbol("when"),cons(mk_symbol("write-char"),cons(mk_symbol("zero?"),cons(mk_symbol("char-alphabetic?"),cons(mk_symbol("char-ci<=?"),cons(mk_symbol("char-ci<?"),cons(mk_symbol("char-ci=?"),cons(mk_symbol("char-ci>=?"),cons(mk_symbol("char-ci>?"),cons(mk_symbol("char-downcase"),cons(mk_symbol("char-lower-case?"),cons(mk_symbol("char-numeric?"),cons(mk_symbol("char-upcase"),cons(mk_symbol("char-upper-case?"),cons(mk_symbol("char-whitespace?"),cons(mk_symbol("string-ci<=?"),cons(mk_symbol("string-ci<?"),cons(mk_symbol("string-ci=?"),cons(mk_symbol("string-ci>=?"),cons(mk_symbol("string-ci>?"),cons(mk_symbol("caaaar"),cons(mk_symbol("caaadr"),cons(mk_symbol("caaar"),cons(mk_symbol("caadar"),cons(mk_symbol("caaddr"),cons(mk_symbol("caadr"),cons(mk_symbol("cadaar"),cons(mk_symbol("cadadr"),cons(mk_symbol("cadar"),cons(mk_symbol("caddar"),cons(mk_symbol("cadddr"),cons(mk_symbol("caddr"),cons(mk_symbol("cdaaar"),cons(mk_symbol("cdaadr"),cons(mk_symbol("cdaar"),cons(mk_symbol("cdadar"),cons(mk_symbol("cdaddr"),cons(mk_symbol("cdadr"),cons(mk_symbol("cddaar"),cons(mk_symbol("cddadr"),cons(mk_symbol("cddar"),cons(mk_symbol("cdddar"),cons(mk_symbol("cddddr"),cons(mk_symbol("cdddr"),cons(mk_symbol("eval"),cons(mk_symbol("call-with-output-file"),cons(mk_symbol("call-with-input-file"),cons(mk_symbol("open-input-file"),cons(mk_symbol("open-input-output-file"),cons(mk_symbol("open-output-file"),cons(mk_symbol("with-input-from-file"),cons(mk_symbol("with-output-to-file"),cons(mk_symbol("acos"),cons(mk_symbol("asin"),cons(mk_symbol("atan"),cons(mk_symbol("cos"),cons(mk_symbol("exp"),cons(mk_symbol("log"),cons(mk_symbol("sin"),cons(mk_symbol("sqrt"),cons(mk_symbol("tan"),cons(mk_symbol("delay"),cons(mk_symbol("force"),cons(mk_symbol("load"),cons(mk_symbol("emergency-exit"),cons(mk_symbol("exit"),cons(mk_symbol("read"),cons(mk_symbol("interaction-environment"),cons(mk_symbol("display"),cons(mk_symbol("write"),NIL)))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))),NIL)),NIL))),NIL)),cons(cons(mk_symbol("environment-define!"),cons(cons(mk_symbol("current-environment"),NIL),cons(cons(mk_symbol("string->symbol"),cons(mk_string("(auto scheme)"),NIL)),cons(cons(mk_symbol("apply"),cons(mk_symbol("environment-only"),cons(cons(mk_symbol("cons"),cons(cons(mk_symbol("current-environment"),NIL),cons(cons(mk_symbol("append"),cons(cons( mk_symbol("quote" ),cons(NIL,NIL)),cons(mk_symbol("identifiers"),NIL))),NIL))),NIL))),NIL)))),cons(cons(mk_symbol("for-each"),cons(cons(mk_symbol("lambda"),cons(cons(mk_symbol("identifier"),NIL),cons(cons(mk_symbol("environment-delete!"),cons(mk_symbol("parent-environment"),cons(mk_symbol("identifier"),NIL))),NIL))),cons(mk_symbol("identifiers"),NIL))),cons(cons(mk_symbol("environment-import!"),cons(mk_symbol("parent-environment"),cons(cons(mk_symbol("environment-only"),cons(cons(mk_symbol("current-environment"),NIL),cons(cons( mk_symbol("quote" ),cons(mk_symbol("define-library"),NIL)),cons(cons( mk_symbol("quote" ),cons(mk_symbol("import"),NIL)),cons(cons(mk_symbol("string->symbol"),cons(mk_string("(auto scheme)"),NIL)),cons(cons( mk_symbol("quote" ),cons(mk_symbol("begin"),NIL)),cons(cons( mk_symbol("quote" ),cons(mk_symbol("let"),NIL)),NIL))))))),NIL))),NIL))))))))))))))),cons(cons(mk_symbol("current-environment"),NIL),NIL)),NIL))))),NIL)),environment);
return 0;
}
