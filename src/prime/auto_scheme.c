/*  This file is part of the 'AutoScheme' project.
 *  Copyright 2021 Steven Wiley <s.wiley@katchitek.com> 
 *  SPDX-License-Identifier: BSD-2-Clause
 */
#include "autoscheme.h"
int LOAD_MODULE__auto_scheme(void);
/*  This file is part of the 'AutoScheme' project.
 *  Copyright 2021 Steven Wiley <s.wiley@katchitek.com> 
 *  SPDX-License-Identifier: BSD-2-Clause
 */
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
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
 int LOAD_MODULE__auto_scheme()
{
/*  This file is part of the 'AutoScheme' project.
 *  Copyright 2021 Steven Wiley <s.wiley@katchitek.com> 
 *  SPDX-License-Identifier: BSD-2-Clause
 */
scheme_register_foreign_func( "make-environment",            make_environment               );
scheme_register_foreign_func( "environment-define!"  ,       ff_environment_define_d        );
scheme_register_foreign_func( "environment-undefine!",       ff_environment_undefine_d      );
scheme_register_foreign_func( "environment-defined-symbols", ff_environment_defined_symbols );
scheme_register_foreign_func( "environment-assoc",           ff_environment_assoc           );
scheme_register_foreign_func( "environment-ref",             ff_environment_ref             );
scheme_register_foreign_func( "environment-update!",         ff_environment_update_d        );
scheme_register_foreign_func( "environment-import!",         ff_environment_import_d        );
scheme_register_foreign_func( "environment-only",            ff_environment_only            );
scheme_register_foreign_func( "environment-except",          ff_environment_except          );
scheme_register_foreign_func( "environment-prefix",          ff_environment_prefix          );
scheme_register_foreign_func( "environment-rename",          ff_environment_rename          );

scheme_register_foreign_func( "environment-delete!",         ff_environment_delete_d        );

 scheme_eval(cons(mk_symbol("begin"),cons(cons(mk_symbol("begin"),cons(T,cons(T,cons(T,cons(cons(cons(mk_symbol("lambda"),cons(cons(mk_symbol("parent-environment"),NIL),cons(cons(mk_symbol("environment-import!"),cons(cons(mk_symbol("current-environment"),NIL),cons(mk_symbol("parent-environment"),NIL))),cons(cons(mk_symbol("define"),cons(mk_symbol("exit"),cons(F,NIL))),cons(cons(mk_symbol("call-with-current-continuation"),cons(cons(mk_symbol("lambda"),cons(cons(mk_symbol("return"),NIL),cons(cons(mk_symbol("let"),cons(cons(cons(mk_symbol("obj"),cons(cons(mk_symbol("call-with-current-continuation"),cons(cons(mk_symbol("lambda"),cons(cons(mk_symbol("_exit"),NIL),cons(cons(mk_symbol("cond"),cons(cons(cons(mk_symbol("not"),cons(mk_symbol("exit"),NIL)),cons(cons(mk_symbol("set!"),cons(mk_symbol("exit"),cons(mk_symbol("_exit"),NIL))),cons(cons(mk_symbol("return"),NIL),NIL))),NIL)),NIL))),NIL)),NIL)),NIL),cons(cons(mk_symbol("emergency-exit"),cons(cons(mk_symbol("cond"),cons(cons(cons(mk_symbol("integer?"),cons(mk_symbol("obj"),NIL)),cons(mk_symbol("obj"),NIL)),cons(cons(cons(mk_symbol("eq?"),cons(mk_symbol("obj"),cons(F,NIL))),cons(mk_integer(1),NIL)),cons(cons(mk_symbol("else"),cons(mk_integer(0),NIL)),NIL)))),NIL)),NIL))),NIL))),NIL)),cons(cons(mk_symbol("define"),cons(mk_symbol("object->string"),cons(cons(mk_symbol("lambda"),cons(cons(mk_symbol("object"),NIL),cons(cons(mk_symbol("let"),cons(cons(cons(mk_symbol("string-port"),cons(cons(mk_symbol("open-output-string"),NIL),NIL)),NIL),cons(cons(mk_symbol("write"),cons(mk_symbol("object"),cons(mk_symbol("string-port"),NIL))),cons(cons(mk_symbol("let"),cons(cons(cons(mk_symbol("output-string"),cons(cons(mk_symbol("get-output-string"),cons(mk_symbol("string-port"),NIL)),NIL)),NIL),cons(cons(mk_symbol("close-output-port"),cons(mk_symbol("string-port"),NIL)),cons(mk_symbol("output-string"),NIL)))),NIL)))),NIL))),NIL))),cons(cons(mk_symbol("begin"),cons(cons(mk_symbol("define-macro"),cons(cons(mk_symbol("define-library"),cons(mk_symbol("name"),mk_symbol("declarations"))),cons(cons(mk_symbol("let"),cons(cons(cons(mk_symbol("name-symbol"),cons(cons(mk_symbol("string->symbol"),cons(cons(mk_symbol("object->string"),cons(mk_symbol("name"),NIL)),NIL)),NIL)),cons(cons(mk_symbol("quoted-declarations"),cons(cons(mk_symbol("map"),cons(cons(mk_symbol("lambda"),cons(cons(mk_symbol("declaration"),NIL),cons(cons( mk_symbol("quasiquote" ),cons(cons(mk_symbol("quote"),cons(cons( mk_symbol("unquote" ),cons(mk_symbol("declaration"),NIL)),NIL)),NIL)),NIL))),cons(mk_symbol("declarations"),NIL))),NIL)),NIL)),cons(cons(mk_symbol("apply"),cons(mk_symbol("environment-define!"),cons(cons(mk_symbol("cons"),cons(cons(mk_symbol("expansion-environment"),NIL),cons(cons(mk_symbol("cons"),cons(mk_symbol("name-symbol"),cons(cons(mk_symbol("list"),cons(cons(mk_symbol("library-eval"),cons(mk_symbol("declarations"),cons(cons(mk_symbol("expansion-environment"),NIL),NIL))),NIL)),NIL))),NIL))),NIL))),NIL))),NIL))),cons(cons(mk_symbol("define-macro"),cons(cons(mk_symbol("import"),mk_symbol("sets")),cons(cons(mk_symbol("apply"),cons(mk_symbol("environment-import-sets!"),cons(cons(mk_symbol("cons"),cons(cons(mk_symbol("expansion-environment"),NIL),cons(cons(mk_symbol("cons"),cons(cons(mk_symbol("expansion-environment"),NIL),cons(mk_symbol("sets"),NIL))),NIL))),NIL))),NIL))),NIL))),cons(cons(mk_symbol("begin"),cons(cons(mk_symbol("define"),cons(mk_symbol("environment-import-sets!"),cons(cons(mk_symbol("lambda"),cons(cons(mk_symbol("target"),cons(mk_symbol("source"),mk_symbol("sets"))),cons(cons(mk_symbol("letrec"),cons(cons(cons(mk_symbol("process-import-set"),cons(cons(mk_symbol("lambda"),cons(cons(mk_symbol("set"),NIL),cons(cons(mk_symbol("if"),cons(cons(mk_symbol("not"),cons(cons(mk_symbol("pair?"),cons(mk_symbol("set"),NIL)),NIL)),cons(cons(mk_symbol("error"),cons(mk_string("improper import-set:"),cons(mk_symbol("set"),NIL))),cons(cons(mk_symbol("cond"),cons(cons(cons(mk_symbol("equal?"),cons(cons(mk_symbol("car"),cons(mk_symbol("set"),NIL)),cons(cons( mk_symbol("quote" ),cons(mk_symbol("only"),NIL)),NIL))),cons(cons(mk_symbol("apply"),cons(mk_symbol("environment-only"),cons(cons(mk_symbol("cons"),cons(cons(mk_symbol("process-import-set"),cons(cons(mk_symbol("cadr"),cons(mk_symbol("set"),NIL)),NIL)),cons(cons(mk_symbol("cddr"),cons(mk_symbol("set"),NIL)),NIL))),NIL))),NIL)),cons(cons(cons(mk_symbol("equal?"),cons(cons(mk_symbol("car"),cons(mk_symbol("set"),NIL)),cons(cons( mk_symbol("quote" ),cons(mk_symbol("except"),NIL)),NIL))),cons(cons(mk_symbol("apply"),cons(mk_symbol("environment-except"),cons(cons(mk_symbol("cons"),cons(cons(mk_symbol("process-import-set"),cons(cons(mk_symbol("cadr"),cons(mk_symbol("set"),NIL)),NIL)),cons(cons(mk_symbol("cddr"),cons(mk_symbol("set"),NIL)),NIL))),NIL))),NIL)),cons(cons(cons(mk_symbol("equal?"),cons(cons(mk_symbol("car"),cons(mk_symbol("set"),NIL)),cons(cons( mk_symbol("quote" ),cons(mk_symbol("prefix"),NIL)),NIL))),cons(cons(mk_symbol("environment-prefix"),cons(cons(mk_symbol("process-import-set"),cons(cons(mk_symbol("cadr"),cons(mk_symbol("set"),NIL)),NIL)),cons(cons(mk_symbol("caddr"),cons(mk_symbol("set"),NIL)),NIL))),NIL)),cons(cons(cons(mk_symbol("equal?"),cons(cons(mk_symbol("car"),cons(mk_symbol("set"),NIL)),cons(cons( mk_symbol("quote" ),cons(mk_symbol("rename"),NIL)),NIL))),cons(cons(mk_symbol("apply"),cons(mk_symbol("environment-rename"),cons(cons(mk_symbol("cons"),cons(cons(mk_symbol("process-import-set"),cons(cons(mk_symbol("cadr"),cons(mk_symbol("set"),NIL)),NIL)),cons(cons(mk_symbol("cddr"),cons(mk_symbol("set"),NIL)),NIL))),NIL))),NIL)),cons(cons(mk_symbol("else"),cons(cons(mk_symbol("environment-ref"),cons(mk_symbol("source"),cons(cons(mk_symbol("string->symbol"),cons(cons(mk_symbol("object->string"),cons(mk_symbol("set"),NIL)),NIL)),NIL))),NIL)),NIL)))))),NIL)))),NIL))),NIL)),NIL),cons(cons(mk_symbol("for-each"),cons(cons(mk_symbol("lambda"),cons(cons(mk_symbol("set"),NIL),cons(cons(mk_symbol("environment-import!"),cons(mk_symbol("target"),cons(cons(mk_symbol("process-import-set"),cons(mk_symbol("set"),NIL)),NIL))),NIL))),cons(mk_symbol("sets"),NIL))),cons(mk_symbol("target"),NIL)))),NIL))),NIL))),cons(cons(mk_symbol("define"),cons(mk_symbol("library-eval"),cons(cons(mk_symbol("lambda"),cons(cons(mk_symbol("declarations"),cons(mk_symbol("environment"),NIL)),cons(cons(mk_symbol("letrec"),cons(cons(cons(mk_symbol("export-declarations"),cons(cons( mk_symbol("quote" ),cons(NIL,NIL)),NIL)),cons(cons(mk_symbol("import-declarations"),cons(cons( mk_symbol("quote" ),cons(NIL,NIL)),NIL)),cons(cons(mk_symbol("begin-declarations"),cons(cons( mk_symbol("quote" ),cons(NIL,NIL)),NIL)),cons(cons(mk_symbol("export-only"),cons(cons( mk_symbol("quote" ),cons(NIL,NIL)),NIL)),cons(cons(mk_symbol("export-rename"),cons(cons( mk_symbol("quote" ),cons(NIL,NIL)),NIL)),cons(cons(mk_symbol("library-environment"),cons(cons(mk_symbol("make-environment"),NIL),NIL)),NIL)))))),cons(cons(mk_symbol("for-each"),cons(cons(mk_symbol("lambda"),cons(cons(mk_symbol("declaration"),NIL),cons(cons(mk_symbol("cond"),cons(cons(cons(mk_symbol("eq?"),cons(cons(mk_symbol("car"),cons(mk_symbol("declaration"),NIL)),cons(cons( mk_symbol("quote" ),cons(mk_symbol("export"),NIL)),NIL))),cons(cons(mk_symbol("set!"),cons(mk_symbol("export-declarations"),cons(cons(mk_symbol("cons"),cons(mk_symbol("declaration"),cons(mk_symbol("export-declarations"),NIL))),NIL))),NIL)),cons(cons(cons(mk_symbol("eq?"),cons(cons(mk_symbol("car"),cons(mk_symbol("declaration"),NIL)),cons(cons( mk_symbol("quote" ),cons(mk_symbol("import"),NIL)),NIL))),cons(cons(mk_symbol("set!"),cons(mk_symbol("import-declarations"),cons(cons(mk_symbol("cons"),cons(mk_symbol("declaration"),cons(mk_symbol("import-declarations"),NIL))),NIL))),NIL)),cons(cons(cons(mk_symbol("eq?"),cons(cons(mk_symbol("car"),cons(mk_symbol("declaration"),NIL)),cons(cons( mk_symbol("quote" ),cons(mk_symbol("begin"),NIL)),NIL))),cons(cons(mk_symbol("set!"),cons(mk_symbol("begin-declarations"),cons(cons(mk_symbol("cons"),cons(mk_symbol("declaration"),cons(mk_symbol("begin-declarations"),NIL))),NIL))),NIL)),cons(cons(mk_symbol("else"),cons(cons(mk_symbol("error"),cons(cons( mk_symbol("quote" ),cons(mk_symbol("define-library"),NIL)),cons(mk_string("unknown declaration type:"),cons(cons(mk_symbol("car"),cons(mk_symbol("declaration"),NIL)),NIL)))),NIL)),NIL))))),NIL))),cons(cons(mk_symbol("reverse"),cons(mk_symbol("declarations"),NIL)),NIL))),cons(cons(mk_symbol("for-each"),cons(cons(mk_symbol("lambda"),cons(cons(mk_symbol("declaration"),NIL),cons(cons(mk_symbol("for-each"),cons(cons(mk_symbol("lambda"),cons(cons(mk_symbol("spec"),NIL),cons(cons(mk_symbol("cond"),cons(cons(cons(mk_symbol("symbol?"),cons(mk_symbol("spec"),NIL)),cons(cons(mk_symbol("set!"),cons(mk_symbol("export-only"),cons(cons(mk_symbol("cons"),cons(mk_symbol("spec"),cons(mk_symbol("export-only"),NIL))),NIL))),NIL)),cons(cons(cons(mk_symbol("and"),cons(cons(mk_symbol("list?"),cons(mk_symbol("spec"),NIL)),cons(cons(mk_symbol("="),cons(cons(mk_symbol("length"),cons(mk_symbol("spec"),NIL)),cons(mk_integer(3),NIL))),cons(cons(mk_symbol("equal?"),cons(cons(mk_symbol("car"),cons(mk_symbol("spec"),NIL)),cons(cons( mk_symbol("quote" ),cons(mk_symbol("rename"),NIL)),NIL))),NIL)))),cons(cons(mk_symbol("set!"),cons(mk_symbol("export-only"),cons(cons(mk_symbol("cons"),cons(cons(mk_symbol("cadr"),cons(mk_symbol("spec"),NIL)),cons(mk_symbol("export-only"),NIL))),NIL))),cons(cons(mk_symbol("set!"),cons(mk_symbol("export-rename"),cons(cons(mk_symbol("cons"),cons(cons(mk_symbol("cons"),cons(cons(mk_symbol("cadr"),cons(mk_symbol("spec"),NIL)),cons(cons(mk_symbol("caddr"),cons(mk_symbol("spec"),NIL)),NIL))),cons(mk_symbol("export-rename"),NIL))),NIL))),NIL))),cons(cons(mk_symbol("else"),cons(cons(mk_symbol("error"),cons(cons( mk_symbol("quote" ),cons(mk_symbol("define-library"),NIL)),cons(mk_string("unknown export spec:"),cons(mk_symbol("spec"),NIL)))),NIL)),NIL)))),NIL))),cons(cons(mk_symbol("cdr"),cons(mk_symbol("declaration"),NIL)),NIL))),NIL))),cons(mk_symbol("export-declarations"),NIL))),cons(cons(mk_symbol("for-each"),cons(cons(mk_symbol("lambda"),cons(cons(mk_symbol("declaration"),NIL),cons(cons(mk_symbol("apply"),cons(mk_symbol("environment-import-sets!"),cons(cons(mk_symbol("cons"),cons(mk_symbol("library-environment"),cons(cons(mk_symbol("cons"),cons(mk_symbol("environment"),cons(cons(mk_symbol("cdr"),cons(mk_symbol("declaration"),NIL)),NIL))),NIL))),NIL))),NIL))),cons(mk_symbol("import-declarations"),NIL))),cons(cons(mk_symbol("eval"),cons(cons( mk_symbol("quasiquote" ),cons(cons(mk_symbol("begin"),cons(cons( mk_symbol("unquote-splicing" ),cons(mk_symbol("begin-declarations"),NIL)),NIL)),NIL)),cons(mk_symbol("library-environment"),NIL))),cons(cons(mk_symbol("apply"),cons(mk_symbol("environment-rename"),cons(cons(mk_symbol("cons"),cons(cons(mk_symbol("apply"),cons(mk_symbol("environment-only"),cons(cons(mk_symbol("cons"),cons(mk_symbol("library-environment"),cons(mk_symbol("export-only"),NIL))),NIL))),cons(mk_symbol("export-rename"),NIL))),NIL))),NIL))))))),NIL))),NIL))),NIL))),cons(cons(mk_symbol("begin"),NIL),cons(cons(mk_symbol("begin"),cons(cons(mk_symbol("define"),cons(mk_symbol("identifiers"),cons(cons( mk_symbol("quote" ),cons(cons(mk_symbol("object->string"),cons(mk_symbol("receive"),cons(mk_symbol("atom?"),cons(mk_symbol("environment?"),cons(mk_symbol("defined?"),cons(mk_symbol("define-macro"),cons(mk_symbol("macro"),cons(mk_symbol("macro-expand"),cons(mk_symbol("macro?"),cons(mk_symbol("gensym"),cons(mk_symbol("make-closure"),cons(mk_symbol("closure?"),cons(mk_symbol("get-closure-code"),cons(mk_symbol("set-output-port"),cons(mk_symbol("set-input-port"),cons(mk_symbol("eager"),cons(mk_symbol("lazy"),cons(mk_symbol("cons-stream"),cons(mk_symbol("last-pair"),cons(mk_symbol("quit"),cons(mk_symbol("gc-verbose"),cons(mk_symbol("gc"),cons(mk_symbol("global-environment"),cons(mk_symbol("current-environment"),cons(mk_symbol("make-environment"),cons(mk_symbol("environment-defined-symbols"),cons(mk_symbol("environment-import!"),cons(mk_symbol("environment-delete!"),cons(mk_symbol("environment-rename"),cons(mk_symbol("environment-prefix"),cons(mk_symbol("environment-except"),cons(mk_symbol("environment-only"),cons(mk_symbol("environment-update!"),cons(mk_symbol("environment-ref"),cons(mk_symbol("environment-assoc"),cons(mk_symbol("environment-undefine!"),cons(mk_symbol("environment-define!"),cons(mk_symbol("environment-import-sets!"),cons(mk_symbol("library-eval"),cons(mk_symbol("import"),cons(mk_symbol("define-library"),cons(mk_symbol("*"),cons(mk_symbol("+"),cons(mk_symbol("/"),cons(mk_symbol("<"),cons(mk_symbol("<="),cons(mk_symbol("="),cons(mk_symbol("=>"),cons(mk_symbol(">"),cons(mk_symbol(">="),cons(mk_symbol("-"),cons(mk_symbol("abs"),cons(mk_symbol("and"),cons(mk_symbol("append"),cons(mk_symbol("apply"),cons(mk_symbol("assoc"),cons(mk_symbol("assq"),cons(mk_symbol("assv"),cons(mk_symbol("begin"),cons(mk_symbol("boolean?"),cons(mk_symbol("caar"),cons(mk_symbol("cadr"),cons(mk_symbol("call-with-current-continuation"),cons(mk_symbol("call-with-values"),cons(mk_symbol("car"),cons(mk_symbol("case"),cons(mk_symbol("cdar"),cons(mk_symbol("cddr"),cons(mk_symbol("cdr"),cons(mk_symbol("ceiling"),cons(mk_symbol("char->integer"),cons(mk_symbol("char-ready?"),cons(mk_symbol("char<=?"),cons(mk_symbol("char<?"),cons(mk_symbol("char=?"),cons(mk_symbol("char>=?"),cons(mk_symbol("char>?"),cons(mk_symbol("char?"),cons(mk_symbol("close-input-port"),cons(mk_symbol("close-output-port"),cons(mk_symbol("close-port"),cons(mk_symbol("cond"),cons(mk_symbol("cons"),cons(mk_symbol("current-input-port"),cons(mk_symbol("current-output-port"),cons(mk_symbol("define"),cons(mk_symbol("define-syntax"),cons(mk_symbol("do"),cons(mk_symbol("dynamic-wind"),cons(mk_symbol("else"),cons(mk_symbol("eof-object?"),cons(mk_symbol("eq?"),cons(mk_symbol("equal?"),cons(mk_symbol("eqv?"),cons(mk_symbol("error"),cons(mk_symbol("even?"),cons(mk_symbol("exact?"),cons(mk_symbol("expt"),cons(mk_symbol("exact->inexact"),cons(mk_symbol("floor"),cons(mk_symbol("for-each"),cons(mk_symbol("gcd"),cons(mk_symbol("get-output-string"),cons(mk_symbol("if"),cons(mk_symbol("inexact->exact"),cons(mk_symbol("inexact?"),cons(mk_symbol("input-port?"),cons(mk_symbol("integer->char"),cons(mk_symbol("integer?"),cons(mk_symbol("lambda"),cons(mk_symbol("lcm"),cons(mk_symbol("length"),cons(mk_symbol("let"),cons(mk_symbol("let*"),cons(mk_symbol("let-syntax"),cons(mk_symbol("letrec"),cons(mk_symbol("letrec*"),cons(mk_symbol("letrec-syntax"),cons(mk_symbol("list"),cons(mk_symbol("list->string"),cons(mk_symbol("list->vector"),cons(mk_symbol("list-ref"),cons(mk_symbol("list-tail"),cons(mk_symbol("list?"),cons(mk_symbol("make-string"),cons(mk_symbol("make-vector"),cons(mk_symbol("map"),cons(mk_symbol("max"),cons(mk_symbol("member"),cons(mk_symbol("memq"),cons(mk_symbol("memv"),cons(mk_symbol("min"),cons(mk_symbol("modulo"),cons(mk_symbol("negative?"),cons(mk_symbol("newline"),cons(mk_symbol("not"),cons(mk_symbol("null?"),cons(mk_symbol("number->string"),cons(mk_symbol("number?"),cons(mk_symbol("odd?"),cons(mk_symbol("open-input-output-string"),cons(mk_symbol("open-input-string"),cons(mk_symbol("open-output-string"),cons(mk_symbol("or"),cons(mk_symbol("output-port?"),cons(mk_symbol("pair?"),cons(mk_symbol("peek-char"),cons(mk_symbol("port?"),cons(mk_symbol("positive?"),cons(mk_symbol("procedure?"),cons(mk_symbol("quasiquote"),cons(mk_symbol("quote"),cons(mk_symbol("quotient"),cons(mk_symbol("read-char"),cons(mk_symbol("real?"),cons(mk_symbol("remainder"),cons(mk_symbol("reverse"),cons(mk_symbol("round"),cons(mk_symbol("set!"),cons(mk_symbol("set-car!"),cons(mk_symbol("set-cdr!"),cons(mk_symbol("string"),cons(mk_symbol("string->list"),cons(mk_symbol("string->number"),cons(mk_symbol("string->symbol"),cons(mk_symbol("string-append"),cons(mk_symbol("string-copy"),cons(mk_symbol("string-fill!"),cons(mk_symbol("string-length"),cons(mk_symbol("string-ref"),cons(mk_symbol("string-set!"),cons(mk_symbol("string<=?"),cons(mk_symbol("string<?"),cons(mk_symbol("string=?"),cons(mk_symbol("string>=?"),cons(mk_symbol("string>?"),cons(mk_symbol("string?"),cons(mk_symbol("substring"),cons(mk_symbol("symbol->string"),cons(mk_symbol("symbol?"),cons(mk_symbol("syntax-rules"),cons(mk_symbol("truncate"),cons(mk_symbol("unless"),cons(mk_symbol("values"),cons(mk_symbol("vector"),cons(mk_symbol("vector->list"),cons(mk_symbol("vector-fill!"),cons(mk_symbol("vector-length"),cons(mk_symbol("vector-ref"),cons(mk_symbol("vector-set!"),cons(mk_symbol("vector?"),cons(mk_symbol("when"),cons(mk_symbol("write-char"),cons(mk_symbol("zero?"),cons(mk_symbol("char-alphabetic?"),cons(mk_symbol("char-ci<=?"),cons(mk_symbol("char-ci<?"),cons(mk_symbol("char-ci=?"),cons(mk_symbol("char-ci>=?"),cons(mk_symbol("char-ci>?"),cons(mk_symbol("char-downcase"),cons(mk_symbol("char-lower-case?"),cons(mk_symbol("char-numeric?"),cons(mk_symbol("char-upcase"),cons(mk_symbol("char-upper-case?"),cons(mk_symbol("char-whitespace?"),cons(mk_symbol("string-ci<=?"),cons(mk_symbol("string-ci<?"),cons(mk_symbol("string-ci=?"),cons(mk_symbol("string-ci>=?"),cons(mk_symbol("string-ci>?"),cons(mk_symbol("caaaar"),cons(mk_symbol("caaadr"),cons(mk_symbol("caaar"),cons(mk_symbol("caadar"),cons(mk_symbol("caaddr"),cons(mk_symbol("caadr"),cons(mk_symbol("cadaar"),cons(mk_symbol("cadadr"),cons(mk_symbol("cadar"),cons(mk_symbol("caddar"),cons(mk_symbol("cadddr"),cons(mk_symbol("caddr"),cons(mk_symbol("cdaaar"),cons(mk_symbol("cdaadr"),cons(mk_symbol("cdaar"),cons(mk_symbol("cdadar"),cons(mk_symbol("cdaddr"),cons(mk_symbol("cdadr"),cons(mk_symbol("cddaar"),cons(mk_symbol("cddadr"),cons(mk_symbol("cddar"),cons(mk_symbol("cdddar"),cons(mk_symbol("cddddr"),cons(mk_symbol("cdddr"),cons(mk_symbol("eval"),cons(mk_symbol("call-with-output-file"),cons(mk_symbol("call-with-input-file"),cons(mk_symbol("open-input-file"),cons(mk_symbol("open-input-output-file"),cons(mk_symbol("open-output-file"),cons(mk_symbol("with-input-from-file"),cons(mk_symbol("with-output-to-file"),cons(mk_symbol("acos"),cons(mk_symbol("asin"),cons(mk_symbol("atan"),cons(mk_symbol("cos"),cons(mk_symbol("exp"),cons(mk_symbol("log"),cons(mk_symbol("sin"),cons(mk_symbol("sqrt"),cons(mk_symbol("tan"),cons(mk_symbol("delay"),cons(mk_symbol("force"),cons(mk_symbol("load"),cons(mk_symbol("emergency-exit"),cons(mk_symbol("exit"),cons(mk_symbol("read"),cons(mk_symbol("interaction-environment"),cons(mk_symbol("display"),cons(mk_symbol("write"),NIL))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))),NIL)),NIL))),NIL)),cons(cons(mk_symbol("environment-define!"),cons(cons(mk_symbol("current-environment"),NIL),cons(cons(mk_symbol("string->symbol"),cons(mk_string("(auto scheme)"),NIL)),cons(cons(mk_symbol("apply"),cons(mk_symbol("environment-only"),cons(cons(mk_symbol("cons"),cons(cons(mk_symbol("current-environment"),NIL),cons(cons(mk_symbol("append"),cons(cons( mk_symbol("quote" ),cons(NIL,NIL)),cons(mk_symbol("identifiers"),NIL))),NIL))),NIL))),NIL)))),cons(cons(mk_symbol("for-each"),cons(cons(mk_symbol("lambda"),cons(cons(mk_symbol("identifier"),NIL),cons(cons(mk_symbol("environment-delete!"),cons(mk_symbol("parent-environment"),cons(mk_symbol("identifier"),NIL))),NIL))),cons(mk_symbol("identifiers"),NIL))),cons(cons(mk_symbol("environment-import!"),cons(mk_symbol("parent-environment"),cons(cons(mk_symbol("environment-only"),cons(cons(mk_symbol("current-environment"),NIL),cons(cons( mk_symbol("quote" ),cons(mk_symbol("define-library"),NIL)),cons(cons( mk_symbol("quote" ),cons(mk_symbol("import"),NIL)),cons(cons(mk_symbol("string->symbol"),cons(mk_string("(auto scheme)"),NIL)),cons(cons( mk_symbol("quote" ),cons(mk_symbol("begin"),NIL)),cons(cons( mk_symbol("quote" ),cons(mk_symbol("let"),NIL)),NIL))))))),NIL))),NIL))))))))))))),cons(cons(mk_symbol("current-environment"),NIL),NIL)),NIL))))),NIL)));
return 0;
}
