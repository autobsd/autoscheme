/*  This file is part of the 'AutoScheme' project.
 *  Copyright 2021 Steven Wiley <s.wiley@katchitek.com> 
 *  SPDX-License-Identifier: BSD-2-Clause
 */
#include "autoscheme.h"
pointer LOAD_MODULE__auto_scheme_compile(pointer environment);
pointer LOAD_MODULE__auto_scheme_compile(pointer environment)
{
pointer return_value = T;
return_value = autoscheme_eval(cons(mk_symbol("define-library"),cons(cons(mk_symbol("auto"),cons(mk_symbol("scheme"),cons(mk_symbol("compile"),NIL))),cons(cons(mk_symbol("export"),cons(mk_symbol("compile-module"),cons(mk_symbol("compile-program"),NIL))),cons(cons(mk_symbol("import"),cons(cons(mk_symbol("auto"),cons(mk_symbol("scheme"),cons(mk_symbol("path"),NIL))),cons(cons(mk_symbol("auto"),cons(mk_symbol("scheme"),cons(mk_symbol("base"),NIL))),cons(cons(mk_symbol("auto"),cons(mk_symbol("scheme"),cons(mk_symbol("file"),NIL))),cons(cons(mk_symbol("scheme"),cons(mk_symbol("read"),NIL)),cons(cons(mk_symbol("auto"),cons(mk_symbol("scheme"),cons(mk_symbol("write"),NIL))),cons(cons(mk_symbol("scheme"),cons(mk_symbol("cxr"),NIL)),NIL))))))),cons(cons(mk_symbol("begin"),cons(cons(mk_symbol("define"),cons(mk_symbol("object->string"),cons(cons(mk_symbol("lambda"),cons(cons(mk_symbol("object"),NIL),cons(cons(mk_symbol("let"),cons(cons(cons(mk_symbol("string-port"),cons(cons(mk_symbol("open-output-string"),NIL),NIL)),NIL),cons(cons(mk_symbol("write-simple"),cons(mk_symbol("object"),cons(mk_symbol("string-port"),NIL))),cons(cons(mk_symbol("let"),cons(cons(cons(mk_symbol("output-string"),cons(cons(mk_symbol("get-output-string"),cons(mk_symbol("string-port"),NIL)),NIL)),NIL),cons(cons(mk_symbol("close-output-port"),cons(mk_symbol("string-port"),NIL)),cons(mk_symbol("output-string"),NIL)))),NIL)))),NIL))),NIL))),cons(cons(mk_symbol("define"),cons(mk_symbol("read-list"),cons(cons(mk_symbol("lambda"),cons(mk_symbol("args"),cons(cons(mk_symbol("reverse"),cons(cons(mk_symbol("let"),cons(mk_symbol("read-expressions"),cons(cons(cons(mk_symbol("expressions"),cons(cons( mk_symbol("quote" ),cons(NIL,NIL)),NIL)),NIL),cons(cons(mk_symbol("let"),cons(cons(cons(mk_symbol("next-expression"),cons(cons(mk_symbol("apply"),cons(mk_symbol("read"),cons(mk_symbol("args"),NIL))),NIL)),NIL),cons(cons(mk_symbol("if"),cons(cons(mk_symbol("eof-object?"),cons(mk_symbol("next-expression"),NIL)),cons(mk_symbol("expressions"),cons(cons(mk_symbol("read-expressions"),cons(cons(mk_symbol("cons"),cons(mk_symbol("next-expression"),cons(mk_symbol("expressions"),NIL))),NIL)),NIL)))),NIL))),NIL)))),NIL)),NIL))),NIL))),cons(cons(mk_symbol("define"),cons(mk_symbol("compile-number"),cons(cons(mk_symbol("lambda"),cons(cons(mk_symbol("num"),NIL),cons(cons(mk_symbol("cond"),cons(cons(cons(mk_symbol("integer?"),cons(mk_symbol("num"),NIL)),cons(cons(mk_symbol("string-append"),cons(mk_string("mk_integer("),cons(cons(mk_symbol("number->string"),cons(mk_symbol("num"),NIL)),cons(mk_string(")"),NIL)))),NIL)),cons(cons(cons(mk_symbol("real?"),cons(mk_symbol("num"),NIL)),cons(cons(mk_symbol("string-append"),cons(mk_string("mk_real("),cons(cons(mk_symbol("number->string"),cons(mk_symbol("num"),NIL)),cons(mk_string(")"),NIL)))),NIL)),cons(cons(mk_symbol("else"),cons(cons(mk_symbol("error"),cons(mk_string("compile error - unknown number type: "),cons(mk_symbol("num"),NIL))),NIL)),NIL)))),NIL))),NIL))),cons(cons(mk_symbol("define"),cons(mk_symbol("*foreign-intializations*"),cons(cons( mk_symbol("quote" ),cons(NIL,NIL)),NIL))),cons(cons(mk_symbol("define"),cons(mk_symbol("*foreign-finalizations*"),cons(cons( mk_symbol("quote" ),cons(NIL,NIL)),NIL))),cons(cons(mk_symbol("define"),cons(mk_symbol("*foreign-definitions*"),cons(cons( mk_symbol("quote" ),cons(NIL,NIL)),NIL))),cons(cons(mk_symbol("define"),cons(mk_symbol("*foreign-declartions*"),cons(cons( mk_symbol("quote" ),cons(NIL,NIL)),NIL))),cons(cons(mk_symbol("define"),cons(mk_symbol("compile-expression"),cons(cons(mk_symbol("lambda"),cons(cons(mk_symbol("expression"),cons(mk_symbol("source"),cons(mk_symbol("quote-level"),NIL))),cons(cons(mk_symbol("letrec"),cons(cons(cons(mk_symbol("compile-time-macros"),cons(cons( mk_symbol("quasiquote" ),cons(cons(cons(mk_symbol("include-string"),cons( mk_symbol("unquote" ),cons(cons(mk_symbol("lambda"),cons(cons(mk_symbol("form"),NIL),cons(cons(mk_symbol("let*"),cons(cons(cons(mk_symbol("included-source"),cons(cons(mk_symbol("path-make-absolute"),cons(cons(mk_symbol("cadr"),cons(mk_symbol("form"),NIL)),cons(cons(mk_symbol("path-directory"),cons(mk_symbol("source"),NIL)),NIL))),NIL)),cons(cons(mk_symbol("included-string"),cons(cons(mk_symbol("with-input-from-file"),cons(mk_symbol("included-source"),cons(mk_symbol("read-string"),NIL))),NIL)),NIL)),cons(mk_symbol("included-string"),NIL))),NIL))),NIL))),cons(cons(mk_symbol("foreign-declare"),cons( mk_symbol("unquote" ),cons(cons(mk_symbol("lambda"),cons(cons(mk_symbol("form"),NIL),cons(cons(mk_symbol("let*"),cons(cons(cons(mk_symbol("included-string"),cons(cons(mk_symbol("apply"),cons(mk_symbol("string-append"),cons(cons(mk_symbol("map"),cons(mk_symbol("expand-compile-time-macro"),cons(cons(mk_symbol("cdr"),cons(mk_symbol("form"),NIL)),NIL))),NIL))),NIL)),NIL),cons(cons(mk_symbol("set!"),cons(mk_symbol("*foreign-declartions*"),cons(cons(mk_symbol("cons"),cons(mk_symbol("included-string"),cons(mk_symbol("*foreign-declartions*"),NIL))),NIL))),cons(T,NIL)))),NIL))),NIL))),cons(cons(mk_symbol("foreign-define"),cons( mk_symbol("unquote" ),cons(cons(mk_symbol("lambda"),cons(cons(mk_symbol("form"),NIL),cons(cons(mk_symbol("let*"),cons(cons(cons(mk_symbol("included-string"),cons(cons(mk_symbol("apply"),cons(mk_symbol("string-append"),cons(cons(mk_symbol("map"),cons(mk_symbol("expand-compile-time-macro"),cons(cons(mk_symbol("cdr"),cons(mk_symbol("form"),NIL)),NIL))),NIL))),NIL)),NIL),cons(cons(mk_symbol("set!"),cons(mk_symbol("*foreign-definitions*"),cons(cons(mk_symbol("cons"),cons(mk_symbol("included-string"),cons(mk_symbol("*foreign-definitions*"),NIL))),NIL))),cons(T,NIL)))),NIL))),NIL))),cons(cons(mk_symbol("foreign-initialize"),cons( mk_symbol("unquote" ),cons(cons(mk_symbol("lambda"),cons(cons(mk_symbol("form"),NIL),cons(cons(mk_symbol("let*"),cons(cons(cons(mk_symbol("included-string"),cons(cons(mk_symbol("apply"),cons(mk_symbol("string-append"),cons(cons(mk_symbol("map"),cons(mk_symbol("expand-compile-time-macro"),cons(cons(mk_symbol("cdr"),cons(mk_symbol("form"),NIL)),NIL))),NIL))),NIL)),NIL),cons(cons(mk_symbol("set!"),cons(mk_symbol("*foreign-intializations*"),cons(cons(mk_symbol("cons"),cons(mk_symbol("included-string"),cons(mk_symbol("*foreign-intializations*"),NIL))),NIL))),cons(T,NIL)))),NIL))),NIL))),cons(cons(mk_symbol("foreign-finalize"),cons( mk_symbol("unquote" ),cons(cons(mk_symbol("lambda"),cons(cons(mk_symbol("form"),NIL),cons(cons(mk_symbol("let*"),cons(cons(cons(mk_symbol("included-string"),cons(cons(mk_symbol("apply"),cons(mk_symbol("string-append"),cons(cons(mk_symbol("map"),cons(mk_symbol("expand-compile-time-macro"),cons(cons(mk_symbol("cdr"),cons(mk_symbol("form"),NIL)),NIL))),NIL))),NIL)),NIL),cons(cons(mk_symbol("set!"),cons(mk_symbol("*foreign-finalizations*"),cons(cons(mk_symbol("cons"),cons(mk_symbol("included-string"),cons(mk_symbol("*foreign-finalizations*"),NIL))),NIL))),cons(T,NIL)))),NIL))),NIL))),NIL))))),NIL)),NIL)),cons(cons(mk_symbol("compile-time-macro?"),cons(cons(mk_symbol("lambda"),cons(cons(mk_symbol("form"),NIL),cons(cons(mk_symbol("and"),cons(cons(mk_symbol("pair?"),cons(mk_symbol("form"),NIL)),cons(cons(mk_symbol("symbol?"),cons(cons(mk_symbol("car"),cons(mk_symbol("form"),NIL)),NIL)),cons(cons(mk_symbol("assoc"),cons(cons(mk_symbol("car"),cons(mk_symbol("form"),NIL)),cons(mk_symbol("compile-time-macros"),NIL))),NIL)))),NIL))),NIL)),cons(cons(mk_symbol("expand-compile-time-macro"),cons(cons(mk_symbol("lambda"),cons(cons(mk_symbol("form"),NIL),cons(cons(mk_symbol("if"),cons(cons(mk_symbol("compile-time-macro?"),cons(mk_symbol("form"),NIL)),cons(cons(cons(mk_symbol("cdr"),cons(cons(mk_symbol("assoc"),cons(cons(mk_symbol("car"),cons(mk_symbol("form"),NIL)),cons(mk_symbol("compile-time-macros"),NIL))),NIL)),cons(mk_symbol("form"),NIL)),cons(mk_symbol("form"),NIL)))),NIL))),NIL)),NIL))),cons(cons(mk_symbol("cond"),cons(cons(cons(mk_symbol("and"),cons(cons(mk_symbol("pair?"),cons(mk_symbol("expression"),NIL)),cons(cons(mk_symbol("equal?"),cons(cons(mk_symbol("car"),cons(mk_symbol("expression"),NIL)),cons(cons( mk_symbol("quote" ),cons(mk_symbol("quote"),NIL)),NIL))),cons(cons(mk_symbol("zero?"),cons(mk_symbol("quote-level"),NIL)),NIL)))),cons(cons(mk_symbol("string-append"),cons(mk_string("cons( mk_symbol(\"quote\" ),"),cons(cons(mk_symbol("compile-expression"),cons(cons(mk_symbol("cdr"),cons(mk_symbol("expression"),NIL)),cons(mk_symbol("source"),cons(mk_integer(-1),NIL)))),cons(mk_string(")"),NIL)))),NIL)),cons(cons(cons(mk_symbol("and"),cons(cons(mk_symbol("pair?"),cons(mk_symbol("expression"),NIL)),cons(cons(mk_symbol("equal?"),cons(cons(mk_symbol("car"),cons(mk_symbol("expression"),NIL)),cons(cons( mk_symbol("quote" ),cons(mk_symbol("quasiquote"),NIL)),NIL))),cons(cons(mk_symbol("not"),cons(cons(mk_symbol("negative?"),cons(mk_symbol("quote-level"),NIL)),NIL)),NIL)))),cons(cons(mk_symbol("string-append"),cons(mk_string("cons( mk_symbol(\"quasiquote\" ),"),cons(cons(mk_symbol("compile-expression"),cons(cons(mk_symbol("cdr"),cons(mk_symbol("expression"),NIL)),cons(mk_symbol("source"),cons(cons(mk_symbol("+"),cons(mk_symbol("quote-level"),cons(mk_integer(1),NIL))),NIL)))),cons(mk_string(")"),NIL)))),NIL)),cons(cons(cons(mk_symbol("and"),cons(cons(mk_symbol("pair?"),cons(mk_symbol("expression"),NIL)),cons(cons(mk_symbol("equal?"),cons(cons(mk_symbol("car"),cons(mk_symbol("expression"),NIL)),cons(cons( mk_symbol("quote" ),cons(mk_symbol("unquote"),NIL)),NIL))),cons(cons(mk_symbol("positive?"),cons(mk_symbol("quote-level"),NIL)),NIL)))),cons(cons(mk_symbol("string-append"),cons(mk_string("cons( mk_symbol(\"unquote\" ),"),cons(cons(mk_symbol("compile-expression"),cons(cons(mk_symbol("cdr"),cons(mk_symbol("expression"),NIL)),cons(mk_symbol("source"),cons(cons(mk_symbol("+"),cons(mk_symbol("quote-level"),cons(mk_integer(1),NIL))),NIL)))),cons(mk_string(")"),NIL)))),NIL)),cons(cons(cons(mk_symbol("and"),cons(cons(mk_symbol("pair?"),cons(mk_symbol("expression"),NIL)),cons(cons(mk_symbol("equal?"),cons(cons(mk_symbol("car"),cons(mk_symbol("expression"),NIL)),cons(cons( mk_symbol("quote" ),cons(mk_symbol("unquote-splicing"),NIL)),NIL))),cons(cons(mk_symbol("positive?"),cons(mk_symbol("quote-level"),NIL)),NIL)))),cons(cons(mk_symbol("string-append"),cons(mk_string("cons( mk_symbol(\"unquote-splicing\" ),"),cons(cons(mk_symbol("compile-expression"),cons(cons(mk_symbol("cdr"),cons(mk_symbol("expression"),NIL)),cons(mk_symbol("source"),cons(cons(mk_symbol("+"),cons(mk_symbol("quote-level"),cons(mk_integer(1),NIL))),NIL)))),cons(mk_string(")"),NIL)))),NIL)),cons(cons(cons(mk_symbol("and"),cons(cons(mk_symbol("pair?"),cons(mk_symbol("expression"),NIL)),cons(cons(mk_symbol("zero?"),cons(mk_symbol("quote-level"),NIL)),cons(cons(mk_symbol("equal?"),cons(cons(mk_symbol("car"),cons(mk_symbol("expression"),NIL)),cons(cons( mk_symbol("quote" ),cons(mk_symbol("foreign-function"),NIL)),NIL))),NIL)))),cons(cons(mk_symbol("string-append"),cons(mk_string("mk_function("),cons(cons(mk_symbol("object->string"),cons(cons(mk_symbol("cadr"),cons(mk_symbol("expression"),NIL)),NIL)),cons(mk_string(",&NIL)"),NIL)))),NIL)),cons(cons(cons(mk_symbol("and"),cons(cons(mk_symbol("pair?"),cons(mk_symbol("expression"),NIL)),cons(cons(mk_symbol("zero?"),cons(mk_symbol("quote-level"),NIL)),cons(cons(mk_symbol("equal?"),cons(cons(mk_symbol("car"),cons(mk_symbol("expression"),NIL)),cons(cons( mk_symbol("quote" ),cons(mk_symbol("foreign-procedure"),NIL)),NIL))),NIL)))),cons(cons(mk_symbol("string-append"),cons(mk_string("mk_operation("),cons(cons(mk_symbol("object->string"),cons(cons(mk_symbol("cadr"),cons(mk_symbol("expression"),NIL)),NIL)),cons(mk_string(",&NIL)"),NIL)))),NIL)),cons(cons(cons(mk_symbol("and"),cons(cons(mk_symbol("pair?"),cons(mk_symbol("expression"),NIL)),cons(cons(mk_symbol("zero?"),cons(mk_symbol("quote-level"),NIL)),cons(cons(mk_symbol("equal?"),cons(cons(mk_symbol("car"),cons(mk_symbol("expression"),NIL)),cons(cons( mk_symbol("quote" ),cons(mk_symbol("foreign-syntax"),NIL)),NIL))),NIL)))),cons(cons(mk_symbol("string-append"),cons(mk_string("mk_syntax("),cons(cons(mk_symbol("object->string"),cons(cons(mk_symbol("cadr"),cons(mk_symbol("expression"),NIL)),NIL)),cons(mk_string(","),cons(cons(mk_symbol("object->string"),cons(cons(mk_symbol("caddr"),cons(mk_symbol("expression"),NIL)),NIL)),cons(mk_string(")"),NIL)))))),NIL)),cons(cons(cons(mk_symbol("and"),cons(cons(mk_symbol("pair?"),cons(mk_symbol("expression"),NIL)),cons(cons(mk_symbol("zero?"),cons(mk_symbol("quote-level"),NIL)),cons(cons(mk_symbol("equal?"),cons(cons(mk_symbol("car"),cons(mk_symbol("expression"),NIL)),cons(cons( mk_symbol("quote" ),cons(mk_symbol("foreign-pointer"),NIL)),NIL))),NIL)))),cons(cons(mk_symbol("object->string"),cons(cons(mk_symbol("cadr"),cons(mk_symbol("expression"),NIL)),NIL)),NIL)),cons(cons(cons(mk_symbol("and"),cons(cons(mk_symbol("pair?"),cons(mk_symbol("expression"),NIL)),cons(cons(mk_symbol("zero?"),cons(mk_symbol("quote-level"),NIL)),cons(cons(mk_symbol("equal?"),cons(cons(mk_symbol("car"),cons(mk_symbol("expression"),NIL)),cons(cons( mk_symbol("quote" ),cons(mk_symbol("include"),NIL)),NIL))),NIL)))),cons(cons(mk_symbol("let*"),cons(cons(cons(mk_symbol("included-source"),cons(cons(mk_symbol("path-make-absolute"),cons(cons(mk_symbol("cadr"),cons(mk_symbol("expression"),NIL)),cons(cons(mk_symbol("path-directory"),cons(mk_symbol("source"),NIL)),NIL))),NIL)),cons(cons(mk_symbol("included-expressions"),cons(cons(mk_symbol("with-input-from-file"),cons(mk_symbol("included-source"),cons(mk_symbol("read-list"),NIL))),NIL)),NIL)),cons(cons(mk_symbol("compile-expression"),cons(cons(mk_symbol("cons"),cons(cons( mk_symbol("quote" ),cons(mk_symbol("begin"),NIL)),cons(mk_symbol("included-expressions"),NIL))),cons(mk_symbol("included-source"),cons(mk_symbol("quote-level"),NIL)))),NIL))),NIL)),cons(cons(cons(mk_symbol("and"),cons(cons(mk_symbol("zero?"),cons(mk_symbol("quote-level"),NIL)),cons(cons(mk_symbol("compile-time-macro?"),cons(mk_symbol("expression"),NIL)),NIL))),cons(cons(mk_symbol("let*"),cons(cons(cons(mk_symbol("expanded-expression"),cons(cons(mk_symbol("expand-compile-time-macro"),cons(mk_symbol("expression"),NIL)),NIL)),cons(cons(mk_symbol("_quote-level"),cons(cons(mk_symbol("if"),cons(cons(mk_symbol("equal?"),cons(mk_symbol("expanded-expression"),cons(mk_symbol("expression"),NIL))),cons(mk_integer(-1),cons(mk_symbol("quote-level"),NIL)))),NIL)),NIL)),cons(cons(mk_symbol("compile-expression"),cons(mk_symbol("expanded-expression"),cons(mk_symbol("source"),cons(mk_symbol("_quote-level"),NIL)))),NIL))),NIL)),cons(cons(cons(mk_symbol("pair?"),cons(mk_symbol("expression"),NIL)),cons(cons(mk_symbol("string-append"),cons(mk_string("cons("),cons(cons(mk_symbol("compile-expression"),cons(cons(mk_symbol("car"),cons(mk_symbol("expression"),NIL)),cons(mk_symbol("source"),cons(mk_symbol("quote-level"),NIL)))),cons(mk_string(","),cons(cons(mk_symbol("compile-expression"),cons(cons(mk_symbol("cdr"),cons(mk_symbol("expression"),NIL)),cons(mk_symbol("source"),cons(mk_symbol("quote-level"),NIL)))),cons(mk_string(")"),NIL)))))),NIL)),cons(cons(cons(mk_symbol("null?"),cons(mk_symbol("expression"),NIL)),cons(cons(mk_symbol("string-append"),cons(mk_string("NIL"),NIL)),NIL)),cons(cons(cons(mk_symbol("boolean?"),cons(mk_symbol("expression"),NIL)),cons(cons(mk_symbol("if"),cons(mk_symbol("expression"),cons(cons(mk_symbol("string-append"),cons(mk_string("T"),NIL)),cons(cons(mk_symbol("string-append"),cons(mk_string("F"),NIL)),NIL)))),NIL)),cons(cons(cons(mk_symbol("char?"),cons(mk_symbol("expression"),NIL)),cons(cons(mk_symbol("string-append"),cons(mk_string("mk_character("),cons(cons(mk_symbol("number->string"),cons(cons(mk_symbol("char->integer"),cons(mk_symbol("expression"),NIL)),NIL)),cons(mk_string(")"),NIL)))),NIL)),cons(cons(cons(mk_symbol("symbol?"),cons(mk_symbol("expression"),NIL)),cons(cons(mk_symbol("string-append"),cons(mk_string("mk_symbol(\""),cons(cons(mk_symbol("symbol->string"),cons(mk_symbol("expression"),NIL)),cons(mk_string("\")"),NIL)))),NIL)),cons(cons(cons(mk_symbol("string?"),cons(mk_symbol("expression"),NIL)),cons(cons(mk_symbol("string-append"),cons(mk_string("mk_string("),cons(cons(mk_symbol("object->string"),cons(mk_symbol("expression"),NIL)),cons(mk_string(")"),NIL)))),NIL)),cons(cons(cons(mk_symbol("number?"),cons(mk_symbol("expression"),NIL)),cons(cons(mk_symbol("compile-number"),cons(mk_symbol("expression"),NIL)),NIL)),cons(cons(mk_symbol("else"),cons(cons(mk_symbol("error"),cons(mk_string("compile error - unknown expression type: "),cons(mk_symbol("expression"),NIL))),NIL)),NIL))))))))))))))))))),NIL))),NIL))),NIL))),cons(cons(mk_symbol("define"),cons(mk_symbol("module-function-name"),cons(cons(mk_symbol("lambda"),cons(cons(mk_symbol("module-name"),NIL),cons(cons(mk_symbol("string-append"),cons(mk_string("LOAD_MODULE__"),cons(mk_symbol("module-name"),NIL))),NIL))),NIL))),cons(cons(mk_symbol("define"),cons(mk_symbol("module-function-declaration"),cons(cons(mk_symbol("lambda"),cons(cons(mk_symbol("module-name"),NIL),cons(cons(mk_symbol("string-append"),cons(mk_string("pointer "),cons(cons(mk_symbol("module-function-name"),cons(mk_symbol("module-name"),NIL)),cons(mk_string("(pointer environment)"),NIL)))),NIL))),NIL))),cons(cons(mk_symbol("define"),cons(mk_symbol("module-function-prototype"),cons(cons(mk_symbol("lambda"),cons(cons(mk_symbol("module-name"),NIL),cons(cons(mk_symbol("string-append"),cons(mk_string("pointer "),cons(cons(mk_symbol("module-function-name"),cons(mk_symbol("module-name"),NIL)),cons(mk_string("(pointer environment)"),NIL)))),NIL))),NIL))),cons(cons(mk_symbol("define"),cons(mk_symbol("compile-module-function"),cons(cons(mk_symbol("lambda"),cons(cons(mk_symbol("name"),cons(mk_symbol("sources"),NIL)),cons(cons(mk_symbol("let*"),cons(cons(cons(mk_symbol("statements"),cons(mk_string(""),NIL)),NIL),cons(cons(mk_symbol("for-each"),cons(cons(mk_symbol("lambda"),cons(cons(mk_symbol("source"),NIL),cons(cons(mk_symbol("with-input-from-file"),cons(mk_symbol("source"),cons(cons(mk_symbol("lambda"),cons(NIL,cons(cons(mk_symbol("let"),cons(mk_symbol("process-expression"),cons(cons(cons(mk_symbol("expression"),cons(cons(mk_symbol("read"),NIL),NIL)),NIL),cons(cons(mk_symbol("cond"),cons(cons(cons(mk_symbol("not"),cons(cons(mk_symbol("eof-object?"),cons(mk_symbol("expression"),NIL)),NIL)),cons(cons(mk_symbol("set!"),cons(mk_symbol("statements"),cons(cons(mk_symbol("string-append"),cons(mk_symbol("statements"),cons(mk_string("return_value = autoscheme_eval("),cons(cons(mk_symbol("compile-expression"),cons(mk_symbol("expression"),cons(mk_symbol("source"),cons(mk_integer(0),NIL)))),cons(mk_string(", environment);\n"),NIL))))),NIL))),cons(cons(mk_symbol("process-expression"),cons(cons(mk_symbol("read"),NIL),NIL)),NIL))),NIL)),NIL)))),NIL))),NIL))),NIL))),cons(mk_symbol("sources"),NIL))),cons(cons(mk_symbol("string-append"),cons(cons(mk_symbol("module-function-prototype"),cons(mk_symbol("name"),NIL)),cons(mk_string("\n"),cons(mk_string("{\n"),cons(mk_string("pointer return_value = T;\n"),cons(cons(mk_symbol("apply"),cons(mk_symbol("string-append"),cons(mk_symbol("*foreign-intializations*"),NIL))),cons(mk_symbol("statements"),cons(cons(mk_symbol("apply"),cons(mk_symbol("string-append"),cons(mk_symbol("*foreign-finalizations*"),NIL))),cons(mk_string("return return_value;\n"),cons(mk_string("}\n"),NIL)))))))))),NIL)))),NIL))),NIL))),cons(cons(mk_symbol("define"),cons(mk_symbol("compile-module"),cons(cons(mk_symbol("lambda"),cons(cons(mk_symbol("source-files"),cons(mk_symbol("module-name"),cons(mk_symbol("output-file"),NIL))),cons(cons(mk_symbol("set!"),cons(mk_symbol("*foreign-intializations*"),cons(cons( mk_symbol("quote" ),cons(NIL,NIL)),NIL))),cons(cons(mk_symbol("set!"),cons(mk_symbol("*foreign-finalizations*"),cons(cons( mk_symbol("quote" ),cons(NIL,NIL)),NIL))),cons(cons(mk_symbol("set!"),cons(mk_symbol("*foreign-definitions*"),cons(cons( mk_symbol("quote" ),cons(NIL,NIL)),NIL))),cons(cons(mk_symbol("set!"),cons(mk_symbol("*foreign-declartions*"),cons(cons( mk_symbol("quote" ),cons(NIL,NIL)),NIL))),cons(cons(mk_symbol("let*"),cons(cons(cons(mk_symbol("module-name"),cons(cons(mk_symbol("or"),cons(mk_symbol("module-name"),cons(mk_string("module"),NIL))),NIL)),cons(cons(mk_symbol("output-file"),cons(cons(mk_symbol("or"),cons(mk_symbol("output-file"),cons(mk_string("module.c"),NIL))),NIL)),cons(cons(mk_symbol("includes-template"),cons(cons(mk_symbol("string-append"),cons(mk_string("#include \"autoscheme.h\"\n"),NIL)),NIL)),cons(cons(mk_symbol("declarations-template"),cons(cons(mk_symbol("string-append"),cons(cons(mk_symbol("module-function-declaration"),cons(mk_symbol("module-name"),NIL)),cons(mk_string(";\n"),NIL))),NIL)),cons(cons(mk_symbol("module-function"),cons(cons(mk_symbol("compile-module-function"),cons(mk_symbol("module-name"),cons(mk_symbol("source-files"),NIL))),NIL)),cons(cons(mk_symbol("output-port"),cons(cons(mk_symbol("open-output-file"),cons(mk_symbol("output-file"),NIL)),NIL)),NIL)))))),cons(cons(mk_symbol("display"),cons(mk_symbol("includes-template"),cons(mk_symbol("output-port"),NIL))),cons(cons(mk_symbol("display"),cons(mk_symbol("declarations-template"),cons(mk_symbol("output-port"),NIL))),cons(cons(mk_symbol("display"),cons(cons(mk_symbol("apply"),cons(mk_symbol("string-append"),cons(mk_symbol("*foreign-declartions*"),NIL))),cons(mk_symbol("output-port"),NIL))),cons(cons(mk_symbol("display"),cons(cons(mk_symbol("apply"),cons(mk_symbol("string-append"),cons(mk_symbol("*foreign-definitions*"),NIL))),cons(mk_symbol("output-port"),NIL))),cons(cons(mk_symbol("display"),cons(mk_symbol("module-function"),cons(mk_symbol("output-port"),NIL))),cons(cons(mk_symbol("close-output-port"),cons(mk_symbol("output-port"),NIL)),NIL)))))))),NIL))))))),NIL))),cons(cons(mk_symbol("define"),cons(mk_symbol("compile-program"),cons(cons(mk_symbol("lambda"),cons(cons(mk_symbol("source-files"),cons(mk_symbol("module-list"),cons(mk_symbol("output-file"),NIL))),cons(cons(mk_symbol("set!"),cons(mk_symbol("*foreign-intializations*"),cons(cons( mk_symbol("quote" ),cons(NIL,NIL)),NIL))),cons(cons(mk_symbol("set!"),cons(mk_symbol("*foreign-definitions*"),cons(cons( mk_symbol("quote" ),cons(NIL,NIL)),NIL))),cons(cons(mk_symbol("set!"),cons(mk_symbol("*foreign-declartions*"),cons(cons( mk_symbol("quote" ),cons(NIL,NIL)),NIL))),cons(cons(mk_symbol("let*"),cons(cons(cons(mk_symbol("includes-template"),cons(cons(mk_symbol("string-append"),cons(mk_string("#include \"autoscheme.h\"\n"),NIL)),NIL)),cons(cons(mk_symbol("declarations-template"),cons(cons(mk_symbol("string-append"),cons(cons(mk_symbol("apply"),cons(mk_symbol("string-append"),cons(cons(mk_symbol("map"),cons(cons(mk_symbol("lambda"),cons(cons(mk_symbol("name"),NIL),cons(cons(mk_symbol("string-append"),cons(cons(mk_symbol("module-function-declaration"),cons(mk_symbol("name"),NIL)),cons(mk_string(";\n"),NIL))),NIL))),cons(mk_symbol("module-list"),NIL))),NIL))),cons(cons(mk_symbol("module-function-declaration"),cons(mk_string("program"),NIL)),cons(mk_string(";\n"),cons(mk_string("int auto_argc;\n"),cons(mk_string("char **auto_argv;\n"),NIL)))))),NIL)),cons(cons(mk_symbol("functions-template"),cons(cons(mk_symbol("string-append"),cons(mk_string("int main( int argc, char **argv )\n"),cons(mk_string("{\n"),cons(mk_string("    auto_argc = argc;\n"),cons(mk_string("    auto_argv = argv;\n"),cons(mk_string("    scheme_init();\n"),cons(cons(mk_symbol("apply"),cons(mk_symbol("string-append"),cons(cons(mk_symbol("map"),cons(cons(mk_symbol("lambda"),cons(cons(mk_symbol("name"),NIL),cons(cons(mk_symbol("string-append"),cons(mk_string("    LOAD_MODULE__"),cons(mk_symbol("name"),cons(mk_string("(global_env);\n"),NIL)))),NIL))),cons(mk_symbol("module-list"),NIL))),NIL))),cons(mk_string("    LOAD_MODULE__program(global_env);\n"),cons(mk_string("    scheme_deinit();\n"),cons(mk_string("    return 0;\n"),cons(mk_string("}\n"),NIL))))))))))),NIL)),cons(cons(mk_symbol("module-function"),cons(cons(mk_symbol("compile-module-function"),cons(mk_string("program"),cons(mk_symbol("source-files"),NIL))),NIL)),cons(cons(mk_symbol("output-file"),cons(cons(mk_symbol("if"),cons(mk_symbol("output-file"),cons(mk_symbol("output-file"),cons(mk_string("program.c"),NIL)))),NIL)),cons(cons(mk_symbol("output-port"),cons(cons(mk_symbol("open-output-file"),cons(mk_symbol("output-file"),NIL)),NIL)),NIL)))))),cons(cons(mk_symbol("display"),cons(mk_symbol("includes-template"),cons(mk_symbol("output-port"),NIL))),cons(cons(mk_symbol("display"),cons(mk_symbol("declarations-template"),cons(mk_symbol("output-port"),NIL))),cons(cons(mk_symbol("display"),cons(mk_symbol("functions-template"),cons(mk_symbol("output-port"),NIL))),cons(cons(mk_symbol("display"),cons(cons(mk_symbol("apply"),cons(mk_symbol("string-append"),cons(mk_symbol("*foreign-declartions*"),NIL))),cons(mk_symbol("output-port"),NIL))),cons(cons(mk_symbol("display"),cons(cons(mk_symbol("apply"),cons(mk_symbol("string-append"),cons(mk_symbol("*foreign-definitions*"),NIL))),cons(mk_symbol("output-port"),NIL))),cons(cons(mk_symbol("display"),cons(mk_symbol("module-function"),cons(mk_symbol("output-port"),NIL))),cons(cons(mk_symbol("close-output-port"),cons(mk_symbol("output-port"),NIL)),NIL))))))))),NIL)))))),NIL))),NIL))))))))))))))),NIL))))), environment);
return return_value;
}
