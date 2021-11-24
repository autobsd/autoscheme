/*  This file is part of the 'AutoScheme' project.
 *  Copyright 2021 Steven Wiley <s.wiley@katchitek.com> 
 *  SPDX-License-Identifier: BSD-2-Clause
 */
#include "autoscheme.h"
pointer LOAD_MODULE__scheme(pointer environment);
pointer LOAD_MODULE__auto_scheme_base(pointer environment);
pointer LOAD_MODULE__auto_scheme_write(pointer environment);
pointer LOAD_MODULE__auto_scheme_args_fold(pointer environment);
pointer LOAD_MODULE__auto_scheme_char_set(pointer environment);
pointer LOAD_MODULE__auto_scheme_string(pointer environment);
pointer LOAD_MODULE__scheme_cxr(pointer environment);
pointer LOAD_MODULE__auto_scheme_args(pointer environment);
pointer LOAD_MODULE__auto_scheme_char(pointer environment);
pointer LOAD_MODULE__auto_scheme_path(pointer environment);
pointer LOAD_MODULE__auto_scheme_file(pointer environment);
pointer LOAD_MODULE__scheme_read(pointer environment);
pointer LOAD_MODULE__auto_scheme_compile(pointer environment);
pointer LOAD_MODULE__scheme_eval(pointer environment);
pointer LOAD_MODULE__scheme_load(pointer environment);
pointer LOAD_MODULE__scheme_process_context(pointer environment);
pointer LOAD_MODULE__auto_scheme_interpret(pointer environment);
pointer LOAD_MODULE__program(pointer environment);

int main( int argc, char **argv )
{
    scheme_init( argc, argv );
    LOAD_MODULE__scheme(global_env);
    LOAD_MODULE__auto_scheme_base(global_env);
    LOAD_MODULE__auto_scheme_write(global_env);
    LOAD_MODULE__auto_scheme_args_fold(global_env);
    LOAD_MODULE__auto_scheme_char_set(global_env);
    LOAD_MODULE__auto_scheme_string(global_env);
    LOAD_MODULE__scheme_cxr(global_env);
    LOAD_MODULE__auto_scheme_args(global_env);
    LOAD_MODULE__auto_scheme_char(global_env);
    LOAD_MODULE__auto_scheme_path(global_env);
    LOAD_MODULE__auto_scheme_file(global_env);
    LOAD_MODULE__scheme_read(global_env);
    LOAD_MODULE__auto_scheme_compile(global_env);
    LOAD_MODULE__scheme_eval(global_env);
    LOAD_MODULE__scheme_load(global_env);
    LOAD_MODULE__scheme_process_context(global_env);
    LOAD_MODULE__auto_scheme_interpret(global_env);
    LOAD_MODULE__program(global_env);
    scheme_deinit();
    return 0;
}
pointer LOAD_MODULE__program(pointer environment)
{
pointer return_value = T;
autoscheme_eval(cons(mk_symbol("import"),cons(cons(mk_symbol("auto"),cons(mk_symbol("scheme"),cons(mk_symbol("base"),NIL))),cons(cons(mk_symbol("auto"),cons(mk_symbol("scheme"),cons(mk_symbol("write"),NIL))),cons(cons(mk_symbol("auto"),cons(mk_symbol("scheme"),cons(mk_symbol("args"),cons(mk_symbol("fold"),NIL)))),cons(cons(mk_symbol("auto"),cons(mk_symbol("scheme"),cons(mk_symbol("args"),NIL))),cons(cons(mk_symbol("scheme"),cons(mk_symbol("cxr"),NIL)),cons(cons(mk_symbol("scheme"),cons(mk_symbol("process-context"),NIL)),cons(cons(mk_symbol("auto"),cons(mk_symbol("scheme"),cons(mk_symbol("string"),NIL))),cons(cons(mk_symbol("scheme"),cons(mk_symbol("read"),NIL)),cons(cons(mk_symbol("auto"),cons(mk_symbol("scheme"),cons(mk_symbol("file"),NIL))),cons(cons(mk_symbol("auto"),cons(mk_symbol("scheme"),cons(mk_symbol("compile"),NIL))),cons(cons(mk_symbol("auto"),cons(mk_symbol("scheme"),cons(mk_symbol("interpret"),NIL))),NIL)))))))))))), environment);
autoscheme_eval(cons(mk_symbol("define"),cons(mk_symbol("program-version"),cons(cons(mk_syntax(LOC_BEGIN,"begin"),cons(mk_string("0.211.0 (rev 1637739904)"),NIL)),NIL))), environment);
autoscheme_eval(cons(mk_symbol("define"),cons(mk_symbol("display-version"),cons(cons(mk_symbol("lambda"),cons(NIL,cons(cons(mk_symbol("display"),cons(cons(mk_symbol("string-append"),cons(mk_string("AutoScheme version "),cons(mk_symbol("program-version"),NIL))),NIL)),cons(cons(mk_symbol("newline"),NIL),NIL)))),NIL))), environment);
autoscheme_eval(cons(mk_symbol("define"),cons(mk_symbol("display-usage"),cons(cons(mk_symbol("lambda"),cons(mk_symbol("args"),cons(cons(mk_symbol("apply"),cons(mk_symbol("display"),cons(cons(mk_symbol("cons"),cons(mk_string("Usage: autoscheme option ... [argument ...]"),cons(mk_symbol("args"),NIL))),NIL))),cons(cons(mk_symbol("apply"),cons(mk_symbol("newline"),cons(mk_symbol("args"),NIL))),cons(cons(mk_symbol("apply"),cons(mk_symbol("args-usage"),cons(cons(mk_symbol("cons"),cons(mk_symbol("option-table"),cons(mk_symbol("args"),NIL))),NIL))),NIL))))),NIL))), environment);
autoscheme_eval(cons(mk_symbol("define"),cons(mk_symbol("recognized-processor"),cons(cons(mk_symbol("lambda"),cons(cons(mk_symbol("option"),cons(mk_symbol("name"),cons(mk_symbol("arg"),mk_symbol("seeds")))),cons(cons(mk_symbol("let"),cons(cons(cons(mk_symbol("options"),cons(cons(mk_symbol("car"),cons(mk_symbol("seeds"),NIL)),NIL)),cons(cons(mk_symbol("arguments"),cons(cons(mk_symbol("cadr"),cons(mk_symbol("seeds"),NIL)),NIL)),NIL)),cons(cons(mk_symbol("values"),cons(cons(mk_symbol("cons"),cons(cons(mk_symbol("list"),cons(mk_symbol("option"),cons(mk_symbol("name"),cons(mk_symbol("arg"),NIL)))),cons(mk_symbol("options"),NIL))),cons(mk_symbol("arguments"),NIL))),NIL))),NIL))),NIL))), environment);
autoscheme_eval(cons(mk_symbol("define"),cons(mk_symbol("repl-processor"),cons(cons(mk_symbol("lambda"),cons(mk_symbol("args"),cons(cons(mk_symbol("repl"),cons(cons(mk_symbol("interaction-environment"),NIL),NIL)),cons(cons(mk_symbol("exit"),NIL),NIL)))),NIL))), environment);
autoscheme_eval(cons(mk_symbol("define"),cons(mk_symbol("version-processor"),cons(cons(mk_symbol("lambda"),cons(mk_symbol("args"),cons(cons(mk_symbol("display-version"),NIL),cons(cons(mk_symbol("exit"),NIL),NIL)))),NIL))), environment);
autoscheme_eval(cons(mk_symbol("define"),cons(mk_symbol("help-processor"),cons(cons(mk_symbol("lambda"),cons(mk_symbol("args"),cons(cons(mk_symbol("display-version"),NIL),cons(cons(mk_symbol("display-usage"),NIL),cons(cons(mk_symbol("exit"),NIL),NIL))))),NIL))), environment);
autoscheme_eval(cons(mk_symbol("define"),cons(mk_symbol("unrecognized-processor"),cons(cons(mk_symbol("lambda"),cons(cons(mk_symbol("option"),cons(mk_symbol("name"),cons(mk_symbol("arg"),mk_symbol("seeds")))),cons(cons(mk_symbol("let"),cons(cons(cons(mk_symbol("options"),cons(cons(mk_symbol("car"),cons(mk_symbol("seeds"),NIL)),NIL)),cons(cons(mk_symbol("arguments"),cons(cons(mk_symbol("cadr"),cons(mk_symbol("seeds"),NIL)),NIL)),cons(cons(mk_symbol("name-string"),cons(cons(mk_symbol("if"),cons(cons(mk_symbol("char?"),cons(mk_symbol("name"),NIL)),cons(cons(mk_symbol("string-append"),cons(mk_string("-"),cons(cons(mk_symbol("string"),cons(mk_symbol("name"),NIL)),NIL))),cons(cons(mk_symbol("string-append"),cons(mk_string("--"),cons(mk_symbol("name"),NIL))),NIL)))),NIL)),NIL))),cons(cons(mk_symbol("cond"),cons(cons(cons(mk_symbol("assoc"),cons(mk_symbol("interpret-option"),cons(mk_symbol("options"),NIL))),cons(cons(mk_symbol("values"),cons(mk_symbol("options"),cons(cons(mk_symbol("if"),cons(cons(mk_symbol("char?"),cons(mk_symbol("name"),NIL)),cons(cons(mk_symbol("cons"),cons(mk_symbol("arg"),cons(cons(mk_symbol("cons"),cons(mk_symbol("name-string"),cons(mk_symbol("arguments"),NIL))),NIL))),cons(cons(mk_symbol("cons"),cons(cons(mk_symbol("string-append"),cons(mk_symbol("name-string"),cons(mk_string("="),cons(mk_symbol("arg"),NIL)))),cons(mk_symbol("arguments"),NIL))),NIL)))),NIL))),NIL)),cons(cons(mk_symbol("else"),cons(cons(mk_symbol("display"),cons(cons(mk_symbol("string-append"),cons(mk_string("autoscheme: unrecognized option "),cons(mk_symbol("name-string"),NIL))),cons(cons(mk_symbol("current-error-port"),NIL),NIL))),cons(cons(mk_symbol("newline"),cons(cons(mk_symbol("current-error-port"),NIL),NIL)),cons(cons(mk_symbol("display-usage"),cons(cons(mk_symbol("current-error-port"),NIL),NIL)),cons(cons(mk_symbol("exit"),cons(mk_integer(1),NIL)),NIL))))),NIL))),NIL))),NIL))),NIL))), environment);
autoscheme_eval(cons(mk_symbol("define"),cons(mk_symbol("operand-processor"),cons(cons(mk_symbol("lambda"),cons(cons(mk_symbol("operand"),mk_symbol("seeds")),cons(cons(mk_symbol("let"),cons(cons(cons(mk_symbol("options"),cons(cons(mk_symbol("car"),cons(mk_symbol("seeds"),NIL)),NIL)),cons(cons(mk_symbol("arguments"),cons(cons(mk_symbol("cadr"),cons(mk_symbol("seeds"),NIL)),NIL)),NIL)),cons(cons(mk_symbol("values"),cons(mk_symbol("options"),cons(cons(mk_symbol("cons"),cons(mk_symbol("operand"),cons(mk_symbol("arguments"),NIL))),NIL))),NIL))),NIL))),NIL))), environment);
autoscheme_eval(cons(mk_symbol("define"),cons(mk_symbol("interpret-option"),cons(cons(mk_symbol("option"),cons(cons(mk_symbol("quote"),cons(cons(mk_character(105),cons(mk_string("interpret"),NIL)),NIL)),cons(T,cons(F,cons(mk_symbol("recognized-processor"),NIL))))),NIL))), environment);
autoscheme_eval(cons(mk_symbol("define"),cons(mk_symbol("option-table"),cons(cons(mk_symbol("quasiquote"),cons(cons(cons(cons(mk_symbol("unquote"),cons(mk_symbol("interpret-option"),NIL)),cons(mk_string("Interpret program files"),cons(mk_string("FILES"),NIL))),cons(cons(cons(mk_symbol("unquote"),cons(cons(mk_symbol("option"),cons(cons(mk_symbol("quote"),cons(cons(mk_character(99),cons(mk_string("compile"),NIL)),NIL)),cons(T,cons(F,cons(mk_symbol("recognized-processor"),NIL))))),NIL)),cons(mk_string("Compile program files"),cons(mk_string("FILES"),NIL))),cons(cons(cons(mk_symbol("unquote"),cons(cons(mk_symbol("option"),cons(cons(mk_symbol("quote"),cons(cons(mk_character(108),cons(mk_string("load-modules"),NIL)),NIL)),cons(T,cons(F,cons(mk_symbol("recognized-processor"),NIL))))),NIL)),cons(mk_string("Load module files"),cons(mk_string("FILES"),NIL))),cons(cons(cons(mk_symbol("unquote"),cons(cons(mk_symbol("option"),cons(cons(mk_symbol("quote"),cons(cons(mk_character(109),cons(mk_string("compile-module"),NIL)),NIL)),cons(T,cons(F,cons(mk_symbol("recognized-processor"),NIL))))),NIL)),cons(mk_string("Compile module files"),cons(mk_string("FILES"),NIL))),cons(cons(cons(mk_symbol("unquote"),cons(cons(mk_symbol("option"),cons(cons(mk_symbol("quote"),cons(cons(mk_character(110),cons(mk_string("module-name"),NIL)),NIL)),cons(T,cons(F,cons(mk_symbol("recognized-processor"),NIL))))),NIL)),cons(mk_string("Specify module name"),cons(mk_string("NAME"),NIL))),cons(cons(cons(mk_symbol("unquote"),cons(cons(mk_symbol("option"),cons(cons(mk_symbol("quote"),cons(cons(mk_character(111),cons(mk_string("output-file"),NIL)),NIL)),cons(T,cons(F,cons(mk_symbol("recognized-processor"),NIL))))),NIL)),cons(mk_string("Specify output file"),cons(mk_string("FILE"),NIL))),cons(cons(cons(mk_symbol("unquote"),cons(cons(mk_symbol("option"),cons(cons(mk_symbol("quote"),cons(cons(mk_character(86),cons(mk_string("version"),NIL)),NIL)),cons(F,cons(F,cons(mk_symbol("version-processor"),NIL))))),NIL)),cons(mk_string("Display version information"),NIL)),cons(cons(cons(mk_symbol("unquote"),cons(cons(mk_symbol("option"),cons(cons(mk_symbol("quote"),cons(cons(mk_character(104),cons(mk_string("help"),NIL)),NIL)),cons(F,cons(F,cons(mk_symbol("help-processor"),NIL))))),NIL)),cons(mk_string("Show this message"),NIL)),NIL)))))))),NIL)),NIL))), environment);
return_value = autoscheme_eval(cons(mk_symbol("let*"),cons(cons(cons(mk_symbol("seeds"),cons(cons(mk_symbol("list"),cons(cons(mk_symbol("quote"),cons(NIL,NIL)),cons(cons(mk_symbol("quote"),cons(NIL,NIL)),NIL))),NIL)),cons(cons(mk_symbol("result"),cons(cons(mk_symbol("call-with-values"),cons(cons(mk_symbol("lambda"),cons(NIL,cons(cons(mk_symbol("apply"),cons(mk_symbol("args-fold"),cons(cons(mk_symbol("append"),cons(cons(mk_symbol("list"),cons(cons(mk_symbol("cdr"),cons(cons(mk_symbol("command-line"),NIL),NIL)),cons(cons(mk_symbol("map"),cons(mk_symbol("car"),cons(mk_symbol("option-table"),NIL))),cons(mk_symbol("unrecognized-processor"),cons(mk_symbol("operand-processor"),NIL))))),cons(mk_symbol("seeds"),NIL))),NIL))),NIL))),cons(mk_symbol("list"),NIL))),NIL)),cons(cons(mk_symbol("options"),cons(cons(mk_symbol("car"),cons(mk_symbol("result"),NIL)),NIL)),cons(cons(mk_symbol("arguments"),cons(cons(mk_symbol("reverse"),cons(cons(mk_symbol("cadr"),cons(mk_symbol("result"),NIL)),NIL)),NIL)),cons(cons(mk_symbol("option-selected?"),cons(cons(mk_symbol("lambda"),cons(cons(mk_symbol("name"),cons(mk_symbol("selected-options"),NIL)),cons(cons(mk_symbol("call/cc"),cons(cons(mk_symbol("lambda"),cons(cons(mk_symbol("return"),NIL),cons(cons(mk_symbol("for-each"),cons(cons(mk_symbol("lambda"),cons(cons(mk_symbol("selected-option"),NIL),cons(cons(mk_symbol("if"),cons(cons(mk_symbol("member"),cons(mk_symbol("name"),cons(cons(mk_symbol("option-names"),cons(cons(mk_symbol("car"),cons(mk_symbol("selected-option"),NIL)),NIL)),NIL))),cons(cons(mk_symbol("return"),cons(mk_symbol("selected-option"),NIL)),NIL))),NIL))),cons(mk_symbol("selected-options"),NIL))),cons(F,NIL)))),NIL)),NIL))),NIL)),cons(cons(mk_symbol("get-selected-arg"),cons(cons(mk_symbol("lambda"),cons(cons(mk_symbol("name"),NIL),cons(cons(mk_symbol("let"),cons(cons(cons(mk_symbol("selected-option"),cons(cons(mk_symbol("option-selected?"),cons(mk_symbol("name"),cons(mk_symbol("options"),NIL))),NIL)),NIL),cons(cons(mk_symbol("if"),cons(mk_symbol("selected-option"),cons(cons(mk_symbol("caddr"),cons(mk_symbol("selected-option"),NIL)),cons(F,NIL)))),NIL))),NIL))),NIL)),cons(cons(mk_symbol("arg->list"),cons(cons(mk_symbol("lambda"),cons(cons(mk_symbol("arg"),NIL),cons(cons(mk_symbol("if"),cons(mk_symbol("arg"),cons(cons(mk_symbol("apply"),cons(mk_symbol("append"),cons(cons(mk_symbol("map"),cons(cons(mk_symbol("lambda"),cons(cons(mk_symbol("token"),NIL),cons(cons(mk_symbol("if"),cons(cons(mk_symbol("="),cons(cons(mk_symbol("string-length"),cons(mk_symbol("token"),NIL)),cons(mk_integer(0),NIL))),cons(cons(mk_symbol("quote"),cons(NIL,NIL)),cons(cons(mk_symbol("list"),cons(mk_symbol("token"),NIL)),NIL)))),NIL))),cons(cons(mk_symbol("string-tokenize"),cons(mk_symbol("arg"),NIL)),NIL))),NIL))),cons(cons(mk_symbol("quote"),cons(NIL,NIL)),NIL)))),NIL))),NIL)),NIL))))))),cons(cons(mk_symbol("cond"),cons(cons(cons(mk_symbol("option-selected?"),cons(mk_string("interpret"),cons(mk_symbol("options"),NIL))),cons(cons(mk_symbol("interpret"),cons(cons(mk_symbol("arg->list"),cons(cons(mk_symbol("get-selected-arg"),cons(mk_string("interpret"),NIL)),NIL)),cons(mk_symbol("arguments"),NIL))),NIL)),cons(cons(cons(mk_symbol("option-selected?"),cons(mk_string("compile"),cons(mk_symbol("options"),NIL))),cons(cons(mk_symbol("compile-program"),cons(cons(mk_symbol("arg->list"),cons(cons(mk_symbol("get-selected-arg"),cons(mk_string("compile"),NIL)),NIL)),cons(cons(mk_symbol("arg->list"),cons(cons(mk_symbol("get-selected-arg"),cons(mk_string("load-modules"),NIL)),NIL)),cons(cons(mk_symbol("get-selected-arg"),cons(mk_string("output-file"),NIL)),NIL)))),NIL)),cons(cons(cons(mk_symbol("option-selected?"),cons(mk_string("compile-module"),cons(mk_symbol("options"),NIL))),cons(cons(mk_symbol("compile-module"),cons(cons(mk_symbol("arg->list"),cons(cons(mk_symbol("get-selected-arg"),cons(mk_string("compile-module"),NIL)),NIL)),cons(cons(mk_symbol("get-selected-arg"),cons(mk_string("module-name"),NIL)),cons(cons(mk_symbol("get-selected-arg"),cons(mk_string("output-file"),NIL)),NIL)))),NIL)),cons(cons(mk_symbol("else"),cons(cons(mk_symbol("help-processor"),NIL),NIL)),NIL))))),NIL))), environment);
return return_value;
}
