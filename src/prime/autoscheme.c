/*  This file is part of the 'AutoScheme' project.
 *  Copyright 2021 Steven Wiley <s.wiley@katchitek.com> 
 *  SPDX-License-Identifier: BSD-2-Clause
 */
#include "autoscheme.h"
pointer LOAD_MODULE__scheme(pointer environment);
pointer LOAD_MODULE__auto_scheme_file(pointer environment);
pointer LOAD_MODULE__auto_scheme_port(pointer environment);
pointer LOAD_MODULE__auto_scheme_char(pointer environment);
pointer LOAD_MODULE__auto_scheme_inexact(pointer environment);
pointer LOAD_MODULE__auto_scheme_lazy(pointer environment);
pointer LOAD_MODULE__auto_scheme_memory(pointer environment);
pointer LOAD_MODULE__auto_scheme_closure(pointer environment);
pointer LOAD_MODULE__scheme_cxr(pointer environment);
pointer LOAD_MODULE__auto_scheme_macro(pointer environment);
pointer LOAD_MODULE__auto_scheme_base(pointer environment);
pointer LOAD_MODULE__scheme_process_context(pointer environment);
pointer LOAD_MODULE__scheme_read(pointer environment);
pointer LOAD_MODULE__auto_scheme_environment(pointer environment);
pointer LOAD_MODULE__scheme_eval(pointer environment);
pointer LOAD_MODULE__scheme_repl(pointer environment);
pointer LOAD_MODULE__scheme_load(pointer environment);
pointer LOAD_MODULE__auto_scheme_list(pointer environment);
pointer LOAD_MODULE__auto_scheme_write(pointer environment);
pointer LOAD_MODULE__auto_scheme_string(pointer environment);
pointer LOAD_MODULE__auto_scheme_args_fold(pointer environment);
pointer LOAD_MODULE__auto_scheme_args(pointer environment);
pointer LOAD_MODULE__auto_scheme_directory(pointer environment);
pointer LOAD_MODULE__auto_scheme_path(pointer environment);
pointer LOAD_MODULE__auto_scheme_compile(pointer environment);
pointer LOAD_MODULE__auto_scheme_interpret(pointer environment);
pointer LOAD_MODULE__program(pointer environment);
int auto_argc;
char **auto_argv;
int main( int argc, char **argv )
{
    auto_argc = argc;
    auto_argv = argv;
    scheme_init();
    LOAD_MODULE__scheme(global_env);
    LOAD_MODULE__auto_scheme_file(global_env);
    LOAD_MODULE__auto_scheme_port(global_env);
    LOAD_MODULE__auto_scheme_char(global_env);
    LOAD_MODULE__auto_scheme_inexact(global_env);
    LOAD_MODULE__auto_scheme_lazy(global_env);
    LOAD_MODULE__auto_scheme_memory(global_env);
    LOAD_MODULE__auto_scheme_closure(global_env);
    LOAD_MODULE__scheme_cxr(global_env);
    LOAD_MODULE__auto_scheme_macro(global_env);
    LOAD_MODULE__auto_scheme_base(global_env);
    LOAD_MODULE__scheme_process_context(global_env);
    LOAD_MODULE__scheme_read(global_env);
    LOAD_MODULE__auto_scheme_environment(global_env);
    LOAD_MODULE__scheme_eval(global_env);
    LOAD_MODULE__scheme_repl(global_env);
    LOAD_MODULE__scheme_load(global_env);
    LOAD_MODULE__auto_scheme_list(global_env);
    LOAD_MODULE__auto_scheme_write(global_env);
    LOAD_MODULE__auto_scheme_string(global_env);
    LOAD_MODULE__auto_scheme_args_fold(global_env);
    LOAD_MODULE__auto_scheme_args(global_env);
    LOAD_MODULE__auto_scheme_directory(global_env);
    LOAD_MODULE__auto_scheme_path(global_env);
    LOAD_MODULE__auto_scheme_compile(global_env);
    LOAD_MODULE__auto_scheme_interpret(global_env);
    LOAD_MODULE__program(global_env);
    scheme_deinit();
    return 0;
}
pointer LOAD_MODULE__program(pointer environment)
{
pointer return_value = T;
autoscheme_eval(cons(mk_symbol("import"),cons(cons(mk_symbol("auto"),cons(mk_symbol("scheme"),cons(mk_symbol("base"),NIL))),cons(cons(mk_symbol("auto"),cons(mk_symbol("scheme"),cons(mk_symbol("write"),NIL))),cons(cons(mk_symbol("auto"),cons(mk_symbol("scheme"),cons(mk_symbol("args"),cons(mk_symbol("fold"),NIL)))),cons(cons(mk_symbol("auto"),cons(mk_symbol("scheme"),cons(mk_symbol("args"),NIL))),cons(cons(mk_symbol("scheme"),cons(mk_symbol("cxr"),NIL)),cons(cons(mk_symbol("scheme"),cons(mk_symbol("process-context"),NIL)),cons(cons(mk_symbol("auto"),cons(mk_symbol("scheme"),cons(mk_symbol("string"),NIL))),cons(cons(mk_symbol("scheme"),cons(mk_symbol("read"),NIL)),cons(cons(mk_symbol("auto"),cons(mk_symbol("scheme"),cons(mk_symbol("file"),NIL))),cons(cons(mk_symbol("auto"),cons(mk_symbol("scheme"),cons(mk_symbol("compile"),NIL))),cons(cons(mk_symbol("auto"),cons(mk_symbol("scheme"),cons(mk_symbol("interpret"),NIL))),NIL)))))))))))), environment);
autoscheme_eval(cons(mk_symbol("define"),cons(mk_symbol("program-version"),cons(cons(mk_syntax(LOC_BEGIN,"begin"),cons(mk_string("0.198.0 (rev 1636760245)"),NIL)),NIL))), environment);
autoscheme_eval(cons(mk_symbol("define"),cons(mk_symbol("display-version"),cons(cons(mk_symbol("lambda"),cons(NIL,cons(cons(mk_symbol("display"),cons(cons(mk_symbol("string-append"),cons(mk_string("AutoScheme version "),cons(mk_symbol("program-version"),NIL))),NIL)),cons(cons(mk_symbol("newline"),NIL),NIL)))),NIL))), environment);
autoscheme_eval(cons(mk_symbol("define"),cons(mk_symbol("recognized-processor"),cons(cons(mk_symbol("lambda"),cons(cons(mk_symbol("option"),cons(mk_symbol("name"),cons(mk_symbol("arg"),mk_symbol("seeds")))),cons(cons(mk_symbol("let"),cons(cons(cons(mk_symbol("options"),cons(cons(mk_symbol("car"),cons(mk_symbol("seeds"),NIL)),NIL)),cons(cons(mk_symbol("source-files"),cons(cons(mk_symbol("cadr"),cons(mk_symbol("seeds"),NIL)),NIL)),NIL)),cons(cons(mk_symbol("values"),cons(cons(mk_symbol("cons"),cons(cons(mk_symbol("list"),cons(mk_symbol("option"),cons(mk_symbol("name"),cons(mk_symbol("arg"),NIL)))),cons(mk_symbol("options"),NIL))),cons(mk_symbol("source-files"),NIL))),NIL))),NIL))),NIL))), environment);
autoscheme_eval(cons(mk_symbol("define"),cons(mk_symbol("repl-processor"),cons(cons(mk_symbol("lambda"),cons(mk_symbol("args"),cons(cons(mk_symbol("repl"),cons(cons(mk_symbol("interaction-environment"),NIL),NIL)),cons(cons(mk_symbol("exit"),NIL),NIL)))),NIL))), environment);
autoscheme_eval(cons(mk_symbol("define"),cons(mk_symbol("version-processor"),cons(cons(mk_symbol("lambda"),cons(mk_symbol("args"),cons(cons(mk_symbol("display-version"),NIL),cons(cons(mk_symbol("exit"),NIL),NIL)))),NIL))), environment);
autoscheme_eval(cons(mk_symbol("define"),cons(mk_symbol("help-processor"),cons(cons(mk_symbol("lambda"),cons(mk_symbol("args"),cons(cons(mk_symbol("display-version"),NIL),cons(cons(mk_symbol("args-usage"),cons(mk_symbol("option-table"),NIL)),cons(cons(mk_symbol("exit"),NIL),NIL))))),NIL))), environment);
autoscheme_eval(cons(mk_symbol("define"),cons(mk_symbol("unrecognized-processor"),cons(cons(mk_symbol("lambda"),cons(cons(mk_symbol("option"),cons(mk_symbol("name"),cons(mk_symbol("arg"),mk_symbol("seeds")))),cons(cons(mk_symbol("let"),cons(cons(cons(mk_symbol("name-string"),cons(cons(mk_symbol("if"),cons(cons(mk_symbol("char?"),cons(mk_symbol("name"),NIL)),cons(cons(mk_symbol("string-append"),cons(mk_string("-"),cons(cons(mk_symbol("string"),cons(mk_symbol("name"),NIL)),NIL))),cons(cons(mk_symbol("string-append"),cons(mk_string("--"),cons(mk_symbol("name"),NIL))),NIL)))),NIL)),NIL),cons(cons(mk_symbol("display"),cons(cons(mk_symbol("string-append"),cons(mk_string("autoscheme: unrecognized option "),cons(mk_symbol("name-string"),NIL))),NIL)),cons(cons(mk_symbol("newline"),NIL),cons(cons(mk_symbol("args-usage"),cons(mk_symbol("option-table"),NIL)),cons(cons(mk_symbol("exit"),cons(mk_integer(1),NIL)),NIL)))))),NIL))),NIL))), environment);
autoscheme_eval(cons(mk_symbol("define"),cons(mk_symbol("operand-processor"),cons(cons(mk_symbol("lambda"),cons(cons(mk_symbol("operand"),mk_symbol("seeds")),cons(cons(mk_symbol("let"),cons(cons(cons(mk_symbol("options"),cons(cons(mk_symbol("car"),cons(mk_symbol("seeds"),NIL)),NIL)),cons(cons(mk_symbol("source-files"),cons(cons(mk_symbol("cadr"),cons(mk_symbol("seeds"),NIL)),NIL)),NIL)),cons(cons(mk_symbol("values"),cons(mk_symbol("options"),cons(cons(mk_symbol("cons"),cons(mk_symbol("operand"),cons(mk_symbol("source-files"),NIL))),NIL))),NIL))),NIL))),NIL))), environment);
autoscheme_eval(cons(mk_symbol("define"),cons(mk_symbol("option-table"),cons(cons(mk_symbol("quasiquote"),cons(cons(cons(cons(mk_symbol("unquote"),cons(cons(mk_symbol("option"),cons(cons(mk_symbol("quote"),cons(cons(mk_character(105),cons(mk_string("interpret"),NIL)),NIL)),cons(F,cons(F,cons(mk_symbol("recognized-processor"),NIL))))),NIL)),cons(mk_string("Interpret sources"),NIL)),cons(cons(cons(mk_symbol("unquote"),cons(cons(mk_symbol("option"),cons(cons(mk_symbol("quote"),cons(cons(mk_character(99),cons(mk_string("compile"),NIL)),NIL)),cons(F,cons(F,cons(mk_symbol("recognized-processor"),NIL))))),NIL)),cons(mk_string("Compile sources"),NIL)),cons(cons(cons(mk_symbol("unquote"),cons(cons(mk_symbol("option"),cons(cons(mk_symbol("quote"),cons(cons(mk_character(108),cons(mk_string("load-modules"),NIL)),NIL)),cons(T,cons(F,cons(mk_symbol("recognized-processor"),NIL))))),NIL)),cons(mk_string("Load modules"),NIL)),cons(cons(cons(mk_symbol("unquote"),cons(cons(mk_symbol("option"),cons(cons(mk_symbol("quote"),cons(cons(mk_character(109),cons(mk_string("compile-module"),NIL)),NIL)),cons(F,cons(F,cons(mk_symbol("recognized-processor"),NIL))))),NIL)),cons(mk_string("Compile module"),NIL)),cons(cons(cons(mk_symbol("unquote"),cons(cons(mk_symbol("option"),cons(cons(mk_symbol("quote"),cons(cons(mk_character(110),cons(mk_string("module-name"),NIL)),NIL)),cons(T,cons(F,cons(mk_symbol("recognized-processor"),NIL))))),NIL)),cons(mk_string("Specify compiled module name"),NIL)),cons(cons(cons(mk_symbol("unquote"),cons(cons(mk_symbol("option"),cons(cons(mk_symbol("quote"),cons(cons(mk_character(111),cons(mk_string("output-file"),NIL)),NIL)),cons(T,cons(F,cons(mk_symbol("recognized-processor"),NIL))))),NIL)),cons(mk_string("Specify output file"),NIL)),cons(cons(cons(mk_symbol("unquote"),cons(cons(mk_symbol("option"),cons(cons(mk_symbol("quote"),cons(cons(mk_character(114),cons(mk_string("repl"),NIL)),NIL)),cons(F,cons(F,cons(mk_symbol("repl-processor"),NIL))))),NIL)),cons(mk_string("Enter interactive REPL"),NIL)),cons(cons(cons(mk_symbol("unquote"),cons(cons(mk_symbol("option"),cons(cons(mk_symbol("quote"),cons(cons(mk_character(115),cons(mk_string("shell"),NIL)),NIL)),cons(F,cons(F,cons(mk_symbol("version-processor"),NIL))))),NIL)),cons(mk_string("Enter command line shell"),NIL)),cons(cons(cons(mk_symbol("unquote"),cons(cons(mk_symbol("option"),cons(cons(mk_symbol("quote"),cons(cons(mk_character(86),cons(mk_string("version"),NIL)),NIL)),cons(F,cons(F,cons(mk_symbol("version-processor"),NIL))))),NIL)),cons(mk_string("Display version information"),NIL)),cons(cons(cons(mk_symbol("unquote"),cons(cons(mk_symbol("option"),cons(cons(mk_symbol("quote"),cons(cons(mk_character(104),cons(mk_string("help"),NIL)),NIL)),cons(F,cons(F,cons(mk_symbol("help-processor"),NIL))))),NIL)),cons(mk_string("Show this message"),NIL)),NIL)))))))))),NIL)),NIL))), environment);
return_value = autoscheme_eval(cons(mk_symbol("let*"),cons(cons(cons(mk_symbol("seeds"),cons(cons(mk_symbol("list"),cons(cons(mk_symbol("quote"),cons(NIL,NIL)),cons(cons(mk_symbol("quote"),cons(NIL,NIL)),NIL))),NIL)),cons(cons(mk_symbol("result"),cons(cons(mk_symbol("call-with-values"),cons(cons(mk_symbol("lambda"),cons(NIL,cons(cons(mk_symbol("apply"),cons(mk_symbol("args-fold"),cons(cons(mk_symbol("append"),cons(cons(mk_symbol("list"),cons(cons(mk_symbol("cdr"),cons(cons(mk_symbol("command-line"),NIL),NIL)),cons(cons(mk_symbol("map"),cons(mk_symbol("car"),cons(mk_symbol("option-table"),NIL))),cons(mk_symbol("unrecognized-processor"),cons(mk_symbol("operand-processor"),NIL))))),cons(mk_symbol("seeds"),NIL))),NIL))),NIL))),cons(mk_symbol("list"),NIL))),NIL)),cons(cons(mk_symbol("options"),cons(cons(mk_symbol("car"),cons(mk_symbol("result"),NIL)),NIL)),cons(cons(mk_symbol("source-files"),cons(cons(mk_symbol("reverse"),cons(cons(mk_symbol("cadr"),cons(mk_symbol("result"),NIL)),NIL)),NIL)),cons(cons(mk_symbol("option-selected?"),cons(cons(mk_symbol("lambda"),cons(cons(mk_symbol("name"),cons(mk_symbol("selected-options"),NIL)),cons(cons(mk_symbol("call/cc"),cons(cons(mk_symbol("lambda"),cons(cons(mk_symbol("return"),NIL),cons(cons(mk_symbol("for-each"),cons(cons(mk_symbol("lambda"),cons(cons(mk_symbol("selected-option"),NIL),cons(cons(mk_symbol("if"),cons(cons(mk_symbol("member"),cons(mk_symbol("name"),cons(cons(mk_symbol("option-names"),cons(cons(mk_symbol("car"),cons(mk_symbol("selected-option"),NIL)),NIL)),NIL))),cons(cons(mk_symbol("return"),cons(mk_symbol("selected-option"),NIL)),NIL))),NIL))),cons(mk_symbol("selected-options"),NIL))),cons(F,NIL)))),NIL)),NIL))),NIL)),cons(cons(mk_symbol("get-selected-arg"),cons(cons(mk_symbol("lambda"),cons(cons(mk_symbol("name"),NIL),cons(cons(mk_symbol("let"),cons(cons(cons(mk_symbol("selected-option"),cons(cons(mk_symbol("option-selected?"),cons(mk_symbol("name"),cons(mk_symbol("options"),NIL))),NIL)),NIL),cons(cons(mk_symbol("if"),cons(mk_symbol("selected-option"),cons(cons(mk_symbol("caddr"),cons(mk_symbol("selected-option"),NIL)),cons(F,NIL)))),NIL))),NIL))),NIL)),cons(cons(mk_symbol("compile-selected"),cons(cons(mk_symbol("option-selected?"),cons(mk_string("compile"),cons(mk_symbol("options"),NIL))),NIL)),cons(cons(mk_symbol("output-file"),cons(cons(mk_symbol("get-selected-arg"),cons(mk_string("output-file"),NIL)),NIL)),cons(cons(mk_symbol("compile-module-selected"),cons(cons(mk_symbol("option-selected?"),cons(mk_string("compile-module"),cons(mk_symbol("options"),NIL))),NIL)),cons(cons(mk_symbol("module-name"),cons(cons(mk_symbol("get-selected-arg"),cons(mk_string("module-name"),NIL)),NIL)),cons(cons(mk_symbol("load-modules"),cons(cons(mk_symbol("get-selected-arg"),cons(mk_string("load-modules"),NIL)),NIL)),cons(cons(mk_symbol("interpret-selected"),cons(cons(mk_symbol("option-selected?"),cons(mk_string("interpret"),cons(mk_symbol("options"),NIL))),NIL)),cons(cons(mk_symbol("module-list"),cons(cons(mk_symbol("if"),cons(mk_symbol("load-modules"),cons(cons(mk_symbol("apply"),cons(mk_symbol("append"),cons(cons(mk_symbol("map"),cons(cons(mk_symbol("lambda"),cons(cons(mk_symbol("token"),NIL),cons(cons(mk_symbol("if"),cons(cons(mk_symbol("="),cons(cons(mk_symbol("string-length"),cons(mk_symbol("token"),NIL)),cons(mk_integer(0),NIL))),cons(cons(mk_symbol("quote"),cons(NIL,NIL)),cons(cons(mk_symbol("list"),cons(mk_symbol("token"),NIL)),NIL)))),NIL))),cons(cons(mk_symbol("string-tokenize"),cons(mk_symbol("load-modules"),NIL)),NIL))),NIL))),cons(cons(mk_symbol("quote"),cons(NIL,NIL)),NIL)))),NIL)),NIL))))))))))))),cons(cons(mk_symbol("cond"),cons(cons(mk_symbol("compile-selected"),cons(cons(mk_symbol("compile-program"),cons(mk_symbol("source-files"),cons(mk_symbol("module-list"),cons(mk_symbol("output-file"),NIL)))),NIL)),cons(cons(mk_symbol("compile-module-selected"),cons(cons(mk_symbol("compile-module"),cons(mk_symbol("source-files"),cons(mk_symbol("module-name"),cons(mk_symbol("output-file"),NIL)))),NIL)),cons(cons(mk_symbol("interpret-selected"),cons(cons(mk_symbol("apply"),cons(mk_symbol("interpret"),cons(mk_symbol("source-files"),NIL))),NIL)),cons(cons(mk_symbol("else"),cons(cons(mk_symbol("help-processor"),NIL),NIL)),NIL))))),NIL))), environment);
return return_value;
}
