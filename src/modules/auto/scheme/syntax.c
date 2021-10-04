/*  This file is part of the 'AutoScheme' project.
 *  Copyright 2021 Steven Wiley <s.wiley@katchitek.com> 
 *  SPDX-License-Identifier: BSD-2-Clause
 */
	scheme_register_syntax(OP_LAMBDA, "lambda", global_env);
	scheme_register_syntax(OP_QUOTE, "quote", global_env);
	scheme_register_syntax(OP_QQUOTE0, "quasiquote", global_env);
	scheme_register_syntax(OP_DEF0, "define", global_env);
	scheme_register_syntax(OP_IF0, "if", global_env);
	scheme_register_syntax(OP_BEGIN, "begin", global_env);
	scheme_register_syntax(OP_SET0, "set!", global_env);
	scheme_register_syntax(OP_LET0, "let", global_env);
	scheme_register_syntax(OP_LET0AST, "let*", global_env);
	scheme_register_syntax(OP_LET0REC, "letrec", global_env);
	scheme_register_syntax(OP_LETRECAST0, "letrec*", global_env);
	scheme_register_syntax(OP_DO0, "do", global_env);
	scheme_register_syntax(OP_COND0, "cond", global_env);
	scheme_register_syntax(OP_ELSE, "else", global_env);
	scheme_register_syntax(OP_FEEDTO, "=>", global_env);
	scheme_register_syntax(OP_DELAY, "delay", global_env);
	scheme_register_syntax(OP_LAZY, "lazy", global_env);
	scheme_register_syntax(OP_AND0, "and", global_env);
	scheme_register_syntax(OP_OR0, "or", global_env);
	scheme_register_syntax(OP_C0STREAM, "cons-stream", global_env);
	scheme_register_syntax(OP_0MACRO, "macro", global_env);
	scheme_register_syntax(OP_DEFMACRO0, "define-macro", global_env);
	scheme_register_syntax(OP_CASE0, "case", global_env);
	scheme_register_syntax(OP_WHEN0, "when", global_env);
	scheme_register_syntax(OP_UNLESS0, "unless", global_env);
	scheme_register_syntax(OP_SYNTAXRULES, "syntax-rules", global_env);
	scheme_register_syntax(OP_DEFSYNTAX0, "define-syntax", global_env);
	scheme_register_syntax(OP_LETSYNTAX0, "let-syntax", global_env);
	scheme_register_syntax(OP_LETRECSYNTAX0, "letrec-syntax", global_env);
	scheme_register_syntax(OP_RECEIVE0, "receive", global_env);
