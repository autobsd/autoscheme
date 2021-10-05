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
