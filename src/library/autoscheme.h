/*  This file is part of the 'AutoScheme' project.
 *  Copyright 2021 Steven Wiley <s.wiley@katchitek.com> 
 *  SPDX-License-Identifier: BSD-2-Clause
 */
/*  This version has been modified by Tatsuya WATANABE.
 *	current version is 0.85w12 (2020)
 *
 *  This software is released under the MIT License.
 *
 *  Copyright (c) 2015 Tatsuya Watanabe
 *  
 *  Permission is hereby granted, free of charge, to any person obtaining
 *  a copy of this software and associated documentation files (the
 *  "Software"), to deal in the Software without restriction, including
 *  without limitation the rights to use, copy, modify, merge, publish,
 *  distribute, sublicense, and/or sell copies of the Software, and to
 *  permit persons to whom the Software is furnished to do so, subject to
 *  the following conditions:
 *  
 *  The above copyright notice and this permission notice shall be
 *  included in all copies or substantial portions of the Software.
 *  
 *  THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
 *  EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
 *  MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
 *  NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE
 *  LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION
 *  OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION
 *  WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
 */
/*
 *      ---------- Mini-Scheme Interpreter Version 0.85 ----------
 *
 *                coded by Atsushi Moriwaki (11/5/1989)
 *
 *            E-MAIL :  moriwaki@kurims.kurims.kyoto-u.ac.jp
 *
 *               THIS SOFTWARE IS IN THE PUBLIC DOMAIN
 *               ------------------------------------
 * This software is completely free to copy, modify and/or re-distribute.
 * But I would appreciate it if you left my name on the code as the author.
 *
 */
/*--
 *
 *  This version has been modified by R.C. Secrist.
 *
 *  Mini-Scheme is now maintained by Akira KIDA.
 *
 *  This is a revised and modified version by Akira KIDA.
 *	current version is 0.85k4 (15 May 1994)
 *--
 */

#ifndef AUTOSCHEME_H
#define AUTOSCHEME_H

#include <stdio.h>
#include <inttypes.h>
#include <setjmp.h>

#ifdef __cplusplus
extern "C" {
#endif



/*
 * Define or undefine following symbols as you need.
 */

/* #define USE_SCHEME_STACK */	/* define this if you want original-Stack */
#define USE_COPYING_GC	/* undef this if you do not want to use Copying GC */

/*
 *  Basic memory allocation units
 */
#define CELL_SEGSIZE 500000	/* # of cells in one segment */
#define MAXFIL 64	/* stack size of loading files */
#define CALL_HISTORY_LENGTH  15   /* length of call history */

typedef struct cell *pointer;
typedef pointer (*foreign_func)(pointer);

/* cell structure */
struct cell {
	unsigned short _flag;
	unsigned char  _extflag;
	unsigned char  _isfixnum;
	union {
		struct {
			char   *_svalue;
			size_t  _length;
		} _string;
		union {
			struct {
				int32_t _ivalue;
				struct cell *_bignum;
			} _integer;
			double  _rvalue;
		} _number;
		struct {
			FILE   *_file;
			char   *_curr;
		} _port;
		foreign_func _ff;
		struct {
			struct cell *_car;
			struct cell *_cdr;
		} _cons;
#ifdef USE_COPYING_GC
		struct cell *_forwarded;
#endif
	} _object;
};

#define T_STRING         1	/* 0000000000000001 */
#define T_NUMBER         2	/* 0000000000000010 */
#define T_SYMBOL         4	/* 0000000000000100 */
#define T_SYNTAX         8	/* 0000000000001000 */
#define T_PROC          16	/* 0000000000010000 */
#define T_PAIR          32	/* 0000000000100000 */
#define T_CLOSURE       64	/* 0000000001000000 */
#define T_CONTINUATION 128	/* 0000000010000000 */
#define T_CHARACTER    256	/* 0000000100000000 */
#define T_PORT         512	/* 0000001000000000 */
#define T_VECTOR      1024	/* 0000010000000000 */
#define T_FOREIGN     2048	/* 0000100000000000 */
#define T_MEMBLOCK    4096	/* 0001000000000000 */
#define T_ENVIRONMENT 8192	/* 0010000000000000 */
#define T_ATOM       16384	/* 0100000000000000 */	/* only for gc */
#define CLRATOM      49151	/* 1011111111111111 */	/* only for gc */
#define MARK         32768	/* 1000000000000000 */
#define UNMARK       32767	/* 0111111111111111 */
#ifdef USE_COPYING_GC
# define T_FORWARDED 32768	/* 1000000000000000 */	/* only for gc */
#endif

#define T_VALUES         1	/* 0000000000000001 */	/* for call-with-values */

#define T_PROMISE        1	/* 00000001 */
#define T_RESULTREADY    2	/* 00000010 */
#define T_MACRO          4	/* 00000100 */
#define T_DEFMACRO       8	/* 00001000 */	/* for define-macro */
#define T_DEFSYNTAX  32768	/* 1000000000000000 */	/* for define-syntax */
#define T_SYNTAXNUM  32767	/* 0111111111111111 */	/* for define-syntax */

/* macros for cell operations */
#define type(p)         ((p)->_flag)
#define exttype(p)      ((p)->_extflag)

#define is_null(p)      (p == NIL)
#define is_string(p)    (type(p)&T_STRING)
#define strvalue(p)     ((p)->_object._string._svalue)
#define strlength(p)    ((p)->_object._string._length)

#define is_number(p)    (type(p)&T_NUMBER)
#define ivalue(p)       ((p)->_object._number._integer._ivalue)
#define bignum(p)       ((p)->_object._number._integer._bignum)
#define rvalue(p)       ((p)->_object._number._rvalue)
#define nvalue(p)       ((p)->_isfixnum ? ivalue(p) : rvalue(p))
#define is_integer(p)   (is_number(p) && ((p)->_isfixnum || floor(rvalue(p) + 0.5) == rvalue(p)))
#define set_num_integer(p)   ((p)->_isfixnum = 1)
#define set_num_real(p)      ((p)->_isfixnum = 0)

#define is_pair(p)      (type(p)&T_PAIR)
#define car(p)          ((p)->_object._cons._car)
#define cdr(p)          ((p)->_object._cons._cdr)

#define is_symbol(p)    (type(p)&T_SYMBOL)
#define symname(p)      strvalue(p)

#define is_syntax(p)    (type(p)&T_SYNTAX)
#define is_proc(p)      (type(p)&T_PROC)
#define syntaxnum(p)    (*(short *)&(p)->_extflag)
#define procnum(p)      (int)ivalue(p)

#define is_closure(p)   (type(p)&T_CLOSURE)
#define is_macro(p)     (exttype(p)&T_MACRO)
#define closure_code(p) car(p)
#define closure_env(p)  cdr(p)

#define is_continuation(p) (type(p)&T_CONTINUATION)
#define cont_dump(p)    cdr(p)

#define is_character(p) (type(p)&T_CHARACTER)

enum {
	port_input = 1,
	port_output = 2,
	port_file = 4,
	port_string = 8,
	port_eof = 16
};
#define is_port(p)      (type(p) & T_PORT)
#define is_inport(p)    (is_port(p) && ((p)->_isfixnum & port_input))
#define is_outport(p)   (is_port(p) && ((p)->_isfixnum & port_output))
#define is_fileport(p)  (is_port(p) && ((p)->_isfixnum & port_file))
#define is_strport(p)   (is_port(p) && ((p)->_isfixnum & port_string))
#define is_eofport(p)   (is_port(p) && ((p)->_isfixnum & port_eof))
#define port_file(p)    ((p)->_object._port._file)
#define port_curr(p)    ((p)->_object._port._curr)

#define is_vector(p)    (type(p) & T_VECTOR)

#define is_foreign(p)   (type(p) & T_FOREIGN)
#define foreignfnc(p)   ((p)->_object._ff)

#define is_memblock(p)  (type(p) & T_MEMBLOCK)

#define is_environment(p) (type(p) & T_ENVIRONMENT)
#define setenvironment(p) type(p) |= T_ENVIRONMENT

#define is_promise(p)   (exttype(p) & T_PROMISE)
#define setpromise(p)   exttype(p) |= T_PROMISE
#define is_resultready(p) (exttype(p) & T_RESULTREADY)
#define setresultready(p) exttype(p) |= T_RESULTREADY

#define is_atom(p)      (type(p)&T_ATOM)
#define setatom(p)      type(p) |= T_ATOM
#define clratom(p)      type(p) &= CLRATOM

#define is_mark(p)      (type(p)&MARK)
#define setmark(p)      type(p) |= MARK
#define clrmark(p)      type(p) &= UNMARK

#define caar(p)         car(car(p))
#define cadr(p)         car(cdr(p))
#define cdar(p)         cdr(car(p))
#define cddr(p)         cdr(cdr(p))
#define cadar(p)        car(cdr(car(p)))
#define caddr(p)        car(cdr(cdr(p)))
#define cadaar(p)       car(cdr(car(car(p))))
#define cadddr(p)       car(cdr(cdr(cdr(p))))
#define cddddr(p)       cdr(cdr(cdr(cdr(p))))


/* true or false value macro */
#define istrue(p)       ((p) != F)
#define isfalse(p)      ((p) == F)

extern pointer UNDEF;
extern pointer NIL;
extern pointer T;
extern pointer F;
extern pointer EOF_OBJ;
extern pointer mark_x;
extern pointer mark_y;
extern jmp_buf error_jmp;

extern pointer global_env;

/* operator code */
 enum eval_op{ OP_T0LVL = 0, OP_T1LVL, OP_READ_INTERNAL, OP_VALUEPRINT, OP_LOAD, OP_EVAL, OP_E0ARGS, OP_E1ARGS, OP_APPLY, OP_APPLYCONT, OP_DOMACRO, OP_GENSYM, OP_LAMBDA, OP_MKCLOSURE, OP_QUOTE, OP_QQUOTE0, OP_QQUOTE1, OP_QQUOTE2, OP_QQUOTE3, OP_QQUOTE4, OP_QQUOTE5, OP_QQUOTE6, OP_QQUOTE7, OP_QQUOTE8, OP_QQUOTE9, OP_DEF0, OP_DEF1, OP_DEFP, OP_BEGIN, OP_IF0, OP_IF1, OP_SET0, OP_SET1, OP_LET0, OP_LET1, OP_LET0AST, OP_LET1AST, OP_LET2AST, OP_LET0REC, OP_LET1REC, OP_LETRECAST0, OP_LETRECAST1, OP_DO0, OP_DO1, OP_DO2, OP_DO3, OP_DO4, OP_DO5, OP_COND0, OP_COND1, OP_COND2, OP_ELSE, OP_FEEDTO, OP_DELAY, OP_LAZY, OP_AND0, OP_AND1, OP_OR0, OP_OR1, OP_C0STREAM, OP_C1STREAM, OP_0MACRO, OP_DEFMACRO0, OP_1MACRO, OP_DEFMACRO1, OP_DEFSYNTAX0, OP_DEFSYNTAX1, OP_LETSYNTAX0, OP_LETSYNTAX1, OP_LETRECSYNTAX0, OP_LETRECSYNTAX1, OP_SYNTAXRULES, OP_EXPANDPATTERN, OP_CASE0, OP_CASE1, OP_CASE2, OP_WHEN0, OP_WHEN1, OP_UNLESS0, OP_UNLESS1, OP_RECEIVE0, OP_RECEIVE1, OP_PEVAL, OP_PAPPLY, OP_MAP0, OP_MAP1, OP_FOREACH0, OP_FOREACH1, OP_CONTINUATION, OP_VALUES, OP_WITHVALUES0, OP_WITHVALUES1, OP_DYNAMICWIND0, OP_DYNAMICWIND1, OP_DYNAMICWIND2, OP_DYNAMICWIND3, OP_DOWINDS0, OP_DOWINDS1, OP_DOWINDS2, OP_DOWINDS3, OP_DOWINDS4, OP_ADD, OP_SUB, OP_MUL, OP_DIV, OP_ABS, OP_QUO, OP_REM, OP_MOD, OP_GCD, OP_LCM, OP_FLOOR, OP_CEILING, OP_TRUNCATE, OP_ROUND, OP_EXP, OP_LOG, OP_SIN, OP_COS, OP_TAN, OP_ASIN, OP_ACOS, OP_ATAN, OP_SQRT, OP_EXPT, OP_EX2INEX, OP_INEX2EX, OP_NUM2STR, OP_STR2NUM, OP_CAR, OP_CDR, OP_CONS, OP_SETCAR, OP_SETCDR, OP_CAAR, OP_CADR, OP_CDAR, OP_CDDR, OP_CAAAR, OP_CAADR, OP_CADAR, OP_CADDR, OP_CDAAR, OP_CDADR, OP_CDDAR, OP_CDDDR, OP_CAAAAR, OP_CAAADR, OP_CAADAR, OP_CAADDR, OP_CADAAR, OP_CADADR, OP_CADDAR, OP_CADDDR, OP_CDAAAR, OP_CDAADR, OP_CDADAR, OP_CDADDR, OP_CDDAAR, OP_CDDADR, OP_CDDDAR, OP_CDDDDR, OP_LIST, OP_LISTTAIL, OP_LISTREF, OP_LASTPAIR, OP_CHAR2INT, OP_INT2CHAR, OP_CHARUPCASE, OP_CHARDNCASE, OP_MKSTRING, OP_STRING, OP_STRLEN, OP_STRREF, OP_STRSET, OP_STREQU, OP_STRLSS, OP_STRGTR, OP_STRLEQ, OP_STRGEQ, OP_STRCIEQU, OP_STRCILSS, OP_STRCIGTR, OP_STRCILEQ, OP_STRCIGEQ, OP_SUBSTR, OP_STRAPPEND, OP_STR2LIST, OP_LIST2STR, OP_STRCOPY, OP_STRFILL, OP_VECTOR, OP_MKVECTOR, OP_VECLEN, OP_VECREF, OP_VECSET, OP_VEC2LIST, OP_LIST2VEC, OP_VECFILL, OP_NOT, OP_BOOL, OP_NULL, OP_EOFOBJP, OP_ZEROP, OP_POSP, OP_NEGP, OP_ODD, OP_EVEN, OP_NEQ, OP_LESS, OP_GRE, OP_LEQ, OP_GEQ, OP_MAX, OP_MIN, OP_SYMBOL, OP_SYM2STR, OP_STR2SYM, OP_NUMBER, OP_STRINGP, OP_INTEGER, OP_REAL, OP_EXACT, OP_INEXACT, OP_CHAR, OP_CHAREQU, OP_CHARLSS, OP_CHARGTR, OP_CHARLEQ, OP_CHARGEQ, OP_CHARCIEQU, OP_CHARCILSS, OP_CHARCIGTR, OP_CHARCILEQ, OP_CHARCIGEQ, OP_CHARAP, OP_CHARNP, OP_CHARWP, OP_CHARUP, OP_CHARLP, OP_PROC, OP_PAIR, OP_LISTP, OP_PORTP, OP_INPORTP, OP_OUTPORTP, OP_VECTORP, OP_ENVP, OP_EQ, OP_EQV, OP_EQUAL, OP_EAGER, OP_FORCE, OP_FORCED, OP_WRITE_CHAR, OP_WRITE, OP_DISPLAY, OP_NEWLINE, OP_ERR0, OP_ERR1, OP_REVERSE, OP_APPEND, OP_PUT, OP_GET, OP_QUIT, OP_EMERGENCY_EXIT, OP_GC, OP_GCVERB, OP_CALL_INFILE0, OP_CALL_INFILE1, OP_CALL_OUTFILE0, OP_CALL_OUTFILE1, OP_CURR_INPORT, OP_CURR_OUTPORT, OP_WITH_INFILE0, OP_WITH_INFILE1, OP_WITH_OUTFILE0, OP_WITH_OUTFILE1, OP_OPEN_INFILE, OP_OPEN_OUTFILE, OP_OPEN_INOUTFILE, OP_OPEN_INSTRING, OP_OPEN_OUTSTRING, OP_OPEN_INOUTSTRING, OP_GET_OUTSTRING, OP_CLOSE_INPORT, OP_CLOSE_OUTPORT, OP_CLOSE_PORT, OP_INT_ENV, OP_CURR_ENV, OP_CALL_ENV, OP_GLOB_ENV, OP_READ, OP_READ_CHAR, OP_PEEK_CHAR, OP_CHAR_READY, OP_SET_INPORT, OP_SET_OUTPORT, OP_RDSEXPR, OP_RDLIST, OP_RDDOT, OP_RDQUOTE, OP_RDQQUOTE, OP_RDQQUOTEVEC, OP_RDUNQUOTE, OP_RDUQTSP, OP_RDVEC, OP_P0LIST, OP_P1LIST, OP_PVECFROM, OP_P0HIST, OP_P1HIST, OP_P2HIST, OP_LIST_LENGTH, OP_MEMQ, OP_MEMV, OP_MEMBER, OP_ASSQ, OP_ASSV, OP_ASSOC, OP_GET_CLOSURE, OP_CLOSUREP, OP_MACROP, OP_MACRO_EXPAND0, OP_MACRO_EXPAND1, OP_MACRO_EXPAND2, OP_ATOMP };

pointer cons(pointer a, pointer b);

pointer mk_foreign_func(foreign_func ff, pointer *pp);
pointer mk_proc(enum eval_op operator, pointer *pp);
pointer mk_syntax( enum eval_op operator, char *name );

pointer mk_character(int c);
pointer mk_integer(int32_t num);
pointer mk_real(double num);
pointer mk_number(pointer v);
pointer mk_string(const char *str);
pointer mk_counted_string(const char *str, size_t len);
pointer mk_empty_string(size_t len, int fill);
pointer mk_symbol(const char *name);
pointer mk_uninterned_symbol(const char *name);
pointer gensym(void);
pointer mk_atom(const char *q);
pointer mk_const(const char *name);
pointer mk_port(FILE *fp, int prop);
pointer mk_port_string(pointer p, int prop);
pointer mk_vector(int len);
pointer vector_elem(pointer v, int i);
pointer set_vector_elem(pointer v, int i, pointer a);
int list_length(pointer a);

int eqv(pointer a, pointer b);
int equal(pointer a, pointer b);
pointer reverse(pointer a);


int member( pointer object, pointer list );
pointer assoc( pointer object, pointer alist );
pointer make_environment( pointer alist );
pointer append(pointer a, pointer b);

void FatalForeignError( char *s );

void scheme_init(void);
void scheme_deinit(void);
int scheme_load_file(FILE *fin);
int scheme_load_string(const char *cmd);


void scheme_register_foreign_func( const char *name, foreign_func ff, pointer environment );
void scheme_register_proc(enum eval_op operator, char *name, pointer environment);
void scheme_register_syntax( enum eval_op operator, char *name, pointer environment );

pointer scheme_apply0(const char *procname);
pointer scheme_apply1(const char *procname, pointer argslist);
pointer scheme_eval(pointer obj);

pointer autoscheme_eval( pointer object, pointer environment );

pointer scheme_call(pointer func, pointer argslist);




#ifdef __cplusplus
}
#endif

#endif /* AUTOSCHEME_H */


