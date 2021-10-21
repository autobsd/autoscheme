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
#define T_OPERATION     16	/* 0000000000010000 */
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
#define is_operation(p) (type(p)&T_OPERATION)
#define syntaxnum(p)    (*(short *)&(p)->_extflag)
#define op_loc(p)      (int)ivalue(p)

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


/* code location */
    enum eval_location{ LOC_T0LVL = 0, LOC_T1LVL, LOC_READ_INTERNAL, LOC_VALUEPRINT, LOC_EVAL, LOC_E0ARGS, LOC_E1ARGS, LOC_APPLY, LOC_APPLYCONT, LOC_DOMACRO, LOC_GENSYM, LOC_LAMBDA, LOC_MKCLOSURE, LOC_QUOTE, LOC_QQUOTE0, LOC_QQUOTE1, LOC_QQUOTE2, LOC_QQUOTE3, LOC_QQUOTE4, LOC_QQUOTE5, LOC_QQUOTE6, LOC_QQUOTE7, LOC_QQUOTE8, LOC_QQUOTE9, LOC_DEF0, LOC_DEF1, LOC_DEFP, LOC_BEGIN, LOC_IF0, LOC_IF1, LOC_SET0, LOC_SET1, LOC_LET0, LOC_LET1, LOC_LET0AST, LOC_LET1AST, LOC_LET2AST, LOC_LET0REC, LOC_LET1REC, LOC_LETRECAST0, LOC_LETRECAST1, LOC_DO0, LOC_DO1, LOC_DO2, LOC_DO3, LOC_DO4, LOC_DO5, LOC_COND0, LOC_COND1, LOC_COND2, LOC_ELSE, LOC_FEEDTO, LOC_DELAY, LOC_LAZY, LOC_AND0, LOC_AND1, LOC_OR0, LOC_OR1, LOC_C0STREAM, LOC_C1STREAM, LOC_DEFMACRO0, LOC_MACRO, LOC_DEFMACRO1, LOC_DEFSYNTAX0, LOC_DEFSYNTAX1, LOC_LETSYNTAX0, LOC_LETSYNTAX1, LOC_LETRECSYNTAX0, LOC_LETRECSYNTAX1, LOC_SYNTAXRULES, LOC_EXPANDPATTERN, LOC_CASE0, LOC_CASE1, LOC_CASE2, LOC_WHEN0, LOC_WHEN1, LOC_UNLESS0, LOC_UNLESS1, LOC_RECEIVE0, LOC_RECEIVE1, LOC_PEVAL, LOC_PAPPLY, LOC_MAP0, LOC_MAP1, LOC_FOREACH0, LOC_FOREACH1, LOC_CONTINUATION, LOC_VALUES, LOC_WITHVALUES0, LOC_WITHVALUES1, LOC_DYNAMICWIND0, LOC_DYNAMICWIND1, LOC_DYNAMICWIND2, LOC_DYNAMICWIND3, LOC_DOWINDS0, LOC_DOWINDS1, LOC_DOWINDS2, LOC_DOWINDS3, LOC_DOWINDS4, LOC_ADD, LOC_SUB, LOC_MUL, LOC_DIV, LOC_ABS, LOC_QUO, LOC_REM, LOC_MOD, LOC_GCD, LOC_LCM, LOC_FLOOR, LOC_CEILING, LOC_TRUNCATE, LOC_ROUND, LOC_EXP, LOC_LOG, LOC_SIN, LOC_COS, LOC_TAN, LOC_ASIN, LOC_ACOS, LOC_ATAN, LOC_SQRT, LOC_EXPT, LOC_EX2INEX, LOC_INEX2EX, LOC_NUM2STR, LOC_STR2NUM, LOC_CAR, LOC_CDR, LOC_CONS, LOC_SETCAR, LOC_SETCDR, LOC_CAAR, LOC_CADR, LOC_CDAR, LOC_CDDR, LOC_CAAAR, LOC_CAADR, LOC_CADAR, LOC_CADDR, LOC_CDAAR, LOC_CDADR, LOC_CDDAR, LOC_CDDDR, LOC_CAAAAR, LOC_CAAADR, LOC_CAADAR, LOC_CAADDR, LOC_CADAAR, LOC_CADADR, LOC_CADDAR, LOC_CADDDR, LOC_CDAAAR, LOC_CDAADR, LOC_CDADAR, LOC_CDADDR, LOC_CDDAAR, LOC_CDDADR, LOC_CDDDAR, LOC_CDDDDR, LOC_LIST, LOC_LISTTAIL, LOC_LISTREF, LOC_LASTPAIR, LOC_CHAR2INT, LOC_INT2CHAR, LOC_CHARUPCASE, LOC_CHARDNCASE, LOC_MKSTRING, LOC_STRING, LOC_STRLEN, LOC_STRREF, LOC_STRSET, LOC_STREQU, LOC_STRLSS, LOC_STRGTR, LOC_STRLEQ, LOC_STRGEQ, LOC_STRCIEQU, LOC_STRCILSS, LOC_STRCIGTR, LOC_STRCILEQ, LOC_STRCIGEQ, LOC_SUBSTR, LOC_STRAPPEND, LOC_STR2LIST, LOC_LIST2STR, LOC_STRCOPY, LOC_STRFILL, LOC_VECTOR, LOC_MKVECTOR, LOC_VECLEN, LOC_VECREF, LOC_VECSET, LOC_VEC2LIST, LOC_LIST2VEC, LOC_VECFILL, LOC_NOT, LOC_BOOL, LOC_NULL, LOC_EOFOBJP, LOC_ZEROP, LOC_POSP, LOC_NEGP, LOC_ODD, LOC_EVEN, LOC_NEQ, LOC_LESS, LOC_GRE, LOC_LEQ, LOC_GEQ, LOC_MAX, LOC_MIN, LOC_SYMBOL, LOC_SYM2STR, LOC_STR2SYM, LOC_NUMBER, LOC_STRINGP, LOC_INTEGER, LOC_REAL, LOC_EXACT, LOC_INEXACT, LOC_CHAR, LOC_CHAREQU, LOC_CHARLSS, LOC_CHARGTR, LOC_CHARLEQ, LOC_CHARGEQ, LOC_CHARCIEQU, LOC_CHARCILSS, LOC_CHARCIGTR, LOC_CHARCILEQ, LOC_CHARCIGEQ, LOC_CHARAP, LOC_CHARNP, LOC_CHARWP, LOC_CHARUP, LOC_CHARLP, LOC_PROC, LOC_PAIR, LOC_LISTP, LOC_PORTP, LOC_INPORTP, LOC_OUTPORTP, LOC_VECTORP, LOC_ENVP, LOC_EQ, LOC_EQV, LOC_EQUAL, LOC_EAGER, LOC_FORCE, LOC_FORCED, LOC_WRITE_CHAR, LOC_WRITE, LOC_DISPLAY, LOC_NEWLINE, LOC_ERR0, LOC_ERR1, LOC_REVERSE, LOC_APPEND, LOC_PUT, LOC_GET, LOC_QUIT, LOC_EMERGENCY_EXIT, LOC_GC, LOC_GCVERB, LOC_CALL_INFILE0, LOC_CALL_INFILE1, LOC_CALL_OUTFILE0, LOC_CALL_OUTFILE1, LOC_CURR_INPORT, LOC_CURR_OUTPORT, LOC_WITH_INFILE0, LOC_WITH_INFILE1, LOC_WITH_OUTFILE0, LOC_WITH_OUTFILE1, LOC_OPEN_INFILE, LOC_OPEN_OUTFILE, LOC_OPEN_INOUTFILE, LOC_OPEN_INSTRING, LOC_OPEN_OUTSTRING, LOC_OPEN_INOUTSTRING, LOC_GET_OUTSTRING, LOC_CLOSE_INPORT, LOC_CLOSE_OUTPORT, LOC_CLOSE_PORT, LOC_CURR_ENV, LOC_CALL_ENV, LOC_GLOB_ENV, LOC_READ, LOC_READ_CHAR, LOC_PEEK_CHAR, LOC_CHAR_READY, LOC_RDSEXPR, LOC_RDLIST, LOC_RDDOT, LOC_RDQUOTE, LOC_RDQQUOTE, LOC_RDQQUOTEVEC, LOC_RDUNQUOTE, LOC_RDUQTSP, LOC_RDVEC, LOC_P0LIST, LOC_P1LIST, LOC_PVECFROM, LOC_P0HIST, LOC_P1HIST, LOC_P2HIST, LOC_LIST_LENGTH, LOC_MEMQ, LOC_MEMV, LOC_MEMBER, LOC_ASSQ, LOC_ASSV, LOC_ASSOC, LOC_GET_CLOSURE, LOC_CLOSUREP, LOC_MACROP, LOC_MACRO_EXPAND0, LOC_MACRO_EXPAND1, LOC_MACRO_EXPAND2, LOC_ATOMP };

pointer cons(pointer a, pointer b);

pointer mk_foreign_func(foreign_func ff, pointer *pp);
pointer mk_proc(enum eval_location location, pointer *pp);
pointer mk_syntax( enum eval_location location, char *name );

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
void scheme_register_proc(enum eval_location location, char *name, pointer environment);
void scheme_register_syntax( enum eval_location location, char *name, pointer environment );

pointer scheme_apply0(const char *procname);
pointer scheme_apply1(const char *procname, pointer argslist);
pointer scheme_eval(pointer obj);

pointer autoscheme_eval( pointer object, pointer environment );

pointer scheme_call(pointer func, pointer argslist);




#ifdef __cplusplus
}
#endif

#endif /* AUTOSCHEME_H */


