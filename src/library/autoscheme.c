/*  This file has been modified as part of the 'AutoScheme' project.
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

#include "autoscheme.h"
#include "bignum.h"
#include "error.h"

/*--
 *  If your machine can't support "forward single quotation character"
 *  i.e., '`',  you may have trouble to use backquote.
 *  So use '^' in place of '`'.
 */
#define BACKQUOTE '`'




#include <ctype.h>

#include <string.h>
#include <stdlib.h>
#include <float.h>
#include <math.h>


#define prompt "> "
#ifdef _WIN32
#define snprintf _snprintf
#define stricmp _stricmp
#endif

/* array for segments */
struct cell cell_seg[CELL_SEGSIZE * 2];

/* We use 4 registers. */
pointer args;			/* register for arguments of procedure */
pointer envir;			/* stack register for current environment */
pointer code;			/* register for current code */
pointer dump;			/* stack register for next evaluation */

struct cell _NIL;
pointer NIL = &_NIL;	/* special cell representing empty cell */
struct cell _T;
pointer T = &_T;		/* special cell representing #t */
struct cell _F;
pointer F = &_F;		/* special cell representing #f */
struct cell _EOF_OBJ;
pointer EOF_OBJ = &_EOF_OBJ;	/* special cell representing end-of-file */
struct cell _UNDEF;
pointer UNDEF = &_UNDEF;	/* special cell representing undefined */
pointer symbol_list = &_NIL;	/* pointer to symbol table */
pointer global_env;		/* pointer to global environment */
pointer call_history;           /* pointer to call history vector */
int call_history_pos;           /* current position within call history vector */
struct cell _ZERO;		/* special cell representing integer 0 */
struct cell _ONE;		/* special cell representing integer 1 */

pointer current_inport = &_NIL;	/* pointer to current-input-port */
pointer current_outport = &_NIL;/* pointer to current-output-port */
pointer current_errport = &_NIL;/* pointer to current-error-port */
pointer current_xhands = &_NIL; /* pointer to current-exception-handlers */
pointer current_source = &_NIL; /* pointer to current-source */

pointer command_line = &_NIL;   /* pointer to command-line */


pointer winders = &_NIL;	/* pointer to winders list */

pointer strbuff = &_NIL;	/* pointer to string buffer */

#ifdef USE_COPYING_GC
pointer gcell_list = &_NIL;	/* pointer to cell table */
#define gcell_next(p) car((p) + 1)
#endif

pointer value;
pointer mark_x;
pointer mark_y;

pointer c_nest;			/* pointer to nested C calls list */
pointer c_sink;			/* pointer to sink arguments list */

/* global pointers to special symbols */
pointer LAMBDA;			/* pointer to syntax lambda */
pointer QUOTE;			/* pointer to syntax quote */

pointer QQUOTE;			/* pointer to symbol quasiquote */
pointer UNQUOTE;		/* pointer to symbol unquote */
pointer UNQUOTESP;		/* pointer to symbol unquote-splicing */

pointer ELLIPSIS;		/* pointer to symbol ... */

pointer free_cell = &_NIL;	/* pointer to top of free cells */
size_t  fcells = 0;			/* # of free cells */

pointer load_stack[MAXFIL];	/* stack of loading files */
int     load_files;			/* # of loading files */

jmp_buf error_jmp;

char    gc_verbose;		/* if gc_verbose is not zero, print gc status */
int     interactive_repl = 0;

void gc(pointer *a, pointer *b);
void FatalError(char *s);

#ifndef USE_SCHEME_STACK
#define dump_prev(p)  car(p)
#define dump_next(p)  cdr(p)
#define dump_op(p)    car((p) + 1)
#define dump_args(p)  cdr((p) + 1)
#define dump_envir(p) car((p) + 2)
#define dump_code(p)  cdr((p) + 2)

pointer dump_base; /* pointer to base of allocated dump stack */
#endif

/* ========== Routines for UTF-8 characters ========== */

unsigned char getc_save[4];	/* getc save buffer */
int getc_save_count = 0;	/* getc save count */

static int internal_fgetc(FILE *fin)
{
	if (fin == stdin && getc_save_count > 0) {
		return getc_save[--getc_save_count];
	}
	return fgetc(fin);
}

static void internal_ungetc(int c, FILE *fin)
{
	if (fin == stdin) {
		if ((unsigned long)getc_save_count < sizeof(getc_save)) {
			getc_save[getc_save_count++] = (unsigned char)c;
		} else {
			ungetc(c, fin);
		}
	} else {
		if (ftell(fin) > 0) {
			fseek(fin, -1, SEEK_CUR);
		} else {
			ungetc(c, fin);
		}
	}
}

/* internal description of "extended" UTF-32
 *
 *  0x00000000 - 0x0000007F (UTF-8  0x00 - 7F)
 *  0x00000080 - 0x000007FF (UTF-8  0xC2 80 - DF BF)
 *  0x00000800 - 0x0000FFFF (UTF-8  0xE0 A0 80 - EF BF BF)
 *  0x00010000 - 0x0010FFFF (UTF-8  0xF0 90 80 80 - F4 8F BF BF)
 *  0xFFFFFF00 - 0xFFFFFF7F (!UTF-8 0x80 - FF)
 *  0xFFFFFFFF              (EOF)
 */
static size_t utf32_to_utf8(const int utf32, char *const utf8)
{
	if (utf32 < 0x00) {
		if (utf8 != NULL) {
			utf8[0] = (char)(-utf32);
		}
		return 1;
	}
	if (utf32 < 0x80) {
		if (utf8 != NULL) {
			utf8[0] = (char)utf32;
		}
		return 1;
	}
	if (utf32 < 0x800) {
		if (utf8 != NULL) {
			utf8[0] = 0xC0 | (char)(utf32 >> 6);
			utf8[1] = 0x80 | (utf32 & 0x3F);
		}
		return 2;
	}
	if (utf32 < 0x10000) {
		if (utf8 != NULL) {
			utf8[0] = 0xE0 | (char)(utf32 >> 12);
			utf8[1] = 0x80 | (utf32 >> 6 & 0x3F);
			utf8[2] = 0x80 | (utf32 & 0x3F);
		}
		return 3;
	}
	if (utf32 < 0x110000) {
		if (utf8 != NULL) {
			utf8[0] = 0xF0 | (char)(utf32 >> 18);
			utf8[1] = 0x80 | (utf32 >> 12 & 0x3F);
			utf8[2] = 0x80 | (utf32 >> 6 & 0x3F);
			utf8[3] = 0x80 | (utf32 & 0x3F);
		}
		return 4;
	}
	return 0;
}

static int utf32_toupper(int c)
{
	return isascii(c) ? toupper(c) : c;	/* only ASCII */
}

static int utf32_tolower(int c)
{
	return isascii(c) ? tolower(c) : c;	/* only ASCII */
}

static int utf32_isalpha(int c)
{
	return isascii(c) && isalpha(c);	/* only ASCII */
}

static int utf32_isdigit(int c)
{
	return isascii(c) && isdigit(c);	/* only ASCII */
}

static int utf32_isspace(int c)
{
	return isascii(c) && isspace(c);	/* only ASCII */
}

static int utf32_isupper(int c)
{
	return isascii(c) && isupper(c);	/* only ASCII */
}

static int utf32_islower(int c)
{
	return isascii(c) && islower(c);	/* only ASCII */
}

static int utf8_fgetc(FILE *fin)
{
	int p[4];

	p[0] = internal_fgetc(fin);
	if (p[0] < 0x80) {
		return p[0];
	} else if (p[0] < 0xC2) {
		return -p[0];
	} else if (p[0] < 0xE0)  {
		p[1] = internal_fgetc(fin);
		if (p[1] < 0x80 || 0xBF < p[1]) {
			internal_ungetc(p[1], fin);
			return -p[0];
		}
		return ((p[0] & 0x1F) << 6) | (p[1] & 0x3F);
	} else if (p[0] < 0xF0) {
		p[1] = internal_fgetc(fin);
		if (p[1] < (p[0] == 0xE0 ? 0xA0 : 0x80) || (p[0] == 0xED ? 0x9F : 0xBF) < p[1]) {
			internal_ungetc(p[1], fin);
			return -p[0];
		}
		p[2] = internal_fgetc(fin);
		if (p[2] < 0x80 || 0xBF < p[2]) {
			internal_ungetc(p[2], fin);
			internal_ungetc(p[1], fin);
			return -p[0];
		}
		return ((p[0] & 0x0F) << 12) | ((p[1] & 0x3F) << 6) | (p[2] & 0x3F);
	} else if (p[0] < 0xF5) {
		p[1] = internal_fgetc(fin);
		if (p[1] < (p[0] == 0xF0 ? 0x90 : 0x80) || (p[0] == 0xF4 ? 0x8F : 0xBF) < p[1]) {
			internal_ungetc(p[1], fin);
			return -p[0];
		}
		p[2] = internal_fgetc(fin);
		if (p[2] < 0x80 || 0xBF < p[2]) {
			internal_ungetc(p[2], fin);
			internal_ungetc(p[1], fin);
			return -p[0];
		}
		p[3] = internal_fgetc(fin);
		if (p[3] < 0x80 || 0xBF < p[3]) {
			internal_ungetc(p[3], fin);
			internal_ungetc(p[2], fin);
			internal_ungetc(p[1], fin);
			return -p[0];
		}
		return ((p[0] & 0x07) << 18) | ((p[1] & 0x3F) << 12) | ((p[2] & 0x3F) << 6) | (p[3] & 0x3F);
	} else {
		return -p[0];
	}
}

static size_t utf8_get_next(const char *utf8, int *utf32)
{
	const unsigned char *p = (unsigned char *)utf8;

	if (p[0] < 0x80) {
		if (utf32 != NULL) {
			*utf32 = p[0];
		}
		return 1;
	} else if (p[0] < 0xC2) {
		if (utf32 != NULL) {
			*utf32 = -p[0];
		}
		return 1;
	} else if (p[0] < 0xE0)  {
		if (p[1] < 0x80 || 0xBF < p[1]) {
			if (utf32 != NULL) {
				*utf32 = -p[0];
			}
			return 1;
		}
		if (utf32 != NULL) {
			*utf32 = ((p[0] & 0x1F) << 6) | (p[1] & 0x3F);
		}
		return 2;
	} else if (p[0] < 0xF0) {
		if (p[1] < (p[0] == 0xE0 ? 0xA0 : 0x80) || (p[0] == 0xED ? 0x9F : 0xBF) < p[1]) {
			if (utf32 != NULL) {
				*utf32 = -p[0];
			}
			return 1;
		}
		if (p[2] < 0x80 || 0xBF < p[2]) {
			if (utf32 != NULL) {
				*utf32 = -p[0];
			}
			return 1;
		}
		if (utf32 != NULL) {
			*utf32 = ((p[0] & 0x0F) << 12) | ((p[1] & 0x3F) << 6) | (p[2] & 0x3F);
		}
		return 3;
	} else if (p[0] < 0xF5) {
		if (p[1] < (p[0] == 0xF0 ? 0x90 : 0x80) || (p[0] == 0xF4 ? 0x8F : 0xBF) < p[1]) {
			if (utf32 != NULL) {
				*utf32 = -p[0];
			}
			return 1;
		}
		if (p[2] < 0x80 || 0xBF < p[2]) {
			if (utf32 != NULL) {
				*utf32 = -p[0];
			}
			return 1;
		}
		if (p[3] < 0x80 || 0xBF < p[3]) {
			if (utf32 != NULL) {
				*utf32 = -p[0];
			}
			return 1;
		}
		if (utf32 != NULL) {
			*utf32 = ((p[0] & 0x07) << 18) | ((p[1] & 0x3F) << 12) | ((p[2] & 0x3F) << 6) | (p[3] & 0x3F);
		}
		return 4;
	} else {
		if (utf32 != NULL) {
			*utf32 = -p[0];
		}
		return 1;
	}
}

static size_t utf8_strlen(const char *s)
{
	size_t count = 0;

	while (*s) {
		s += utf8_get_next(s, NULL);
		count++;
	}
	return count;
}

static int utf8_strref(const char *s, size_t pos)
{
	int c;

	while (*s) {
		s += utf8_get_next(s, &c);
		if (pos-- == 0) return c;
	}
	return -1;
}

static int utf8_strpos(const char *s, size_t pos)
{
	const char *t = s;

	while (*s || pos == 0) {
		if (pos-- == 0) return (int)(s - t);
		s += utf8_get_next(s, NULL);
	}
	return -1;
}

static int utf8_stricmp(const char *s1, const char *s2)
{
	const char *p1 = s1, *p2 = s2;
	int c1, c2;

	do {
		p1 += utf8_get_next(p1, &c1);
		p2 += utf8_get_next(p2, &c2);
		c1 = utf32_tolower(c1);
		c2 = utf32_tolower(c2);
	} while (c1 != 0 && c2 != 0 && c1 == c2);

	return c1 - c2;
}

/* ========== Routines for Cells ========== */

/* allocate new cell segment */
#ifdef USE_COPYING_GC
pointer from_space;
pointer to_space;

static void alloc_cellseg(void)
{
	fcells = CELL_SEGSIZE;
	free_cell = from_space = cell_seg;
	to_space = cell_seg + CELL_SEGSIZE;
}
#else
static void alloc_cellseg(void)
{
	pointer p;
	int i;

	p = free_cell = cell_seg;
	fcells += CELL_SEGSIZE;
	for (i = 0; i < CELL_SEGSIZE - 1; i++, p++) {
		type(p) = 0;
		car(p) = NIL;
		cdr(p) = p + 1;
	}
	type(p) = 0;
	car(p) = NIL;
	cdr(p) = NIL;
}
#endif

/* get new cell.  parameter a, b is marked by gc. */
static pointer get_cell(pointer *a, pointer *b)
{
#ifndef USE_COPYING_GC
	pointer x;
#endif

	if (fcells == 0) {
		gc(a, b);
		if (fcells == 0) {
			FatalError("run out of cells --- unable to recover cells");
		}
	}

#ifdef USE_COPYING_GC
	--fcells;
	return free_cell++;
#else
	x = free_cell;
	free_cell = cdr(x);
	--fcells;
	return x;
#endif
}

#ifdef USE_COPYING_GC
static pointer find_consecutive_cells(size_t n)
{
	if (fcells >= n) {
		pointer p = free_cell;

		free_cell += n;
		fcells -= n;
		return p;
	}
	return NIL;
}
#else
pointer find_consecutive_cells(size_t n)
{
	pointer *pp = &free_cell;

	while (*pp != NIL) {
		pointer p = *pp;
		size_t cnt;
		for (cnt = 1; cnt < n; cnt++) {
			if (cdr(p) != p + 1) {
				break;
			}
			p = cdr(p);
		}
		if (cnt >= n) {
			pointer x = *pp;
			*pp = cdr(*pp + n - 1);
			fcells -= n;
			return x;
		}
		pp = &cdr(*pp + cnt - 1);
	}
	return NIL;
}
#endif

static pointer get_consecutive_cells(size_t n, pointer *a, pointer *b)
{
	pointer x;

	x = find_consecutive_cells(n);
	if (x == NIL) {
		gc(a, b);
		x = find_consecutive_cells(n);
		if (x == NIL) {
			FatalError("run out of cells  --- unable to recover consecutive cells");
		}
	}
	return x;
}

static void push_recent_alloc(pointer recent)
{
	pointer holder = get_cell(&recent, &NIL);

	type(holder) = T_PAIR;
	car(holder) = recent;
	cdr(holder) = c_sink;
	c_sink = holder;
}

/* get new cons cell */
pointer cons(pointer a, pointer b)
{
	pointer x = get_cell(&a, &b);

	type(x) = T_PAIR;
	exttype(x) = 0;
	car(x) = a;
	cdr(x) = b;
	return x;
}

pointer mk_character(int c)
{
	pointer x = get_cell(&NIL, &NIL);

	type(x) = (T_CHARACTER | T_ATOM);
	exttype(x) = 0;
	ivalue(x) = c;
	set_num_integer(x);
	return x;
}

pointer mk_integer(int32_t num)
{
	pointer x = get_cell(&NIL, &NIL);

	type(x) = (T_NUMBER | T_ATOM);
	exttype(x) = 0;
	ivalue(x) = num;
	bignum(x) = NIL;
	set_num_integer(x);
	return x;
}

static pointer mk_bignum(int32_t col, pointer bn)
{
	pointer x = get_cell(&bn, &NIL);

	type(x) = (T_NUMBER | T_ATOM);
	exttype(x) = 0;
	ivalue(x) = col;
	bignum(x) = bn;
	set_num_integer(x);
	return x;
}

pointer mk_real(double num)
{
	pointer x = get_cell(&NIL, &NIL);

	type(x) = (T_NUMBER | T_ATOM);
	exttype(x) = 0;
	rvalue(x) = num;
	set_num_real(x);
	return x;
}

/* get number atom */
pointer mk_number(pointer v)
{
	return v->_isfixnum ? (bignum(v) == NIL ? mk_integer(ivalue(v)) : mk_bignum(ivalue(v), bignum(v))) : mk_real(rvalue(v));
}

/* get new memblock */
static pointer mk_memblock(size_t len, pointer *a, pointer *b)
{
	pointer x = get_consecutive_cells(2 + len / sizeof(struct cell), a, b);

#ifdef USE_COPYING_GC
	strvalue(x) = (char *)(x + 1);
#else
	x += 1 + len / sizeof(struct cell);
	strvalue(x) = (char *)(x - (1 + len / sizeof(struct cell)));
#endif
	type(x) = (T_MEMBLOCK | T_ATOM);
	strlength(x) = len;
	return x;
}

static void bignum_adjust(pointer z, pointer m, int32_t col, int32_t sign)
{
	type(z) = T_NUMBER | T_ATOM;
	set_num_integer(z);
	if (col == 0) {
		ivalue(z) = 0;
		bignum(z) = NIL;
	} else if (col == 1) {
		int64_t d = (int64_t)sign * ((uint32_t *)strvalue(m))[0];
		if (INT32_MIN <= d && d <= INT32_MAX) {
			ivalue(z) = (int32_t)d;
			bignum(z) = NIL;
		} else {
			ivalue(z) = sign;
			bignum(z) = m;
		}
	} else {
		ivalue(z) = sign * col;
		bignum(z) = m;
	}
}

static pointer mk_integer_from_str(const char *s, size_t len, int b)
{
	int32_t i, col, sign;
	pointer m, x;

	for (i = 0; isspace(s[i]); i++);
	if (s[i] == '-') {
		sign = -1;
		i++;
	} else if (s[i] == '+') {
		sign = 1;
		i++;
	} else {
		sign = 1;
	}
	while (s[i] == '0') i++;
	if (len <= (size_t)i) {
		return mk_integer(0);
	}
	if (b == 10) {
		col = ((int32_t)((len - i) * log(10) / log(2) + 1) + 31) / 32;
		m = mk_memblock(col * sizeof(uint32_t), &NIL, &NIL);
		col = bn_str2num_base10(s + i, (int32_t)len - i, (uint32_t *)strvalue(m), col);
	} else if (b == 16) {
		col = (int32_t)(len - i - 1) / 8 + 1;
		m = mk_memblock(col * sizeof(uint32_t), &NIL, &NIL);
		col = bn_str2num_base16(s + i, (int32_t)len - i, (uint32_t *)strvalue(m), col);
	} else if (b == 8) {
		col = (int32_t)(len - i - 1) / 32 * 3 + ((len - i - 1) % 32 + 1) / 11 + 1;
		m = mk_memblock(col * sizeof(uint32_t), &NIL, &NIL);
		col = bn_str2num_base8(s + i, (int32_t)len - i, (uint32_t *)strvalue(m), col);
	} else if (b == 2) {
		col = (int32_t)(len - i - 1) / 32 + 1;
		m = mk_memblock(col * sizeof(uint32_t), &NIL, &NIL);
		col = bn_str2num_base2(s + i, (int32_t)len - i, (uint32_t *)strvalue(m), col);
	} else {
		return F;
	}
	if (col < 0) return F;
	x = get_cell(&m, &NIL);
	bignum_adjust(x, m, col, sign);
	return x;
}

/* get new string */
static pointer get_string_cell(size_t len, pointer *a)
{
	pointer x = mk_memblock(len, a, &NIL);
	pointer y = get_cell(&x, a);

	type(y) = (T_STRING | T_ATOM);
	syntaxnum(y) = 0;
	strvalue(y) = (char *)car(x);
	strlength(y) = len;
	return y;
}

pointer mk_string(const char *str)
{
	size_t len = strlen(str);
	pointer x = get_string_cell(len, &NIL);
	snprintf(strvalue(x), len + 1, "%s", str);
	return x;
}

pointer mk_counted_string(const char *str, size_t len)
{
	pointer x = get_string_cell(len, &NIL);
	snprintf(strvalue(x), len + 1, "%s", str);
	return x;
}

pointer mk_empty_string(size_t len, int fill)
{
	char utf8[4];
	size_t i, n = utf32_to_utf8(fill, utf8);
	pointer x = get_string_cell(n * len, &NIL);
	for (i = 0; i < len; i++) {
		memcpy(strvalue(x) + n * i, utf8, n);
	}
	strvalue(x)[n * len] = 0;
	return x;
}

/* get new symbol */
pointer mk_symbol(const char *name)
{
	pointer x, y = NIL;

	/* fisrt check symbol_list */
	for (x = symbol_list; x != NIL; y = x, x = cdr(x)) {
		if (!strcmp(name, symname(car(x)))) {
			if (y != NIL) {
				cdr(y) = cdr(x);
				cdr(x) = symbol_list;
				symbol_list = x;
			}
			return car(x);
		}
	}

	x = mk_string(name);
	type(x) |= T_SYMBOL;
	symbol_list = cons(x, symbol_list);
	return car(symbol_list);
}

/* get new uninterned-symbol */
pointer mk_uninterned_symbol(const char *name)
{
	pointer x;

	x = mk_string(name);
	type(x) |= T_SYMBOL;
	return x;
}

pointer gensym( const char *prefix )
{
	char name[40];
	static unsigned long gensym_cnt = 1;

	snprintf(name, 40, "%s%lu", prefix, gensym_cnt++);
	return mk_uninterned_symbol(name);
}

/* make symbol or number atom from string */
pointer mk_atom(const char *q)
{
	const char *p;
	char c;
	int has_dec_point = 0;
	int has_fp_exp = 0;

	p = q;
	c = *p++;
	if ((c == '+') || (c == '-')) {
		c = *p++;
		if (c == '.') {
			has_dec_point = 1;
			c = *p++;
		}
		if (!isdigit((unsigned char)c)) {
			return mk_symbol(q);
		}
	} else if (c == '.') {
		has_dec_point = 1;
		c = *p++;
		if (!isdigit((unsigned char)c)) {
			return mk_symbol(q);
		}
	} else if (!isdigit((unsigned char)c)) {
		return mk_symbol(q);
	}

	for ( ; (c = *p) != 0; ++p) {
		if (!isdigit((unsigned char)c)) {
			if (c == '.') {
				if (!has_dec_point) {
					has_dec_point = 1;
					continue;
				}
			} else if ((c == 'e') || (c == 'E')) {
				if (!has_fp_exp) {
					has_fp_exp = 1;
					has_dec_point = 1;
					p++;
					if ((*p == '-') || (*p == '+') || isdigit((unsigned char)*p)) {
						continue;
					}
				}
			}
			return mk_symbol(q);
		}
	}
	if (has_dec_point) {
		return mk_real(atof(q));
	}
	return mk_integer_from_str(q, p - q, 10);
}

/* make constant */
pointer mk_const(const char *name)
{
	if (!strcmp(name, "t")) {
		return T;
	} else if (!strcmp(name, "f")) {
		return F;
	} else if (*name == 'b') {	/* #b (binary) */
		return mk_integer_from_str(&name[1], strlen(&name[1]), 2);
	} else if (*name == 'o') {	/* #o (octal) */
		return mk_integer_from_str(&name[1], strlen(&name[1]), 8);
	} else if (*name == 'd') {	/* #d (decimal) */
		return mk_integer_from_str(&name[1], strlen(&name[1]), 10);
	} else if (*name == 'x') {	/* #x (hex) */
		return mk_integer_from_str(&name[1], strlen(&name[1]), 16);
	} else if (*name == '\\') { /* #\w (character) */
		int c = 0;
		if (utf8_stricmp(name + 1, "space") == 0) {
			return mk_character(' ');
		} else if (utf8_stricmp(name + 1, "newline") == 0) {
			return mk_character('\n');
		} else if (utf8_stricmp(name + 1, "return") == 0) {
			return mk_character('\r');
		} else if (utf8_stricmp(name + 1, "tab") == 0) {
			return mk_character('\t');
		} else if (name[1] == 'x' && name[2] != 0) {
			if (sscanf(name + 2, "%x", (unsigned int *)&c) == 1 && c < 0x110000) {
				return mk_character(c);
			} else {
				return NIL;
			}
		} else if (name[utf8_get_next(name + 1, &c) + 1] == '\0') {
			return mk_character(c);
		} else {
			return NIL;
		}
	} else {
		return NIL;
	}
}

pointer mk_port(FILE *fp, unsigned char port_type)
{
	pointer x = get_consecutive_cells(2, &NIL, &NIL);

	type(x + 1) = type(x) = (T_PORT | T_ATOM);
	exttype(x) = 0;
	(x + 1)->_isfixnum = x->_isfixnum = ( port_type | T_PORT_FILE );
	port_file(x) = fp;
#ifdef USE_COPYING_GC
	gcell_next(x) = gcell_list;
	gcell_list = x;
#endif
	return x;
}

pointer mk_port_string(pointer p, int prop)
{
	pointer x = get_cell(&p, &NIL);

	type(x) = (T_PORT | T_ATOM);
	exttype(x) = 0;
	x->_isfixnum = (unsigned char)(prop | T_PORT_STRING);
	port_file(x) = (FILE *)p;
	port_curr(x) = strvalue(p);
	return x;
}

static void fill_vector(pointer v, pointer a)
{
	int i;
	int n = 1 + (int)ivalue(v) / 2 + (int)ivalue(v) % 2;

	for (i = 1; i < n; i++) {
		type(v + i) = T_PAIR;
		cdr(v + i) = car(v + i) = a;
	}
}

pointer mk_vector(int len)
{
	int n = 1 + len / 2 + len % 2;
	pointer x = get_consecutive_cells(n, &NIL, &NIL);

	type(x) = (T_VECTOR | T_ATOM);
	exttype(x) = 0;
	ivalue(x) = len;
	set_num_integer(x);
	fill_vector(x, NIL);
	return x;
}

pointer vector_elem(pointer v, int i)
{
	pointer x = v + 1 + i / 2;

	if (i % 2 == 0) {
		return car(x);
	} else {
		return cdr(x);
	}
}

pointer set_vector_elem(pointer v, int i, pointer a)
{
	pointer x = v + 1 + i / 2;

	if (i % 2 == 0) {
		return car(x) = a;
	} else {
		return cdr(x) = a;
	}
}

pointer mk_function(foreign_function *ff, pointer *pp)
{
	pointer x = get_cell(pp, &NIL);

	type(x) = (T_FUNCTION | T_ATOM);
	exttype(x) = 0;
	foreignfnc(x) = ff;
	return x;
}

#ifndef USE_SCHEME_STACK
/* get dump stack */
static pointer mk_dumpstack(pointer next)
{
	pointer x = get_consecutive_cells(3, &next, &NIL);

	type(x) = T_PAIR;
	type(x + 1) = T_NUMBER | T_ATOM;
	type(x + 2) = T_NUMBER | T_ATOM;
	car(x) = NIL;
	cdr(x) = next;
	return x;
}
#endif

/* ========== garbage collector ========== */
#ifdef USE_COPYING_GC
pointer next;

static pointer forward(pointer x)
{
	if (x < from_space || from_space + CELL_SEGSIZE <= x) {
		return x;
	}

	if (type(x) == T_FORWARDED) {
		return x->_object._forwarded;
	}

	*next = *x;
	type(x) = T_FORWARDED;
	x->_object._forwarded = next;
	if (is_memblock(next)) {
		size_t i;
		size_t n = 1 + strlength(next) / sizeof(struct cell);
		strvalue(next) = (char *)(next + 1);
		for (i = 0; i < n; i++) {
			*++next = *++x;
		}
		return next++ - n;
	} else if (is_fileport(next)) {
		*++next = *++x;
		type(x) = T_FORWARDED;
		x->_object._forwarded = next;
		return next++ - 1;
	} else if (is_vector(next)) {
		int i;
		int n = (int)ivalue(next) / 2 + (int)ivalue(next) % 2;
		for (i = 0; i < n; i++) {
			*++next = *++x;
			type(x) = T_FORWARDED;
			x->_object._forwarded = next;
		}
		return next++ - n;
	}
	return next++;
}

#ifndef USE_SCHEME_STACK
static void forward_dump(pointer base, pointer curr)
{
	pointer p, q;

	for (p = base; p != curr; p = dump_prev(p)) {
		q = forward(p);
		forward(p + 1);
		forward(p + 2);
		dump_args(q) = forward(dump_args(q));
		dump_envir(q) = forward(dump_envir(q));
		dump_code(q) = forward(dump_code(q));
	}
	for (; p != NIL; p = dump_prev(p)) {
		forward(p);
		forward(p + 1);
		forward(p + 2);
	}
}
#endif

void gc(pointer *a, pointer *b)
{
	pointer scan;
	pointer p, q;
	char temp[32];
	int i;

	if (gc_verbose)
		printf("Starting GC...");

	scan = next = to_space;

	/* forward system globals */
	symbol_list = forward(symbol_list);
	global_env = forward(global_env);
	call_history = forward(call_history);
	current_inport = forward(current_inport);
	current_outport = forward(current_outport);
	current_errport = forward(current_errport);
	current_xhands = forward(current_xhands);
	current_source = forward(current_source);

	command_line = forward(command_line);

	winders = forward(winders);
	strbuff = forward(strbuff);

	/* forward special symbols */
	LAMBDA = forward(LAMBDA);
	QUOTE = forward(QUOTE);
	QQUOTE = forward(QQUOTE);
	UNQUOTE = forward(UNQUOTE);
	UNQUOTESP = forward(UNQUOTESP);
	ELLIPSIS = forward(ELLIPSIS);

	/* forward current registers */
	args = forward(args);
	envir = forward(envir);
	code = forward(code);
#ifndef USE_SCHEME_STACK
	forward_dump(dump_base, dump);
	dump_base = forward(dump_base);

	for (p = c_nest; p != NIL; p = cdr(p)) {
		q = cdr(cdar(c_nest));
		forward_dump(cdr(q), car(q));
	}
#endif
	dump = forward(dump);

	value = forward(value);
	mark_x = forward(mark_x);
	mark_y = forward(mark_y);
	c_nest = forward(c_nest);
	c_sink = forward(c_sink);
	for (i = 0; i < load_files; i++) {
		load_stack[i] = forward(load_stack[i]);
	}

	/* forward variables a, b */
	*a = forward(*a);
	*b = forward(*b);

	while (scan < next) {
		switch (type(scan) & 0x1fff) {
		case T_NUMBER:
			if (scan->_isfixnum) {
				bignum(scan) = forward(bignum(scan));
			}
			break;
		case T_OPERATION:
		case T_CHARACTER:
		case T_VECTOR:
		case T_FUNCTION:
			break;
		case T_STRING:
		case T_STRING | T_SYMBOL:
		case T_STRING | T_SYMBOL | T_SYNTAX:
			p = forward(car(scan) - 1);
			strvalue(scan) = strvalue(p);
			break;
		case T_MEMBLOCK:
			scan += 1 + strlength(scan) / sizeof(struct cell);
			break;
		case T_PORT:
			if (is_strport(scan) && port_file(scan) != NULL) {
				size_t curr_len = port_curr(scan) - strvalue(car(scan));
				car(scan) = forward(car(scan));
				p = forward(caar(scan) - 1);
				port_curr(scan) = strvalue(p) + curr_len;
			}
			break;
		case T_PAIR:
		case T_CLOSURE:
		case T_CONTINUATION:
			car(scan) = forward(car(scan));
			cdr(scan) = forward(cdr(scan));
			break;
		default:
			sprintf(temp, "GC: Unknown type %d", type(scan));
			FatalError(temp);
			break;
		}
		++scan;
	}

	for (p = gcell_list, gcell_list = NIL; p != NIL; ) {
		if (type(p) == T_FORWARDED) {
			q = p->_object._forwarded;
			p = gcell_next(q);
			gcell_next(q) = gcell_list;
			gcell_list = q;
		} else {
			if (port_file(p) != NULL) {
				if (is_fileport(p)) {
					fclose(port_file(p));
				}
			}
			p = gcell_next(p);
		}
	}

	fcells = CELL_SEGSIZE - (scan - to_space);
	free_cell = scan;

	if (from_space == cell_seg) {
		from_space = cell_seg + CELL_SEGSIZE;
		to_space = cell_seg;
	} else {
		from_space = cell_seg;
		to_space = cell_seg + CELL_SEGSIZE;
	}

	if (gc_verbose)
		printf(" DONE %zu cells were recovered.\n", fcells);
}

#else /* USE_COPYING_GC */

/*--
 *  We use algorithm E (Knuth, The Art of Computer Programming Vol.1,
 *  sec.3.5) for marking.
 */
void mark(pointer p)
{
	pointer t = 0, q;

E2:	setmark(p);
	if (is_number(p) && p->_isfixnum) {
		setmark(bignum(p));
	} else if (is_string(p)) {
		size_t n = 1 + strlength(p) / sizeof(struct cell);
		setmark((pointer)strvalue(p) + n);
	} else if (is_port(p)) {
		if (is_fileport(p)) {
			setmark(p + 1);
		} else {
			mark(car(p));
		}
	} else if (is_vector(p)) {
		int i;
		int n = 1 + (int)ivalue(p) / 2 + (int)ivalue(p) % 2;
		for (i = 1; i < n; i++) {
			mark(p + i);
		}
	}
	if (is_atom(p))
		goto E6;
	q = car(p);
	if (q && !is_mark(q)) {
		setatom(p);
		car(p) = t;
		t = p;
		p = q;
		goto E2;
	}
E5:	q = cdr(p);
	if (q && !is_mark(q)) {
		cdr(p) = t;
		t = p;
		p = q;
		goto E2;
	}
E6:	if (!t)
		return;
	q = t;
	if (is_atom(q)) {
		clratom(q);
		t = car(q);
		car(q) = p;
		p = q;
		goto E5;
	} else {
		t = cdr(q);
		cdr(q) = p;
		p = q;
		goto E6;
	}
}

#ifndef USE_SCHEME_STACK
void mark_dump(pointer base, pointer curr)
{
	pointer p;

	for (p = base; p != curr; p = dump_prev(p)) {
		setmark(p);
		setmark(p + 1);
		setmark(p + 2);
		mark(dump_args(p));
		mark(dump_envir(p));
		mark(dump_code(p));
	}
	for ( ; p != NIL; p = dump_prev(p)) {
		setmark(p);
		setmark(p + 1);
		setmark(p + 2);
	}
}
#endif

/* garbage collection. parameter a, b is marked. */
void gc(pointer *a, pointer *b)
{
	pointer p;
	int i;

	if (gc_verbose)
		printf("gc...");

	/* mark system globals */
	mark(symbol_list);
	mark(global_env);
	mark(call_history);
	mark(current_inport);
	mark(current_outport);
	mark(current_errport);
	mark(current_xhands);
	mark(current_source);
	mark(command_line);
	mark(winders);
	mark(strbuff);

	/* mark current registers */
	mark(args);
	mark(envir);
	mark(code);
#ifndef USE_SCHEME_STACK
	mark_dump(dump_base, dump);

	for (p = c_nest; p != NIL; p = cdr(p)) {
		pointer q = cdr(cdar(c_nest));
		mark_dump(cdr(q), car(q));
	}
#else
	mark(dump);
#endif

	mark(value);
	mark(mark_x);
	mark(mark_y);
	mark(c_nest);
	mark(c_sink);
	for (i = 0; i < load_files; i++) {
		mark(load_stack[i]);
	}

	/* mark variables a, b */
	mark(*a);
	mark(*b);

	/* garbage collect */
	clrmark(NIL);
	fcells = 0;
	free_cell = NIL;
	p = cell_seg + CELL_SEGSIZE;
	while (--p >= cell_seg) {
		if (is_mark(p)) {
			clrmark(p);
			if (is_memblock(p)) {
				p = (pointer)strvalue(p);
			}
		} else {
			if (is_memblock(p)) {
				pointer q = (pointer)strvalue(p);
				do {
					type(p) = 0;
					cdr(p) = free_cell;
					car(p) = NIL;
					free_cell = p;
					++fcells;
				} while (--p > q);
			} else if (is_fileport(p)) {
				type(p) = 0;
				cdr(p) = free_cell;
				car(p) = NIL;
				free_cell = p;
				++fcells;
				if (port_file(--p) != NULL) {
					fclose(port_file(p));
				}
			}
			type(p) = 0;
			cdr(p) = free_cell;
			car(p) = NIL;
			free_cell = p;
			++fcells;
		}
	}

	if (gc_verbose)
		printf(" done %zu cells are recovered.\n", fcells);
}
#endif /* USE_COPYING_GC */

/* ========== Routines for Ports ========== */

static pointer port_from_filename(const char *filename, unsigned char port_type)
{
	FILE *fp = NULL;
	const char *mode;

	if( port_type == ( T_PORT_INPUT | T_PORT_TEXT ))
	    mode = "r";
	else if( port_type == ( T_PORT_OUTPUT | T_PORT_TEXT ))
	    mode = "w";
	else if (port_type == (T_PORT_INPUT | T_PORT_OUTPUT | T_PORT_TEXT ))
	    mode = "a+";
	else if( port_type == ( T_PORT_INPUT | T_PORT_BINARY ))
	    mode = "rb";
	else if( port_type == ( T_PORT_OUTPUT | T_PORT_BINARY ))
	    mode = "wb";
	else if (port_type == (T_PORT_INPUT | T_PORT_OUTPUT | T_PORT_BINARY ))
	    mode = "a+b";
	
	fp = fopen( filename, mode );

	if (fp == NULL) {
		return NIL;
	}
	return mk_port(fp, port_type);
}

#define BLOCK_SIZE 256

static pointer port_from_scratch(void)
{
	return mk_port_string(mk_empty_string(BLOCK_SIZE, '\0'), T_PORT_OUTPUT);
}

static pointer port_from_string(const char *str, int prop)
{
	return mk_port_string(mk_string(str), prop);
}

static pointer realloc_port_string(pointer p)
{
	size_t curr_len = port_curr(p) - strvalue(car(p));
	size_t new_size = strlength(car(p)) + BLOCK_SIZE;
	pointer x = get_string_cell(new_size, &p);

	memcpy(strvalue(x), strvalue(car(p)), strlength(car(p)));
	memset(strvalue(x) + strlength(car(p)), 0, BLOCK_SIZE);
	car(p) = x;
	port_curr(p) = strvalue(x) + curr_len;
	return p;
}

static void port_close(pointer p)
{
	if (port_file(p) != NULL) {
		if (is_fileport(p)) {
			fclose(port_file(p));
		}
		port_file(p) = NULL;
	}
}

/* ========== Routines for Reading ========== */

#define TOK_EOF     (-1)
#define TOK_LPAREN  0
#define TOK_RPAREN  1
#define TOK_DOT     2
#define TOK_ATOM    3
#define TOK_QUOTE   4
#define TOK_COMMENT 5
#define TOK_DQUOTE  6
#define TOK_BQUOTE  7
#define TOK_COMMA   8
#define TOK_ATMARK  9
#define TOK_SHARP   10
#define TOK_VEC     11

#define TOK_PAREN_SQUARE 16
#define TOK_PAREN_CURLY  32

/* get new character from input file */
static int inchar(void)
{
	int c;

	if (port_file(current_inport) == NULL || is_eofport(current_inport)) {
		return EOF;
	}
	if (is_fileport(current_inport)) {
		if (feof(port_file(current_inport))) {
			current_inport->_isfixnum |= T_PORT_EOF;
			return EOF;
		}

		c = utf8_fgetc(port_file(current_inport));
		if (c == EOF) {
			current_inport->_isfixnum |= T_PORT_EOF;
			if (port_file(current_inport) == stdin) {
				fprintf(stderr, "Good-bye\n");
				port_file(current_inport) = NULL;
			}
		}
	} else {
		if (port_curr(current_inport) == strvalue(car(current_inport)) + strlength(car(current_inport))) {
			current_inport->_isfixnum |= T_PORT_EOF;
			return EOF;
		} else {
			port_curr(current_inport) += utf8_get_next(port_curr(current_inport), &c);
		}
	}
	return c;
}

/* back to standard input */
static void flushinput(void)
{
	while (1) {
		if (is_fileport(current_inport) && port_file(current_inport) != stdin && port_file(current_inport) != NULL) {
			fclose(port_file(current_inport));
			port_file(current_inport) = NULL;
		}
		if (load_files == 1) {
			break;
		}
		current_inport = load_stack[--load_files];
	}

	current_inport = load_stack[0];
}

/* check c is delimiter */
static int isdelim(char *s, int c)
{
	if (c == EOF) return 0;
	while (*s)
		if (*s++ == c)
			return 0;
	return 1;
}

/* back character to input buffer */
static void backchar(int c)
{
	if (c != EOF) {
		if (is_fileport(current_inport)) {
			char utf8[4];
			size_t n = utf32_to_utf8(c, utf8);
			while (n > 0) {
				internal_ungetc(utf8[--n], port_file(current_inport));
			}
		} else if (port_curr(current_inport) != strvalue(car(current_inport))) {
			port_curr(current_inport) -= utf32_to_utf8(c, NULL);
		}
	}
}

static void putstr(const char *s)
{
	if (is_fileport(current_outport)) {
		fputs(s, port_file(current_outport));
	} else {
		char *endp = strvalue(car(current_outport)) + strlength(car(current_outport));
		while (*s) {
			if (port_curr(current_outport) < endp) {
				*port_curr(current_outport)++ = *s++;
				if (port_curr(current_outport) == endp) {
					current_outport = realloc_port_string(current_outport);
					endp = strvalue(car(current_outport)) + strlength(car(current_outport));
				}
			}
		}
	}
}

static void putcharacter(const int c)
{
	if (is_fileport(current_outport)) {
		fputc(c, port_file(current_outport));
	} else {
		char *endp = strvalue(car(current_outport)) + strlength(car(current_outport));
		if (port_curr(current_outport) < endp) {
			*port_curr(current_outport)++ = (unsigned char)c;
			if (port_curr(current_outport) == endp) {
				current_outport = realloc_port_string(current_outport);
			}
		}
	}
}

/* read chacters to delimiter */
static char *readstr(char *delim)
{
	char *p = strvalue(strbuff);
	int c;
	do {
		size_t len;
		c = inchar();
		if (c == EOF) {
			*p = '\0';
			return strvalue(strbuff);
		} else if ((len = p - strvalue(strbuff)) + 4 > strlength(strbuff)) {
			pointer x = mk_memblock(strlength(strbuff) + 256, &NIL, &NIL);
			memcpy(strvalue(x), strvalue(strbuff), strlength(strbuff));
			strbuff = x;
			p = strvalue(strbuff) + len;
		}
		p += utf32_to_utf8(c, p);
	} while (isdelim(delim, c));
	if (p != strvalue(strbuff) + 2 || p[-2] != '\\') {
		backchar(*--p);
	}
	*p = '\0';
	return strvalue(strbuff);
}

/* read string expression "xxx...xxx" */
static pointer readstrexp(void)
{
	char *p = strvalue(strbuff);
	int c, c1 = 0;
	enum { st_ok, st_bsl, st_x1, st_x2, st_oct1, st_oct2 } state = st_ok;

	for (;;) {
		size_t len;
		c = inchar();
		if (c == EOF) {
			return F;
		} else if ((len = p - strvalue(strbuff)) + 4 > strlength(strbuff)) {
			pointer x = mk_memblock(strlength(strbuff) + 256, &NIL, &NIL);
			memcpy(strvalue(x), strvalue(strbuff), strlength(strbuff));
			strbuff = x;
			p = strvalue(strbuff) + len;
		}
		if (state == st_ok) {
			switch (c) {
			case '\\':
				state = st_bsl;
				break;
			case '"':
				*p = 0;
				return mk_counted_string(strvalue(strbuff), p - strvalue(strbuff));
			default:
				p += utf32_to_utf8(c, p);
				break;
			}
		} else if (state == st_bsl) {
			switch (c) {
			case '0':
			case '1':
			case '2':
			case '3':
			case '4':
			case '5':
			case '6':
			case '7':
				state = st_oct1;
				c1 = c - '0';
				break;
			case 'x':
			case 'X':
				state = st_x1;
				c1 = 0;
				break;
			case 'n':
				*p++ = '\n';
				state = st_ok;
				break;
			case 't':
				*p++ = '\t';
				state = st_ok;
				break;
			case 'r':
				*p++ = '\r';
				state = st_ok;
				break;
			case '"':
				*p++ = '"';
				state = st_ok;
				break;
			default:
				p += utf32_to_utf8(c, p);
				state = st_ok;
				break;
			}
		} else if (state == st_x1 || state == st_x2) {
			c = toupper(c);
			if (c >= '0' && c <= 'F') {
				if (c <= '9') {
					c1 = (c1 << 4) + c - '0';
				} else {
					c1 = (c1 << 4) + c - 'A' + 10;
				}
				if (state == st_x1) {
					state = st_x2;
				} else {
					*p++ = (char)c1;
					state = st_ok;
				}
			} else {
				return F;
			}
		} else {
			if (c < '0' || c > '7') {
				*p++ = (char)c1;
				backchar(c);
				state = st_ok;
			} else {
				if (state == st_oct2 && c1 >= 32) {
					return F;
				}
				c1 = (c1 << 3) + (c - '0');
				if (state == st_oct1) {
					state = st_oct2;
				} else {
					*p++ = (char)c1;
					state = st_ok;
				}
			}
		}
	}
}

/* skip white characters */
static int skipspace(void)
{
	int c;

	while (utf32_isspace(c = inchar()))
		;
	backchar(c);
	return c;
}

/* get token */
static int token(void)
{
	int c = skipspace();
	if (c == EOF) {
		return TOK_EOF;
	}
	switch (c = inchar()) {
	case '(':
		return TOK_LPAREN;
	case ')':
		return TOK_RPAREN;
	case '[':
		return TOK_LPAREN | TOK_PAREN_SQUARE;
	case ']':
		return TOK_RPAREN | TOK_PAREN_SQUARE;
	case '{':
		return TOK_LPAREN | TOK_PAREN_CURLY;
	case '}':
		return TOK_RPAREN | TOK_PAREN_CURLY;
	case '.':
		if ((c = inchar()) == '.') {
			backchar(c);
			backchar(c);
			return TOK_ATOM;
		}
		backchar(c);
		return TOK_DOT;
	case '\'':
		return TOK_QUOTE;
	case ';':
		while ((c = inchar()) != '\n' && c != '\r' && c != EOF)
			;
		if (c == EOF) {
			return TOK_EOF;
		}
		return token();
	case '"':
		return TOK_DQUOTE;
	case BACKQUOTE:
		return TOK_BQUOTE;
	case ',':
		if ((c = inchar()) == '@')
			return TOK_ATMARK;
		else {
			backchar(c);
			return TOK_COMMA;
		}
	case '#':
		if ((c = inchar()) == '(') {
			return TOK_VEC;
		} else if (c == '[') {
			return TOK_VEC | TOK_PAREN_SQUARE;
		} else if (c == '{') {
			return TOK_VEC | TOK_PAREN_CURLY;
		} else {
			backchar(c);
			return TOK_SHARP;
		}
	case EOF:
		return TOK_EOF;
	default:
		backchar(c);
		return TOK_ATOM;
	}
}

/* ========== Routines for Printing ========== */
#define	ok_abbrev(x)	(is_pair(x) && cdr(x) == NIL)

static void printslashstring(unsigned char *s)
{
	int d;

	putcharacter('"');
	for ( ; *s; s++) {
		if (*s == 0xff || *s == '"' || *s < ' ' || *s == '\\') {
			putcharacter('\\');
			switch (*s) {
			case '"':
				putcharacter('"');
				break;
			case '\n':
				putcharacter('n');
				break;
			case '\t':
				putcharacter('t');
				break;
			case '\r':
				putcharacter('r');
				break;
			case '\\':
				putcharacter('\\');
				break;
			default:
				putcharacter('x');
				d = *s / 16;
				putcharacter(d < 10 ? d + '0' : d - 10 + 'A');
				d = *s % 16;
				putcharacter(d < 10 ? d + '0' : d - 10 + 'A');
				break;
			}
		} else {
			putcharacter(*s);
		}
	}
	putcharacter('"');
}

static char *atom2str(pointer l, int f)
{
	char *p;
	if (l == NIL)
		p = "()";
	else if (l == T)
		p = "#t";
	else if (l == F)
		p = "#f";
	else if (l == EOF_OBJ)
		p = "#<EOF>";
	else if (is_number(l)) {
		p = strvalue(strbuff);
		if (f <= 1 || f == 10) {
			if (l->_isfixnum) {
				if (bignum(l) == NIL) {
					sprintf(p, "%d", ivalue(l));
				} else {
					int32_t col = abs(ivalue(l)), sign = ivalue(l) < 0 ? -1 : 1;
					size_t len = (size_t)((col * 32 * log(2) + 3) / log(10)) + 1;
					pointer q = mk_memblock(col * sizeof(uint32_t), &l, &NIL);
					memcpy(strvalue(q), strvalue(bignum(l)), col * sizeof(uint32_t));
					if (len >= strlength(strbuff)) {
						strbuff = mk_memblock((len + 255) / 256 * 256, &q, &NIL);
					}
					p = bn_num2str_base10(strvalue(strbuff) + strlength(strbuff) - 1, (uint32_t *)strvalue(q), col);
					if (sign < 0) *--p = '-';
				}
			} else {
				sprintf(p, "%.10g", rvalue(l));
				f = (int)strcspn(p, ".e");
				if (p[f] == 0) {
					p[f] = '.';
					p[f + 1] = '0';
					p[f + 2] = 0;
				}
			}
		} else if (f == 16) {
			if (bignum(l) == NIL) {
				if (ivalue(l) >= 0) {
					sprintf(p, "%x", ivalue(l));
				} else {
					sprintf(p, "-%x", -ivalue(l));
				}
			} else {
				int32_t col = abs(ivalue(l));
				size_t len = col * 8 + 1;
				if (len >= strlength(strbuff)) {
					strbuff = mk_memblock((len + 255) / 256 * 256, &l, &NIL);
				}
				p = bn_num2str_base16(strvalue(strbuff) + strlength(strbuff) - 1, (uint32_t *)strvalue(bignum(l)), col);
				if (ivalue(l) < 0) *--p = '-';
			}
		} else if (f == 8) {
			if (bignum(l) == NIL) {
				if (ivalue(l) >= 0)
					sprintf(p, "%o", ivalue(l));
				else
					sprintf(p, "-%o", -ivalue(l));
			} else {
				int32_t col = abs(ivalue(l));
				size_t len = col * 11 + 1;
				if (len >= strlength(strbuff)) {
					strbuff = mk_memblock((len + 255) / 256 * 256, &l, &NIL);
				}
				p = bn_num2str_base8(strvalue(strbuff) + strlength(strbuff) - 1, (uint32_t *)strvalue(bignum(l)), col);
				if (ivalue(l) < 0) *--p = '-';
			}
		} else if (f == 2) {
			if (bignum(l) == NIL) {
				uint32_t b = (ivalue(l) < 0) ? -ivalue(l) : ivalue(l);
				p = &p[strlength(strbuff) - 1];
				*p = 0;
				do { *--p = (b & 1) ? '1' : '0'; b >>= 1; } while (b != 0);
				if (ivalue(l) < 0) *--p = '-';
			} else {
				int32_t col = abs(ivalue(l));
				size_t len = col * 32 + 1;
				if (len >= strlength(strbuff)) {
					strbuff = mk_memblock((len + 255) / 256 * 256, &l, &NIL);
				}
				p = bn_num2str_base2(strvalue(strbuff) + strlength(strbuff) - 1, (uint32_t *)strvalue(bignum(l)), col);
				if (ivalue(l) < 0) *--p = '-';
			}
		} else {
			p = NULL;
		}
	} else if (is_symbol(l)) {
		if (syntaxnum(l) & T_DEFSYNTAX) {
			p = symname(cdr(l));
		} else {
			p = symname(l);
		}
	} else if (is_string(l)) {
		if (!f) {
			p = strvalue(l);
		} else {
			printslashstring((unsigned char *)strvalue(l));
			p = NULL;
		}
	} else if (is_character(l)) {
		int c = (int)ivalue(l);
		p = strvalue(strbuff);
		if (!f) {
			*(p + utf32_to_utf8(c, p)) = '\0';
		} else {
			switch (c) {
			case ' ':
				sprintf(p, "#\\space");
				break;
			case '\n':
				sprintf(p, "#\\newline");
				break;
			case '\r':
				sprintf(p, "#\\return");
				break;
			case '\t':
				sprintf(p, "#\\tab");
				break;
			default:
				if (c < 32) {
					sprintf(p, "#\\x%x", c < 0 ? -c : c);
				} else {
					p[0] = '#';
					p[1] = '\\';
					*(p + 2 + utf32_to_utf8(c, p + 2)) = '\0';
				}
				break;
			}
		}
	} else if (is_operation(l)) {
		p = strvalue(strbuff);
		sprintf(p, "#<OPERATION %d>", op_loc(l));
	} else if (is_port(l)) {
		if (port_file(l) != NULL) {
			p = "#<PORT>";
		} else {
			p = "#<PORT (CLOSED)>";
		}
	} else if (is_closure(l)) {
		if (is_promise(l)) {
			if (is_resultready(l)) {
				p = "#<PROMISE (FORCED)>";
			} else {
				p = "#<PROMISE>";
			}
		} else if (is_operation(l)) {
		    p = strvalue(strbuff);
		    sprintf(p, "#<OPERATION %d>", op_loc(l));
		} else if (is_macro(l)) {
			p = "#<MACRO>";
		} else {
			p = "#<CLOSURE>";
		}
	} else if (is_continuation(l)) {
		p = "#<CONTINUATION>";
	} else if (is_function(l)) {
		p = strvalue(strbuff);
		sprintf(p, "#<FUNCTION %d>", op_loc(l));
	} else {
		p = "#<ERROR>";
	}
	return p;
}

/* print atoms */
static size_t printatom(pointer l, int f)
{
	char *p = atom2str(l, f);

	if (p == NULL) {
		return 0;
	}
	if (f < 0) {
		return strlen(p);
	}
	putstr(p);
	return 0;
}

/* ========== Routines for Numerical operations ========== */

static double get_rvalue(pointer x)
{
	if (x->_isfixnum) {
		if (bignum(x) == NIL) {
			return (double)ivalue(x);
		} else {
			int32_t colx = abs(ivalue(x)), bitx = find1_32(((uint32_t *)strvalue(bignum(x)))[colx - 1]);
			uint64_t d = (uint64_t)((uint32_t *)strvalue(bignum(x)))[colx - 1] << (64 - bitx);
			d += (uint64_t)((uint32_t *)strvalue(bignum(x)))[colx - 2] << (32 - bitx);
			if (bitx < 32 && colx > 2) {
				d += (uint64_t)((uint32_t *)strvalue(bignum(x)))[colx - 3] >> bitx;
			}
			return (ivalue(x) < 0 ? -1 : 1) * ldexp((double)d, bitx + (colx - 3) * 32);
		}
	} else {
		return rvalue(x);
	}
}

static void bignum_from_int64(pointer x, int64_t d)
{
	type(x) = T_NUMBER | T_ATOM;
	set_num_integer(x);
	if (INT32_MIN <= d && d <= INT32_MAX) {
		ivalue(x) = (int32_t)d;
		bignum(x) = NIL;
	} else if (-((int64_t)UINT32_MAX + 1) <= d && d <= UINT32_MAX) {
		ivalue(x) = d < 0 ? -1 : 1;
		bignum(x) = mk_memblock(1 * sizeof(uint32_t), &x, &NIL);
		if (d < 0) d = (uint32_t)~d + 1;
		((uint32_t *)strvalue(bignum(x)))[0] = (uint32_t)d;
	} else {
		ivalue(x) = d < 0 ? -2 : 2;
		bignum(x) = mk_memblock(2 * sizeof(uint32_t), &x, &NIL);
		if (d < 0) d = (uint64_t)~d + 1;
		((uint32_t *)strvalue(bignum(x)))[1] = (uint32_t)(d >> 32);
		((uint32_t *)strvalue(bignum(x)))[0] = (uint32_t)d;
	}
}

/* if |x| == |y| */
static int bignum_eq(pointer x, pointer y)
{
	return bn_eq((uint32_t *)strvalue(bignum(x)), abs(ivalue(x)), (uint32_t *)strvalue(bignum(y)), abs(ivalue(y)));
}

/* if |x| > |y| */
static int bignum_gt(pointer x, pointer y)
{
	return bn_gt((uint32_t *)strvalue(bignum(x)), abs(ivalue(x)), (uint32_t *)strvalue(bignum(y)), abs(ivalue(y)));
}

/* if |x| >= |y| */
static int bignum_ge(pointer x, pointer y)
{
	return bn_ge((uint32_t *)strvalue(bignum(x)), abs(ivalue(x)), (uint32_t *)strvalue(bignum(y)), abs(ivalue(y)));
}

/* z = |x| */
static int bignum_abs(pointer z, pointer x)
{
	if (bignum(x) == NIL) {
		bignum_from_int64(z, ivalue(x) < 0 ? (uint32_t)~ivalue(x) + 1 : ivalue(x));
	} else {
		pointer m = mk_memblock(abs(ivalue(x)) * sizeof(uint32_t), &z, &x);
		memcpy(strvalue(m), strvalue(bignum(x)), abs(ivalue(x)) * sizeof(uint32_t));
		bignum_adjust(z, m, abs(ivalue(x)), 1);
	}
	return 1;
}

/* z = sign * (|x|+|y|) */
static void bignum_add(pointer z, pointer x, pointer y, int32_t sign)
{
	int32_t colx = abs(ivalue(x)), coly = abs(ivalue(y)), col = (colx > coly ? colx : coly) + 1;
	pointer m = mk_memblock(col * sizeof(uint32_t), &x, &y);
	bn_add((uint32_t *)strvalue(m), &col, (uint32_t *)strvalue(bignum(x)), colx, (uint32_t *)strvalue(bignum(y)), coly);
	bignum_adjust(z, m, col, sign);
}

/* z = sign * (|x|+|val|) */
static void bignum_add_imm(pointer z, pointer x, int32_t val, int32_t sign)
{
	uint32_t y = val < 0 ? (uint32_t)~val + 1 : val;
	int32_t colx = abs(ivalue(x)), col = (colx > 1 ? colx : 1) + 1;
	pointer m = mk_memblock(col * sizeof(uint32_t), &x, &z);
	bn_add((uint32_t *)strvalue(m), &col, (uint32_t *)strvalue(bignum(x)), colx, (uint32_t *)&y, 1);
	bignum_adjust(z, m, col, sign);
}

/* z = sign * (|x|-|y|) */
static void bignum_sub(pointer z, pointer x, pointer y, int32_t sign)
{
	int32_t colx = abs(ivalue(x)), coly = abs(ivalue(y)), col = colx;
	pointer m = mk_memblock(col * sizeof(uint32_t), &x, &y);
	bn_sub((uint32_t *)strvalue(m), &col, (uint32_t *)strvalue(bignum(x)), colx, (uint32_t *)strvalue(bignum(y)), coly);
	bignum_adjust(z, m, col, sign);
}

/* z = sign * (|x|-|val|) */
static void bignum_sub_imm(pointer z, pointer x, int32_t val, int32_t sign)
{
	uint32_t y = val < 0 ? (uint32_t)~val + 1 : val;
	int32_t colx = abs(ivalue(x)), col = colx;
	pointer m = mk_memblock(col * sizeof(uint32_t), &x, &z);
	bn_sub((uint32_t *)strvalue(m), &col, (uint32_t *)strvalue(bignum(x)), colx, (uint32_t *)&y, 1);
	bignum_adjust(z, m, col, sign);
}

/* z = x * y */
static void bignum_mul(pointer z, pointer x, pointer y)
{
	int32_t colx = abs(ivalue(x)), coly = abs(ivalue(y)), col = colx + coly;
	pointer m = mk_memblock(col * sizeof(uint32_t), &x, &y);
	bn_mul((uint32_t *)strvalue(m), &col, (uint32_t *)strvalue(bignum(x)), colx, ((uint32_t *)strvalue(bignum(y))), coly);
	bignum_adjust(z, m, col, (ivalue(x) < 0 ? -1 : 1) * (ivalue(y) < 0 ? -1 : 1));
}

/* z = x * val */
static void bignum_mul_imm(pointer z, pointer x, int32_t val)
{
	uint32_t y = val < 0 ? (uint32_t)~val + 1 : val;
	int32_t colx = abs(ivalue(x)), col = colx + 1;
	pointer m = mk_memblock(col * sizeof(uint32_t), &x, &z);
	bn_mul((uint32_t *)strvalue(m), &col, (uint32_t *)strvalue(bignum(x)), colx, (uint32_t *)&y, 1);
	bignum_adjust(z, m, col, (ivalue(x) < 0 ? -1 : 1) * (val < 0 ? -1 : 1));
}

/* q = x / y + r*/
static void bignum_div(pointer q, pointer r, pointer x, pointer y)
{
	int32_t colx = abs(ivalue(x)), coly = abs(ivalue(y)), colq = colx, colr = coly, signx = ivalue(x) < 0 ? -1 : 1, signy = ivalue(y) < 0 ? -1 : 1;
	pointer a = cons(x, y), m = mk_memblock(colq * sizeof(uint32_t), &a, &NIL);
	pointer b = mk_memblock(2 * (colq + colr + 1) * sizeof(uint32_t), &m, &a);
	uint32_t *t_q = (uint32_t *)strvalue(m), *t_r = (uint32_t *)strvalue(b);
	bn_div(t_q, &colq, t_r, &colr, (uint32_t *)strvalue(bignum(car(a))), colx, (uint32_t *)strvalue(bignum(cdr(a))), coly);
	bignum_adjust(q, m, colq, signx * signy);

	m = mk_memblock(colr * sizeof(uint32_t), &q, &b);
	memcpy((uint32_t *)strvalue(m), t_r, colr * sizeof(uint32_t));
	bignum_adjust(r, m, colr, signx);
}

/* q = x / val + r */
static void bignum_div_imm(pointer q, pointer r, pointer x, int32_t val)
{
	int32_t colx = abs(ivalue(x)), colq = colx, colr, sign = (ivalue(x) < 0 ? -1 : 1) * (val < 0 ? -1 : 1);
	pointer m = mk_memblock(colq * sizeof(uint32_t), &x, &NIL);
	uint32_t *t_q = (uint32_t *)strvalue(m), tr, y = val < 0 ? (uint32_t)~val + 1 : val;
	bn_div(t_q, &colq, &tr, &colr, (uint32_t *)strvalue(bignum(x)), colx, (uint32_t *)&y, 1);
	bignum_adjust(q, m, colq, sign);

	type(r) = T_NUMBER | T_ATOM;
	set_num_integer(r);
	ivalue(r) = (int32_t)tr * (ivalue(x) < 0 ? -1 : 1);
	bignum(r) = NIL;
}

/* r = gcd(x, y) */
static void bignum_gcd(pointer r, pointer x, pointer y)
{
	int32_t cols = abs(ivalue(x)), colt = abs(ivalue(y));
	pointer s, t, u;
	t = mk_memblock(colt * sizeof(uint32_t), &x, &y);
	memcpy(strvalue(t), strvalue(bignum(y)), colt * sizeof(uint32_t));
	s = mk_memblock(cols * sizeof(uint32_t), &x, &t);
	memcpy(strvalue(s), strvalue(bignum(x)), cols * sizeof(uint32_t));
	u = mk_memblock(2 * (colt + cols + 1) * sizeof(uint32_t), &s, &t);
	while (colt > 1 || (colt == 1 && ((uint32_t *)strvalue(t))[0] > 0)) {
		int32_t colq = cols, colr = colt;
		uint32_t *t_q = (uint32_t *)strvalue(s), *t_r = (uint32_t *)strvalue(u);
		bn_div(t_q, &colq, t_r, &colr, (uint32_t *)strvalue(s), cols, (uint32_t *)strvalue(t), colt);
		memcpy(strvalue(s), strvalue(t), colt * sizeof(uint32_t));
		cols = colt;
		bn_sub((uint32_t *)strvalue(t), &colt, (uint32_t *)strvalue(t), colt, t_r, colr);
		if (bn_gt((uint32_t *)strvalue(t), colt, t_r, colr)) {
			memcpy(strvalue(t), t_r, colr * sizeof(uint32_t));
			colt = colr;
		}
	}
	bignum_adjust(r, s, cols, 1);
}

/* r = gcd(x, val) */
static void bignum_gcd_imm(pointer r, pointer x, int32_t val)
{
	int32_t colx = abs(ivalue(x)), colq = colx, colr;
	pointer m = mk_memblock(colq * sizeof(uint32_t), &x, &NIL), n = mk_memblock(sizeof(uint32_t), &x, &m);
	uint32_t *t_q = (uint32_t *)strvalue(m), *t_r = (uint32_t *)strvalue(n), y = val < 0 ? (uint32_t)~val + 1 : val;
	bn_div(t_q, &colq, t_r, &colr, (uint32_t *)strvalue(bignum(x)), colx, (uint32_t *)&y, 1);
	if (t_r[0] > 0) {
		t_r[0] = (uint32_t)gcd(t_r[0], y);
	} else {
		t_r[0] = y;
	}
	bignum_adjust(r, n, 1, 1);
}

/* r = lcm(x, y) */
static void bignum_lcm(pointer r, pointer x, pointer y)
{
	int32_t colq, colr, cols = abs(ivalue(x)), colt = abs(ivalue(y));
	pointer s, t, u;
	uint32_t *t_q, *t_r;
	t = mk_memblock((colt + cols) * sizeof(uint32_t), &x, &y);
	memcpy(strvalue(t), strvalue(bignum(y)), colt * sizeof(uint32_t));
	s = mk_memblock(cols * sizeof(uint32_t), &x, &t);
	memcpy(strvalue(s), strvalue(bignum(x)), cols * sizeof(uint32_t));
	u = mk_memblock(2 * (colt + cols + 1) * sizeof(uint32_t), &s, &t);
	t_q = (uint32_t *)strvalue(s);
	t_r = (uint32_t *)strvalue(u);
	while (colt > 1 || (colt == 1 && ((uint32_t *)strvalue(t))[0] > 0)) {
		colq = cols;
		colr = colt;
		bn_div(t_q, &colq, t_r, &colr, (uint32_t *)strvalue(s), cols, (uint32_t *)strvalue(t), colt);
		memcpy(strvalue(s), strvalue(t), colt * sizeof(uint32_t));
		cols = colt;
		bn_sub((uint32_t *)strvalue(t), &colt, (uint32_t *)strvalue(t), colt, t_r, colr);
		if (bn_gt((uint32_t *)strvalue(t), colt, t_r, colr)) {
			memcpy(strvalue(t), t_r, colr * sizeof(uint32_t));
			colt = colr;
		}
	}
	bn_div(t_q, &colq, t_r, &colr, (uint32_t *)strvalue(bignum(x)), abs(ivalue(x)), (uint32_t *)strvalue(s), cols);
	bn_mul((uint32_t *)strvalue(t), &colt, t_q, colq, (uint32_t *)strvalue(bignum(y)), abs(ivalue(y)));
	bignum_adjust(r, t, colt, 1);
}

/* r = lcm(x, val) */
static void bignum_lcm_imm(pointer r, pointer x, int32_t val)
{
	int32_t colx = abs(ivalue(x)), colq = colx, colr;
	pointer m = mk_memblock(colq * sizeof(uint32_t), &x, &NIL), n = mk_memblock(sizeof(uint32_t), &x, &m);
	uint32_t *t_q = (uint32_t *)strvalue(m), *t_r = (uint32_t *)strvalue(n), y = val < 0 ? (uint32_t)~val + 1 : val, z;
	bn_div(t_q, &colq, t_r, &colr, (uint32_t *)strvalue(bignum(x)), colx, (uint32_t *)&y, 1);
	if (t_r[0] > 0) {
		z = (uint32_t)gcd(t_r[0], y);
	} else {
		z = y;
	}
	bn_div(t_q, &colq, t_r, &colr, (uint32_t *)strvalue(bignum(x)), colx, (uint32_t *)&z, 1);
	n = mk_memblock((colq + 1) * sizeof(uint32_t), &x, &m);
	bn_mul((uint32_t *)strvalue(n), &colr, (uint32_t *)strvalue(m), colq, (uint32_t *)&y, 1);
	bignum_adjust(r, n, colr, 1);
}

/* z = pow(x, val) */
static void bignum_pow(pointer z, pointer x, int32_t val)
{
	int32_t colx = bignum(x) == NIL ? 1 : abs(ivalue(x));
	uint32_t v_x = bignum(x) == NIL ? (ivalue(x) < 0 ? (uint32_t)~ivalue(x) + 1 : ivalue(x)) : ((uint32_t *)strvalue(bignum(x)))[colx - 1];
	int32_t colz = ((find1_32(v_x) + 32 * (colx - 1)) * val + 31) / 32;
	int32_t sign = ivalue(x) < 0 && (val & 0x1) ? -1 : 1;
	pointer m_x = mk_memblock(2 * colz * sizeof(uint32_t), &z, &x), m_z;
	uint32_t *t_x = (uint32_t *)strvalue(m_x), *tmp = t_x + colz, *t_z;
	if (bignum(x) == NIL) {
		t_x[0] = v_x;
	} else {
		memcpy(t_x, strvalue(bignum(x)), colx * sizeof(uint32_t));
	}
	m_z = mk_memblock(colz * sizeof(uint32_t), &z, &m_x);
	t_z = (uint32_t *)strvalue(m_z);
	t_z[0] = 1;
	colz = 1;
	while (val > 0) {
		if (val & 0x1) {
			val--;
			bn_mul(tmp, &colz, t_z, colz, t_x, colx);
			memcpy(t_z, tmp, colz * sizeof(uint32_t));
		} else {
			val >>= 1;
			bn_sqr(tmp, &colx, t_x, colx);
			memcpy(t_x, tmp, colx * sizeof(uint32_t));
		}
	}
	bignum_adjust(z, m_z, colz, sign);
}

/* ========== Routines for Evaluation Cycle ========== */

/* make closure. c is code. e is environment */
static pointer mk_closure(pointer c, pointer e)
{
	pointer x = get_cell(&c, &e);

	type(x) = T_CLOSURE;
	syntaxnum(x) = 0;
	car(x) = c;
	cdr(x) = e;
	return x;
}

/* make continuation. */
static pointer mk_continuation(pointer d)
{
	pointer x = get_cell(&NIL, &d);

	type(x) = T_CONTINUATION;
	exttype(x) = 0;
	car(x) = NIL;
	cont_dump(x) = d;
	return x;
}

/* reverse list -- make new cells */
pointer reverse(pointer a) /* a must be checked by gc */
{
	pointer p = NIL;

	for (mark_x = a; is_pair(mark_x); mark_x = cdr(mark_x)) {
		p = cons(car(mark_x), p);
	}
	return p;
}

/* reverse list (without last arg) -- make new cells */
static void short_reverse(void)
{
	if (is_pair(args)) {
		pointer p = cons(value, NIL);

		for (; is_pair(cdr(args)); args = cdr(args)) {
			p = cons(car(args), p);
		}
		code = car(args);
		args = p;
	} else {
		code = value;
		args = NIL;
	}
}

/* reverse list --- no make new cells */
static pointer non_alloc_rev(pointer term, pointer list)
{
	pointer p = list, result = term, q;

	while (p != NIL) {
		q = cdr(p);
		cdr(p) = result;
		result = p;
		p = q;
	}
	return result;
}

/* append list -- make new cells */
pointer append(pointer a, pointer b)
{
	pointer q;

	if (a != NIL) {
		mark_y = b;
		a = reverse(a);
		b = mark_y;
		while (a != NIL) {
			q = cdr(a);
			cdr(a) = b;
			b = a;
			a = q;
		}
	}
	return b;
}

/* list length */
int list_length(pointer a)
{
	int i = 0;
	pointer slow, fast;

	slow = fast = a;
	while (1) {
		if (fast == NIL)
			return i;
		if (!is_pair(fast))
			return -2 - i;
		fast = cdr(fast);
		++i;
		if (fast == NIL)
			return i;
		if (!is_pair(fast))
			return -2 - i;
		++i;
		fast = cdr(fast);
		slow = cdr(slow);
		if (fast == slow) {
			return -1;
		}
	}
}

/* shared tail */
static pointer shared_tail(pointer a, pointer b)
{
	int alen = list_length(a);
	int blen = list_length(b);

	if (alen > blen) {
		while (alen > blen) {
			a = cdr(a);
			--alen;
		}
	} else {
		while (alen < blen) {
			b = cdr(b);
			--blen;
		}
	}

	while (a != b) {
		a = cdr(a);
		b = cdr(b);
	}
	return a;
}

/* equivalence of atoms */
int eqv(pointer a, pointer b)
{
	if (is_string(a)) {
		if (is_string(b))
			return (strvalue(a) == strvalue(b));
		else
			return 0;
	} else if (is_number(a)) {
		if (is_number(b)) {
			if (a->_isfixnum && b->_isfixnum) {
				if (bignum(a) == NIL && bignum(b) == NIL) {
					return (ivalue(a) == ivalue(b));
				} else if (bignum(a) != NIL && bignum(b) != NIL && (ivalue(a) < 0) == (ivalue(b) < 0)) {
					return bignum_eq(a, b);
				} else {
					return 0;
				}
			} else if (!a->_isfixnum && !b->_isfixnum) {
				return (rvalue(a) == rvalue(b));
			} else {
				return 0;
			}
		} else {
			return 0;
		}
	} else if (is_character(a)) {
		if (is_character(b))
			return (ivalue(a) == ivalue(b));
		else
			return 0;
	} else
		return (a == b);
}

/* equivalence of pairs, vectors and strings recursively */
int equal(pointer a, pointer b)
{
	if (is_pair(a)) {
		if (is_pair(b))
			return equal(car(a), car(b)) && equal(cdr(a), cdr(b));
		else
			return 0;
	} else if (is_vector(a)) {
		if (is_vector(b))
			if (ivalue(a) == ivalue(b)) {
				int i;
				for (i = 0; i < ivalue(a); i++) {
					if (!equal(vector_elem(a, i), vector_elem(b, i)))
						return 0;
				}
				return 1;
			} else {
				return 0;
			}
		else
			return 0;
	} else if (is_string(a)) {
		if (is_string(b))
			return strcmp(strvalue(a), strvalue(b)) == 0;
		else
			return 0;
	} else {
		return eqv(a, b);
	}
}

static int is_ellipsis(pointer p)
{
	pointer x, y;
	for (x = envir; x != NIL; x = cdr(x)) {
		for (y = car(x); y != NIL; y = cdr(y)) {
			if (cdar(y) == ELLIPSIS) {
				return caar(y) == p;
			}
		}
	}
	return p == ELLIPSIS;
}

static int matchpattern(pointer p, pointer f, pointer keyword, int *s)
{
	pointer x;
	if (is_symbol(p)) {
		if (syntaxnum(p) & T_DEFSYNTAX) p = cdr(p);
		for (x = keyword; x != NIL; x = cdr(x)) {
			if (p == car(x)) {
				return p == f;
			}
		}
		(*s)++;
		return 1;
	} else if (is_pair(p)) {
		long len_f;
		if (is_pair(cdr(p)) && is_ellipsis(cadr(p)) && (len_f = list_length(f)) >= 0) {
			len_f -= list_length(cddr(p));
			for (x = f; len_f-- > 0; x = cdr(x)) {
				if (!matchpattern(car(p), car(x), keyword, s)) {
					return 0;
				}
			}
			if (is_pair(x) && !matchpattern(cddr(p), x, keyword, s)) {
				return 0;
			}
			(*s)++;
			return 1;
		} else if (is_pair(f)) {
			return matchpattern(car(p), car(f), keyword, s) && matchpattern(cdr(p), cdr(f), keyword, s);
		} else {
			return 0;
		}
	} else if (is_vector(p)) {
		if (is_vector(f)) {
			long i, j;
			for (i = 0, j = 0; i < ivalue(p) && j < ivalue(f); i++, j++) {
				if (i + 1 < ivalue(p) && is_ellipsis(vector_elem(p, i + 1))) {
					for (; j < ivalue(f) && j - i < 2 + ivalue(f) - ivalue(p); j++) {
						if (!matchpattern(vector_elem(p, i), vector_elem(f, j), keyword, s)) {
							return 0;
						}
					}
					(*s)++;
					if (j < ivalue(f)) { i++; j--; continue; }
					i += 2;
					break;
				}
				if (!matchpattern(vector_elem(p, i), vector_elem(f, j), keyword, s)) {
					return 0;
				}
			}
			if (i != ivalue(p) || j != ivalue(f)) {
				return 0;
			}
			return 1;
		} else {
			return 0;
		}
	} else {
		return equal(p, f);
	}
}

/* note: value = (vector of bindings, list of keywords) */
static void bindpattern(pointer p, pointer f, int d, int n, int *s)
{
	pointer x;
	if (is_symbol(p)) {
		if (syntaxnum(p) & T_DEFSYNTAX) p = cdr(p);
		for (x = cdr(value); x != NIL; x = cdr(x)) {
			if (p == car(x)) {
				return;
			}
		}
		set_vector_elem(car(value), (*s)++, p);
		x = vector_elem(car(value), (*s)++);
		ivalue(car(x)) = d;
		ivalue(cdr(x)) = n;
		set_vector_elem(car(value), (*s)++, f);
	} else if (is_pair(p)) {
		long len_f;
		if (is_pair(cdr(p)) && is_ellipsis(cadr(p)) && (len_f = list_length(f)) >= 0) {
			long i = 0;
			set_vector_elem(car(value), (*s)++, car(p));
			x = vector_elem(car(value), (*s)++);
			ivalue(car(x)) = d;
			ivalue(cdr(x)) = n;
			set_vector_elem(car(value), (*s)++, NULL);
			len_f -= list_length(cddr(p));
			for (x = f; len_f-- > 0; x = cdr(x)) {
				bindpattern(car(p), car(x), d + 1, i++, s);
			}
			if (is_pair(x)) {
				bindpattern(cddr(p), x, d, n, s);
			}
		} else if (is_pair(f)) {
			bindpattern(car(p), car(f), d, n, s);
			bindpattern(cdr(p), cdr(f), d, n, s);
		}
	} else if (is_vector(p)) {
		if (is_vector(f)) {
			long i, j;
			for (i = 0, j = 0; i < ivalue(p) && j < ivalue(f); i++, j++) {
				if (i + 1 < ivalue(p) && is_ellipsis(vector_elem(p, i + 1))) {
					set_vector_elem(car(value), (*s)++, vector_elem(p, i));
					x = vector_elem(car(value), (*s)++);
					ivalue(car(x)) = d;
					ivalue(cdr(x)) = n;
					set_vector_elem(car(value), (*s)++, NULL);
					for (; j < ivalue(f) && j - i < 2 + ivalue(f) - ivalue(p); j++) {
						bindpattern(vector_elem(p, i), vector_elem(f, j), d + 1, j - i, s);
					}
					if (j < ivalue(f)) { i++; j--; continue; }
					i += 2;
					break;
				}
				bindpattern(vector_elem(p, i), vector_elem(f, j), d, n, s);
			}
		}
	}
}

static pointer expandsymbol(pointer p)
{
	pointer x, y;
	if (is_symbol(p)) {
		if (syntaxnum(p) & T_DEFSYNTAX) {
			car(p) = cons(cadr(args), car(p));
			syntaxnum(car(p)) |= T_DEFSYNTAX;
		} else {
			p = cons(cdr(args), p);
			type(p) = type(cdr(p));
			syntaxnum(p) |= T_DEFSYNTAX;
		}
		return p;
	} else if (is_pair(p)) {
		mark_x = cons(p, mark_x);
		x = expandsymbol(caar(mark_x));
		if (is_symbol(x) && is_syntax(cdr(x)) && !strcmp(symname(cdr(x)), "quote")) {
			y = cdar(mark_x);
			mark_x = cdr(mark_x);
			return cons(x, y);
		}
		mark_y = cons(x, mark_y);
		y = expandsymbol(cdar(mark_x));
		mark_x = cdr(mark_x);
		x = car(mark_y);
		mark_y = cdr(mark_y);
		return cons(x, y);
	} else if (is_vector(p)) {
		int i, len = (int)ivalue(p);
		mark_x = cons(p, mark_x);
		y = NIL;
		for (i = 0; i < len; i++) {
			mark_y = cons(y, mark_y);
			x = expandsymbol(vector_elem(car(mark_x), i));
			y = car(mark_y);
			mark_y = cdr(mark_y);
			y = cons(x, y);
		}
		mark_x = cdr(mark_x);
		mark_y = cons(y, mark_y);
		i = list_length(y);
		x = mk_vector(i);
		y = car(mark_y);
		mark_y = cdr(mark_y);
		for (; y != NIL; y = cdr(y)) {
			set_vector_elem(x, --i, car(y));
		}
		return x;
	} else {
		return p;
	}
}

static pointer expandpattern(pointer p, int d, int n, int *e)
{
	pointer x, y = NULL;
	int i, j;
	*((int *)strvalue(car(code)) + d) = n;
	if (is_symbol(p)) {
		int find = 0;
		if (syntaxnum(p) & T_DEFSYNTAX) p = cdr(p);
		for (x = cdr(value); x != NIL; x = cdr(x)) {
			if (p == car(x)) {
				return p;
			}
		}
		for (i = 0; i < ivalue(car(value)); i += 3) {
			int e_d, e_n;
			x = vector_elem(car(value), i);
			if (x == p) {
				find = 1;
			}
			e_d = (int)ivalue(car(vector_elem(car(value), i + 1)));
			e_n = (int)ivalue(cdr(vector_elem(car(value), i + 1)));
			if (e_d < d) {
				if (*((int *)strvalue(car(code)) + e_d) == e_n) {
					*((int *)strvalue(cdr(code)) + e_d) = 1;
					if (p == x) {
						for (j = 0; j < e_d; j++) {
							if (*((int *)strvalue(cdr(code)) + j) == 0) break;
						}
						if (j < e_d) continue;
						if (*e < e_d) {
							*e = e_d;
						}
						y = vector_elem(car(value), i + 2);
						if (y == NULL) continue;
						y = expandsymbol(y);
					}
				} else {
					*((long *)strvalue(cdr(code)) + e_d) = 0;
				}
			} else if (p == x && e_d == d && e_n == n) {
				for (j = 0; j < d; j++) {
					if (*((int *)strvalue(cdr(code)) + j) == 0) break;
				}
				if (j < d) continue;
				if (*e < d) {
					*e = d;
				}
				y = vector_elem(car(value), i + 2);
				if (y == NULL) return NIL;
				return expandsymbol(y);
			}
		}
		if (d > 0 && find) {
			return y;
		}
		return p;
	} else if (is_pair(p)) {
		mark_x = cons(p, mark_x);
		if (is_pair(cdar(mark_x)) && is_ellipsis(car(cdar(mark_x)))) {
			if (expandpattern(caar(mark_x), d, n, e) == NULL) {
				mark_x = cdr(mark_x);
				return NULL;
			}
			y = NIL;
			for (i = 0; ; i++) {
				mark_y = cons(y, mark_y);
				*e = 0;
				x = expandpattern(caar(mark_x), d + 1, i, e);
				y = car(mark_y);
				mark_y = cdr(mark_y);
				if (x == NULL || *e < d + 1) break;
				y = cons(x, y);
			}
			if (is_pair(cdar(mark_x))) {
				mark_y = cons(y, mark_y);
				x = expandpattern(cdr(cdar(mark_x)), d, n, e);
				y = car(mark_y);
				mark_y = cdr(mark_y);
			}
			mark_x = cdr(mark_x);
			if (x == NULL) {
				x = NIL;
			}
			return non_alloc_rev(x, y);
		}
		x = expandpattern(caar(mark_x), d, n, e);
		mark_y = cons(x, mark_y);
		y = expandpattern(cdar(mark_x), d, n, e);
		mark_x = cdr(mark_x);
		x = car(mark_y);
		mark_y = cdr(mark_y);
		if (x == NULL || y == NULL) {
			return NULL;
		}
		return cons(x, y);
	} else if (is_vector(p)) {
		int len = (int)ivalue(p);
		mark_x = cons(p, mark_x);
		y = NIL;
		for (i = 0; i < len; i++) {
			if (i + 1 < len && is_ellipsis(vector_elem(car(mark_x), i + 1))) {
				if (expandpattern(vector_elem(car(mark_x), i), d, n, e) == NULL) {
					mark_x = cdr(mark_x);
					return NULL;
				}
				for (j = 0; ; j++) {
					p = car(mark_x);
					mark_y = cons(y, mark_y);
					*e = 0;
					x = expandpattern(vector_elem(car(mark_x), i), d + 1, j, e);
					y = car(mark_y);
					mark_y = cdr(mark_y);
					if (x == NULL || *e < d + 1) break;
					y = cons(x, y);
				}
				i++;
				continue;
			}
			mark_y = cons(y, mark_y);
			x = expandpattern(vector_elem(car(mark_x), i), d, n, e);
			y = car(mark_y);
			mark_y = cdr(mark_y);
			if (x == NULL) {
				mark_x = cdr(mark_x);
				return NULL;
			}
			y = cons(x, y);
		}
		mark_x = cdr(mark_x);
		i = list_length(y);
		mark_y = cons(y, mark_y);
		x = mk_vector(i);
		y = car(mark_y);
		mark_y = cdr(mark_y);
		for (; y != NIL; y = cdr(y)) {
			set_vector_elem(x, --i, car(y));
		}
		return x;
	} else {
		return p;
	}
}

/* make cons list for quasiquote */
static pointer mcons(pointer f, pointer l, pointer r)
{
	pointer x;

	if (is_pair(r) && car(r) == QUOTE && cadr(r) == cdr(f) &&
		is_pair(l) && car(l) == QUOTE && cadr(l) == cdr(f)) {
		x = cons(f, NIL);
		return cons(QUOTE, x);
	} else {
		args = l;
		x = cons(r, NIL);
		args = cons(args, x);
		x = mk_symbol("cons");
		return cons(x, args);
	}
}

/* make append list for quasiquote */
static pointer mappend(pointer f, pointer l, pointer r)
{
	pointer x;

	if (car(f) == NIL ||
		(is_pair(r) && car(l) == QUOTE && cadr(r) == NIL)) {
		return l;
	} else {
		args = l;
		x = cons(r, NIL);
		args = cons(args, x);
		x = mk_symbol("append");
		return cons(x, args);
	}
}


/* Error macro */
#define	BEGIN	do {
#define	END	} while (0)

#define Error_0(s) BEGIN                       \
	args = cons(mk_string((s)), NIL);          \
	location = LOC_ERROR;                        \
	goto LOOP; END

#define Error_1(s, a) BEGIN                    \
	args = cons((a), NIL);                     \
	code = mk_string(s);                       \
	args = cons(code, args);                   \
	location = LOC_ERROR;                        \
	goto LOOP; END

/* control macros for Eval_Cycle */
#define s_goto(a) BEGIN                        \
	location = (int)(a);                       \
	goto LOOP; END

#ifndef USE_SCHEME_STACK

#define s_save(a, b, c) BEGIN                  \
	if (dump_prev(dump) == NIL) {              \
		pointer d = mk_dumpstack(dump);        \
		dump_prev(dump) = d;                   \
	}                                          \
	dump_op(dump) = (pointer)(intptr_t)(a);    \
	dump_args(dump) = (b);                     \
	dump_envir(dump) = envir;                  \
	dump_code(dump) = (c);                     \
	dump = dump_prev(dump); END

#define s_return(a) BEGIN                      \
	value = (a);                               \
	if (dump == dump_base) return 0;           \
	dump = dump_next(dump);                    \
	location = (int)(intptr_t)dump_op(dump);   \
	args = dump_args(dump);                    \
	envir = dump_envir(dump);                  \
	code = dump_code(dump);                    \
	goto LOOP; END

#define s_next_op() ((int)(intptr_t)dump_op(dump_next(dump)))

static pointer s_clone(pointer d) {
	pointer p;

	if (d == NIL) return dump_base;

	p = s_clone(cddddr(d));
	dump_op(p) = (pointer)(intptr_t)ivalue(car(d));
	dump_args(p) = cadr(d);
	dump_envir(p) = caddr(d);
	dump_code(p) = cadddr(d);
	return dump_prev(p);
}

static pointer s_clone_save(void) {
	pointer p = NIL;

	for (mark_x = dump_base; mark_x != dump; mark_x = dump_prev(mark_x)) {
		p = cons(dump_code(mark_x), p);
		p = cons(dump_envir(mark_x), p);
		args = cons(dump_args(mark_x), p);
		p = mk_integer((int32_t)(intptr_t)dump_op(mark_x));
		p = cons(p, args);
	}
	return p;
}

#else

#define s_save(a, b, c)  (                     \
	dump = cons((c), dump),                    \
	dump = cons(envir, dump),                  \
	dump = cons((b), dump),                    \
	x = mk_integer((long)(a)),                 \
	dump = cons(x, dump))

#define s_return(a) BEGIN                      \
	value = (a);                               \
	if (dump == NIL) return 0;                 \
	location = (intptr_t)ivalue(car(dump));    \
	args = cadr(dump);                         \
	envir = caddr(dump);                       \
	code = cadddr(dump);                       \
	dump = cddddr(dump);                       \
	goto LOOP; END

#define s_next_op() ((int)ivalue(car(dump)))

#endif /* USE_SCHEME_STACK */

#define s_retbool(tf)	s_return((tf) ? T : F)

/* ========== Evaluation Cycle ========== */



#define TST_NONE 0
#define TST_ANY "\001"
#define TST_STRING "\002"
#define TST_SYMBOL "\003"
#define TST_PORT "\004"
#define TST_INPORT "\005"
#define TST_OUTPORT "\006"
#define TST_ENVIRONMENT "\007"
#define TST_PAIR "\010"
#define TST_LIST "\011"
#define TST_CHAR "\012"
#define TST_VECTOR "\013"
#define TST_NUMBER "\014"
#define TST_INTEGER "\015"
#define TST_NATURAL "\016"

char msg[256];

static int validargs(char *name, int min_arity, int max_arity, char *arg_tests)
{
	pointer x;
	int n = 0, i = 0;

	for (x = args; is_pair(x); x = cdr(x)) {
		++n;
	}

	if (n < min_arity) {
		snprintf(msg, sizeof(msg), "Call error - '%s' requires%s %d argument(s)",
			name, min_arity == max_arity ? "" : " at least", min_arity);
		return 0;
	} else if (n > max_arity) {
		snprintf(msg, sizeof(msg), "Call error - '%s' requires%s %d argument(s)",
			name, min_arity == max_arity ? "" : " at most", max_arity);
		return 0;
	} else if (arg_tests) {
		for (x = args; i++ < n; x = cdr(x)) {
			switch (arg_tests[0]) {
			case '\001': /* TST_ANY */
				break;
			case '\002': /* TST_STRING */
				if (!is_string(car(x))) {
				    snprintf(msg, sizeof(msg), "Type error - argument %d in call to '%s' is not a string", i, name);
					return 0;
				}
				break;
			case '\003': /* TST_SYMBOL */
				if (!is_symbol(car(x))) {
				    snprintf(msg, sizeof(msg), "Type error - argument %d in call to '%s' is not a symbol", i, name);
					return 0;
				}
				break;
			case '\004': /* TST_PORT */
				if (!is_port(car(x))) {
				    snprintf(msg, sizeof(msg), "Type error - argument %d in call to '%s' is not a port", i, name);
					return 0;
				}
				break;
			case '\005': /* TST_INPORT */
				if (!is_inport(car(x))) {
				    snprintf(msg, sizeof(msg), "Type error - argument %d in call to '%s' is not a port", i, name);
					return 0;
				}
				break;
			case '\006': /* TST_OUTPORT */
				if (!is_outport(car(x))) {
				    snprintf(msg, sizeof(msg), "Type error - argument %d in call to '%s' is not a port", i, name);
					return 0;
				}
				break;
			case '\007': /* TST_ENVIRONMENT */
				if (!is_environment(car(x))) {
				    snprintf(msg, sizeof(msg), "Type error - argument %d in call to '%s' is not an environment", i, name);
					return 0;
				}
				break;
			case '\010': /* TST_PAIR */
				if (!is_pair(car(x))) {
				    snprintf(msg, sizeof(msg), "Type error - argument %d in call to '%s' is not a pair", i, name);
					return 0;
				}
				break;
			case '\011': /* TST_LIST */
				if (!is_pair(car(x)) && car(x) != NIL) {
				    snprintf(msg, sizeof(msg), "Type error - argument %d in call to '%s' is not a pair or '()", i, name);
					return 0;
				}
				break;
			case '\012': /* TST_CHAR */
				if (!is_character(car(x))) {
				    snprintf(msg, sizeof(msg), "Type error - argument %d in call to '%s' is not a character", i, name);
					return 0;
				}
				break;
			case '\013': /* TST_VECTOR */
				if (!is_vector(car(x))) {
				    snprintf(msg, sizeof(msg), "Type error - argument %d in call to '%s' is not a vector", i, name);
					return 0;
				}
				break;
			case '\014': /* TST_NUMBER */
				if (!is_number(car(x))) {
				    snprintf(msg, sizeof(msg), "Type error - argument %d in call to '%s' is not a number", i, name);
					return 0;
				}
				break;
			case '\015': /* TST_INTEGER */
				if (!is_integer(car(x))) {
				    snprintf(msg, sizeof(msg), "Type error - argument %d in call to '%s' is not an integer", i, name);
					return 0;
				}
				break;
			case '\016': /* TST_NATURAL */
				if (!is_integer(car(x)) || ivalue(car(x)) < 0) {
					snprintf(msg, sizeof(msg), "Type error - argument %d in call to '%s' is not an integer", i, name);
					return 0;
				}
				break;
			default:
				break;
			}

			if (arg_tests[1] != 0) { /* last test is replicated as necessary */
				arg_tests++;
			}
		}
	}

	return 1;
}

/* kernel of this intepreter */
static int Eval_Cycle(enum eval_location location)
{
	int tok = 0;
	int print_flag = 0;
	pointer x, y;
	struct cell v;
	int64_t w;

LOOP:
	c_sink = NIL;

	switch (location) {
	case LOC_EVAL:		/* main part of evalution */
	    if (is_syntax(code)) {	/* syntax */
		
		if (syntaxnum(code) & T_DEFSYNTAX) {
		    args = car(code);
		    code = cdr(code);
		    for (x = car(envir); x != NIL; x = cdr(x)) {
			if ((syntaxnum(caar(x)) & T_DEFSYNTAX) && cdr(caar(x)) == code) {
			    s_return(cdar(x));
			}
		    }
		    for (x = args; x != NIL; x = cdr(x)) {
			if (syntaxnum(x) & T_DEFSYNTAX) {
			    for (y = car(x); y != NIL; y = cdr(y)) {
				if ((syntaxnum(caar(y)) & T_DEFSYNTAX) && cdr(caar(y)) == code) {
				    s_return(cdar(y));
				}
			    }
			} else {
			    for (y = car(x); y != NIL; y = cdr(y)) {
				if ((syntaxnum(caar(y)) & T_DEFSYNTAX ? cdr(caar(y)) : caar(y)) == code) {
				    s_return(cdar(y));
				}
			    }
			}
		    }
		    for (x = cdr(envir); x != NIL; x = cdr(x)) {
			for (y = car(x); y != NIL; y = cdr(y)) {
			    if (caar(y) == code) {
				s_return(cdar(y));
			    }
			}
		    }
		} else {
		    s_return(code); 
		}
	    }
			
	    else if (is_symbol(code)) {	/* symbol */
		for (x = envir; x != NIL; x = cdr(x)) {
		    pointer z = NIL;
		    for (y = car(x); y != NIL; z = y, y = cdr(y)) {
			if (caar(y) == code && cdar(y) != UNDEF) {
			    if (z != NIL) {
				cdr(z) = cdr(y);
				cdr(y) = car(x);
				car(x) = y;
			    }
			    s_return(cdar(y));
			}
		    }
		}
		Error_1("unbound variable", code);

	    } else if (is_pair(code) && !is_environment(code)) {
		s_save(LOC_E0ARGS, NIL, code);

		set_vector_elem(call_history, call_history_pos++, code);
		if( call_history_pos == CALL_HISTORY_LENGTH ) call_history_pos = 0;

		code = car(code);
		s_goto(LOC_EVAL);
		/* } else if (is_null(code)) { */
		/*     Error_1("illegal expression", code); */
	    } else {
		s_return(code);
	    }

	case LOC_E0ARGS:	/* eval arguments */
		if (is_syntax(value)) {
			if (syntaxnum(value) & T_DEFSYNTAX) {
				value = cdr(value);
			}
			code = cdr(code);
			s_goto( syntaxnum(value) & T_SYNTAXNUM );
		}
		if (is_closure(value) && is_macro(value)) {	/* macro expansion */
			if (syntaxnum(value) & T_DEFSYNTAX) {
				args = cons(NIL, envir);
				envir = closure_env(value);
				s_save(LOC_DOMACRO, NIL, NIL);
				if (is_symbol(caar(value))) {
					x = cons(caar(value), ELLIPSIS);
					x = cons(x, NIL);
					envir = cons(x, envir);
					setenvironment(envir);
					car(value) = cdar(value);
				}
				s_goto(LOC_EXPANDPATTERN);
			} else if (exttype(value) & T_DEFMACRO) {
				args = cdr(code);
			} else {
				args = cons(code, NIL);
			}
			code = value;
			s_save(LOC_DOMACRO, NIL, NIL);
			s_goto(LOC_APPLY);
		}
		code = cdr(code);
		/* fall through */

	case LOC_E1ARGS:	/* eval arguments */
		if (is_pair(code)) {	/* continue */
			args = cons(value, args);
			s_save(LOC_E1ARGS, args, cdr(code));
			code = car(code);
			args = NIL;
			s_goto(LOC_EVAL);
		}
		/* end */
		short_reverse();
		/* fall through */

	case LOC_APPLY:		/* apply 'code' to 'args' */

		if (is_operation(code)) {	/* OPERATION */
		    s_goto( op_loc(code) );
		} else if (is_function(code)) {	/* FUNCTION */
			push_recent_alloc(args);
			x = (foreignfnc(code))(args);
			if( is_fftailcall( x ))
			{
			    code = car( x );
			    args = cdr( x );
			    s_goto( LOC_APPLY );
			}
			s_return(x);
		} else if (is_closure(code)) {	/* CLOSURE */
			/* make environment */

		    if( is_synlam( code ))
		    {
			args = cons( envir, args );
			envir = cons(NIL, closure_env(code));
		    }
		    else if( is_macro( code ))
		    {
			x = cons( cons( cons( mk_symbol( "_expansion-environment" ), envir ), NIL ), NIL );
			setenvironment( x );
			y = mk_closure( cons( NIL, cons( mk_symbol( "_expansion-environment" ), NIL )), x );
			x = cons( cons( mk_symbol( "expansion-environment" ), y ), NIL );

			envir = cons(x, closure_env(code));
		    }
		    else
		    {
			envir = cons(NIL, closure_env(code));
		    }

			setenvironment(envir);
			for (mark_x = car(closure_code(code));
			     is_pair(mark_x); mark_x = cdr(mark_x), args = cdr(args)) {
				if (args == NIL) {
					Error_0("too few arguments");
				} else {
					y = cons(car(mark_x), car(args));
					y = cons(y, car(envir));
					car(envir) = y;
				}
			}
			if (mark_x == NIL) {
				/*--
				 * if (args != NIL) {
				 * 	Error_0("too many arguments");
				 * }
				 */
			} else if (is_symbol(mark_x)) {
				mark_x = cons(mark_x, args);
				mark_x = cons(mark_x, car(envir));
				car(envir) = mark_x;
			} else {
				Error_0("bad syntax in closure");
			}
			code = cdr(closure_code(code));
			args = NIL;
			s_goto(LOC_BEGIN);
		} else if (is_continuation(code)) {	/* CONTINUATION */
			code = cont_dump(code);
			if (winders != car(code)) {
				s_save(LOC_APPLYCONT, args, code);
				args = winders;
				code = car(code);
				s_goto(LOC_DOWINDS0);
			}
			s_goto(LOC_APPLYCONT);
		} else {
		    Error_1("call of non-procedure", code);
		}

	case LOC_APPLYCONT:

#ifndef USE_SCHEME_STACK
		dump = s_clone(cdr(code));
#else
		dump = cdr(code);
#endif
		w = s_next_op();
		if (w == LOC_WITHVALUES1 || w == LOC_RECEIVE1 || w == LOC_DEFVALS1) {
			type(args) |= T_VALUES;
			s_return(args);
		} else {
			s_return(args != NIL ? car(args) : NIL);
		}
	default:
		break;
	}

	switch (location) {

	case LOC_T0LVL:	/* top level */

		if (port_file(current_inport) == NULL || is_eofport(current_inport)) {
			if (is_fileport(current_inport) && port_file(current_inport) != stdin && port_file(current_inport) != NULL) {
				fclose(port_file(current_inport));
				port_file(current_inport) = NULL;
			}
			if (load_files == 1) {
				break;
			}
			current_inport = load_stack[--load_files];
		}
		if (port_file(current_inport) == stdin) {
			putstr("\n");
		}
#ifndef USE_SCHEME_STACK
		dump = dump_base;
#else
		dump = NIL;
#endif
		envir = global_env;
		s_save(LOC_VALUEPRINT, NIL, NIL);
		s_save(LOC_T1LVL, NIL, NIL);
		if (port_file(current_inport) == stdin) {
			printf(prompt);
		}
		s_goto(LOC_READ_INTERNAL);

	case LOC_T1LVL:	/* top level */
		code = value;
		s_goto(LOC_EVAL);

	case LOC_READ_INTERNAL:		/* read internal */

		tok = token();
		s_goto(LOC_RDSEXPR);

	case LOC_VALUEPRINT:	/* print evalution result */
		print_flag = 1;
		args = value;
		if (port_file(current_inport) == stdin) {
			s_save(LOC_T0LVL, NIL, NIL);
			s_goto(LOC_P0LIST);
		} else {
			s_goto(LOC_T0LVL);
		}

	case LOC_DOMACRO:	/* do macro */
		code = value;
		s_goto(LOC_EVAL);

	case LOC_GENSYM:
		if (!validargs("gensym", 0, 1, TST_STRING)) Error_0(msg);
		if( is_null( car( args )))
		    s_return(gensym("gensym_"));
		else 
		    s_return( gensym( strvalue( car( args ))));

	case LOC_LAMBDA:	/* lambda */
		s_return(mk_closure(code, envir));

	case LOC_MKCLOSURE:	/* make-closure */
		if (!validargs("make-closure", 1, 2, TST_PAIR TST_ENVIRONMENT)) Error_0(msg);
		x = car(args);
		if (car(x) == LAMBDA) {
			x = cdr(x);
		}
		if (cdr(args) == NIL) {
			y = envir;
		} else {
			y = cadr(args);
		}
		s_return(mk_closure(x, y));

	case LOC_QUOTE:		/* quote */
		s_return(car(code));

	case LOC_QQUOTE0:	/* quasiquote */
		args = mk_integer(0);
		code = car(code);
		s_save(LOC_QQUOTE9, NIL, NIL);
		/* fall through */

	case LOC_QQUOTE1:	/* quasiquote -- expand */

		if (is_vector(code)) {
			s_save(LOC_QQUOTE2, NIL, NIL);
			x = NIL;
			for (w = ivalue(code) - 1; w >= 0; w--) {
				x = cons(vector_elem(code, (int)w), x);
			}
			code = x;
			s_goto(LOC_QQUOTE1);
		} else if (!is_pair(code)) {
			x = cons(code, NIL);
			s_return(cons(QUOTE, x));
		} else if (QQUOTE == car(code)) {
			s_save(LOC_QQUOTE3, NIL, NIL);
			args = mk_integer(ivalue(args) + 1);
			code = cdr(code);
			s_goto(LOC_QQUOTE1);
		} else if (ivalue(args) > 0) {
			if (UNQUOTE == car(code)) {
				s_save(LOC_QQUOTE4, NIL, NIL);
				args = mk_integer(ivalue(args) - 1);
				code = cdr(code);
				s_goto(LOC_QQUOTE1);
			} else if (UNQUOTESP == car(code)) {
				s_save(LOC_QQUOTE5, NIL, NIL);
				args = mk_integer(ivalue(args) - 1);
				code = cdr(code);
				s_goto(LOC_QQUOTE1);
			} else {
				s_save(LOC_QQUOTE6, args, code);
				code = car(code);
				s_goto(LOC_QQUOTE1);
			}
		} else {
			if (UNQUOTE == car(code)) {
				s_return(cadr(code));
			} else if (UNQUOTESP == car(code)) {
				Error_1("unquote-splicing outside list:", code);
			} else if (is_pair(car(code)) && UNQUOTESP == caar(code)) {
				s_save(LOC_QQUOTE8, NIL, code);
				code = cdr(code);
				s_goto(LOC_QQUOTE1);
			} else {
				s_save(LOC_QQUOTE6, args, code);
				code = car(code);
				s_goto(LOC_QQUOTE1);
			}
		}

	case LOC_QQUOTE2:	/* quasiquote -- 'vector */
		args = cons(value, NIL);
		x = mk_symbol("vector");
		args = cons(x, args);
		x = mk_symbol("apply");
		s_return(cons(x, args));

	case LOC_QQUOTE3:	/* quasiquote -- 'quasiquote */
		x = cons(QQUOTE, NIL);
		x = cons(QUOTE, x);
		s_return(mcons(code, x, value));

	case LOC_QQUOTE4:	/* quasiquote -- 'unquote */
		x = cons(UNQUOTE, NIL);
		x = cons(QUOTE, x);
		s_return(mcons(code, x, value));

	case LOC_QQUOTE5:	/* quasiquote -- 'unquote-splicing */
		x = cons(UNQUOTESP, NIL);
		x = cons(QUOTE, x);
		s_return(mcons(code, x, value));

	case LOC_QQUOTE6:	/* quasiquote -- 'cons */
		s_save(LOC_QQUOTE7, value, code);
		code = cdr(code);
		s_goto(LOC_QQUOTE1);

	case LOC_QQUOTE7:	/* quasiquote -- 'cons */
		s_return(mcons(code, args, value));

	case LOC_QQUOTE8:	/* quasiquote -- 'append */
		s_return(mappend(code, cadar(code), value));

	case LOC_QQUOTE9:	/* quasiquote -- return */
		code = value;
		s_goto(LOC_EVAL);

	case LOC_DEF0:	/* define */
		if (is_pair(car(code))) {
			y = cons(cdar(code), cdr(code));
			/* y = cons(LAMBDA, y); */
			args = caar(code);
			/* code = y; */
			code = mk_closure( y, envir );
		} else {
			args = car(code);
			code = cadr(code);
		}
		if (!is_symbol(args)) {
			Error_0("variable is not symbol");
		}
		s_save(LOC_DEF1, NIL, args);
		args = NIL;
		s_goto(LOC_EVAL);

	case LOC_DEF1:	/* define */
		if (syntaxnum(code) & T_DEFSYNTAX) {
			args = car(code);
			code = cdr(code);
			for (x = car(envir); x != NIL; x = cdr(x)) {
				if ((syntaxnum(caar(x)) & T_DEFSYNTAX) && cdr(caar(x)) == code) {
					break;
				}
			}
			if (x == NIL) {
				envir = args;
				if (syntaxnum(envir) & T_DEFSYNTAX) {
					for (x = car(envir); x != NIL; x = cdr(x)) {
						if ((syntaxnum(caar(x)) & T_DEFSYNTAX) && cdr(caar(x)) == code) {
							break;
						}
					}
				} else {
					for (x = car(envir); x != NIL; x = cdr(x)) {
						if ((syntaxnum(caar(x)) & T_DEFSYNTAX ? cdr(caar(x)) : caar(x)) == code) {
							break;
						}
					}
				}
			}
		} else {
			for (x = car(envir); x != NIL; x = cdr(x)) {
				if (caar(x) == code) {
					break;
				}
			}
		}
		if (x != NIL) {
			cdar(x) = value;
		} else {
			x = cons(code, value);
			x = cons(x, car(envir));
			car(envir) = x;
		}
		s_return( value );

	case LOC_DEFP:	/* defined? */
		if (!validargs("defined?", 1, 2, TST_SYMBOL TST_ENVIRONMENT)) Error_0(msg);
		code = car(args);
		if (syntaxnum(code) & T_DEFSYNTAX) {
			if (cdr(args) != NIL) {
				args = cadr(args);
			} else {
				args = envir;
			}
			if (args == envir) {
				for (x = car(envir); x != NIL; x = cdr(x)) {
					if ((syntaxnum(caar(x)) & T_DEFSYNTAX) && cdr(caar(x)) == cdr(code)) {
						s_return(T);
					}
				}
			}
			if (args == envir || args == caar(code)) {
				for (x = car(code); x != NIL; x = cdr(x)) {
					if (syntaxnum(x) & T_DEFSYNTAX) {
						for (y = car(x); y != NIL; y = cdr(y)) {
							if ((syntaxnum(caar(y)) & T_DEFSYNTAX) && cdr(caar(y)) == code) {
								s_return(T);
							}
						}
					} else {
						for (y = car(x); y != NIL; y = cdr(y)) {
							if ((syntaxnum(caar(y)) & T_DEFSYNTAX ? cdr(caar(y)) : caar(y)) == cdr(code)) {
								s_return(T);
							}
						}
					}
				}
				args = cdr(envir);
			}
			for (x = args; x != NIL; x = cdr(x)) {
				for (y = car(x); y != NIL; y = cdr(y)) {
					if (caar(y) == cdr(code)) {
						s_return(T);
					}
				}
			}
		} else {
			if (cdr(args) != NIL) {
				envir = cadr(args);
			}
			for (x = envir; x != NIL; x = cdr(x)) {
				for (y = car(x); y != NIL; y = cdr(y)) {
					if (caar(y) == code) {
						s_return(T);
					}
				}
			}
		}
		s_return(F);

	case LOC_SET0:		/* set! */
		s_save(LOC_SET1, NIL, car(code));
		code = cadr(code);
		s_goto(LOC_EVAL);

	case LOC_SET1:		/* set! */
		if (syntaxnum(code) & T_DEFSYNTAX) {
			args = car(code);
			code = cdr(code);
			for (x = car(envir); x != NIL; x = cdr(x)) {
				if ((syntaxnum(caar(x)) & T_DEFSYNTAX) && cdr(caar(x)) == code) {
					s_return(cdar(x) = value);
				}
			}
			for (x = args; x != NIL; x = cdr(x)) {
				if (syntaxnum(x) & T_DEFSYNTAX) {
					for (y = car(x); y != NIL; y = cdr(y)) {
						if ((syntaxnum(caar(y)) & T_DEFSYNTAX) && cdr(caar(y)) == code) {
							s_return(cdar(y) = value);
						}
					}
				} else {
					for (y = car(x); y != NIL; y = cdr(y)) {
						if ((syntaxnum(caar(y)) & T_DEFSYNTAX ? cdr(caar(y)) : caar(y)) == code) {
							s_return(cdar(y) = value);
						}
					}
				}
			}
			for (x = cdr(envir); x != NIL; x = cdr(x)) {
				for (y = car(x); y != NIL; y = cdr(y)) {
					if (caar(y) == code) {
						s_return(cdar(y) = value);
					}
				}
			}
		} else {
			for (x = envir; x != NIL; x = cdr(x)) {
				for (y = car(x); y != NIL; y = cdr(y)) {
					if (caar(y) == code) {
						s_return(cdar(y) = value);
					}
				}
			}
		}
		Error_1("unbound variable", code);

	case LOC_BEGIN:		/* begin */

		if (!is_pair(code)) {
			s_return(code);
		}
		if (cdr(code) != NIL) {
			s_save(LOC_BEGIN, NIL, cdr(code));
		}
		code = car(code);
		s_goto(LOC_EVAL);

	case LOC_IF0:		/* if */
		s_save(LOC_IF1, NIL, cdr(code));
		code = car(code);
		s_goto(LOC_EVAL);

	case LOC_IF1:		/* if */
		if (istrue(value))
			code = car(code);
		else
		    if( is_pair( cdr( code )))
			code = cadr( code );
		    else
			code = F;
			/* code = cadr(code);	/\* (if #f 1) ==> () because */
			/* 			 * car(NIL) = NIL *\/ */
		s_goto(LOC_EVAL);

	case LOC_LET0:		/* let */
		args = NIL;
		value = code;
		code = is_symbol(car(code)) ? cadr(code) : car(code);
		if (code != NIL && !is_pair(code)) {
			Error_1("bad binding syntax in let :", code);
		}
		/* fall through */

	case LOC_LET1:		/* let (caluculate parameters) */
		if (is_pair(code)) {	/* continue */
			args = cons(value, args);
			s_save(LOC_LET1, args, cdr(code));
			if (!is_pair(car(code)) || !is_pair(cdar(code))) {
				Error_1("bad binding syntax in let :", car(code));
			}
			code = cadar(code);
			args = NIL;
			s_goto(LOC_EVAL);
		}
		/* end */
		short_reverse();

		envir = cons(NIL, envir);
		setenvironment(envir);
		for (mark_x = is_symbol(car(code)) ? cadr(code) : car(code);
		     args != NIL; mark_x = cdr(mark_x), args = cdr(args)) {
			y = cons(caar(mark_x), car(args));
			y = cons(y, car(envir));
			car(envir) = y;
		}
		if (is_symbol(car(code))) {	/* named let */
			for (mark_x = cadr(code), y = NIL; mark_x != NIL; mark_x = cdr(mark_x))
				y = cons(caar(mark_x), y);
			y = cons(non_alloc_rev(NIL, y), cddr(code));
			y = mk_closure(y, envir);
			y = cons(car(code), y);
			y = cons(y, car(envir));
			car(envir) = y;
			code = cddr(code);
		} else {
			code = cdr(code);
		}
		args = NIL;
		s_goto(LOC_BEGIN);

	case LOC_LET0AST:	/* let* */
		if (car(code) == NIL) {
			envir = cons(NIL, envir);
			setenvironment(envir);
			code = cdr(code);
			s_goto(LOC_BEGIN);
		}
		if (!is_pair(car(code)) || !is_pair(caar(code))) {
			Error_1("bad binding syntax in let* :", car(code));
		}
		s_save(LOC_LET1AST, cdr(code), car(code));
		code = cadaar(code);
		s_goto(LOC_EVAL);

	case LOC_LET1AST:	/* let* (make new frame) */
		envir = cons(NIL, envir);
		setenvironment(envir);
		/* fall through */

	case LOC_LET2AST:	/* let* (caluculate parameters) */
		x = cons(caar(code), value);
		x = cons(x, car(envir));
		car(envir) = x;
		code = cdr(code);
		if (is_pair(code)) {	/* continue */
			s_save(LOC_LET2AST, args, code);
			if (!is_pair(car(code)) || !is_pair(cdar(code))) {
				Error_1("bad binding syntax in let* :", car(code));
			}
			code = cadar(code);
			args = NIL;
			s_goto(LOC_EVAL);
		} else {	/* end */
			code = args;
			args = NIL;
			s_goto(LOC_BEGIN);
		}

	case LOC_LET0REC:	/* letrec */
		envir = cons(NIL, envir);
		setenvironment(envir);
		if (!is_pair(car(code))) {
			Error_1("bad binding syntax in letrec :", car(code));
		}
		for (mark_x = car(code); is_pair(mark_x); mark_x = cdr(mark_x)) {
			if (!is_pair(car(mark_x))) {
				Error_1("bad binding syntax in letrec :", car(mark_x));
			}
			y = cons(caar(mark_x), UNDEF);
			car(envir) = cons(y, car(envir));
		}
		args = NIL;
		value = code;
		code = car(code);
		/* fall through */

	case LOC_LET1REC:	/* letrec (caluculate parameters) */
		if (is_pair(code)) {	/* continue */
			args = cons(value, args);
			s_save(LOC_LET1REC, args, cdr(code));
			if (!is_pair(car(code)) || !is_pair(cdar(code))) {
				Error_1("bad binding syntax in letrec :", car(code));
			}
			code = cadar(code);
			args = NIL;
			s_goto(LOC_EVAL);
		}
		/* end */
		short_reverse();

		for (x = car(code); args != NIL; x = cdr(x), args = cdr(args)) {
			for (y = car(envir); y != NIL; y = cdr(y)) {
				if (caar(y) == caar(x)) {
					cdar(y) = car(args);
				}
			}
		}
		code = cdr(code);
		args = NIL;
		s_goto(LOC_BEGIN);

	case LOC_LETRECAST0:	/* letrec* */
		envir = cons(NIL, envir);
		setenvironment(envir);
		if (car(code) == NIL) {
			code = cdr(code);
			s_goto(LOC_BEGIN);
		}
		if (!is_pair(car(code))) {
			Error_1("bad binding syntax in letrec* :", car(code));
		}
		for (mark_x = car(code); is_pair(mark_x); mark_x = cdr(mark_x)) {
			if (!is_pair(car(mark_x))) {
				Error_1("bad binding syntax in letrec* :", car(mark_x));
			}
			y = cons(caar(mark_x), UNDEF);
			car(envir) = cons(y, car(envir));
		}
		if (!is_pair(caar(code)) || !is_pair(cdr(caar(code)))) {
			Error_1("bad binding syntax in letrec* :", caar(code));
		}
		s_save(LOC_LETRECAST1, cdr(code), car(code));
		code = cadaar(code);
		s_goto(LOC_EVAL);

	case LOC_LETRECAST1:	/* letrec* (caluculate parameters) */
		for (y = car(envir); y != NIL; y = cdr(y)) {
			if (caar(y) == caar(code)) {
				cdar(y) = value;
			}
		}
		code = cdr(code);
		if (is_pair(code)) {	/* continue */
			s_save(LOC_LETRECAST1, args, code);
			if (!is_pair(car(code)) || !is_pair(cdar(code))) {
				Error_1("bad binding syntax in letrec* :", car(code));
			}
			code = cadar(code);
			args = NIL;
			s_goto(LOC_EVAL);
		}
		/* end */
		code = args;
		args = NIL;
		s_goto(LOC_BEGIN);

	case LOC_DO0:		/* do */
		envir = cons(NIL, envir);
		setenvironment(envir);
		args = NIL;
		value = code;
		code = car(code);
		if (!is_pair(code)) {
			Error_1("bad binding syntax in do :", code);
		}
		/* fall through */

	case LOC_DO1:		/* do -- init */
		if (is_pair(code)) {
			args = cons(value, args);
			s_save(LOC_DO1, args, cdr(code));
			if (!is_pair(car(code)) || !is_pair(cdar(code))) {
				Error_1("bad binding syntax in do :", car(code));
			}
			code = cadar(code);
			args = NIL;
			s_goto(LOC_EVAL);
		}
		short_reverse();
		/* fall through */

	case LOC_DO2:		/* do -- test */

		for (mark_x = car(code); args != NIL; mark_x = cdr(mark_x), args = cdr(args)) {
			y = cons(caar(mark_x), car(args));
			y = cons(y, car(envir));
			car(envir) = y;
		}
		s_save(LOC_DO3, NIL, code);
		if (!is_pair(cadr(code))) {
			Error_1("bad binding syntax in do :", cadr(code));
		}
		code = car(cadr(code));
		s_goto(LOC_EVAL);

	case LOC_DO3:		/* do -- command */
		if (value == F) {
			s_save(LOC_DO4, NIL, code);
			code = cddr(code);
		} else {		/* expression */
			code = cdr(cadr(code));
		}
		s_goto(LOC_BEGIN);

	case LOC_DO4:		/* do -- step */
		value = code;
		code = car(code);
		/* fall through */

	case LOC_DO5:		/* do -- step */
		if (is_pair(code)) {
			args = cons(value, args);
			s_save(LOC_DO5, args, cdr(code));
			code = car(code);
			if (is_pair(cddr(code))) {
				code = caddr(code);
			} else {
				code = car(code);
			}
			args = NIL;
			s_goto(LOC_EVAL);
		}
		envir = cons(NIL, envir);
		setenvironment(envir);
		short_reverse();
		s_goto(LOC_DO2);

	case LOC_COND0:		/* cond */
		if (!is_pair(car(code))) {
			Error_1("bad syntax in cond :", car(code));
		}
		s_save(LOC_COND1, NIL, code);
		code = caar(code);
		s_goto(LOC_EVAL);

	case LOC_COND1:		/* cond */
		if (istrue(value)) {
			if ((code = cdar(code)) == NIL) {
				s_return(value);
			}
			if (cdr(code) == NIL || (is_syntax(value) && (syntaxnum(value) & T_SYNTAXNUM) == LOC_ELSE)) {
				s_goto(LOC_BEGIN);
			}
			s_save(LOC_COND2, value, cdr(code));
			code = car(code);
			s_goto(LOC_EVAL);
		}
		if ((code = cdr(code)) == NIL) {
			s_return(NIL);
		} else {
			if (!is_pair(car(code))) {
				Error_1("bad syntax in cond :", car(code));
			}
			s_save(LOC_COND1, NIL, code);
			code = caar(code);
			s_goto(LOC_EVAL);
		}

	case LOC_COND2:		/* cond */
		if (is_syntax(value) && (syntaxnum(value) & T_SYNTAXNUM) == LOC_FEEDTO) {
			x = cons(args, NIL);
			x = cons(QUOTE, x);
			x = cons(x, NIL);
			code = cons(car(code), x);
			args = NIL;
			s_goto(LOC_EVAL);
		}
		args = NIL;
		s_goto(LOC_BEGIN);

	case LOC_ELSE:		/* else */
		Error_0("bad syntax in else");

	case LOC_FEEDTO:		/* => */
		Error_0("bad syntax in =>");

	case LOC_DELAY:		/* delay */
		x = cons(NIL, code);
		code = mk_closure(x, envir);
		setpromise(code);
		setresultready(code);
		/* fall through */

	case LOC_LAZY:		/* lazy */
		x = cons(NIL, code);
		x = mk_closure(x, envir);
		setpromise(x);
		s_return(x);

	case LOC_AND0:		/* and */
		if (code == NIL) {
			s_return(T);
		}
		s_save(LOC_AND1, NIL, cdr(code));
		code = car(code);
		s_goto(LOC_EVAL);

	case LOC_AND1:		/* and */
		if (isfalse(value)) {
			s_return(value);
		} else if (code == NIL) {
			s_return(value);
		} else {
			s_save(LOC_AND1, NIL, cdr(code));
			code = car(code);
			s_goto(LOC_EVAL);
		}

	case LOC_OR0:		/* or */
		if (code == NIL) {
			s_return(F);
		}
		s_save(LOC_OR1, NIL, cdr(code));
		code = car(code);
		s_goto(LOC_EVAL);

	case LOC_OR1:		/* or */
		if (istrue(value)) {
			s_return(value);
		} else if (code == NIL) {
			s_return(value);
		} else {
			s_save(LOC_OR1, NIL, cdr(code));
			code = car(code);
			s_goto(LOC_EVAL);
		}

	case LOC_C0STREAM:	/* cons-stream */
		s_save(LOC_C1STREAM, NIL, cdr(code));
		code = car(code);
		s_goto(LOC_EVAL);

	case LOC_C1STREAM:	/* cons-stream */
		args = value;	/* save value to register args for gc */
		x = cons(NIL, code);
		x = mk_closure(x, envir);
		setpromise(x);
		s_return(cons(args, x));

	case LOC_DEFSYNLAM0:	/* define-syntax-lambda */

	    if ( !is_symbol( caar( code ))) {
		Error_0("variable is not a symbol");
	    }
	    s_save( LOC_DEFSYNLAM1, NIL, caar( code ));
	    code = cons( cadr( car( code )), cons( cadr( cdar( code )), cdr( code )));
	    /* fall through */
		
	case LOC_SYNLAM:	/* syntax-lambda */
	    code = cons( cons( car( code ), cadr( code )), cddr( code ));
	    x = mk_closure( code, envir );
	    exttype( x ) |= ( T_SYNLAM | T_DEFSYNLAM | T_MACRO | T_DEFMACRO );
	    s_return( x );

	case LOC_DEFSYNLAM1:	
	    for( x = car( envir ); x != NIL; x = cdr( x ))
		if( caar( x ) == code)
		    break;
	    if( x != NIL )
		cdar( x ) = value;
	    else {
		x = cons( code, value );
		x = cons( x, car( envir ));
		car( envir ) = x;
	    }
	    s_return( code );

	case LOC_DEFMACRO0:	/* define-macro */

	    if ( !is_symbol( caar( code ))) {
		Error_0("variable is not a symbol");
	    }
	    s_save( LOC_DEFMACRO1, NIL, caar( code ));
	    code = cons( cdar( code ), cdr( code ));
	    /* fall through */
		
	case LOC_MACRO:	/* macro */
	    x = mk_closure( code, envir );
	    exttype( x ) |= ( T_MACRO | T_DEFMACRO );
	    s_return( x );

	case LOC_DEFMACRO1:	
	    for( x = car( envir ); x != NIL; x = cdr( x ))
		if( caar( x ) == code)
		    break;
	    if( x != NIL )
		cdar( x ) = value;
	    else {
		x = cons( code, value );
		x = cons( x, car( envir ));
		car( envir ) = x;
	    }
	    s_return( code );

	case LOC_SYNTAXRULES:	/* syntax-rules */
		w = s_next_op();
		if (w != LOC_DEFSYNTAX1 && w != LOC_LETSYNTAX1 && w != LOC_LETRECSYNTAX1) {
			Error_0("malformed syntax-rules");
		}
		s_return(mk_closure(code, envir));

	case LOC_EXPANDPATTERN:	/* expand pattern */

		if (!is_pair(code)) {
			Error_0("bad syntax in syntax-rules");
		}
		for (x = cdar(value); x != NIL; x = cdr(x)) {
			if (car(x) == NIL) {
				Error_0("bad syntax in syntax-rules");
			}
			w = 0;
			if (matchpattern(cdr(caar(x)), cdr(code), caar(value), (int *)&w)) {
				int i, j, m = 0;
				car(args) = car(x);
				x = mk_vector((int)w * 3);
				value = cons(x, caar(value));
				for (i = 0; i < ivalue(car(value)); i += 3) {
					mark_x = mk_integer(0);
					mark_y = mk_integer(0);
					set_vector_elem(car(value), i + 1, cons(mark_x, mark_y));
				}
				w = 0;
				bindpattern(cdr(caar(args)), cdr(code), 0, 0, (int *)&w);
				for (i = 0; i < ivalue(car(value)); i += 3) {
					j = (int)ivalue(car(vector_elem(car(value), i + 1)));
					if (j > m) m = j;
				}
				mark_x = mk_memblock(m * sizeof(int), &NIL, &NIL);
				mark_y = mk_memblock(m * sizeof(int), &NIL, &NIL);
				code = cons(mark_x, mark_y);
				s_return(car(expandpattern(cdar(args), 0, 0, (int *)&w)));
			}
		}
		s_return(NIL);

	case LOC_DEFSYNTAX0:		/* define-syntax */
		if (!is_symbol(car(code))) {
			Error_0("variable is not a symbol");
		}
		args = car(code);
		code = cadr(code);
		s_save(LOC_DEFSYNTAX1, NIL, args);
		args = NIL;
		s_goto(LOC_EVAL);

	case LOC_DEFSYNTAX1:		/* define-syntax */
		exttype(value) |= T_MACRO;
		syntaxnum(value) |= T_DEFSYNTAX;
		for (x = car(envir); x != NIL; x = cdr(x))
			if (caar(x) == code)
				break;
		if (x != NIL) {
			cdar(x) = value;
		} else {
			x = cons(code, value);
			x = cons(x, car(envir));
			car(envir) = x;
		}
		s_return(code);

	case LOC_LETSYNTAX0:		/* let-syntax */
		args = NIL;
		value = code;
		code = car(code);
		/* fall through */

	case LOC_LETSYNTAX1:		/* let-syntax */
		if (is_pair(code)) {
			/* continue */
			args = cons(value, args);
			s_save(LOC_LETSYNTAX1, args, cdr(code));
			if (!is_pair(car(code)) || !is_pair(cdar(code))) {
				Error_1("bad binding syntax in let-syntax :", car(code));
			}
			if (!is_symbol(caar(code))) {
				Error_0("variable is not a symbol");
			}
			code = cadar(code);
			args = NIL;
			s_goto(LOC_EVAL);
		}
		/* end */
		short_reverse();

		envir = cons(NIL, envir);
		setenvironment(envir);
		for (mark_x = car(code); args != NIL; mark_x = cdr(mark_x), args = cdr(args)) {
			exttype(car(args)) |= T_MACRO;
			syntaxnum(car(args)) |= T_DEFSYNTAX;
			y = cons(caar(mark_x), car(args));
			y = cons(y, car(envir));
			car(envir) = y;
		}
		code = cdr(code);
		args = NIL;
		s_goto(LOC_BEGIN);

	case LOC_LETRECSYNTAX0:	/* letrec-syntax */
		envir = cons(NIL, envir);
		setenvironment(envir);
		args = NIL;
		value = code;
		code = car(code);
		/* fall through */

	case LOC_LETRECSYNTAX1:	/* letrec-syntax */
		if (is_pair(code)) {
			/* continue */
			args = cons(value, args);
			s_save(LOC_LETRECSYNTAX1, args, cdr(code));
			if (!is_pair(car(code)) || !is_pair(cdar(code))) {
				Error_1("bad binding syntax in letrec-syntax :", car(code));
			}
			if (!is_symbol(caar(code))) {
				Error_0("variable is not a symbol");
			}
			code = cadar(code);
			args = NIL;
			s_goto(LOC_EVAL);
		}
		/* end */
		short_reverse();

		for (mark_x = car(code); args != NIL; mark_x = cdr(mark_x), args = cdr(args)) {
			exttype(car(args)) |= T_MACRO;
			syntaxnum(car(args)) |= T_DEFSYNTAX;
			y = cons(caar(mark_x), car(args));
			y = cons(y, car(envir));
			car(envir) = y;
		}
		code = cdr(code);
		args = NIL;
		s_goto(LOC_BEGIN);

	case LOC_CASE0:		/* case */
		s_save(LOC_CASE1, NIL, cdr(code));
		code = car(code);
		s_goto(LOC_EVAL);

	case LOC_CASE1:		/* case */
		for (x = code; x != NIL; x = cdr(x)) {
			if (!is_pair(car(x))) {
				Error_1("bad syntax in case :", car(x));
			}
			if (!is_pair(y = caar(x)))
				break;
			for ( ; y != NIL; y = cdr(y))
				if (eqv(car(y), value))
					break;
			if (y != NIL)
				break;
		}
		if (x != NIL) {
			if (is_pair(caar(x))) {
				code = cdar(x);
				s_goto(LOC_BEGIN);
			} else {/* else */
				code = car(x);
				s_save(LOC_CASE2, NIL, cdr(code));
				code = car(code);
				s_goto(LOC_EVAL);
			}
		} else {
			s_return(NIL);
		}

	case LOC_CASE2:		/* case */
		if (is_syntax(value) && (syntaxnum(value) & T_SYNTAXNUM) == LOC_ELSE) {
			s_goto(LOC_BEGIN);
		} else {
			s_return(NIL);
		}

	case LOC_WHEN0:		/* when */
		s_save(LOC_WHEN1, NIL, cdr(code));
		code = car(code);
		s_goto(LOC_EVAL);

	case LOC_WHEN1:		/* when */
		if (istrue(value)) {
			s_goto(LOC_BEGIN);
		} else {
			s_return(NIL);
		}

	case LOC_UNLESS0:	/* unless */
		s_save(LOC_UNLESS1, NIL, cdr(code));
		code = car(code);
		s_goto(LOC_EVAL);

	case LOC_UNLESS1:	/* unless */
		if (isfalse(value)) {
			s_goto(LOC_BEGIN);
		} else {
			s_return(NIL);
		}

	case LOC_RECEIVE0:	/* receive */
		s_save(LOC_RECEIVE1, NIL, code);
		code = cadr(code);
		s_goto(LOC_EVAL);

	case LOC_RECEIVE1:	/* receive */
		if (type(value) & T_VALUES) {
			type(value) &= ~T_VALUES;
			args = value;
		} else {
			args = cons(value, NIL);
		}
		envir = cons(NIL, envir);
		setenvironment(envir);
		for (mark_x = car(code); is_pair(mark_x); mark_x = cdr(mark_x), args = cdr(args)) {
			if (args == NIL) {
				Error_0("too few arguments");
			} else {
				y = cons(car(mark_x), car(args));
				y = cons(y, car(envir));
				car(envir) = y;
			}
		}
		if (is_symbol(mark_x)) {
			mark_x = cons(mark_x, args);
			mark_x = cons(mark_x, car(envir));
			car(envir) = mark_x;
		}
		code = cddr(code);
		args = NIL;
		s_goto(LOC_BEGIN);

	case LOC_DEFVALS0:	/* define-values */
		s_save(LOC_DEFVALS1, NIL, code);
		code = cadr(code);
		s_goto(LOC_EVAL);

	case LOC_DEFVALS1:	
		if (type(value) & T_VALUES) {
			type(value) &= ~T_VALUES;
			args = value;
		} else {
			args = cons(value, NIL);
		}
		mark_y = args;
		for (mark_x = car(code); is_pair(mark_x); mark_x = cdr(mark_x), args = cdr(args)) {
			if (args == NIL) {
				Error_0("too few arguments");
			} else {
				y = cons(car(mark_x), car(args));
				y = cons(y, car(envir));
				car(envir) = y;
			}
		}
		if (is_symbol(mark_x)) {
			mark_x = cons(mark_x, args);
			mark_x = cons(mark_x, car(envir));
			car(envir) = mark_x;
		}
		args = mark_y;
		s_goto(LOC_VALUES);

	case LOC_PAPPLY:	/* apply */
		if (!validargs("apply", 2, 65535, TST_NONE)) Error_0(msg);
		code = car(args);
		args = cdr(args);
		for (x = args, y = car(x); cdr(x) != NIL; x = cdr(x)) {
			if (cddr(x) == NIL) {
				y = cadr(x);
				cdr(x) = NIL;
				x = NIL;
				break;
			}
		}
		if (list_length(y) < 0) {
			Error_0("apply: last argument is not a proper list");
		}
		if (x != NIL) {
			args = y;
		} else {
			args = append(args, y);
		}
		s_goto(LOC_APPLY);

	case LOC_PEVAL:	/* eval */
		if (!validargs("eval", 1, 2, TST_ANY TST_ENVIRONMENT)) Error_0(msg);
		code = car(args);
		if (is_pair(cdr(args))) envir = cadr(args);
		args = NIL;
		s_goto(LOC_EVAL);

	case LOC_MAP0:	/* map */
		if (!validargs("map", 2, 65535, TST_ANY TST_LIST)) Error_0(msg);
		code = car(args);
		/* fall through */

	case LOC_MAP1:	/* map */
		if (location == LOC_MAP0) {
			car(args) = NIL;
		} else {
			car(args) = cons(value, car(args));
		}
		mark_y = NIL;
		for (mark_x = cdr(args); mark_x != NIL; mark_x = cdr(mark_x)) {
			if (car(mark_x) == NIL) {
				s_return(non_alloc_rev(NIL, car(args)));
			}
			mark_y = cons(caar(mark_x), mark_y);
			car(mark_x) = cdar(mark_x);
		}
		s_save(LOC_MAP1, args, code);
		args = non_alloc_rev(NIL, mark_y);
		s_goto(LOC_APPLY);

	case LOC_FOREACH0:	/* for-each */
		if (!validargs("for-each", 2, 65535, TST_ANY TST_LIST)) Error_0(msg);
		code = car(args);
		args = cdr(args);
		value = T;

	case LOC_FOREACH1:	/* for-each */
		mark_y = NIL;
		for (mark_x = args; mark_x != NIL; mark_x = cdr(mark_x)) {
			if (car(mark_x) == NIL) {
			    s_return( value );
			}
			mark_y = cons(caar(mark_x), mark_y);
			car(mark_x) = cdar(mark_x);
		}
		s_save(LOC_FOREACH1, args, code);
		args = non_alloc_rev(NIL, mark_y);
		s_goto(LOC_APPLY);

	case LOC_CONTINUATION:	/* call-with-current-continuation */
		if (!validargs("call-with-current-continuation", 1, 1, TST_NONE)) Error_0(msg);
		code = car(args);
#ifndef USE_SCHEME_STACK
		args = cons(mk_continuation(cons(winders, s_clone_save())), NIL);
#else
		args = cons(mk_continuation(cons(winders, dump)), NIL);
#endif
		s_goto(LOC_APPLY);


	case LOC_VALUES:			/* values */
		if (!validargs("values", 0, 65535, TST_NONE)) Error_0(msg);
		w = s_next_op();
		if (w == LOC_WITHVALUES1 || w == LOC_RECEIVE1 || w == LOC_DEFVALS1) {
			type(args) |= T_VALUES;
			s_return(args);
		} else {
			s_return(args != NIL ? car(args) : NIL);
		}

	case LOC_WITHVALUES0:	/* call-with-values */
		if (!validargs("call-with-values", 2, 2, TST_NONE)) Error_0(msg);
		s_save(LOC_WITHVALUES1, args, code);
		code = car(args);
		args = NIL;
		s_goto(LOC_APPLY);

	case LOC_WITHVALUES1:	/* call-with-values */
		code = cadr(args);
		if (type(value) & T_VALUES) {
			type(value) &= ~T_VALUES;
			args = value;
		} else {
			args = cons(value, NIL);
		}
		s_goto(LOC_APPLY);

	case LOC_DYNAMICWIND0:	/* dynamic-wind -- before */
		if (!validargs("dynamic-wind", 3, 3, TST_NONE)) Error_0(msg);
		s_save(LOC_DYNAMICWIND1, args, code);
		code = car(args);
		args = NIL;
		s_goto(LOC_APPLY);

	case LOC_DYNAMICWIND1:	/* dynamic-wind -- body */
		x = cons(car(args), caddr(args));
		winders = cons(x, winders);
		s_save(LOC_DYNAMICWIND2, args, code);
		code = cadr(args);
		args = NIL;
		s_goto(LOC_APPLY);

	case LOC_DYNAMICWIND2:	/* dynamic-wind -- after */
		winders = cdr(winders);
		value = cons(value, args);
		s_save(LOC_DYNAMICWIND3, value, code);
		code = caddr(args);
		args = NIL;
		s_goto(LOC_APPLY);

	case LOC_DYNAMICWIND3:	/* dynamic-wind -- return */
		s_return(car(args));

	case LOC_DOWINDS0:		/* winding -- after, before */

		args = shared_tail(args, code);
		if (winders != args && winders != NIL) {
			s_save(LOC_DOWINDS2, args, code);
			code = args;
			args = winders;
			s_goto(LOC_DOWINDS1);
		}
		if (args != code && code != NIL) {
			s_goto(LOC_DOWINDS2);
		}
		s_return(T);

	case LOC_DOWINDS1:		/* winding -- after */

		winders = args;
		if (args != code && args != NIL) {
			s_save(LOC_DOWINDS1, cdr(args), code);
			code = cdar(args);
			args = NIL;
			s_goto(LOC_APPLY);
		}
		s_return(T);

	case LOC_DOWINDS2:		/* winding -- before */

		if (args != code && code != NIL) {
			s_save(LOC_DOWINDS3, args, code);
			code = cdr(code);
			s_goto(LOC_DOWINDS2);
		}
		winders = code;
		s_return(T);

	case LOC_DOWINDS3:		/* winding -- before */
		s_save(LOC_DOWINDS4, args, code);
		code = caar(code);
		args = NIL;
		s_goto(LOC_APPLY);

	case LOC_DOWINDS4:		/* winding -- before */
		winders = code;
		s_return(T);

	case LOC_ADD:		/* + */
		if (!validargs("+", 0, 65535, TST_NUMBER)) Error_0(msg);
		for (mark_x = mk_number(&_ZERO); args != NIL; args = cdr(args)) {
			if (mark_x->_isfixnum) {
				if (car(args)->_isfixnum) {
					if (bignum(mark_x) == NIL) {
						if (bignum(car(args)) == NIL) {
							bignum_from_int64(mark_x, (int64_t)ivalue(mark_x) + ivalue(car(args)));
						} else {
							int32_t signx = ivalue(mark_x) < 0 ? -1 : 1;
							int32_t signy = ivalue(car(args)) < 0 ? -1 : 1;
							if (signx == signy) {
								bignum_add_imm(mark_x, car(args), ivalue(mark_x), signx);
							} else {
								bignum_sub_imm(mark_x, car(args), ivalue(mark_x), -signx);
							}
						}
					} else {
						int32_t signx = ivalue(mark_x) < 0 ? -1 : 1;
						int32_t signy = ivalue(car(args)) < 0 ? -1 : 1;
						if (bignum(car(args)) == NIL) {
							if (signx == signy) {
								bignum_add_imm(mark_x, mark_x, ivalue(car(args)), signx);
							} else {
								bignum_sub_imm(mark_x, mark_x, ivalue(car(args)), signx);
							}
						} else {
							if (signx == signy) {
								bignum_add(&v, mark_x, car(args), signx);
							} else {
								if (bignum_gt(car(args), mark_x)) {
									bignum_sub(&v, car(args), mark_x, -signx);
								} else {
									bignum_sub(&v, mark_x, car(args), signx);
								}
							}
							*mark_x = v;
						}
					}
				} else {
					rvalue(mark_x) = get_rvalue(mark_x) + rvalue(car(args));
					set_num_real(mark_x);
				}
			} else {
				rvalue(mark_x) += get_rvalue(car(args));
			}
		}
		s_return(mark_x);

	case LOC_SUB:		/* - */
		if (!validargs("-", 1, 65535, TST_NUMBER)) Error_0(msg);
		if (cdr(args) == NIL) {
			mark_x = mk_number(&_ZERO);
		} else {
			mark_x = mk_number(car(args));
			args = cdr(args);
		}
		for ( ; args != NIL; args = cdr(args)) {
			if (mark_x->_isfixnum) {
				if (car(args)->_isfixnum) {
					if (bignum(mark_x) == NIL) {
						if (bignum(car(args)) == NIL) {
							bignum_from_int64(mark_x, (int64_t)ivalue(mark_x) - ivalue(car(args)));
						} else {
							int32_t signx = ivalue(mark_x) < 0 ? -1 : 1;
							int32_t signy = ivalue(car(args)) < 0 ? -1 : 1;
							if (signx == signy) {
								bignum_sub_imm(mark_x, car(args), ivalue(mark_x), -signx);
							} else {
								bignum_add_imm(mark_x, car(args), ivalue(mark_x), signx);
							}
						}
					} else {
						int32_t signx = ivalue(mark_x) < 0 ? -1 : 1;
						int32_t signy = ivalue(car(args)) < 0 ? -1 : 1;
						if (bignum(car(args)) == NIL) {
							if (signx == signy) {
								bignum_sub_imm(mark_x, mark_x, ivalue(car(args)), signx);
							} else {
								bignum_add_imm(mark_x, mark_x, ivalue(car(args)), signx);
							}
						} else {
							if (signx == signy) {
								if (bignum_gt(car(args), mark_x)) {
									bignum_sub(&v, car(args), mark_x, -signx);
								} else {
									bignum_sub(&v, mark_x, car(args), signx);
								}
							} else {
								bignum_add(&v, mark_x, car(args), signx);
							}
							*mark_x = v;
						}
					}
				} else {
					rvalue(mark_x) = get_rvalue(mark_x) - rvalue(car(args));
					set_num_real(mark_x);
				}
			} else {
				rvalue(mark_x) -= get_rvalue(car(args));
			}
		}
		s_return(mark_x);

	case LOC_MUL:		/* * */
		if (!validargs("*", 0, 65535, TST_NUMBER)) Error_0(msg);
		for (mark_x = mk_number(&_ONE); args != NIL; args = cdr(args)) {
			if (mark_x->_isfixnum) {
				if (car(args)->_isfixnum) {
					if (bignum(mark_x) == NIL) {
						if (bignum(car(args)) == NIL) {
							bignum_from_int64(mark_x, (int64_t)ivalue(mark_x) * ivalue(car(args)));
						} else {
							bignum_mul_imm(mark_x, car(args), ivalue(mark_x));
						}
					} else {
						if (bignum(car(args)) == NIL) {
							bignum_mul_imm(mark_x, mark_x, ivalue(car(args)));
						} else {
							bignum_mul(&v, mark_x, car(args));
							*mark_x = v;
						}
					}
				} else {
					rvalue(mark_x) = get_rvalue(mark_x) * rvalue(car(args));
					set_num_real(mark_x);
				}
			} else {
				rvalue(mark_x) *= get_rvalue(car(args));
			}
		}
		s_return(mark_x);

	case LOC_DIV:		/* / */
		if (!validargs("/", 1, 65535, TST_NUMBER)) Error_0(msg);
		if (cdr(args) == NIL) {
			mark_x = mk_number(&_ONE);
		} else {
			mark_x = mk_number(car(args));
			args = cdr(args);
		}
		for ( ; args != NIL; args = cdr(args)) {
			double d = get_rvalue(car(args));
			if (-DBL_MIN < d && d < DBL_MIN) {
				Error_0("division by zero");
			}
			if (mark_x->_isfixnum) {
				if (car(args)->_isfixnum) {
					if (bignum(mark_x) == NIL) {
						if (bignum(car(args)) == NIL) {
							if ((int64_t)ivalue(mark_x) % ivalue(car(args)) == 0) {
								bignum_from_int64(mark_x, (int64_t)ivalue(mark_x) / ivalue(car(args)));
							} else {
								rvalue(mark_x) = ivalue(mark_x) / d;
								set_num_real(mark_x);
							}
						} else {
							if (ivalue(mark_x) == INT32_MIN
								&& ivalue(car(args)) == 1 && ((uint32_t *)strvalue(bignum(car(args))))[0] == (uint32_t)1 << 31) {
								ivalue(mark_x) = -1;
								bignum(mark_x) = NIL;
							} else {
								rvalue(mark_x) = ivalue(mark_x) / d;
								set_num_real(mark_x);
							}
						}
					} else {
						struct cell q, r;
						if (bignum(car(args)) == NIL) {
							bignum_div_imm(&q, &r, mark_x, ivalue(car(args)));
							if (ivalue(&r) == 0) {
								*mark_x = q;
							} else {
								rvalue(mark_x) = get_rvalue(mark_x) / d;
								set_num_real(mark_x);
							}
						} else {
							bignum_div(&q, &r, mark_x, car(args));
							if (ivalue(&r) == 0) {
								*mark_x = q;
							} else {
								rvalue(mark_x) = get_rvalue(mark_x) / d;
								set_num_real(mark_x);
							}
						}
					}
				} else {
					rvalue(mark_x) = get_rvalue(mark_x) / d;
					set_num_real(mark_x);
				}
			} else {
				rvalue(mark_x) /= d;
			}
		}
		s_return(mark_x);

	case LOC_ABS:		/* abs */
		if (!validargs("abs", 1, 1, TST_NUMBER)) Error_0(msg);
		mark_x = mk_number(car(args));
		if (mark_x->_isfixnum) {
			bignum_abs(mark_x, mark_x);
		} else {
			rvalue(mark_x) = fabs(rvalue(mark_x));
		}
		s_return(mark_x);

	case LOC_QUO:		/* quotient */
		if (!validargs("quotient", 2, 2, TST_INTEGER)) Error_0(msg);
		mark_x = mk_number(car(args));
		mark_y = cadr(args);
		w = mark_y->_isfixnum ? ivalue(mark_y) : (int32_t)rvalue(mark_y);
		if (w == 0) {
			Error_0("division by zero");
		}
		if (mark_x->_isfixnum) {
			if (mark_y->_isfixnum) {
				if (bignum(mark_x) == NIL) {
					if (bignum(mark_y) == NIL) {
						bignum_from_int64(mark_x, (int64_t)ivalue(mark_x) / ivalue(mark_y));
					} else {
						if (ivalue(mark_x) == INT32_MIN
							&& ivalue(mark_y) == 1 && ((uint32_t *)strvalue(bignum(mark_y)))[0] == (uint32_t)1 << 31) {
							ivalue(mark_x) = -1;
							bignum(mark_x) = NIL;
						} else {
							ivalue(mark_x) = 0;
							bignum(mark_x) = NIL;
						}
					}
				} else {
					struct cell q, r;
					if (bignum(mark_y) == NIL) {
						bignum_div_imm(&q, &r, mark_x, ivalue(mark_y));
					} else {
						bignum_div(&q, &r, mark_x, mark_y);
					}
					*mark_x = q;
				}
			} else {
				rvalue(mark_x) = get_rvalue(mark_x) / rvalue(mark_y);
				rvalue(mark_x) = (rvalue(mark_x) < 0) ? ceil(rvalue(mark_x)) : floor(rvalue(mark_x));
				set_num_real(mark_x);
			}
		} else {
			rvalue(mark_x) = rvalue(mark_x) / get_rvalue(mark_y);
			rvalue(mark_x) = (rvalue(mark_x) < 0) ? ceil(rvalue(mark_x)) : floor(rvalue(mark_x));
		}
		s_return(mark_x);

	case LOC_REM:		/* remainder */
		if (!validargs("remainder", 2, 2, TST_INTEGER)) Error_0(msg);
		mark_x = mk_number(car(args));
		mark_y = cadr(args);
		w = mark_y->_isfixnum ? ivalue(mark_y) : (int32_t)rvalue(mark_y);
		if (w == 0) {
			Error_0("division by zero");
		}
		if (mark_x->_isfixnum) {
			if (mark_y->_isfixnum) {
				if (bignum(mark_x) == NIL) {
					if (bignum(mark_y) == NIL) {
						bignum_from_int64(mark_x, (int64_t)ivalue(mark_x) % ivalue(mark_y));
					} else {
						if (ivalue(mark_x) == INT32_MIN
							&& ivalue(mark_y) == 1 && ((uint32_t *)strvalue(bignum(mark_y)))[0] == (uint32_t)1 << 31) {
							ivalue(mark_x) = 0;
							bignum(mark_x) = NIL;
						}
					}
				} else {
					struct cell q, r;
					if (bignum(mark_y) == NIL) {
						bignum_div_imm(&q, &r, mark_x, ivalue(mark_y));
					} else {
						bignum_div(&q, &r, mark_x, mark_y);
					}
					*mark_x = r;
				}
			} else {
				rvalue(mark_x) = fmod(get_rvalue(mark_x), rvalue(mark_y));
				set_num_real(mark_x);
			}
		} else {
			rvalue(mark_x) = fmod(rvalue(mark_x), get_rvalue(mark_y));
		}
		s_return(mark_x);

	case LOC_MOD:		/* modulo */
		if (!validargs("modulo", 2, 2, TST_INTEGER)) Error_0(msg);
		mark_x = mk_number(car(args));
		mark_y = cadr(args);
		w = mark_y->_isfixnum ? ivalue(mark_y) : (int32_t)rvalue(mark_y);
		if (w == 0) {
			Error_0("division by zero");
		}
		if (mark_x->_isfixnum) {
			if (mark_y->_isfixnum) {
				if (bignum(mark_x) == NIL) {
					if (bignum(mark_y) == NIL) {
						int64_t r = (int64_t)ivalue(mark_x) % ivalue(mark_y);
						if (r * ivalue(mark_y) < 0) {
							r += ivalue(mark_y);
						}
						bignum_from_int64(mark_x, r);
					} else {
						if (ivalue(mark_x) == INT32_MIN
							&& ivalue(mark_y) == 1 && ((uint32_t *)strvalue(bignum(mark_y)))[0] == (uint32_t)1 << 31) {
							ivalue(mark_x) = 0;
							bignum(mark_x) = NIL;
						}
						if (ivalue(mark_x) * w < 0) {
							bignum_sub_imm(mark_x, mark_y, ivalue(mark_x), ivalue(mark_y) < 0 ? -1 : 1);
						}
					}
				} else {
					struct cell q, r;
					if (bignum(mark_y) == NIL) {
						bignum_div_imm(&q, &r, mark_x, ivalue(mark_y));
						if (ivalue(&r) * w < 0) {
							ivalue(&r) += ivalue(mark_y);
						}
					} else {
						bignum_div(&q, &r, mark_x, mark_y);
						if (ivalue(&r) * w < 0) {
							if (bignum(&r) == NIL) {
								bignum_sub_imm(&r, mark_y, ivalue(&r), ivalue(mark_y) < 0 ? -1 : 1);
							} else {
								bignum_sub(&r, mark_y, &r, ivalue(mark_y) < 0 ? -1 : 1);
							}
						}
					}
					*mark_x = r;
				}
			} else {
				rvalue(mark_x) = fmod(get_rvalue(mark_x), rvalue(mark_y));
				set_num_real(mark_x);
				if (rvalue(mark_x) * w < 0) {
					rvalue(mark_x) += w;
				}
			}
		} else {
			rvalue(mark_x) = fmod(rvalue(mark_x), get_rvalue(mark_y));
			if (rvalue(mark_x) * w < 0) {
				rvalue(mark_x) += w;
			}
		}
		s_return(mark_x);

	case LOC_GCD:		/* gcd */
		if (!validargs("gcd", 0, 65535, TST_NUMBER)) Error_0(msg);
		if (args == NIL) {
			mark_x = mk_number(&_ZERO);
		} else if (cdr(args) == NIL) {
			mark_x = mk_number(car(args));
			if (mark_x->_isfixnum) {
				bignum_abs(mark_x, mark_x);
			} else {
				rvalue(mark_x) = fabs(rvalue(mark_x));
			}
		} else {
			mark_x = mk_number(car(args));
			for (mark_y = cdr(args); mark_y != NIL; mark_y = cdr(mark_y)) {
				if (mark_x->_isfixnum) {
					if (car(mark_y)->_isfixnum) {
						if (bignum(mark_x) == NIL) {
							if (bignum(car(mark_y)) == NIL) {
								bignum_from_int64(mark_x, gcd(ivalue(mark_x), ivalue(car(mark_y))));
							} else {
								if (ivalue(mark_x) != 0) {
									struct cell r;
									bignum_gcd_imm(&r, car(mark_y), ivalue(mark_x));
									*mark_x = r;
								}
							}
						} else {
							if (bignum(car(mark_y)) == NIL) {
								if (ivalue(car(mark_y)) != 0) {
									struct cell r;
									bignum_gcd_imm(&r, mark_x, ivalue(car(mark_y)));
									*mark_x = r;
								}
							} else {
								struct cell r;
								if (bignum_gt(mark_x, car(mark_y))) {
									bignum_gcd(&r, mark_x, car(mark_y));
								} else {
									bignum_gcd(&r, car(mark_y), mark_x);
								}
								*mark_x = r;
							}
						}
					} else {
						rvalue(mark_x) = (double)gcd((int32_t)get_rvalue(mark_x), (int32_t)rvalue(car(mark_y)));
						set_num_real(mark_x);
					}
				} else {
					rvalue(mark_x) = (double)gcd((int32_t)rvalue(mark_x), (int32_t)get_rvalue(car(mark_y)));
				}
			}			
		}
		s_return(mark_x);

	case LOC_LCM:		/* lcm */
		if (!validargs("lcm", 0, 65535, TST_NUMBER)) Error_0(msg);
		if (args == NIL) {
			mark_x = mk_number(&_ONE);
		} else if (cdr(args) == NIL) {
			mark_x = mk_number(car(args));
			if (mark_x->_isfixnum) {
				bignum_abs(mark_x, mark_x);
			} else {
				rvalue(mark_x) = fabs(rvalue(mark_x));
			}
		} else {
			mark_x = mk_number(car(args));
			for (mark_y = cdr(args); mark_y != NIL; mark_y = cdr(mark_y)) {
				if (mark_x->_isfixnum) {
					if (car(mark_y)->_isfixnum) {
						if (bignum(mark_x) == NIL) {
							if (bignum(car(mark_y)) == NIL) {
								bignum_from_int64(mark_x, lcm(ivalue(mark_x), ivalue(car(mark_y))));
							} else {
								if (ivalue(mark_x) == 0) {
									mark_x = mk_number(&_ZERO);
									break;
								} else {
									struct cell r;
									bignum_lcm_imm(&r, car(mark_y), ivalue(mark_x));
									*mark_x = r;
								}
							}
						} else {
							if (bignum(car(mark_y)) == NIL) {
								if (ivalue(car(mark_y)) == 0) {
									mark_x = mk_number(&_ZERO);
									break;
								} else {
									struct cell r;
									bignum_lcm_imm(&r, mark_x, ivalue(car(mark_y)));
									*mark_x = r;
								}
							} else {
								struct cell r;
								if (bignum_gt(mark_x, car(mark_y))) {
									bignum_lcm(&r, mark_x, car(mark_y));
								} else {
									bignum_lcm(&r, car(mark_y), mark_x);
								}
								*mark_x = r;
							}
						}
					} else {
						rvalue(mark_x) = (double)lcm((int32_t)get_rvalue(mark_x), (int32_t)rvalue(car(mark_y)));
						set_num_real(mark_x);
					}
				} else {
					rvalue(mark_x) = (double)lcm((int32_t)rvalue(mark_x), (int32_t)get_rvalue(car(mark_y)));
				}
			}
		}
		s_return(mark_x);

	case LOC_FLOOR:		/* floor */
		if (!validargs("floor", 1, 1, TST_NUMBER)) Error_0(msg);
		s_return(mk_real(floor(get_rvalue(car(args)))));

	case LOC_CEILING:	/* ceiling */
		if (!validargs("ceiling", 1, 1, TST_NUMBER)) Error_0(msg);
		s_return(mk_real(ceil(get_rvalue(car(args)))));

	case LOC_TRUNCATE:	/* truncate */
		if (!validargs("truncate", 1, 1, TST_NUMBER)) Error_0(msg);
		x = car(args);
		if (get_rvalue(x) > 0) {
			s_return(mk_real(floor(get_rvalue(x))));
		} else {
			s_return(mk_real(ceil(get_rvalue(x))));
		}

	case LOC_ROUND:		/* round */
		if (!validargs("round", 1, 1, TST_NUMBER)) Error_0(msg);
		x = car(args);
		if (x->_isfixnum) {
			s_return(x);
		} else {
			double fl = floor(rvalue(x));
			double ce = ceil(rvalue(x));
			double dfl = rvalue(x) - fl;
			double dce = ce - rvalue(x);
			if (dfl > dce) {
				s_return(mk_real(ce));
			} else if (dfl < dce) {
				s_return(mk_real(fl));
			} else {
				/* Round to even if midway */
				if (fmod(fl, 2.0) == 0.0) {
					s_return(mk_real(fl));
				} else {
					s_return(mk_real(ce));
				}
			}
		}

	case LOC_EXP:		/* exp */
		if (!validargs("exp", 1, 1, TST_NUMBER)) Error_0(msg);
		s_return(mk_real(exp(get_rvalue(car(args)))));

	case LOC_LOG:		/* log */
		if (!validargs("log", 1, 1, TST_NUMBER)) Error_0(msg);
		s_return(mk_real(log(get_rvalue(car(args)))));

	case LOC_SIN:		/* sin */
		if (!validargs("sin", 1, 1, TST_NUMBER)) Error_0(msg);
		s_return(mk_real(sin(get_rvalue(car(args)))));

	case LOC_COS:		/* cos */
		if (!validargs("cos", 1, 1, TST_NUMBER)) Error_0(msg);
		s_return(mk_real(cos(get_rvalue(car(args)))));

	case LOC_TAN:		/* tan */
		if (!validargs("tan", 1, 1, TST_NUMBER)) Error_0(msg);
		s_return(mk_real(tan(get_rvalue(car(args)))));

	case LOC_ASIN:		/* asin */
		if (!validargs("asin", 1, 1, TST_NUMBER)) Error_0(msg);
		s_return(mk_real(asin(get_rvalue(car(args)))));

	case LOC_ACOS:		/* acos */
		if (!validargs("acos", 1, 1, TST_NUMBER)) Error_0(msg);
		s_return(mk_real(acos(get_rvalue(car(args)))));

	case LOC_ATAN:		/* atan */
		if (!validargs("atan", 1, 2, TST_NUMBER)) Error_0(msg);
		if (cdr(args) == NIL) {
			s_return(mk_real(atan(get_rvalue(car(args)))));
		} else {
			s_return(mk_real(atan2(get_rvalue(car(args)), get_rvalue(cadr(args)))));
		}

	case LOC_SQRT:		/* sqrt */
		if (!validargs("sqrt", 1, 1, TST_NUMBER)) Error_0(msg);
		s_return(mk_real(sqrt(get_rvalue(car(args)))));

	case LOC_EXPT:		/* expt */
		if (!validargs("expt", 2, 2, TST_NUMBER)) Error_0(msg);
		mark_x = mk_number(car(args));
		y = cadr(args);
		if (mark_x->_isfixnum && y->_isfixnum && ((bignum(mark_x) == NIL && ivalue(mark_x) == 1) || ivalue(y) == 0)) {
			*mark_x = _ONE;
		} else if (mark_x->_isfixnum && y->_isfixnum && ivalue(mark_x) == 0) {
			*mark_x = _ZERO;
		} else if (mark_x->_isfixnum) {
			if (y->_isfixnum && bignum(y) == NIL && ivalue(y) > 0) {
				bignum_pow(mark_x, mark_x, ivalue(y));
			} else {
				rvalue(mark_x) = pow(get_rvalue(mark_x), get_rvalue(y));
				set_num_real(mark_x);
			}
		} else {
			rvalue(mark_x) = pow(rvalue(mark_x), get_rvalue(y));
		}
		s_return(mark_x);

	case LOC_EX2INEX:	/* exact->inexact */
		if (!validargs("exact->inexact", 1, 1, TST_NUMBER)) Error_0(msg);
		s_return(mk_real(get_rvalue(car(args))));

	case LOC_INEX2EX:	/* inexact->exact */
		if (!validargs("inexact->exact", 1, 1, TST_NUMBER)) Error_0(msg);
		x = car(args);
		if (x->_isfixnum) {
			s_return(x);
		} else if (rvalue(x) == (int64_t)rvalue(x)) {
			bignum_from_int64(&v, (int64_t)rvalue(x));
			s_return(mk_number(&v));
		} else {
			Error_1("inexact->exact: cannot express :", x);
		}

	case LOC_NUM2STR:	/* number->string */
		if (!validargs("number->string", 1, 2, TST_NUMBER TST_NATURAL)) Error_0(msg);
		if (cdr(args) != NIL) {
			w = ivalue(cadr(args));
			if (w != 16 && w != 10 && w != 8 && w != 2) {
				Error_1("number->string: bad base:", cadr(args));
			}
		} else {
			w = 10;
		}
		s_return(mk_string(atom2str(car(args), (int)w)));

	case LOC_STR2NUM:	/* string->number */
		if (!validargs("string->number", 1, 2, TST_STRING TST_NATURAL)) Error_0(msg);
		if (cdr(args) != NIL) {
			w = ivalue(cadr(args));
			if (w != 16 && w != 10 && w != 8 && w != 2) {
				Error_1("string->number: bad base:", cadr(args));
			}
		} else {
			w = 10;
		}
		if (*strvalue(car(args)) == '#') {
			s_return(mk_const(strvalue(car(args)) + 1));
		} else if (w == 10) {
			s_return(mk_atom(strvalue(car(args))));
		} else {
			s_return(mk_integer_from_str(strvalue(car(args)), strlen(strvalue(car(args))), (int)w));
		}

	case LOC_CAR:		/* car */
		if (!validargs("car", 1, 1, TST_PAIR)) Error_0(msg);
		s_return(caar(args));

	case LOC_CDR:		/* cdr */
		if (!validargs("cdr", 1, 1, TST_PAIR)) Error_0(msg);
		s_return(cdar(args));

	case LOC_CONS:		/* cons */
		if (!validargs("cons", 2, 2, TST_NONE)) Error_0(msg);
		cdr(args) = cadr(args);
		s_return(args);

	case LOC_SETCAR:	/* set-car! */
		if (!validargs("set-car!", 2, 2, TST_PAIR TST_ANY)) Error_0(msg);
		caar(args) = cadr(args);
		s_return(car(args));

	case LOC_SETCDR:	/* set-cdr! */
		if (!validargs("set-cdr!", 2, 2, TST_PAIR TST_ANY)) Error_0(msg);
		cdar(args) = cadr(args);
		s_return(car(args));

	case LOC_CAAR: /* caar */
		if (!validargs("caar", 1, 1, TST_PAIR)) Error_0(msg);
		x = car(car(args));
		if (!is_pair(x)) Error_1("caar: must be pair :", x);
		s_return(car(x));
	case LOC_CADR: /* cadr */
		if (!validargs("cadr", 1, 1, TST_PAIR)) Error_0(msg);
		x = cdr(car(args));
		if (!is_pair(x)) Error_1("cadr: must be pair :", x);
		s_return(car(x));
	case LOC_CDAR: /* cdar */
		if (!validargs("cdar", 1, 1, TST_PAIR)) Error_0(msg);
		x = car(car(args));
		if (!is_pair(x)) Error_1("cdar: must be pair :", x);
		s_return(cdr(x));
	case LOC_CDDR: /* cddr */
		if (!validargs("cddr", 1, 1, TST_PAIR)) Error_0(msg);
		x = cdr(car(args));
		if (!is_pair(x)) Error_1("cddr: must be pair :", x);
		s_return(cdr(x));

	case LOC_CAAAR: /* caaar */
		if (!validargs("caaar", 1, 1, TST_PAIR)) Error_0(msg);
		x = car(car(args));
		if (!is_pair(x)) Error_1("caaar: must be pair :", x);
		x = car(x);
		if (!is_pair(x)) Error_1("caaar: must be pair :", x);
		s_return(car(x));
	case LOC_CAADR: /* caadr */
		if (!validargs("caadr", 1, 1, TST_PAIR)) Error_0(msg);
		x = cdr(car(args));
		if (!is_pair(x)) Error_1("caadr: must be pair :", x);
		x = car(x);
		if (!is_pair(x)) Error_1("caadr: must be pair :", x);
		s_return(car(x));
	case LOC_CADAR: /* cadar */
		if (!validargs("cadar", 1, 1, TST_PAIR)) Error_0(msg);
		x = car(car(args));
		if (!is_pair(x)) Error_1("cadar: must be pair :", x);
		x = cdr(x);
		if (!is_pair(x)) Error_1("cadar: must be pair :", x);
		s_return(car(x));
	case LOC_CADDR: /* caddr */
		if (!validargs("caddr", 1, 1, TST_PAIR)) Error_0(msg);
		x = cdr(car(args));
		if (!is_pair(x)) Error_1("caddr: must be pair :", x);
		x = cdr(x);
		if (!is_pair(x)) Error_1("caddr: must be pair :", x);
		s_return(car(x));
	case LOC_CDAAR: /* cdaar */
		if (!validargs("cdaar", 1, 1, TST_PAIR)) Error_0(msg);
		x = car(car(args));
		if (!is_pair(x)) Error_1("cdaar: must be pair :", x);
		x = car(x);
		if (!is_pair(x)) Error_1("cdaar: must be pair :", x);
		s_return(cdr(x));
	case LOC_CDADR: /* cdadr */
		if (!validargs("cdadr", 1, 1, TST_PAIR)) Error_0(msg);
		x = cdr(car(args));
		if (!is_pair(x)) Error_1("cdadr: must be pair :", x);
		x = car(x);
		if (!is_pair(x)) Error_1("cdadr: must be pair :", x);
		s_return(cdr(x));
	case LOC_CDDAR: /* cddar */
		if (!validargs("cddar", 1, 1, TST_PAIR)) Error_0(msg);
		x = car(car(args));
		if (!is_pair(x)) Error_1("cddar: must be pair :", x);
		x = cdr(x);
		if (!is_pair(x)) Error_1("cddar: must be pair :", x);
		s_return(cdr(x));
	case LOC_CDDDR: /* cdddr */
		if (!validargs("cdddr", 1, 1, TST_PAIR)) Error_0(msg);
		x = cdr(car(args));
		if (!is_pair(x)) Error_1("cdddr: must be pair :", x);
		x = cdr(x);
		if (!is_pair(x)) Error_1("cdddr: must be pair :", x);
		s_return(cdr(x));

	case LOC_CAAAAR: /* caaaar */
		if (!validargs("caaaar", 1, 1, TST_PAIR)) Error_0(msg);
		x = car(car(args));
		if (!is_pair(x)) Error_1("caaaar: must be pair :", x);
		x = car(x);
		if (!is_pair(x)) Error_1("caaaar: must be pair :", x);
		x = car(x);
		if (!is_pair(x)) Error_1("caaaar: must be pair :", x);
		s_return(car(x));
	case LOC_CAAADR: /* caaadr */
		if (!validargs("caaadr", 1, 1, TST_PAIR)) Error_0(msg);
		x = cdr(car(args));
		if (!is_pair(x)) Error_1("caaadr: must be pair :", x);
		x = car(x);
		if (!is_pair(x)) Error_1("caaadr: must be pair :", x);
		x = car(x);
		if (!is_pair(x)) Error_1("caaadr: must be pair :", x);
		s_return(car(x));
	case LOC_CAADAR: /* caadar */
		if (!validargs("caadar", 1, 1, TST_PAIR)) Error_0(msg);
		x = car(car(args));
		if (!is_pair(x)) Error_1("caadar: must be pair :", x);
		x = cdr(x);
		if (!is_pair(x)) Error_1("caadar: must be pair :", x);
		x = car(x);
		if (!is_pair(x)) Error_1("caadar: must be pair :", x);
		s_return(car(x));
	case LOC_CAADDR: /* caaddr */
		if (!validargs("caaddr", 1, 1, TST_PAIR)) Error_0(msg);
		x = cdr(car(args));
		if (!is_pair(x)) Error_1("caaddr: must be pair :", x);
		x = cdr(x);
		if (!is_pair(x)) Error_1("caaddr: must be pair :", x);
		x = car(x);
		if (!is_pair(x)) Error_1("caaddr: must be pair :", x);
		s_return(car(x));
	case LOC_CADAAR: /* cadaar */
		if (!validargs("cadaar", 1, 1, TST_PAIR)) Error_0(msg);
		x = car(car(args));
		if (!is_pair(x)) Error_1("cadaar: must be pair :", x);
		x = car(x);
		if (!is_pair(x)) Error_1("cadaar: must be pair :", x);
		x = cdr(x);
		if (!is_pair(x)) Error_1("cadaar: must be pair :", x);
		s_return(car(x));
	case LOC_CADADR: /* cadadr */
		if (!validargs("cadadr", 1, 1, TST_PAIR)) Error_0(msg);
		x = cdr(car(args));
		if (!is_pair(x)) Error_1("cadadr: must be pair :", x);
		x = car(x);
		if (!is_pair(x)) Error_1("cadadr: must be pair :", x);
		x = cdr(x);
		if (!is_pair(x)) Error_1("cadadr: must be pair :", x);
		s_return(car(x));
	case LOC_CADDAR: /* caddar */
		if (!validargs("caddar", 1, 1, TST_PAIR)) Error_0(msg);
		x = car(car(args));
		if (!is_pair(x)) Error_1("caddar: must be pair :", x);
		x = cdr(x);
		if (!is_pair(x)) Error_1("caddar: must be pair :", x);
		x = cdr(x);
		if (!is_pair(x)) Error_1("caddar: must be pair :", x);
		s_return(car(x));
	case LOC_CADDDR: /* cadddr" */
		if (!validargs("cadddr", 1, 1, TST_PAIR)) Error_0(msg);
		x = cdr(car(args));
		if (!is_pair(x)) Error_1("cadddr: must be pair :", x);
		x = cdr(x);
		if (!is_pair(x)) Error_1("cadddr: must be pair :", x);
		x = cdr(x);
		if (!is_pair(x)) Error_1("cadddr: must be pair :", x);
		s_return(car(x));
	case LOC_CDAAAR: /* cdaaar */
		if (!validargs("cdaaar", 1, 1, TST_PAIR)) Error_0(msg);
		x = car(car(args));
		if (!is_pair(x)) Error_1("cdaaar: must be pair :", x);
		x = car(x);
		if (!is_pair(x)) Error_1("cdaaar: must be pair :", x);
		x = car(x);
		if (!is_pair(x)) Error_1("cdaaar: must be pair :", x);
		s_return(cdr(x));
	case LOC_CDAADR: /* cdaadr */
		if (!validargs("cdaadr", 1, 1, TST_PAIR)) Error_0(msg);
		x = cdr(car(args));
		if (!is_pair(x)) Error_1("cdaadr: must be pair :", x);
		x = car(x);
		if (!is_pair(x)) Error_1("cdaadr: must be pair :", x);
		x = car(x);
		if (!is_pair(x)) Error_1("cdaadr: must be pair :", x);
		s_return(cdr(x));
	case LOC_CDADAR: /* cdadar" */
		if (!validargs("cdadar", 1, 1, TST_PAIR)) Error_0(msg);
		x = car(car(args));
		if (!is_pair(x)) Error_1("cdadar: must be pair :", x);
		x = cdr(x);
		if (!is_pair(x)) Error_1("cdadar: must be pair :", x);
		x = car(x);
		if (!is_pair(x)) Error_1("cdadar: must be pair :", x);
		s_return(cdr(x));
	case LOC_CDADDR: /* cdaddr */
		if (!validargs("cdaddr", 1, 1, TST_PAIR)) Error_0(msg);
		x = cdr(car(args));
		if (!is_pair(x)) Error_1("cdaddr: must be pair :", x);
		x = cdr(x);
		if (!is_pair(x)) Error_1("cdaddr: must be pair :", x);
		x = car(x);
		if (!is_pair(x)) Error_1("cdaddr: must be pair :", x);
		s_return(cdr(x));
	case LOC_CDDAAR: /* cddaar */
		if (!validargs("cddaar", 1, 1, TST_PAIR)) Error_0(msg);
		x = car(car(args));
		if (!is_pair(x)) Error_1("cddaar: must be pair :", x);
		x = car(x);
		if (!is_pair(x)) Error_1("cddaar: must be pair :", x);
		x = cdr(x);
		if (!is_pair(x)) Error_1("cddaar: must be pair :", x);
		s_return(cdr(x));
	case LOC_CDDADR: /* cddadr */
		if (!validargs("cddadr", 1, 1, TST_PAIR)) Error_0(msg);
		x = cdr(car(args));
		if (!is_pair(x)) Error_1("cddadr: must be pair :", x);
		x = car(x);
		if (!is_pair(x)) Error_1("cddadr: must be pair :", x);
		x = cdr(x);
		if (!is_pair(x)) Error_1("cddadr: must be pair :", x);
		s_return(cdr(x));
	case LOC_CDDDAR: /* cdddar */
		if (!validargs("cdddar", 1, 1, TST_PAIR)) Error_0(msg);
		x = car(car(args));
		if (!is_pair(x)) Error_1("cdddar: must be pair :", x);
		x = cdr(x);
		if (!is_pair(x)) Error_1("cdddar: must be pair :", x);
		x = cdr(x);
		if (!is_pair(x)) Error_1("cdddar: must be pair :", x);
		s_return(cdr(x));
	case LOC_CDDDDR: /* cddddr */
		if (!validargs("cddddr", 1, 1, TST_PAIR)) Error_0(msg);
		x = cdr(car(args));
		if (!is_pair(x)) Error_1("cddddr: must be pair :", x);
		x = cdr(x);
		if (!is_pair(x)) Error_1("cddddr: must be pair :", x);
		x = cdr(x);
		if (!is_pair(x)) Error_1("cddddr: must be pair :", x);
		s_return(cdr(x));

	case LOC_LIST:		/* list */
		if (!validargs("list", 0, 65535, TST_NONE)) Error_0(msg);
		s_return(args);

	case LOC_LISTTAIL:	/* list-tail */
		if (!validargs("list-tail", 2, 2, TST_LIST TST_NATURAL)) Error_0(msg);
		x = car(args);
		w = list_length(x);
		if (w < 0) {
			Error_1("list-tail: not a proper list:", x);
		}
		args = cadr(args);
		if (w < ivalue(args)) {
			Error_1("list-tail: index too large:", args);
		}
		for (w = 0; w < ivalue(args); w++) {
			x = cdr(x);
		}
		s_return(x);

	case LOC_LISTREF:	/* list-ref */
		if (!validargs("list-ref", 2, 2, TST_LIST TST_NATURAL)) Error_0(msg);
		x = car(args);
		w = list_length(x);
		if (w < 0) {
			Error_1("list-ref: not a proper list:", x);
		}
		args = cadr(args);
		if (w <= ivalue(args)) {
			Error_1("list-ref: index too large:", args);
		}
		for (w = 0; w < ivalue(args); w++) {
			x = cdr(x);
		}
		s_return(car(x));

	case LOC_LASTPAIR:	/* "last-pair" */
		if (!validargs("last-pair", 1, 1, TST_PAIR)) Error_0(msg);
		x = car(args);
		while (is_pair(cdr(x))) {
			x = cdr(x);
		}
		s_return(x);

	case LOC_CHAR2INT:	/* char->integer */
		if (!validargs("char->integer", 1, 1, TST_CHAR)) Error_0(msg);
		s_return(mk_integer(ivalue(car(args))));

	case LOC_INT2CHAR:	/* integer->char */
		if (!validargs("integer->char", 1, 1, TST_NATURAL)) Error_0(msg);
		s_return(mk_character((int)ivalue(car(args))));

	case LOC_CHARUPCASE:	/* char-upcase */
		if (!validargs("char-upcase", 1, 1, TST_CHAR)) Error_0(msg);
		s_return(mk_character(utf32_toupper((int)ivalue(car(args)))));

	case LOC_CHARDNCASE:	/* char-downcase */
		if (!validargs("char-downcase", 1, 1, TST_CHAR)) Error_0(msg);
		s_return(mk_character(utf32_tolower((int)ivalue(car(args)))));

	case LOC_MKSTRING:	/* make-string */
		if (!validargs("make-string", 1, 2, TST_NATURAL TST_CHAR)) Error_0(msg);
		if (cdr(args) != NIL) {
			s_return(mk_empty_string((size_t)ivalue(car(args)), (int)ivalue(cadr(args))));
		} else {
			s_return(mk_empty_string((size_t)ivalue(car(args)), ' '));
		}

	case LOC_STRING:		/* string */
		if (!validargs("string", 0, 65535, TST_CHAR)) Error_0(msg);
		for (w = 0, x = args; x != NIL; x = cdr(x)) {
			w += utf32_to_utf8((int)ivalue(car(x)), NULL);
		}
		y = mk_counted_string("", (size_t)w);
		for (w = 0, x = args; x != NIL; x = cdr(x)) {
			w += utf32_to_utf8((int)ivalue(car(x)), strvalue(y) + w);
		}
		strvalue(y)[w] = '\0';
		s_return(y);

	case LOC_STRLEN:		/* string-length */
		if (!validargs("string-length", 1, 1, TST_STRING)) Error_0(msg);
		s_return(mk_integer((int32_t)utf8_strlen(strvalue(car(args)))));

	case LOC_STRREF:		/* string-ref */
		if (!validargs("string-ref", 2, 2, TST_STRING TST_NATURAL)) Error_0(msg);
		w = utf8_strref(strvalue(car(args)), (size_t)ivalue(cadr(args)));
		if (w == -1) {
			Error_1("string-ref: out of bounds:", cadr(args));
		}
		s_return(mk_character((int)w));

	case LOC_STRSET:		/* string-set! */
		if (!validargs("string-set!", 3, 3, TST_STRING TST_NATURAL TST_CHAR)) Error_0(msg);
		w = utf8_strpos(strvalue(car(args)), (size_t)ivalue(cadr(args)));
		if (w == -1 || strvalue(car(args))[w] == '\0') {
			Error_1("string-set!: out of bounds:", cadr(args));
		} else {
			char utf8[4];
			size_t len1 = utf8_get_next(strvalue(car(args)) + w, NULL);
			size_t len2 = utf32_to_utf8((int)ivalue(caddr(args)), utf8);
			if (len1 == len2) {
				memcpy(strvalue(car(args)) + w, utf8, len2);
			} else {
				size_t n = strlength(car(args)) + len2 - len1;
				x = mk_memblock(n, &NIL, &NIL);
				memcpy(strvalue(x), strvalue(car(args)), (size_t)w);
				memcpy(strvalue(x) + w, utf8, len2);
				memcpy(strvalue(x) + w + len2, strvalue(car(args)) + w + len1, (size_t)(n - w - len2));
				strvalue(x)[n] = '\0';
				strvalue(car(args)) = strvalue(x);
				strlength(car(args)) = strlength(x);
			}
		}
		s_return(car(args));

	case LOC_STREQU:		/* string=? */
		if (!validargs("string=?", 2, 2, TST_STRING TST_STRING)) Error_0(msg);
		s_retbool(strcmp(strvalue(car(args)), strvalue(cadr(args))) == 0);
	case LOC_STRLSS:		/* string<? */
		if (!validargs("string<?", 2, 2, TST_STRING TST_STRING)) Error_0(msg);
		s_retbool(strcmp(strvalue(car(args)), strvalue(cadr(args))) < 0);
	case LOC_STRGTR:		/* string>? */
		if (!validargs("string>?", 2, 2, TST_STRING TST_STRING)) Error_0(msg);
		s_retbool(strcmp(strvalue(car(args)), strvalue(cadr(args))) > 0);
	case LOC_STRLEQ:		/* string<=? */
		if (!validargs("string<=?", 2, 2, TST_STRING TST_STRING)) Error_0(msg);
		s_retbool(strcmp(strvalue(car(args)), strvalue(cadr(args))) <= 0);
	case LOC_STRGEQ:		/* string>=? */
		if (!validargs("string>=?", 2, 2, TST_STRING TST_STRING)) Error_0(msg);
		s_retbool(strcmp(strvalue(car(args)), strvalue(cadr(args))) >= 0);

	case LOC_STRCIEQU:	/* string-ci=? */
		if (!validargs("string-ci=?", 2, 2, TST_STRING TST_STRING)) Error_0(msg);
		s_retbool(utf8_stricmp(strvalue(car(args)), strvalue(cadr(args))) == 0);
	case LOC_STRCILSS:		/* string-ci<? */
		if (!validargs("string-ci<?", 2, 2, TST_STRING TST_STRING)) Error_0(msg);
		s_retbool(utf8_stricmp(strvalue(car(args)), strvalue(cadr(args))) < 0);
	case LOC_STRCIGTR:		/* string-ci>? */
		if (!validargs("string-ci>?", 2, 2, TST_STRING TST_STRING)) Error_0(msg);
		s_retbool(utf8_stricmp(strvalue(car(args)), strvalue(cadr(args))) > 0);
	case LOC_STRCILEQ:		/* string-ci<=? */
		if (!validargs("string-ci<=?", 2, 2, TST_STRING TST_STRING)) Error_0(msg);
		s_retbool(utf8_stricmp(strvalue(car(args)), strvalue(cadr(args))) <= 0);
	case LOC_STRCIGEQ:		/* string-ci>=? */
		if (!validargs("string-ci>=?", 2, 2, TST_STRING TST_STRING)) Error_0(msg);
		s_retbool(utf8_stricmp(strvalue(car(args)), strvalue(cadr(args))) >= 0);

	case LOC_SUBSTR:		/* substring */
		if (!validargs("substring", 2, 3, TST_STRING TST_NATURAL)) Error_0(msg);
		w = utf8_strlen(strvalue(car(args)));
		if (ivalue(cadr(args)) > w) {
			Error_1("substring: start out of bounds:", cadr(args));
		} else {
			int start = utf8_strpos(strvalue(car(args)), (size_t)ivalue(cadr(args))), n;
			if (cddr(args) != NIL) {
				if (ivalue(caddr(args)) > w || ivalue(caddr(args)) < ivalue(cadr(args))) {
					Error_1("substring: end out of bounds:", caddr(args));
				}
				n = utf8_strpos(strvalue(car(args)), (size_t)ivalue(caddr(args))) - start;
			} else {
				n = (int)strlength(car(args)) - start;
			}
			x = mk_counted_string("", n);
			memcpy(strvalue(x), strvalue(car(args)) + start, n);
			strvalue(x)[n] = '\0';
		}
		s_return(x);

	case LOC_STRAPPEND:	/* string-append */
		if (!validargs("string-append", 0, 65535, TST_STRING)) Error_0(msg);
		for (w = 0, x = args; x != NIL; x = cdr(x)) {
			w += strlength(car(x));
		}
		y = mk_counted_string("", (size_t)w);
		for (w = 0, x = args; x != NIL; x = cdr(x)) {
			memcpy(strvalue(y) + w, strvalue(car(x)), strlength(car(x)));
			w += strlength(car(x));
		}
		strvalue(y)[w] = '\0';
		s_return(y);

	case LOC_STR2LIST:	/* string->list */
		if (!validargs("string->list", 1, 1, TST_STRING)) Error_0(msg);
		code = NIL;
		w = 0;
		while ((size_t)w < strlength(car(args))) {
			int c;
			w += utf8_get_next(strvalue(car(args)) + w, &c);
			x = mk_character(c);
			code = cons(x, code);
		}
		s_return(non_alloc_rev(NIL, code));

	case LOC_LIST2STR:	/* list->string */
		if (!validargs("list->string", 1, 1, TST_LIST)) Error_0(msg);
		if (list_length(car(args)) < 0) {
			Error_1("list->string: not a proper list:", car(args));
		}
		for (w = 0, x = car(args); x != NIL; x = cdr(x)) {
			if (!is_character(car(x))) {
				Error_1("list->string: not a charactor:", car(x));
			}
			w += utf32_to_utf8((int)ivalue(car(x)), NULL);
		}
		y = mk_counted_string("", (size_t)w);
		for (w = 0, x = car(args); x != NIL; x = cdr(x)) {
			w += utf32_to_utf8((int)ivalue(car(x)), strvalue(y) + w);
		}
		strvalue(y)[w] = '\0';
		s_return(y);

	case LOC_STRCOPY:	/* string-copy */
		if (!validargs("string-copy", 1, 1, TST_STRING)) Error_0(msg);
		s_return(mk_string(strvalue(car(args))));

	case LOC_STRFILL:	/* string-fill! */
		if (!validargs("string-fill!", 2, 2, TST_STRING TST_CHAR)) Error_0(msg);
		x = car(args);
		w = utf8_strlen(strvalue(x));
		if (w > 0) {
			char utf8[4];
			size_t len = utf32_to_utf8((int)ivalue(cadr(args)), utf8);
			if (strlength(x) != (size_t)w * len) {
				y = mk_memblock((size_t)w * len, &x, &NIL);
				strvalue(x) = strvalue(y);
				strlength(x) = strlength(y);
			}
			strvalue(x)[w * len] = '\0';
			while (w > 0) {
				memcpy(strvalue(x) + --w * len, utf8, len);
			}
		}
		s_return(x);

	case LOC_VECTOR:		/* vector */

		if (!validargs("vector", 0, 65535, TST_NONE)) Error_0(msg);
		w = list_length(args);
		if (w < 0) {
			Error_1("vector: not a proper list:", args);
		}
		y = mk_vector((int)w);
		for (x = args, w = 0; is_pair(x); x = cdr(x)) {
			set_vector_elem(y, (int)w++, car(x));
		}
		s_return(y);

	case LOC_MKVECTOR:	/* make-vector */
		if (!validargs("make-vector", 1, 2, TST_NATURAL TST_ANY)) Error_0(msg);
		w = ivalue(car(args));
		x = mk_vector((int)w);
		if (cdr(args) != NIL) {
			fill_vector(x, cadr(args));
		}
		s_return(x);

	case LOC_VECLEN:		/* vector-length */
		if (!validargs("vector-length", 1, 1, TST_VECTOR)) Error_0(msg);
		s_return(mk_integer(ivalue(car(args))));

	case LOC_VECREF:		/* vector-ref */
		if (!validargs("vector-ref", 2, 2, TST_VECTOR TST_NATURAL)) Error_0(msg);
		w = ivalue(cadr(args));
		if (w >= ivalue(car(args))) {
		   Error_1("vector-ref: out of bounds:", cadr(args));
		}
		s_return(vector_elem(car(args), (int)w));

	case LOC_VECSET:		/* vector-set! */
		if (!validargs("vector-set!", 3, 3, TST_VECTOR TST_NATURAL TST_ANY)) Error_0(msg);
		w = ivalue(cadr(args));
		if (w >= ivalue(car(args))) {
		   Error_1("vector-set!: out of bounds:", cadr(args));
		}
		set_vector_elem(car(args), (int)w, caddr(args));
		s_return(car(args));

	case LOC_VEC2LIST:		/* vector->list */
		if (!validargs("vector->list", 1, 1, TST_VECTOR)) Error_0(msg);
		y = NIL;
		for (w = ivalue(car(args)) - 1; w >= 0; w--) {
			y = cons(vector_elem(car(args), (int)w), y);
		}
		s_return(y);

	case LOC_LIST2VEC:		/* list->vector */
		if (!validargs("list->vector", 1, 1, TST_LIST)) Error_0(msg);
		args = car(args);
		w = list_length(args);
		if (w < 0) {
			Error_1("list->vector: not a proper list:", args);
		}
		y = mk_vector((int)w);
		for (w = 0; args != NIL; args = cdr(args)) {
			set_vector_elem(y, (int)w++, car(args));
		}
		s_return(y);

	case LOC_VECFILL:	/* vector-fill! */
		if (!validargs("vector-fill!", 2, 2, TST_VECTOR TST_ANY)) Error_0(msg);
		for (x = car(args), w = ivalue(x) - 1; w >= 0; w--) {
			set_vector_elem(car(args), (int)w, cadr(args));
		}
		s_return(car(args));

	case LOC_NOT:		/* not */
		if (!validargs("not", 1, 1, TST_NONE)) Error_0(msg);
		s_retbool(isfalse(car(args)));
	case LOC_BOOL:		/* boolean? */
		if (!validargs("boolean?", 1, 1, TST_NONE)) Error_0(msg);
		s_retbool(car(args) == F || car(args) == T);
	case LOC_NULL:		/* null? */
		if (!validargs("null?", 1, 1, TST_NONE)) Error_0(msg);
		s_retbool(car(args) == NIL);
	case LOC_EOFOBJP:	/* eof-object? */
		if (!validargs("eof-object?", 1, 1, TST_NONE)) Error_0(msg);
		s_retbool(car(args) == EOF_OBJ);
	case LOC_ZEROP:		/* zero? */
		if (!validargs("zero?", 1, 1, TST_NUMBER)) Error_0(msg);
		s_retbool(nvalue(car(args)) == 0);
	case LOC_POSP:		/* positive? */
		if (!validargs("positive?", 1, 1, TST_NUMBER)) Error_0(msg);
		s_retbool(nvalue(car(args)) > 0);
	case LOC_NEGP:		/* negative? */
		if (!validargs("negative?", 1, 1, TST_NUMBER)) Error_0(msg);
		s_retbool(nvalue(car(args)) < 0);
	case LOC_ODD:		/* odd? */
		if (!validargs("odd?", 1, 1, TST_INTEGER)) Error_0(msg);
		if (car(args)->_isfixnum) {
			if (bignum(car(args)) == NIL) {
				s_retbool(ivalue(car(args)) % 2 != 0);
			} else {
				s_retbool(((uint32_t *)strvalue(bignum(car(args))))[0] % 2 != 0);
			}
		} else {
			s_retbool((int64_t)rvalue(car(args)) % 2 != 0);
		}
	case LOC_EVEN:		/* even? */
		if (!validargs("even?", 1, 1, TST_INTEGER)) Error_0(msg);
		if (car(args)->_isfixnum) {
			if (bignum(car(args)) == NIL) {
				s_retbool(ivalue(car(args)) % 2 == 0);
			} else {
				s_retbool(((uint32_t *)strvalue(bignum(car(args))))[0] % 2 == 0);
			}
		} else {
			s_retbool((int64_t)rvalue(car(args)) % 2 == 0);
		}
	case LOC_NEQ:		/* = */
		if (!validargs("=", 2, 65535, TST_NUMBER)) Error_0(msg);
		for (x = car(args), args = cdr(args); args != NIL; args = cdr(args)) {
			y = car(args);
			if (x->_isfixnum && y->_isfixnum) {
				if (bignum(x) == NIL) {
					if (bignum(y) == NIL) {
						if (ivalue(x) != ivalue(y)) { s_return(F); }
					} else {
						s_return(F);
					}
				} else {
					if (bignum(y) == NIL) {
						s_return(F);
					} else {
						if ((ivalue(x) < 0) == (ivalue(y) < 0)) {
							if (!bignum_eq(x, y)) { s_return(F); }
						} else {
							s_return(F);
						}
					}
				}
			} else {
				if (get_rvalue(x) != get_rvalue(y)) { s_return(F); }
			}
		}
		s_return(T);

	case LOC_LESS:		/* < */
		if (!validargs("<", 2, 65535, TST_NUMBER)) Error_0(msg);
		for (x = car(args), args = cdr(args); args != NIL; x = y, args = cdr(args)) {
			y = car(args);
			if (x->_isfixnum && y->_isfixnum) {
				if (bignum(x) == NIL) {
					if (bignum(y) == NIL) {
						if (ivalue(x) >= ivalue(y)) { s_return(F); }
					} else {
						if (ivalue(y) <= 0) { s_return(F); }
					}
				} else {
					if (bignum(y) == NIL) {
						if (ivalue(x) >= 0) { s_return(F); }
					} else {
						int32_t signx = ivalue(x) < 0 ? -1 : 1;
						int32_t signy = ivalue(y) < 0 ? -1 : 1;
						if (signx == signy) {
							if (signx > 0) {
								if (bignum_ge(x, y)) { s_return(F); }
							} else {
								if (bignum_ge(y, x)) { s_return(F); }
							}
						} else {
							if (signx >= signy) { s_return(F); }
						}
					}
				}
			} else {
				if (get_rvalue(x) >= get_rvalue(y)) { s_return(F); }
			}
		}
		s_return(T);

	case LOC_GRE:		/* > */
		if (!validargs(">", 2, 65535, TST_NUMBER)) Error_0(msg);
		for (x = car(args), args = cdr(args); args != NIL; x = y, args = cdr(args)) {
			y = car(args);
			if (x->_isfixnum && y->_isfixnum) {
				if (bignum(x) == NIL) {
					if (bignum(y) == NIL) {
						if (ivalue(x) <= ivalue(y)) { s_return(F); }
					} else {
						if (ivalue(y) >= 0) { s_return(F); }
					}
				} else {
					if (bignum(y) == NIL) {
						if (ivalue(x) <= 0) { s_return(F); }
					} else {
						int32_t signx = ivalue(x) < 0 ? -1 : 1;
						int32_t signy = ivalue(y) < 0 ? -1 : 1;
						if (signx == signy) {
							if (signx < 0) {
								if (bignum_ge(x, y)) { s_return(F); }
							} else {
								if (bignum_ge(y, x)) { s_return(F); }
							}
						} else {
							if (signx <= signy) { s_return(F); }
						}
					}
				}
			} else {
				if (get_rvalue(x) <= get_rvalue(y)) { s_return(F); }
			}
		}
		s_return(T);

	case LOC_LEQ:		/* <= */
		if (!validargs("<=", 2, 65535, TST_NUMBER)) Error_0(msg);
		for (x = car(args), args = cdr(args); args != NIL; x = y, args = cdr(args)) {
			y = car(args);
			if (x->_isfixnum && y->_isfixnum) {
				if (bignum(x) == NIL) {
					if (bignum(y) == NIL) {
						if (ivalue(x) > ivalue(y)) { s_return(F); }
					} else {
						if (ivalue(y) <= 0) { s_return(F); }
					}
				} else {
					if (bignum(y) == NIL) {
						if (ivalue(x) >= 0) { s_return(F); }
					} else {
						int32_t signx = ivalue(x) < 0 ? -1 : 1;
						int32_t signy = ivalue(y) < 0 ? -1 : 1;
						if (signx == signy) {
							if (signx > 0) {
								if (bignum_gt(x, y)) { s_return(F); }
							} else {
								if (bignum_gt(y, x)) { s_return(F); }
							}
						} else {
							if (signx >= signy) { s_return(F); }
						}
					}
				}
			} else {
				if (get_rvalue(x) > get_rvalue(y)) { s_return(F); }
			}
		}
		s_return(T);

	case LOC_GEQ:		/* >= */
		if (!validargs(">=", 2, 65535, TST_NUMBER)) Error_0(msg);
		for (x = car(args), args = cdr(args); args != NIL; x = y, args = cdr(args)) {
			y = car(args);
			if (x->_isfixnum && y->_isfixnum) {
				if (bignum(x) == NIL) {
					if (bignum(y) == NIL) {
						if (ivalue(x) < ivalue(y)) { s_return(F); }
					} else {
						if (ivalue(y) >= 0) { s_return(F); }
					}
				} else {
					if (bignum(y) == NIL) {
						if (ivalue(x) <= 0) { s_return(F); }
					} else {
						int32_t signx = ivalue(x) < 0 ? -1 : 1;
						int32_t signy = ivalue(y) < 0 ? -1 : 1;
						if (signx == signy) {
							if (signx < 0) {
								if (bignum_gt(x, y)) { s_return(F); }
							} else {
								if (bignum_gt(y, x)) { s_return(F); }
							}
						} else {
							if (signx <= signy) { s_return(F); }
						}
					}
				}
			} else {
				if (get_rvalue(x) < get_rvalue(y)) { s_return(F); }
			}
		}
		s_return(T);

	case LOC_MAX:		/* max */
		if (!validargs("max", 1, 65535, TST_NUMBER)) Error_0(msg);
		mark_x = mk_number(car(args));
		for (mark_y = cdr(args); mark_y != NIL; mark_y = cdr(mark_y)) {
			if (mark_x->_isfixnum) {
				if (car(mark_y)->_isfixnum) {
					if (bignum(mark_x) == NIL) {
						if (bignum(car(mark_y)) == NIL) {
							if (ivalue(mark_x) < ivalue(car(mark_y))) {
								ivalue(mark_x) = ivalue(car(mark_y));
							}
						} else {
							if (ivalue(car(mark_y)) > 0) {
								pointer m = mk_memblock(ivalue(car(mark_y)) * sizeof(uint32_t), &NIL, &NIL);
								memcpy(strvalue(m), strvalue(bignum(car(mark_y))), ivalue(car(mark_y)) * sizeof(uint32_t));
								bignum_adjust(mark_x, m, ivalue(car(mark_y)), 1);
							}
						}
					} else {
						if (bignum(car(mark_y)) == NIL) {
							if (ivalue(mark_x) < 0) {
								bignum_from_int64(mark_x, ivalue(car(mark_y)));
							}
						} else {
							int32_t signx = ivalue(mark_x) < 0 ? -1 : 1;
							int32_t signy = ivalue(car(mark_y)) < 0 ? -1 : 1;
							if (signx == signy) {
								if (signx > 0) {
									if (bignum_gt(car(mark_y), mark_x)) {
										pointer m = mk_memblock(ivalue(car(mark_y)) * sizeof(uint32_t), &NIL, &NIL);
										memcpy(strvalue(m), strvalue(bignum(car(mark_y))), ivalue(car(mark_y)) * sizeof(uint32_t));
										bignum_adjust(mark_x, m, ivalue(car(mark_y)), 1);
									}
								} else {
									if (bignum_gt(mark_x, car(mark_y))) {
										pointer m = mk_memblock(abs(ivalue(car(mark_y))) * sizeof(uint32_t), &NIL, &NIL);
										memcpy(strvalue(m), strvalue(bignum(car(mark_y))), abs(ivalue(car(mark_y))) * sizeof(uint32_t));
										bignum_adjust(mark_x, m, abs(ivalue(car(mark_y))), -1);
									}
								}
							} else if (signx < signy) {
								pointer m = mk_memblock(ivalue(car(mark_y)) * sizeof(uint32_t), &NIL, &NIL);
								memcpy(strvalue(m), strvalue(bignum(car(mark_y))), ivalue(car(mark_y)) * sizeof(uint32_t));
								bignum_adjust(mark_x, m, ivalue(car(mark_y)), 1);
							}
						}
					}
				} else {
					if (get_rvalue(mark_x) < rvalue(car(mark_y))) {
						rvalue(mark_x) = rvalue(car(mark_y));
					} else {
						rvalue(mark_x) = get_rvalue(mark_x);
					}
					set_num_real(mark_x);
				}
			} else {
				if (rvalue(mark_x) < get_rvalue(car(mark_y))) {
					rvalue(mark_x) = get_rvalue(car(mark_y));
				}
			}
		}
		s_return(mark_x);

	case LOC_MIN:		/* min */
		if (!validargs("min", 1, 65535, TST_NUMBER)) Error_0(msg);
		mark_x = mk_number(car(args));
		for (mark_y = cdr(args); mark_y != NIL; mark_y = cdr(mark_y)) {
			if (mark_x->_isfixnum) {
				if (car(mark_y)->_isfixnum) {
					if (bignum(mark_x) == NIL) {
						if (bignum(car(mark_y)) == NIL) {
							if (ivalue(mark_x) > ivalue(car(mark_y))) {
								ivalue(mark_x) = ivalue(car(mark_y));
							}
						} else {
							if (ivalue(car(mark_y)) < 0) {
								pointer m = mk_memblock(abs(ivalue(car(mark_y))) * sizeof(uint32_t), &NIL, &NIL);
								memcpy(strvalue(m), strvalue(bignum(car(mark_y))), abs(ivalue(car(mark_y))) * sizeof(uint32_t));
								bignum_adjust(mark_x, m, abs(ivalue(car(mark_y))), -1);
							}
						}
					} else {
						if (bignum(car(mark_y)) == NIL) {
							if (ivalue(mark_x) > 0) {
								bignum_from_int64(mark_x, ivalue(car(mark_y)));
							}
						} else {
							int32_t signx = ivalue(mark_x) < 0 ? -1 : 1;
							int32_t signy = ivalue(car(mark_y)) < 0 ? -1 : 1;
							if (signx == signy) {
								if (signx < 0) {
									if (bignum_gt(car(mark_y), mark_x)) {
										pointer m = mk_memblock(abs(ivalue(car(mark_y))) * sizeof(uint32_t), &NIL, &NIL);
										memcpy(strvalue(m), strvalue(bignum(car(mark_y))), abs(ivalue(car(mark_y))) * sizeof(uint32_t));
										bignum_adjust(mark_x, m, abs(ivalue(car(mark_y))), -1);
									}
								} else {
									if (bignum_gt(mark_x, car(mark_y))) {
										pointer m = mk_memblock(ivalue(car(mark_y)) * sizeof(uint32_t), &NIL, &NIL);
										memcpy(strvalue(m), strvalue(bignum(car(mark_y))), ivalue(car(mark_y)) * sizeof(uint32_t));
										bignum_adjust(mark_x, m, ivalue(car(mark_y)), 1);
									}
								}
							} else if (signx > signy) {
								pointer m = mk_memblock(abs(ivalue(car(mark_y))) * sizeof(uint32_t), &NIL, &NIL);
								memcpy(strvalue(m), strvalue(bignum(car(mark_y))), abs(ivalue(car(mark_y))) * sizeof(uint32_t));
								bignum_adjust(mark_x, m, abs(ivalue(car(mark_y))), -1);
							}
						}
					}
				} else {
					if (get_rvalue(mark_x) > rvalue(car(mark_y))) {
						rvalue(mark_x) = rvalue(car(mark_y));
					} else {
						rvalue(mark_x) = get_rvalue(mark_x);
					}
					set_num_real(mark_x);
				}
			} else {
				if (rvalue(mark_x) > get_rvalue(car(mark_y))) {
					rvalue(mark_x) = get_rvalue(car(mark_y));
				}
			}
		}
		s_return(mark_x);

	case LOC_SYMBOL:		/* symbol? */
		if (!validargs("symbol?", 1, 1, TST_ANY)) Error_0(msg);
		s_retbool(is_symbol(car(args)));
	case LOC_SYM2STR:	/* symbol->string */
		if (!validargs("symbol->string", 1, 1, TST_SYMBOL)) Error_0(msg);
		s_return(mk_string(symname(car(args))));
	case LOC_STR2SYM:	/* string->symbol */
		if (!validargs("string->symbol", 1, 1, TST_STRING)) Error_0(msg);
		s_return(mk_symbol(strvalue(car(args))));
	case LOC_NUMBER:		/* number? */
		if (!validargs("number?", 1, 1, TST_ANY)) Error_0(msg);
		s_retbool(is_number(car(args)));
	case LOC_STRINGP:	/* string? */
		if (!validargs("string?", 1, 1, TST_ANY)) Error_0(msg);
		s_retbool(is_string(car(args)) && !is_symbol(car(args)));
	case LOC_INTEGER:	/* integer? */
		if (!validargs("integer?", 1, 1, TST_ANY)) Error_0(msg);
		s_retbool(is_integer(car(args)));
	case LOC_REAL:		/* real? */
		if (!validargs("real?", 1, 1, TST_ANY)) Error_0(msg);
		s_retbool(is_number(car(args)));
	case LOC_EXACT:		/* exact? */
		if (!validargs("exact?", 1, 1, TST_ANY)) Error_0(msg);
		s_retbool(is_number(car(args)) && car(args)->_isfixnum);
	case LOC_INEXACT:	/* inexact? */
		if (!validargs("inexact?", 1, 1, TST_ANY)) Error_0(msg);
		s_retbool(!is_number(car(args)) || !car(args)->_isfixnum);
	case LOC_CHAR:		/* char? */
		if (!validargs("char?", 1, 1, TST_ANY)) Error_0(msg);
		s_retbool(is_character(car(args)));
	case LOC_CHAREQU:	/* char=? */
		if (!validargs("char=?", 2, 2, TST_CHAR TST_CHAR)) Error_0(msg);
		s_retbool(ivalue(car(args)) == ivalue(cadr(args)));
	case LOC_CHARLSS:	/* char<? */
		if (!validargs("char<?", 2, 2, TST_CHAR TST_CHAR)) Error_0(msg);
		s_retbool(ivalue(car(args)) < ivalue(cadr(args)));
	case LOC_CHARGTR:	/* char>? */
		if (!validargs("char>?", 2, 2, TST_CHAR TST_CHAR)) Error_0(msg);
		s_retbool(ivalue(car(args)) > ivalue(cadr(args)));
	case LOC_CHARLEQ:	/* char<=? */
		if (!validargs("char<=?", 2, 2, TST_CHAR TST_CHAR)) Error_0(msg);
		s_retbool(ivalue(car(args)) <= ivalue(cadr(args)));
	case LOC_CHARGEQ:	/* char>=? */
		if (!validargs("char>=?", 2, 2, TST_CHAR TST_CHAR)) Error_0(msg);
		s_retbool(ivalue(car(args)) >= ivalue(cadr(args)));
	case LOC_CHARCIEQU:	/* char-ci=? */
		if (!validargs("char-ci=?", 2, 2, TST_CHAR TST_CHAR)) Error_0(msg);
		s_retbool(utf32_tolower((int)ivalue(car(args))) == utf32_tolower((int)ivalue(cadr(args))));
	case LOC_CHARCILSS:	/* char-ci<? */
		if (!validargs("char-ci<?", 2, 2, TST_CHAR TST_CHAR)) Error_0(msg);
		s_retbool(utf32_tolower((int)ivalue(car(args))) < utf32_tolower((int)ivalue(cadr(args))));
	case LOC_CHARCIGTR:	/* char-ci>? */
		if (!validargs("char-ci>?", 2, 2, TST_CHAR TST_CHAR)) Error_0(msg);
		s_retbool(utf32_tolower((int)ivalue(car(args))) > utf32_tolower((int)ivalue(cadr(args))));
	case LOC_CHARCILEQ:	/* char-ci<=? */
		if (!validargs("char-ci<=?", 2, 2, TST_CHAR TST_CHAR)) Error_0(msg);
		s_retbool(utf32_tolower((int)ivalue(car(args))) <= utf32_tolower((int)ivalue(cadr(args))));
	case LOC_CHARCIGEQ:	/* char-ci>=? */
		if (!validargs("char-ci>=?", 2, 2, TST_CHAR TST_CHAR)) Error_0(msg);
		s_retbool(utf32_tolower((int)ivalue(car(args))) >= utf32_tolower((int)ivalue(cadr(args))));
	case LOC_CHARAP:		/* char-alphabetic? */
		if (!validargs("char-alphabetic?", 1, 1, TST_CHAR)) Error_0(msg);
		s_retbool(utf32_isalpha((int)ivalue(car(args))));
	case LOC_CHARNP:		/* char-numeric? */
		if (!validargs("char-numeric?", 1, 1, TST_CHAR)) Error_0(msg);
		s_retbool(utf32_isdigit((int)ivalue(car(args))));
	case LOC_CHARWP:		/* char-whitespace? */
		if (!validargs("char-whitespace?", 1, 1, TST_CHAR)) Error_0(msg);
		s_retbool(utf32_isspace((int)ivalue(car(args))));
	case LOC_CHARUP:		/* char-upper-case? */
		if (!validargs("char-upper-case?", 1, 1, TST_CHAR)) Error_0(msg);
		s_retbool(utf32_isupper((int)ivalue(car(args))));
	case LOC_CHARLP:		/* char-lower-case? */
		if (!validargs("char-lower-case?", 1, 1, TST_CHAR)) Error_0(msg);
		s_retbool(utf32_islower((int)ivalue(car(args))));
	case LOC_PROCP:		/* procedure? */
		if (!validargs("procedure?", 1, 1, TST_ANY)) Error_0(msg);
		/*--
		 * continuation should be procedure by the example
		 * (call-with-current-continuation procedure?) ==> #t
		 * in R^3 report sec. 6.9
		 */
		s_retbool(is_operation(car(args)) || is_function(car(args)) || is_closure(car(args))
			  || is_continuation(car(args)));
	case LOC_PAIR:		/* pair? */
		if (!validargs("pair?", 1, 1, TST_ANY)) Error_0(msg);
		s_retbool(is_pair(car(args)));
	case LOC_LISTP:		/* list? */
		if (!validargs("list?", 1, 1, TST_ANY)) Error_0(msg);
		s_retbool(list_length(car(args)) >= 0);
	case LOC_PORTP:		/* port? */
		if (!validargs("port?", 1, 1, TST_ANY)) Error_0(msg);
		s_retbool(is_port(car(args)));
	case LOC_INPORTP:	/* input-port? */
		if (!validargs("input-port?", 1, 1, TST_ANY)) Error_0(msg);
		s_retbool(is_inport(car(args)));
	case LOC_OUTPORTP:	/* output-port? */
		if (!validargs("output-port?", 1, 1, TST_ANY)) Error_0(msg);
		s_retbool(is_outport(car(args)));
	case LOC_VECTORP:	/* vector? */
		if (!validargs("vector?", 1, 1, TST_ANY)) Error_0(msg);
		s_retbool(is_vector(car(args)));
	case LOC_ENVP:		/* environment? */
		if (!validargs("environment?", 1, 1, TST_ANY)) Error_0(msg);
		s_retbool(is_environment(car(args)));
	case LOC_EQ:		/* eq? */
		if (!validargs("eq?", 2, 2, TST_ANY)) Error_0(msg);
		s_retbool(car(args) == cadr(args));
	case LOC_EQV:		/* eqv? */
		if (!validargs("eqv?", 2, 2, TST_ANY)) Error_0(msg);
		s_retbool(eqv(car(args), cadr(args)));
	case LOC_EQUAL:		/* equal? */
		if (!validargs("equal?", 2, 2, TST_ANY)) Error_0(msg);
		s_retbool(equal(car(args), cadr(args)));

	case LOC_EAGER:		/* eagar */
		if (!validargs("eagar", 1, 1, TST_ANY)) Error_0(msg);
		x = cons(NIL, car(args));
		x = mk_closure(x, envir);
		setpromise(x);
		setresultready(x);
		s_return(x);

	case LOC_FORCE:		/* force */
		if (!validargs("force", 1, 1, TST_ANY)) Error_0(msg);
		code = car(args);

	case LOC_FORCE1:	/* force */
		if (is_promise(code)) {
			if (is_resultready(code)) {
				s_return(cdar(code));
			}
			s_save(LOC_FORCED, NIL, code);
			args = NIL;
			if (is_promise(cdar(code))) {
				code = cdar(code);
			}
			s_goto(LOC_APPLY);
		}
		s_return(code);

	case LOC_FORCED:		/* force */
		if (is_resultready(code)) {
			s_return(cdar(code));
		}
		if (is_promise(cdar(code)) || !is_promise(value)) {
			cdar(code) = value;
			setresultready(code);
		} else {
			cdar(code) = cdar(value);
			cdr(code) = cdr(value);
			car(value) = car(code);
			if (is_resultready(value)) {
				setresultready(code);
			} else {
				setresultready(value);
			}
		}
		s_goto(LOC_FORCE1);

	case LOC_WRITE_CHAR:	/* write-char */
	case LOC_WRITE:		/* write */
	case LOC_DISPLAY:	/* display */
		switch (location) {
		case LOC_WRITE_CHAR:
			if (!validargs("write-char", 1, 2, TST_CHAR TST_OUTPORT)) Error_0(msg);
			break;
		case LOC_WRITE:
			if (!validargs("write", 1, 2, TST_ANY TST_OUTPORT)) Error_0(msg);
			break;
		case LOC_DISPLAY:
			if (!validargs("display", 1, 2, TST_ANY TST_OUTPORT)) Error_0(msg);
			break;
		default:
			break;
		}
		if (is_pair(cdr(args))) {
			if (cadr(args) != current_outport) {
				current_outport = cons(current_outport, NIL);
				s_save(LOC_CURR_OUTPORT, current_outport, NIL);
				current_outport = cadr(args);
			}
		}
		args = car(args);
		print_flag = (location == LOC_WRITE) ? 1 : 0;
		s_goto(LOC_P0LIST);

	case LOC_NEWLINE:	/* newline */
		if (!validargs("newline", 0, 1, TST_OUTPORT)) Error_0(msg);
		if (is_pair(args)) {
			if (car(args) != current_outport) {
				current_outport = cons(current_outport, NIL);
				s_save(LOC_CURR_OUTPORT, current_outport, NIL);
				current_outport = car(args);
			}
		}
		putstr("\n");
		s_return(T);

	case LOC_FLUSH_OUTPORT:	/* flush-output-port */
	    if (!validargs("flush-output-port", 0, 1, TST_OUTPORT)) Error_0(msg);
	    if( is_pair( args )) 
		args = car( args );
	    else
		args = current_outport;
	    if( is_fileport( args ))
		fflush( port_file( args ));
	    s_return( T );

	case LOC_DFLT_XHAND0:	/* default exception handler */
	    if (!validargs("default-exception-handler", 1, 1, TST_ANY)) Error_0(msg);
	    s_save( LOC_DFLT_XHAND1, args, NIL );
	    s_save( LOC_ERROBJP, args, NIL );
	    args = cons( current_outport, NIL );
	    s_goto(LOC_FLUSH_OUTPORT);

	case LOC_DFLT_XHAND1:
	    x = cons( current_outport, NIL );
	    s_save( LOC_DFLT_XHAND3, NIL, NIL );
	    s_save( LOC_CURR_OUTPORT, x, NIL );
	    current_outport = current_errport;
	    x = car( args );
	    if( istrue( value ))
	    {
		putstr( strvalue( vector_elem( car( args ), 2 )));
		x = vector_elem( car( args ), 3);
		if( x == NIL )
		{
		    putstr( "\n");
		    s_return( T );
		}
		else
		{
		    putstr( ":");
		    args = x;
		}
	    }
	    else
	    {
		putstr( "Exception - raised with:" );
		args = cons( x, NIL);
	    }
	    /* fall through */

	case LOC_DFLT_XHAND2:
	    if( is_null( args ))
	    {
		putstr( "\n" );
		s_return( T );
	    }
	    putstr( "\n" );
	    x = cdr( args );
	    s_save( LOC_DFLT_XHAND2, x, NIL );
	    args = car( args );
	    print_flag = 1;
	    s_goto( LOC_P0LIST );

	case LOC_DFLT_XHAND3:
	    s_return( F );


	case LOC_RAISE0: /* raise */
	    if (!validargs("raise", 1, 1, TST_ANY)) Error_0(msg);
	    x = cons( current_xhands, NIL );
	    s_save( LOC_CURR_XHANDS, x, NIL );
	    x = cons( current_outport, NIL );
	    s_save( LOC_CURR_OUTPORT, x, NIL );
	    current_outport = current_errport;

	    /* fall through */

	case LOC_RAISE1:
	    if( is_null( current_xhands ))
	    {
		x = cons( value, NIL);
		s_save(LOC_EMERGENCY_EXIT, x, NIL);

		args = cons( call_history, NIL);
		print_flag = 1;
		s_goto( LOC_P0HIST );
	    }
	    s_save( LOC_RAISE1, args, NIL );
	    code = car( current_xhands );
	    current_xhands = cdr( current_xhands );
	    s_goto( LOC_APPLY );

	case LOC_ERROR:  /* error */
	    if (!validargs("error", 1, 65535, TST_STRING TST_ANY)) Error_0(msg);
	    x = mk_vector(4);
	    set_vector_elem( x, 0, mk_symbol( "<ERROR-OBJECT>" ));
	    set_vector_elem( x, 1, mk_symbol( "record-type_0" ));
	    set_vector_elem( x, 2, car( args ));
	    set_vector_elem( x, 3, cdr( args ));
	    args = cons( x, NIL );
	    s_goto( LOC_RAISE0 );


	case LOC_ERROBJP:  /* error-object? */
	    if (!validargs("error-object?", 1, 1, TST_ANY)) Error_0(msg);
	    x = car( args );
	    if( is_vector( x ) && 
	    	ivalue( x ) >= 4 &&
		is_symbol( vector_elem( x, 0 )) &&
	    	!strcmp( strvalue( vector_elem( x, 0 )), "<ERROR-OBJECT>" ) &&
		is_symbol( vector_elem( x, 1 )) &&
		is_string( vector_elem( x, 2 )) &&
		( is_null( vector_elem( x, 3 )) || is_pair( vector_elem( x, 3 ))))
	    {
		s_return( T );
	    }
	    s_return( F );

	case LOC_ERRMSG0:  /* error-object-message */
	    if (!validargs("error-object-message", 1, 1, TST_ANY)) Error_0(msg);
	    s_save( LOC_ERRMSG1, args, NIL );
	    s_goto( LOC_ERROBJP );

	case LOC_ERRMSG1:
	    x = car( args );
	    if( isfalse( value ))
	    {
		args = cons( mk_string( "Type error - argument 1 in call to 'error-object-message' is not a proper error-object" ), cons( x, NIL ));
		s_goto( LOC_ERROR );
	    }
	    s_return( vector_elem( car( args ), 2 ));

	case LOC_ERRIRR0:  /* error-object-irritants */
	    if (!validargs("error-object-irritants", 1, 1, TST_ANY)) Error_0(msg);
	    s_save( LOC_ERRIRR1, args, NIL );
	    s_goto( LOC_ERROBJP );

	case LOC_ERRIRR1:
	    x = car( args );
	    if( isfalse( value ))
	    {
		args = cons( mk_string( "Type error - argument 1 in call to 'error-object-irritants' is not a proper error-object" ), cons( x, NIL ));
		s_goto( LOC_ERROR );
	    }
	    s_return( vector_elem( car( args ), 3 ));

	case LOC_FILERRP0:  /* file-error? */
	    if (!validargs("file-error?", 1, 1, TST_ANY)) Error_0(msg);
	    s_save( LOC_FILERRP1, args, NIL );
	    s_goto( LOC_ERROBJP );

	case LOC_FILERRP1:
	    if( isfalse( value ))
		s_return( F );
	    s_save( LOC_FILERRP2, args, NIL );
	    s_goto( LOC_ERRMSG1 );

	case LOC_FILERRP2:
	    x = car( args );
	    if( strlength( value ) < 4 )
		s_return( F );
	    if( tolower( strvalue( value )[0] ) == 'f' &&
		tolower( strvalue( value )[1] ) == 'i' &&
		tolower( strvalue( value )[2] ) == 'l' &&
		tolower( strvalue( value )[3] ) == 'e')
		s_return( T );
	    s_return( F );

	case LOC_REDERRP0:  /* read-error? */
	    if (!validargs("read-error?", 1, 1, TST_ANY)) Error_0(msg);
	    s_save( LOC_REDERRP1, args, NIL );
	    s_goto( LOC_ERROBJP );

	case LOC_REDERRP1:
	    if( isfalse( value ))
		s_return( F );
	    s_save( LOC_REDERRP2, args, NIL );
	    s_goto( LOC_ERRMSG1 );

	case LOC_REDERRP2:
	    x = car( args );
	    if( strlength( value ) < 4 )
		s_return( F );
	    if( tolower( strvalue( value )[0] ) == 'r' &&
		tolower( strvalue( value )[1] ) == 'e' &&
		tolower( strvalue( value )[2] ) == 'a' &&
		tolower( strvalue( value )[3] ) == 'd')
		s_return( T );
	    s_return( F );

	case LOC_REVERSE:	/* reverse */
		if (!validargs("reverse", 1, 1, TST_LIST)) Error_0(msg);
		s_return(reverse(car(args)));

	case LOC_APPEND:	/* append */
		if (!validargs("append", 0, 65535, TST_NONE)) Error_0(msg);
		if( args == NIL ) s_return( NIL );
		x = reverse( args );
		y = cdr( x );
		x = car( x );
		while( is_pair( y )) 
		{
		    x = append( car( y ), x );
		    y = cdr( y );
		}
		s_return( x );

	case LOC_QUIT:		/* quit */
		if (!validargs("quit", 0, 1, TST_INTEGER)) Error_0(msg);
		if (is_pair(args)) {
			return (int)ivalue(car(args));
		}
		return 0;

	case LOC_EMERGENCY_EXIT:	/* emergency-exit */
		if (!validargs("emergency-exit", 0, 1, TST_ANY)) Error_0(msg);
		if( is_pair( args )) 
		{
		    x = car( args );
		    if( is_integer( x ))
			exit( (int)ivalue(car(args)));
		    else if( isfalse( x ))
			exit( 1 );			
		}
		exit( 0 );

	case LOC_GC:		/* gc */
		if (!validargs("gc", 0, 0, TST_NONE)) Error_0(msg);
		gc(&NIL, &NIL);
		s_return(T);

	case LOC_GCVERB:		/* gc-verbose */
		if (!validargs("gc-verbose", 0, 1, TST_NONE)) Error_0(msg);
		w = gc_verbose;
		gc_verbose = (car(args) != F);
		s_retbool(w);

	case LOC_CALL_INFILE0:	/* call-with-input-file */
		if (!validargs("call-with-input-file", 2, 2, TST_STRING TST_ANY)) Error_0(msg);
		x = port_from_filename(strvalue(car(args)), T_PORT_INPUT | T_PORT_TEXT );
		if (x == NIL) {
			s_return(F);
		}
		code = cadr(args);
		args = cons(x, NIL);
		s_save(LOC_CALL_INFILE1, args, NIL);
		s_goto(LOC_APPLY);

	case LOC_CALL_INFILE1:	/* call-with-input-file */
		port_close(car(args));
		s_return(value);

	case LOC_CALL_OUTFILE0:	/* call-with-output-file */
		if (!validargs("call-with-output-file", 2, 2, TST_STRING TST_ANY)) Error_0(msg);
		x = port_from_filename(strvalue(car(args)), T_PORT_OUTPUT | T_PORT_TEXT );
		if (x == NIL) {
			s_return(F);
		}
		code = cadr(args);
		args = cons(x, NIL);
		s_save(LOC_CALL_OUTFILE1, args, NIL);
		s_goto(LOC_APPLY);

	case LOC_CALL_OUTFILE1:	/* call-with-output-file */
		port_close(car(args));
		s_return(value);

	case LOC_CURR_INPORT:	/* current-input-port */
		if (!validargs("current-input-port", 0, 1, TST_INPORT)) Error_0(msg);
		if (is_pair(args)) current_inport = car(args);
		s_return(current_inport);

	case LOC_CURR_OUTPORT:	/* current-output-port */
		if (!validargs("current-output-port", 0, 1, TST_OUTPORT)) Error_0(msg);
		if (is_pair(args)) current_outport = car(args);
		s_return(current_outport);

	case LOC_CURR_ERRPORT:	/* current-error-port */
		if (!validargs("current-error-port", 0, 1, TST_OUTPORT)) Error_0(msg);
		if (is_pair(args)) current_errport = car(args);
		s_return(current_errport);

	case LOC_CURR_XHANDS:	/* current-exception-handlers */
		if (!validargs("current-exception-handlers", 0, 1, TST_LIST)) Error_0(msg);
		if (is_pair(args)) current_xhands = car(args);
		s_return(current_xhands);

	case LOC_CURR_SOURCE:	/* current-source */
		if (!validargs("current-source", 0, 1, TST_STRING)) Error_0(msg);
		if (is_pair(args)) current_source = car(args);
		s_return(current_source);

	case LOC_CMD_LINE:	/* command-line */
		if (!validargs("command-line", 0, 1, TST_LIST)) Error_0(msg);
		if (is_pair(args)) command_line = car(args);
		s_return(command_line);

	case LOC_WITH_INFILE0:	/* with-input-from-file */
		if (!validargs("with-input-from-file", 2, 2, TST_STRING TST_ANY)) Error_0(msg);
		x = port_from_filename(strvalue(car(args)), T_PORT_INPUT | T_PORT_TEXT );
		if (x == NIL) {
		    Error_1("Read error - unable to open file", car(args));
		    /* s_return(F); */
		}
		code = cadr(args);
		args = cons(x, current_inport);
		current_inport = car(args);
		s_save(LOC_WITH_INFILE1, args, NIL);
		args = NIL;
		s_goto(LOC_APPLY);

	case LOC_WITH_INFILE1:	/* with-input-from-file */
		port_close(car(args));
		current_inport = cdr(args);
		s_return(value);

	case LOC_WITH_OUTFILE0:	/* with-output-to-file */
		if (!validargs("with-output-to-file", 2, 2, TST_STRING TST_ANY)) Error_0(msg);
		x = port_from_filename(strvalue(car(args)), T_PORT_OUTPUT | T_PORT_TEXT );
		if (x == NIL) {
			s_return(F);
		}
		code = cadr(args);
		args = cons(x, current_outport);
		current_outport = car(args);
		s_save(LOC_WITH_OUTFILE1, args, NIL);
		args = NIL;
		s_goto(LOC_APPLY);

	case LOC_WITH_OUTFILE1:	/* with-output-to-file */
		port_close(car(args));
		current_outport = cdr(args);
		s_return(value);

	case LOC_OPEN_INFILE:	/* open-input-file */
		if (!validargs("open-input-file", 1, 1, TST_STRING)) Error_0(msg);
		x = port_from_filename(strvalue(car(args)), T_PORT_INPUT | T_PORT_TEXT );
		if (x == NIL) {
			s_return(F);
		}
		s_return(x);

	case LOC_OPEN_OUTFILE:	/* open-output-file */
		if (!validargs("open-output-file", 1, 1, TST_STRING)) Error_0(msg);
		x = port_from_filename(strvalue(car(args)), T_PORT_OUTPUT | T_PORT_TEXT );
		if (x == NIL) {
			s_return(F);
		}
		s_return(x);

	case LOC_OPEN_INOUTFILE:	/* open-input-output-file */
		if (!validargs("open-input-output-file", 1, 1, TST_STRING)) Error_0(msg);
		x = port_from_filename(strvalue(car(args)), T_PORT_INPUT | T_PORT_OUTPUT | T_PORT_TEXT );
		if (x == NIL) {
			s_return(F);
		}
		s_return(x);



	case LOC_OPEN_BINFILE:	/* open-binary-input-file */
		if (!validargs("open-binary-input-file", 1, 1, TST_STRING)) Error_0(msg);
		x = port_from_filename(strvalue(car(args)), T_PORT_INPUT | T_PORT_BINARY );
		if (x == NIL) {
			s_return(F);
		}
		s_return(x);

	case LOC_OPEN_BOUTFILE:	/* open-binary-output-file */
		if (!validargs("open-binary-output-file", 1, 1, TST_STRING)) Error_0(msg);
		x = port_from_filename(strvalue(car(args)), T_PORT_OUTPUT | T_PORT_BINARY );
		if (x == NIL) {
			s_return(F);
		}
		s_return(x);

	case LOC_OPEN_BINOUTFILE:	/* open-binary-input-output-file */
		if (!validargs("open-binary-input-output-file", 1, 1, TST_STRING)) Error_0(msg);
		x = port_from_filename(strvalue(car(args)), T_PORT_INPUT | T_PORT_OUTPUT | T_PORT_BINARY );
		if (x == NIL) {
			s_return(F);
		}
		s_return(x);



	case LOC_OPEN_INSTRING:	/* open-input-string */
		if (!validargs("open-input-string", 1, 1, TST_STRING)) Error_0(msg);
		x = port_from_string(strvalue(car(args)), T_PORT_INPUT | T_PORT_TEXT );
		if (x == NIL) {
			s_return(F);
		}
		s_return(x);

	case LOC_OPEN_OUTSTRING:	/* open-output-string */
		if (!validargs("open-output-string", 0, 0, TST_NONE)) Error_0(msg);
		x = port_from_scratch();
		if (x == NIL) {
			s_return(F);
		}
		s_return(x);

	case LOC_OPEN_INOUTSTRING:	/* open-input-output-string */
		if (!validargs("open-input-output-string", 1, 1, TST_STRING)) Error_0(msg);
		x = port_from_string(strvalue(car(args)), T_PORT_INPUT | T_PORT_OUTPUT);
		if (x == NIL) {
			s_return(F);
		}
		s_return(x);

	case LOC_GET_OUTSTRING:	/* get-output-string */
		if (!validargs("get-output-string", 1, 1, TST_OUTPORT)) Error_0(msg);
		x = car(args);
		if (is_strport(x) && port_file(x) != NULL) {
			s_return(mk_string(strvalue(car(x))));
		}
		s_return(F);

	case LOC_CLOSE_INPORT: /* close-input-port */
		if (!validargs("close-input-port", 1, 1, TST_INPORT)) Error_0(msg);
		port_close(car(args));
		s_return(T);

	case LOC_CLOSE_OUTPORT: /* close-output-port */
		if (!validargs("close-output-port", 1, 1, TST_OUTPORT)) Error_0(msg);
		port_close(car(args));
		s_return(T);

	case LOC_CLOSE_PORT: /* close-port */
		if (!validargs("close-port", 1, 1, TST_PORT)) Error_0(msg);
		port_close(car(args));
		s_return(T);

	case LOC_CURR_ENV:	/* current-environment */
		if (!validargs("current-environment", 0, 0, TST_NONE)) Error_0(msg);
		s_return(envir);

	case LOC_GLOB_ENV:	/* global-environment */
		if (!validargs("global-environment", 0, 0, TST_NONE)) Error_0(msg);
		s_return(global_env);

		/* ========== reading part ========== */
	case LOC_READ:			/* read */
		if (!validargs("read", 0, 1, TST_INPORT)) Error_0(msg);
		if (is_pair(args)) {
			if (port_file(car(args)) == NULL) {
				Error_0("input port was closed");
			}
			if (car(args) != current_inport) {
				current_inport = cons(current_inport, NIL);
				s_save(LOC_CURR_INPORT, current_inport, NIL);
				current_inport = car(args);
			}
		}
		s_goto(LOC_READ_INTERNAL);

	case LOC_READ_CHAR:		/* read-char */
	case LOC_PEEK_CHAR:		/* peek-char */
		switch (location) {
		case LOC_READ_CHAR:
			if (!validargs("read-char", 0, 1, TST_INPORT)) Error_0(msg);
			break;
		case LOC_PEEK_CHAR:
			if (!validargs("peek-char", 0, 1, TST_INPORT)) Error_0(msg);
			break;
		default:
			break;
		}
		if (is_pair(args)) {
			if (port_file(car(args)) == NULL) {
				Error_0("input port was closed");
			}
			if (car(args) != current_inport) {
				current_inport = cons(current_inport, NIL);
				s_save(LOC_CURR_INPORT, current_inport, NIL);
				current_inport = car(args);
			}
		}
		w = inchar();
		if (w == EOF) {
			s_return(EOF_OBJ);
		}
		if (location == LOC_PEEK_CHAR) {
			backchar((int)w);
		}
		s_return(mk_character((int)w));

	case LOC_CHAR_READY:		/* char-ready? */
		if (!validargs("char-ready?", 0, 1, TST_INPORT)) Error_0(msg);
		if (is_pair(args)) {
			x = car(args);
		} else {
			x = current_inport;
		}
		s_retbool(is_fileport(x) || is_strport(x));

	case LOC_RDSEXPR:

		if (tok == TOK_EOF) {
			s_return(EOF_OBJ);
		}
		switch (tok & 0x0f) {
		case TOK_VEC:
			s_save(LOC_RDVEC, NIL, code);
			/* fall through */
		case TOK_LPAREN:
			w = tok >> 4;
			tok = token();
			if ((tok & 0x0f) == TOK_RPAREN) {
				if (w == (tok >> 4)) {
					s_return(NIL);
				}
				Error_0("syntax error -- unexpected parenthesis");
			} else if (tok == TOK_DOT) {
				Error_0("syntax error -- illegal dot expression");
			} else {
				code = mk_integer((int32_t)w);
				s_save(LOC_RDLIST, NIL, code);
				s_goto(LOC_RDSEXPR);
			}
		case TOK_QUOTE:
			s_save(LOC_RDQUOTE, NIL, code);
			tok = token();
			s_goto(LOC_RDSEXPR);
		case TOK_BQUOTE:
			tok = token();
			if ((tok & 0x0f) == TOK_VEC) {
				s_save(LOC_RDQQUOTEVEC, NIL, code);
				tok = TOK_LPAREN | (tok >> 4);
			} else {
				s_save(LOC_RDQQUOTE, NIL, code);
			}
			s_goto(LOC_RDSEXPR);
		case TOK_COMMA:
			s_save(LOC_RDUNQUOTE, NIL, code);
			tok = token();
			s_goto(LOC_RDSEXPR);
		case TOK_ATMARK:
			s_save(LOC_RDUQTSP, NIL, code);
			tok = token();
			s_goto(LOC_RDSEXPR);
		case TOK_ATOM:
			s_return(mk_atom(readstr("()[]{};\t\n\r ")));
		case TOK_DQUOTE:
			s_return(readstrexp());
		case TOK_SHARP:
			if ((x = mk_const(readstr("()[]{};\t\n\r "))) == NIL) {
				Error_0("undefined sharp expression");
			} else {
				s_return(x);
			}
		default:
			Error_0("syntax error -- illegal token");
		}
		break;

	case LOC_RDLIST:
		args = cons(value, args);
		tok = token();
		if (tok == TOK_EOF) {
			s_return(EOF_OBJ);
		} else if ((tok & 0x0f) == TOK_RPAREN) {
			if (ivalue(code) == (tok >> 4)) {
				s_return(non_alloc_rev(NIL, args));
			}
			Error_0("syntax error -- unexpected parenthesis");
		} else if (tok == TOK_DOT) {
			s_save(LOC_RDDOT, args, code);
			tok = token();
			s_goto(LOC_RDSEXPR);
		} else {
			s_save(LOC_RDLIST, args, code);
			s_goto(LOC_RDSEXPR);
		}

	case LOC_RDDOT:
		tok = token();
		if ((tok & 0x0f) == TOK_RPAREN) {
			if (ivalue(code) == (tok >> 4)) {
				s_return(non_alloc_rev(value, args));
			}
			Error_0("syntax error -- unexpected parenthesis");
		} else {
			Error_0("syntax error -- illegal dot expression");
		}

	case LOC_RDQUOTE:
		x = cons(value, NIL);
		s_return(cons(QUOTE, x));

	case LOC_RDQQUOTE:
		x = cons(value, NIL);
		s_return(cons(QQUOTE, x));

	case LOC_RDQQUOTEVEC:
		x = cons(value, NIL);
		x = cons(QQUOTE, x);
		args = cons(x, NIL);
		x = mk_symbol("vector");
		args = cons(x, args);
		x = mk_symbol("apply");
		s_return(cons(x, args));

	case LOC_RDUNQUOTE:
		x = cons(value, NIL);
		s_return(cons(UNQUOTE, x));

	case LOC_RDUQTSP:
		x = cons(value, NIL);
		s_return(cons(UNQUOTESP, x));

	case LOC_RDVEC:
		args = value;
		s_goto(LOC_VECTOR);

	/* ========== printing part ========== */
	case LOC_P0LIST:

		if (is_vector(args)) {
			putstr("#(");
			x = mk_integer(0);
			args = cons(args, x);
			s_goto(LOC_PVECFROM);
		} else if (is_environment(args)) {
		        putstr("#<ENVIRONMENT>");
			s_return(T);
		} else if (!is_pair(args)) {
			printatom(args, print_flag);
			s_return(T);
		} else if (car(args) == QUOTE && ok_abbrev(cdr(args))) {
			putstr("'");
			args = cadr(args);
			s_goto(LOC_P0LIST);
		} else if (car(args) == QQUOTE && ok_abbrev(cdr(args))) {
			putstr("`");
			args = cadr(args);
			s_goto(LOC_P0LIST);
		} else if (car(args) == UNQUOTE && ok_abbrev(cdr(args))) {
			putstr(",");
			args = cadr(args);
			s_goto(LOC_P0LIST);
		} else if (car(args) == UNQUOTESP && ok_abbrev(cdr(args))) {
			putstr(",@");
			args = cadr(args);
			s_goto(LOC_P0LIST);
		} else {
			putstr("(");
			s_save(LOC_P1LIST, cdr(args), NIL);
			args = car(args);
			s_goto(LOC_P0LIST);
		}

	case LOC_P1LIST:
		if (is_pair(args)) {
			s_save(LOC_P1LIST, cdr(args), NIL);
			putstr(" ");
			args = car(args);
			s_goto(LOC_P0LIST);
		} else if (is_vector(args)) {
			s_save(LOC_P1LIST, NIL, NIL);
			putstr(" . ");
			s_goto(LOC_P0LIST);
		} else {
			if (args != NIL) {
				putstr(" . ");
				printatom(args, print_flag);
			}
			putstr(")");
			s_return(T);
		}

	case LOC_PVECFROM:

		w = ivalue(cdr(args));
		if (w == ivalue(car(args))) {
			putstr(")");
			s_return(T);
		} else {
			ivalue(cdr(args)) = (int32_t)w + 1;
			s_save(LOC_PVECFROM, args, NIL);
			args = vector_elem(car(args), (int)w);
			if (w > 0) putstr(" ");
			s_goto(LOC_P0LIST);
		}


	    /* ========== printing call history ========== */

	case LOC_P0HIST:
	    putstr( "\nCall history:\n" );
	    x = mk_integer( call_history_pos - 1 );
	    args = cons( car( args ), x );
	    s_goto( LOC_P1HIST );

	case LOC_P1HIST:
	    w = ivalue( cdr( args ));
	    if( w == -1 ) 
	    {
		ivalue( cdr( args )) = (int32_t)CALL_HISTORY_LENGTH - 1;
		s_goto( LOC_P2HIST );
	    } 
	    else 
	    {
		putstr( "\n     " );
		ivalue( cdr( args )) = (int32_t)w - 1;
		s_save( LOC_P1HIST, args, NIL );
		args = vector_elem( car( args ), (int)w );
		s_goto( LOC_P0LIST );
	    }

	case LOC_P2HIST:
	    w = ivalue( cdr( args ));
	    x = vector_elem( car( args ), (int)w );
	    if( w < call_history_pos ||  x == NIL )
	    {
		putstr( "\n" );
		s_return( T );
	    }
	    else
	    {
	    	putstr( "\n     " );
	    	ivalue( cdr( args )) = (int32_t)w - 1;
	    	s_save( LOC_P2HIST, args, NIL );
	    	args = x;
	    	s_goto( LOC_P0LIST );
	    }

	    /* =========================================== */


	case LOC_LIST_LENGTH:	/* length */	/* a.k */
		if (!validargs("length", 1, 1, TST_LIST)) Error_0(msg);
		w = list_length(car(args));
		if (w < 0) {
			Error_1("length: not a list:", car(args));
		}
		s_return(mk_integer((int32_t)w));

	case LOC_MEMQ:		/* memq */
		if (!validargs("memq", 2, 2, TST_ANY TST_LIST)) Error_0(msg);
		x = car(args);
		for (y = cadr(args); y != NIL; y = cdr(y)) {
			if (x == car(y)) s_return(y);
		}
		s_return(F);

	case LOC_MEMV:		/* memv */
		if (!validargs("memv", 2, 2, TST_ANY TST_LIST)) Error_0(msg);
		x = car(args);
		for (y = cadr(args); y != NIL; y = cdr(y)) {
			if (eqv(x, car(y))) s_return(y);
		}
		s_return(F);

	case LOC_MEMBER:		/* member*/
		if (!validargs("member", 2, 2, TST_ANY TST_LIST)) Error_0(msg);
		x = car(args);
		for (y = cadr(args); y != NIL; y = cdr(y)) {
			if (equal(x, car(y))) s_return(y);
		}
		s_return(F);

	case LOC_ASSQ:		/* assq */
		if (!validargs("assq", 2, 2, TST_ANY TST_LIST)) Error_0(msg);
		x = car(args);
		for (y = cadr(args); is_pair(y); y = cdr(y)) {
			if (!is_pair(car(y))) {
				Error_1("Syntax error - improper alist in call to 'assq'", y);
			}
			if (x == caar(y)) s_return(car(y));
		}
		s_return(F);

	case LOC_ASSV:		/* assv*/
		if (!validargs("assv", 2, 2, TST_ANY TST_LIST)) Error_0(msg);
		x = car(args);
		for (y = cadr(args); is_pair(y); y = cdr(y)) {
			if (!is_pair(car(y))) {
				Error_1("Syntax error - improper alist in call to 'assv'", y);
			}
			if (eqv(x, caar(y))) s_return(car(y));
		}
		s_return(F);

	case LOC_ASSOC:		/* assoc */
		if (!validargs("assoc", 2, 2, TST_ANY TST_LIST)) Error_0(msg);
		x = car(args);
		for (y = cadr(args); is_pair(y); y = cdr(y)) {
			if (!is_pair(car(y))) {
			    Error_1("Syntax error - improper alist in call to 'assoc'", y);
			}
			if (equal(x, caar(y))) s_return(car(y));
		}
		s_return(F);

	case LOC_GET_CLOSURE:	/* get-closure-code */	/* a.k */
		if (!validargs("get-closure-code", 1, 1, TST_NONE)) Error_0(msg);
		args = car(args);
		if (args == NIL) {
			s_return(F);
		} else if (is_closure(args)) {
			s_return(cons(LAMBDA, closure_code(value)));
		} else {
			s_return(F);
		}

	case LOC_CLOSUREP:		/* closure? */
		if (!validargs("closure?", 1, 1, TST_NONE)) Error_0(msg);
		/*
		 * Note, macro object is also a closure.
		 * Therefore, (closure? <#MACRO>) ==> #t
		 */
		if (car(args) == NIL) {
			s_return(F);
		}
		s_retbool(is_closure(car(args)));

	case LOC_MACROP:		/* macro? */
		if (!validargs("macro?", 1, 1, TST_NONE)) Error_0(msg);
		if (car(args) == NIL) {
			s_return(F);
		}
		s_retbool(is_closure(car(args)) && is_macro(car(args)));

	case LOC_MACRO_EXPAND0:	/* macro-expand */
		if (!validargs("macro-expand", 1, 1, TST_LIST)) Error_0(msg);
		s_save(LOC_MACRO_EXPAND1, args, NIL);
		code = caar(args);
		args = NIL;
		s_goto(LOC_EVAL);

	case LOC_MACRO_EXPAND1:	/* macro-expand */
		if (is_closure(value) && is_macro(value)) {
			code = value;
			s_save(LOC_MACRO_EXPAND2, args, code);
			code = cons(LAMBDA, closure_code(code));
			args = NIL;
			s_goto(LOC_EVAL);
		}
		s_return(car(args));

	case LOC_MACRO_EXPAND2:	/* macro-expand */
		if (syntaxnum(code) & T_DEFSYNTAX) {
			value = code;
			code = car(args);
			s_goto(LOC_EXPANDPATTERN);
		} else if (exttype(code) & T_DEFMACRO) {
			args = cdar(args);
		}
		code = value;
		s_goto(LOC_APPLY);

	case LOC_ATOMP:		/* atom? */
		if (!validargs("atom?", 1, 1, TST_NONE)) Error_0(msg);
		s_retbool(is_atom(car(args)));

	default:
		sprintf(msg, "%d is illegal location", location);
		Error_0(msg);
	}

	return 0;
}

/* ========== Initialization of internal keywords ========== */

pointer mk_syntax( enum eval_location location, char *name )
{
    pointer x;
    x = mk_string( name );
    type( x ) |= T_SYMBOL | T_SYNTAX;
    syntaxnum( x ) = (short)location;
    return x;
}

void scheme_register_syntax( enum eval_location location, char *name, pointer environment )
{
    pointer x,y;

    x = mk_symbol( name );
    y = mk_syntax( location, name );
    x = cons( x, y );
    x = cons( x, car(environment ));
    car( environment ) = x;
}

pointer mk_operation( enum eval_location location, pointer *pp )
{
    pointer x = get_cell( pp, &NIL );
    type( x ) = ( T_OPERATION | T_ATOM );
    ivalue( x ) = location;
    set_num_integer( x );
    return x;
}

void scheme_register_operation( enum eval_location location, char *name, pointer environment )
{
    pointer x, y;
    x = mk_symbol( name );
    y = mk_operation( location, &x );
    x = cons( x, y );
    x = cons( x, car( environment ));
    car( environment ) = x;
}

static void init_vars_global( int argc, char **argv )
{
	symbol_list = NIL;
	winders = NIL;
#ifdef USE_COPYING_GC
	gcell_list = NIL;
#endif
	/* init NIL */
	type(NIL) = (T_ATOM | MARK);
	car(NIL) = cdr(NIL) = NIL;
	/* init T */
	type(T) = (T_ATOM | MARK);
	car(T) = cdr(T) = T;
	/* init F */
	type(F) = (T_ATOM | MARK);
	car(F) = cdr(F) = F;
	/* init EOF_OBJ */
	type(EOF_OBJ) = (T_ATOM | MARK);
	car(EOF_OBJ) = cdr(EOF_OBJ) = EOF_OBJ;
	/* init UNDEF */
	type(UNDEF) = (T_ATOM | MARK);
	car(UNDEF) = cdr(UNDEF) = UNDEF;
	/* init global_env */
	global_env = cons(NIL, NIL);
	setenvironment(global_env);
	/* init call_history */
	call_history = mk_vector(CALL_HISTORY_LENGTH);
	{ int i = 0; for( i = 0; i < CALL_HISTORY_LENGTH; i++) set_vector_elem(call_history, i, NIL); }
	call_history_pos = 0;

	type(&_ZERO) = T_NUMBER | T_ATOM;
	set_num_integer(&_ZERO);
	ivalue(&_ZERO) = 0;
	bignum(&_ZERO) = NIL;
	type(&_ONE) = T_NUMBER | T_ATOM;
	set_num_integer(&_ONE);
	ivalue(&_ONE) = 1;
	bignum(&_ONE) = NIL;
	load_stack[0] = mk_port(stdin, T_PORT_INPUT);
	load_files = 1;

	current_outport = mk_port(stdout, T_PORT_OUTPUT | T_PORT_INPUT );
	current_errport = mk_port(stderr, T_PORT_OUTPUT | T_PORT_INPUT );
	current_xhands = cons( mk_operation( LOC_DFLT_XHAND0, &NIL), NIL );
	current_source = mk_string( "" );

	command_line = NIL;
	{ int i; for( i = argc - 1; i >= 0; i-- ) command_line = cons( mk_string( argv[i] ), command_line ); }

	strbuff = mk_memblock(256, &NIL, &NIL);
}


/* initialization of AutoScheme */
void scheme_init( int argc, char **argv )
{
	alloc_cellseg();
	gc_verbose = 0;

	/* initialize several globals */
	init_vars_global( argc, argv );

	/* intialization of global pointers to special symbols */
	LAMBDA = mk_symbol("lambda");
	QUOTE = mk_symbol("quote");
	QQUOTE = mk_symbol("quasiquote");
	UNQUOTE = mk_symbol("unquote");
	UNQUOTESP = mk_symbol("unquote-splicing");
	ELLIPSIS = mk_symbol("...");
#ifndef USE_SCHEME_STACK
	dump_base = mk_dumpstack(NIL);
	dump = dump_base;
#else
	dump = NIL;
#endif
	envir = global_env;
	code = NIL;
	value = NIL;
	mark_x = NIL;
	mark_y = NIL;
	c_nest = NIL;
	c_sink = NIL;
}

void scheme_deinit(void)
{
	symbol_list = NIL;
	current_inport = NIL;
	current_outport = NIL;
	current_errport = NIL;
	current_xhands = NIL;
	current_source = NIL;
	command_line = NIL;
	global_env = NIL;
	call_history = NIL;
	winders = NIL;
#ifdef USE_COPYING_GC
	gcell_list = NIL;
#endif
	args = NIL;
	envir = NIL;
	code = NIL;
#ifndef USE_SCHEME_STACK
	dump_base = NIL;
#endif
	dump = NIL;
	value = NIL;
	mark_x = NIL;
	mark_y = NIL;
	c_nest = NIL;
	c_sink = NIL;

	gc_verbose = 0;
	gc(&NIL, &NIL);
}

int scheme_load_file(FILE *fin)
{
	int op;

	if (fin == stdin) {
		interactive_repl = 1;
	}
	current_inport = mk_port(fin, T_PORT_INPUT);
	if (setjmp(error_jmp) == 0) {
		op = LOC_T0LVL;
	} else {
		op = LOC_QUIT;
	}
	return Eval_Cycle(op);
}

int scheme_load_string(const char *cmd)
{
	int op;

	interactive_repl = 0;
	current_inport = port_from_string(cmd, T_PORT_INPUT);
	if (setjmp(error_jmp) == 0) {
		op = LOC_T0LVL;
	} else {
		op = LOC_QUIT;
	}
	return Eval_Cycle(op);
}


void scheme_register_function( const char *name, foreign_function *ff, pointer environment )
{
	pointer s = mk_symbol(name);
	pointer f = mk_function(ff, &s), x;

	for (x = car(environment); x != NIL; x = cdr(x)) {
		if (caar(x) == s) {
			cdar(x) = f;
			return;
		}
	}
	x = cons(s, f);
	x = cons(x, car(environment));
	car(environment) = x;
}

static void save_from_C_call(void)
{
	pointer x;
#ifndef USE_SCHEME_STACK
	x = cons(dump, dump_base);
	x = cons(envir, x);
	x = cons(c_sink, x);
	c_nest = cons(x, c_nest);
	dump_base = mk_dumpstack(NIL);
	dump = dump_base;
#else
	x = cons(envir, dump);
	x = cons(c_sink, x);
	c_nest = cons(x, c_nest);
	dump = NIL;
#endif
	envir = global_env;
}

static void restore_from_C_call(void)
{
#ifndef USE_SCHEME_STACK
	c_sink = caar(c_nest);
	envir = car(cdar(c_nest));
	dump = cadr(cdar(c_nest));
	dump_base = cddr(cdar(c_nest));
#else
	c_sink = caar(c_nest);
	envir = car(cdar(c_nest));
	dump = cdr(cdar(c_nest));
#endif
	c_nest = cdr(c_nest);
}

pointer scheme_call(pointer procedure, pointer argslist)
{
	int old_repl = interactive_repl;
	interactive_repl = 0;
	save_from_C_call();
	envir = global_env;
	args = argslist;	/* assumed to be already evaluated */
	code = procedure;	/* assumed to be already evaluated */
	Eval_Cycle(LOC_APPLY);
	interactive_repl = old_repl;
	restore_from_C_call();
	return value;
}

pointer scheme_eval(pointer obj)
{
	int old_repl = interactive_repl;
	interactive_repl = 0;
	save_from_C_call();
	args = NIL;
	code = obj;
	Eval_Cycle(LOC_EVAL);
	interactive_repl = old_repl;
	restore_from_C_call();
	return value;
}
pointer autoscheme_eval( pointer object, pointer environment )
{
	int old_repl = interactive_repl;
	interactive_repl = 0;
	save_from_C_call();
	envir = environment;
	args = NIL;
	code = object;
	Eval_Cycle(LOC_EVAL);
	interactive_repl = old_repl;
	restore_from_C_call();
	return value;
}

/* ========== Error ==========  */

void FatalError(char *s)
{
	fprintf(stderr, "Fatal error: %s\n", s);
	flushinput();
	args = NIL;
	longjmp(error_jmp, LOC_QUIT);
}
void FatalForeignError( char *s )
{
    fprintf(stderr, "Fatal error: %s\n", s);
    flushinput();
    exit( 1 );
}


pointer tail_error( pointer message, pointer irritants, int error_num )
{
    pointer error_call;

    if( error_num ) 
	message = string_append( message, mk_string( error_num_to_msg( error_num )));

    error_call = cons( mk_operation( LOC_ERROR, &NIL), cons( message, irritants ));
    setfftailcall( error_call );
    return error_call;
}

/* ========== Helper Functions ========== */

int member( pointer object, pointer list )
{
    pointer x;
    for( x = list; x != NIL; x = cdr( x )) 
	if( equal( car( x ),  object )) return 1;
    return 0;
}

pointer assoc( pointer object, pointer alist )
{
    pointer x;
    for( x = alist; x != NIL; x = cdr( x )) 
	if( equal( caar( x ),  object )) return car( x );
    return F;
}

pointer make_environment( pointer alist )
{
    pointer environment = cons( alist, NIL );
    setenvironment( environment );
    return environment;
}

pointer string_append( pointer s1, pointer s2 )
{
    pointer result =  mk_counted_string( "", strlength( s1 ) + strlength( s2 ));
    memcpy( strvalue( result ), strvalue( s1 ), strlength( s1 ));
    memcpy( strvalue( result ) + strlength( s1 ), strvalue( s2 ), strlength( s2 ));
    strvalue( result )[ strlength( result ) ] = '\0';
    return result;
}
