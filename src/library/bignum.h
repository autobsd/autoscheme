/*  The content of this file has not been modified. 
 *  It is included with its original attribution as 
 *  part of the 'AutoScheme' project.
 */
/*  This software is released under the MIT License.
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

#ifndef BIGNUM_H
#define BIGNUM_H

#include <inttypes.h>

#ifdef __cplusplus
extern "C" {
#endif

int64_t gcd(int32_t x, int32_t y);
int64_t lcm(int32_t x, int32_t y);
int32_t find1_32(uint32_t val);
int32_t bn_eq(uint32_t x[], int32_t colx, uint32_t y[], int32_t coly);
int32_t bn_gt(uint32_t x[], int32_t colx, uint32_t y[], int32_t coly);
int32_t bn_ge(uint32_t x[], int32_t colx, uint32_t y[], int32_t coly);
void bn_add(uint32_t z[], int32_t *colz, uint32_t x[], int32_t colx, uint32_t y[], int32_t coly);
void bn_sub(uint32_t z[], int32_t *colz, uint32_t x[], int32_t colx, uint32_t y[], int32_t coly);
void bn_mul(uint32_t z[], int32_t *colz, uint32_t x[], int32_t colx, uint32_t y[], int32_t coly);
void bn_sqr(uint32_t z[], int32_t *colz, uint32_t x[], int32_t colx);
void bn_sftl(uint32_t z[], int32_t *colz, uint32_t x[], int32_t colx, int32_t n);
void bn_sftr(uint32_t z[], int32_t *colz, uint32_t x[], int32_t colx, int32_t n);
void bn_div(uint32_t q[], int32_t *colq, uint32_t r[], int32_t *colr, uint32_t x[], int32_t colx, uint32_t y[], int32_t coly);
int32_t bn_str2num_base2(const char s[], int32_t len, uint32_t x[], int32_t col);
int32_t bn_str2num_base8(const char s[], int32_t len, uint32_t x[], int32_t col);
int32_t bn_str2num_base10(const char s[], int32_t len, uint32_t x[], int32_t col);
int32_t bn_str2num_base16(const char s[], int32_t len, uint32_t x[], int32_t col);
char *bn_num2str_base2(char *p, uint32_t x[], int32_t col);
char *bn_num2str_base8(char *p, uint32_t x[], int32_t col);
char *bn_num2str_base10(char *p, uint32_t x[], int32_t col);
char *bn_num2str_base16(char *p, uint32_t x[], int32_t col);

#ifdef __cplusplus
}
#endif

#endif /* BIGNUM_H */
