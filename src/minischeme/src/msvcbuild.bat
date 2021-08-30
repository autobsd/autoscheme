REM  This software is released under the MIT License.

REM  Copyright (c) 2015 Tatsuya Watanabe

REM  Permission is hereby granted, free of charge, to any person obtaining
REM  a copy of this software and associated documentation files (the
REM  "Software"), to deal in the Software without restriction, including
REM  without limitation the rights to use, copy, modify, merge, publish,
REM  distribute, sublicense, and/or sell copies of the Software, and to
REM  permit persons to whom the Software is furnished to do so, subject to
REM  the following conditions:

REM  The above copyright notice and this permission notice shall be
REM  included in all copies or substantial portions of the Software.

REM  THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
REM  EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
REM  MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
REM  NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE
REM  LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION
REM  OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION
REM  WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.


@setlocal
@set MSCOMPILE=cl /nologo /O2 /W3 /c /D_CRT_SECURE_NO_WARNINGS
@set MSLINK=link /nologo
@set MSLIB=lib /nologo

@if "%1" == "static" goto STATIC
@if not exist bin\ (
  mkdir bin
)
%MSCOMPILE% /MT /DSTANDALONE=1 miniscm.c
%MSCOMPILE% /MT bignum.c
%MSLINK% /out:bin\miniscm.exe miniscm.obj bignum.obj

@goto END

:STATIC
@if not exist lib\ (
  mkdir lib
)
%MSCOMPILE% /MT /DSTANDALONE=0 miniscm.c
%MSCOMPILE% /MT bignum.c
%MSLIB% /out:lib\miniscm.lib miniscm.obj bignum.obj

:END
del miniscm.obj bignum.obj
