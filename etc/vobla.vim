" Copyright (c) 2013-2014, ARM Limited
"
" Permission is hereby granted, free of charge, to any person obtaining a copy
" of this software and associated documentation files (the "Software"), to deal
" in the Software without restriction, including without limitation the rights
" to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
" copies of the Software, and to permit persons to whom the Software is
" furnished to do so, subject to the following conditions:
"
" The above copyright notice and this permission notice shall be included in
" all copies or substantial portions of the Software.
"
" THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
" IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
" FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
" AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
" LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
" OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
" THE SOFTWARE.
"
" Vim syntax file
" Language : VOBLA
" Latest Revision : 24 April 2013

if exists("b:current_syntax")
  finish
endif

" Comments
syn match voblaComment '/\*\([^*]\n*\**[\n^/]*\)*\**/'
syn match voblaComment '//.*'

" Names
syn match voblaId '[_A-Za-z][_A-Za-z0-9]*'

" Keywords
syn keyword voblaKeywords function export interface type storage import view
syn keyword voblaKeywords as let is implements parameter
syn keyword voblaKeywords by in out for while if else and or not forall return yield
syn keyword voblaTypes Value Index Complex Real Double Float
syn keyword voblaTypes Row Column Transposed Conjugated Reversed ApplyConj Array Diag Antidiag
syn keyword voblaBuiltin Re Im range len sum min max ceild floord abs sqrt sign base this

" Numbers
syn match voblaNumber '\d\+'
syn match voblaNumber '[-+]\d\+'
syn match voblaNumber '\d\+\.\d*'
syn match voblaNumber '[-+]\d\+\.\d*'

" links
let b:current_syntax = "vobla"

hi def link voblaKeywords Statement
hi def link voblaTypes Type
hi def link voblaBuiltin Identifier
hi def link voblaNumber Number
hi def link voblaComment Comment
