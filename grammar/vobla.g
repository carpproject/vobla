/*
 * Copyright (c) 2013-2014, ARM Limited
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy
 * of this software and associated documentation files (the "Software"), to deal
 * in the Software without restriction, including without limitation the rights
 * to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
 * copies of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in
 * all copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
 * THE SOFTWARE.
 */

grammar vobla;

options {
  output=AST;
  ASTLabelType=CommonTree;
  memoize=true;
  backtrack=true;
}

tokens
{
  // Lexer tokens
  EQ='==';
  MOV='=';
  ARRAY_LEFT='[';
  ARRAY_RIGHT=']';
  BRACKET_LEFT='(';
  BRACKET_RIGHT=')';
  BLOCK_LEFT='{';
  BLOCK_RIGHT='}';
  SUM='sum';
  IN='in';
  OUT='out';
  IF='if';
  ELSE='else';
  FOR='for';
  WHILE='while';
  COMMA=',';
  COLON=':';
  SEMICOLON=';';
  DIV='/';
  MULT='*';
  PLUS='+';
  PLUS_MOV='+=';
  MULT_MOV='*=';
  DIV_MOV='/=';
  MINUS_MOV='-=';
  MINUS='-';
  LOR='or';
  LAND='and';
  LNOT='not';
  DECLV='Value';
  DECLI='Index';
  DECLC='Complex';
  DECLR='Real';
  VAR_DECL='let';
  FUNCTION='function';
  MORE='>';
  LESS='<';
  NEQ='!=';
  MOREEQ='>=';
  LESSEQ='<=';
  RANGE='range';
  LEN='len';
  BY='by';
  RETURN='return';
  NONAME='_';
  FORALL='forall';
  EXPORT='export';
  DOUBLE='Double';
  FLOAT='Float';
  AS='as';
  IS='is';
  TRANSPOSED='Transposed';
  CONJUGATED='Conjugated';
  APPLY_CONJUGATED='ApplyConj';
  ROW='Row';
  COLUMN='Column';
  MAIN_DIAGONAL='Diag';
  ANTI_DIAGONAL='Antidiag';
  REVERSED='Reversed';
  TYPEOF='typeof';
  RE='Re';
  IM='Im';
  STORAGE='storage';
  TYPE='type';
  INTERFACE='interface';
  IMPLEMENTS='implements';
  PARAMETER='parameter';
  VIEW='view';
  REF='&';
  DOT='.';
  YIELD='yield';
  IMPORT='import';

  // Tree tokens
  NONE;
  LIST;
  PROGRAM;
  METHOD;
  METHOD_HDR;
  COMPLEX_FLOAT;
  COMPLEX_DOUBLE;
  MEMBER;
  SCALAR_PARAM;
  ARRAY_PARAM;
  INTERFACE_PARAM;
  CALL;
  ARRAY_TYPE;
  ITERATION;
  SLICE;
  ARRAY_CST;
  OFFSET;
  VAR_DECL_ASSIGN;
  INTERFACE_NAME;
  TYPE_VERSION;
  TYPEDEF;
}

@lexer::header {
package com.arm.carp.vobla.parser;
}

@parser::header {
package com.arm.carp.vobla.parser;
}

@parser::members {
  private boolean correct = true;

  public boolean isCorrect() {
    return correct;
  }

  public void displayRecognitionError(String[] tokenNames, RecognitionException e) {
    String hdr = getErrorHeader(e);
    String msg = getErrorMessage(e, tokenNames);
    System.err.println(hdr + " " + msg);
    correct = false;
  }
}

// Skip whitespaces
WS: (' '|'\r'|'\t'|'\u000C'|'\n') {skip();};

// Coments
COMMENT:'/*' ( options {greedy=false;} : . )* '*/' {skip();};
LINE_COMMENT:'//' ~('\n'|'\r')* '\r'? '\n' {skip();};

// Lexer
NAME: ('A'..'Z'|'a'..'z'|'_') ('A'..'Z'|'a'..'z'|'_'|'0'..'9')*;
INT_NUMBER: '0'..'9'+;
DOUBLE_NUMBER: INT_NUMBER | '0'..'9'+ '.' '0'..'9'+;

// Rules
program: imp* toplevelDef* EOF -> ^(PROGRAM ^(LIST imp*) ^(LIST toplevelDef*));

imp: IMPORT^ NAME (DOT! NAME)* SEMICOLON!;

toplevelDef: function | export | storageDecl | typeDecl | interfaceDecl | viewDecl | typedef;

function: FUNCTION^ NAME BRACKET_LEFT! functionParameters BRACKET_RIGHT! functionReturnType functionBody;

functionReturnType: COLON scalarType -> scalarType | -> NONE;

export: EXPORT^ NAME LESS! valueType MORE! exportParameters AS! NAME SEMICOLON!;

storageDecl: STORAGE^ NAME BLOCK_LEFT! storageMember* BLOCK_RIGHT!;

typeDecl: TYPE^ NAME COLON! NAME implementList typeBody;

typedef: TYPE NAME MOV typeVersion SEMICOLON -> ^(TYPEDEF NAME typeVersion);

typeBody: BLOCK_LEFT PARAMETER COLON scalarMember* INTERFACE COLON methodImplementation* BLOCK_RIGHT ->
  ^(LIST scalarMember*) ^(LIST methodImplementation*);

interfaceDecl: INTERFACE^ interfaceName implementList BLOCK_LEFT! methodDefinition* BLOCK_RIGHT!;

interfaceName: IN? NAME (ARRAY_LEFT ARRAY_RIGHT)* -> ^(INTERFACE_NAME IN? NAME ARRAY_LEFT*);

implementList: IMPLEMENTS interfaceType (COMMA interfaceType)* -> ^(LIST interfaceType*) | -> ^(LIST);

viewDecl: VIEW^ NAME COLON! interfaceName implementList BLOCK_LEFT! methodImplementation* BLOCK_RIGHT!;

methodDefinition: methodHeader SEMICOLON!;

methodImplementation: methodHeader functionBody -> ^(METHOD methodHeader functionBody);

methodHeader: NAME BRACKET_LEFT methodParameters BRACKET_RIGHT COLON methodReturnType -> ^(METHOD_HDR NAME methodParameters methodReturnType);

exportParameters:  (BRACKET_LEFT (exportParameter (COMMA exportParameter)*)? BRACKET_RIGHT)? -> ^(LIST exportParameter*);

exportParameter: NAME IS^ (CONJUGATED | TRANSPOSED | REVERSED | ROW | COLUMN | MAIN_DIAGONAL | ANTI_DIAGONAL | typeVersion)+;

typeVersion: NAME (BRACKET_LEFT typeVersion BRACKET_RIGHT)? -> ^(TYPE_VERSION NAME typeVersion*);

valueType: DECLC DOUBLE -> COMPLEX_DOUBLE | DECLC FLOAT -> COMPLEX_FLOAT | FLOAT | DOUBLE;

functionParameters: (functionParameter (COMMA functionParameter)*)? -> ^(LIST functionParameter*);

methodParameters: (scalarParameter (COMMA scalarParameter)*)? -> ^(LIST scalarParameter*);

functionParameter: scalarParameter | interfaceParameter | arrayParameter;

scalarParameter: NAME COLON scalarType -> ^(SCALAR_PARAM NAME scalarType);

interfaceParameter: (IN | OUT) NAME COLON NAME LESS scalarType MORE arraySize* ->  ^(INTERFACE_PARAM IN? OUT? NAME* scalarType arraySize*);

arrayParameter: (IN | OUT) NAME COLON scalarType arraySize+ -> ^(ARRAY_PARAM IN? OUT? NAME scalarType arraySize*);

arraySize: ARRAY_LEFT ARRAY_RIGHT -> NONE | ARRAY_LEFT! (NAME | INT_NUMBER) ARRAY_RIGHT!;

functionBody: BLOCK_LEFT operation* BLOCK_RIGHT -> ^(LIST operation*);

storageMember: scalarMember | arrayMember;

scalarMember: NAME COLON scalarType SEMICOLON -> ^(MEMBER NAME scalarType);

arrayMember: NAME COLON scalarType (ARRAY_LEFT expression ARRAY_RIGHT)+ SEMICOLON -> ^(MEMBER NAME scalarType expression+);

methodReturnType: scalarType | referenceType | rangeType | interfaceType;

interfaceType: IN? NAME LESS scalarType MORE (ARRAY_LEFT ARRAY_RIGHT)* -> ^(INTERFACE_NAME IN? NAME scalarType ARRAY_LEFT*);

rangeType: RANGE^ LESS! (scalarType | referenceType | interfaceType) (COMMA! (scalarType | referenceType | interfaceType))* MORE!;

referenceType: REF^ scalarType;

arrayType: scalarType (ARRAY_LEFT expression ARRAY_RIGHT)+ -> ^(ARRAY_TYPE scalarType expression+);

scalarType: DECLI | DECLV | DECLC | DECLR;

operation: simpleOperation SEMICOLON! | forOperation | ifOperation | whileOperation;

simpleOperation: assignment | returnOperation | yieldOperation | varDeclOperation | callExpression;

ifOperation: IF^ BRACKET_LEFT! booleanExpression BRACKET_RIGHT! functionBody (ELSE! functionBody)?;

whileOperation: WHILE^ BRACKET_LEFT! booleanExpression BRACKET_RIGHT! functionBody;

forOperation: (FOR | FORALL)^ BRACKET_LEFT! iterations BRACKET_RIGHT! functionBody;

returnOperation: RETURN^ expression?;

yieldOperation: YIELD^ expressions suffix?;

suffix: (FOR | FORALL)^ iterations;

varDeclOperation: VAR_DECL^ NAME COLON! (arrayType | scalarType | TYPEOF! BRACKET_LEFT! expression BRACKET_RIGHT!);

assignment: VAR_DECL NAME MOV expression -> ^(VAR_DECL_ASSIGN NAME expression) | expression movOp^ expression suffix?;

movOp: MOV | PLUS_MOV | MINUS_MOV | MULT_MOV | DIV_MOV;

expressions: (expression (COMMA expression)*)? -> ^(LIST expression*);

iterations: iteration (COMMA iteration)* -> ^(LIST iteration*);

iteration: iterationVars IN expression -> ^(ITERATION iterationVars expression);

iterationVars: iterationVar (COMMA iterationVar)* -> ^(LIST iterationVar*);

iterationVar: NAME | NONAME;

booleanExpression: orExpression;

orExpression: andExpression LOR^ orExpression | andExpression;

andExpression: booleanTerm LAND^ andExpression | booleanTerm;

booleanTerm: LNOT^ booleanTerm | BRACKET_LEFT! booleanExpression BRACKET_RIGHT! | compare;

compare: expression compareOperator^ expression;

compareOperator: EQ | NEQ | MORE | LESS | LESSEQ | MOREEQ;

expression: slice;

slice: plusExpression COLON expression by? -> ^(SLICE plusExpression expression by*) | plusExpression;

by: BY! (INT_NUMBER | MINUS^ INT_NUMBER);

noSliceExpression: plusExpression;

plusExpression: multExpression ((PLUS | MINUS)^ multExpression)*;

multExpression: methodCallExpression ((MULT | DIV)^ methodCallExpression)*;

methodCallExpression: offset (DOT^ NAME (BRACKET_LEFT! expressions BRACKET_RIGHT!)?)*;

offset: term (ARRAY_LEFT offsetLen ARRAY_RIGHT)+ -> ^(OFFSET term offsetLen+) | term;

offsetLen: MULT | noSliceExpression COLON^ noSliceExpression | noSliceExpression;

term: BRACKET_LEFT! expression BRACKET_RIGHT! | MINUS^ term | sum | len | callExpression | constant | NAME;

callExpression: (builtIn | NAME) BRACKET_LEFT expressions BRACKET_RIGHT -> ^(CALL NAME? builtIn? expressions);

builtIn: MAIN_DIAGONAL | ANTI_DIAGONAL | TRANSPOSED | CONJUGATED | IM | RE | REVERSED | APPLY_CONJUGATED;

sum: SUM^ BRACKET_LEFT! expression FORALL! iterations BRACKET_RIGHT!;

len: LEN^ BRACKET_LEFT! expression (COMMA! INT_NUMBER)? BRACKET_RIGHT!;

constant: numConstant | arrayConstant;

numConstant: INT_NUMBER | DOUBLE_NUMBER;

arrayConstant: ARRAY_LEFT (numConstant+ | arrayConstant+) ARRAY_RIGHT -> ^(ARRAY_CST numConstant* arrayConstant*);
