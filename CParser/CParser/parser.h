#pragma once
/*************************************************************************************
File name :		parser.h
Compiler :		MS Visual Studio 15
Author :		Sharusshan Sinnadurai
Course :		CST 8152 – Compilers, Lab Section : 10
Assignment :	3 - Parser
Date :			December 07, 2018
Professor :		Sv.Ranev
Functions :		None
*************************************************************************************/

#include <process.h>
#include "buffer.h"
#include "token.h"



#define NO_ATTR 0
#define ELSE 0
#define FALSE 1
#define IF 2
#define PLATYPUS 3
#define READ 4
#define REPEAT 5
#define THEN 6
#define TRUE 7
#define WHILE 8
#define WRITE 9

/* Global variables */
static Token lookahead;
int synerrno;
extern Token malar_next_token(void);
extern char* kw_table[];
extern int as_table[];
extern Buffer * str_LTBL;
extern int line;

void syn_eh(int sync_token_code);
void syn_printe();
void parser(void);
void match(int, int);
void gen_incode(char*);

void program(void);
void opt_statements(void);
void statements(void);
void statementsP(void);
void statement(void);
void assignmentStatement(void);
void assignmentExpression(void);
void selectionStatement(void);
void iterationStatement(void);
void preCondition(void);
void inputStatement(void);
void variableList(void);
void variableIdentifier(void);
void variableListP(void);
void outputStatement(void);
void outputList(void);
void arithmeticExpression(void);
void unaryArithmeticExpression(void);
void additiveArithmeticExpression(void);
void additiveArithmeticExpressionP(void);
void multiplicativeArithmeticExpression(void);
void multiplicativeArithmeticExpressionP(void);
void primaryArithmeticExpression(void);
void stringExpression(void);
void stringExpressionP(void);
void stringLiteral(void);
void primaryStringExpression(void);
void conditionalExpression(void);
void logicalORexpression(void);
void logicalORexpressionP(void);
void logicalANDexpression(void);
void logicalANDexpressionP(void);
void relationalExpression(void);
void primarySRelationalExpression(void);
void primaryARelationalExpression(void);
void primaryARelationalExpressionP(void);
void primarySRelationalExpressionP(void);