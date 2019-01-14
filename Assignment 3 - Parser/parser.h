/***********************************************************************************************************************************************
*	Filename: parser.h
*	Compiler: MS Visual Studio 2015 / 2017
*	Author: John Dobie, 040659609 & Ahnaf Faiz, 040802394
*	Course: CST8152 - Compilers, Lab Sections [12, 13]
*	Assignment: 3
*	Date: Thursday, December 6th 2018
*	Professor: Sv. Ranev
*	Purpose: PARSER.h: To write a Recursive Descent Predictive Parser (RDPP) for the PLATYPUS language by integrating the Parser with 
					   the existing lexical analyzer and symbol table in order to complete the front-end of the PLATYPUS compiler. 
*
*   Provided by: Svillen Ranev
*   Version: 1.0
*	Function list: match(),syn_printe(),syn_eh(),gen_incode(),program(),opt_statements(),statements(),statementsPrime(),
*				   statement(),assignmentStatement(),assignmentExpression(),selectionStatement(),iterationStatement(), inputStatement()
				   outputStatement(),arithmeticExpression(),unaryArithmeticExpression(),additiveArithmeticExpression(),
				   primaryArithmeticExpression(),multiplicativeArithmeticExpression(),additiveArithmeticExpressionPrime(),
				   multiplicativeArithmeticExpressionPrime(),stringExpression(),stringExpressionPrime(),primaryStringExpression(),
				   preconditon(),conditionalExpression(),logicalORExpression(),logicalORExpressionPrime(),logicalANDExpression(),
				   logicalANDExpressionPrime(),relationalExpression(),primaryArithmeticRelationalExpression(),
				   primaryArithmeticRelationalExpressionPrime(),primaryStringRelationalExpression(),primaryStringRelationalExpressionPrime(),
				   variableList(),variableListPrime(),opt_variableList();
***********************************************************************************************************************************************/
#pragma once

//////Defines
#define NO_ATTR -1
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
/*Global Variables*/
static Token lookahead;		/*The next Token to parse*/
int synerrno;				/*the error number that we will increment*/
extern int line;			/*the current line number we are parsing*/
extern Buffer* str_LTBL;	/*the string literal table*/
extern char * kw_table[];	/*the keyword table*/
/*Functions*/
extern Token malar_next_token(void);
void match(int pr_token_code, int pr_token_attribute);
void syn_printe();
void syn_eh(int sync_token_code);
void gen_incode(char* argument);
void program(void);
void opt_statements(void);
void statements(void);
void statementsPrime(void);
void statement(void);
void assignmentStatement(void);
void assignmentExpression(void);
void selectionStatement(void);
void iterationStatement(void);
void inputStatement(void);
void outputStatement(void);
void arithmeticExpression(void);
void unaryArithmeticExpression(void);
void additiveArithmeticExpression(void);
void primaryArithmeticExpression(void);
void multiplicativeArithmeticExpression(void);
void additiveArithmeticExpressionPrime(void);
void multiplicativeArithmeticExpressionPrime(void);
void stringExpression(void);
void stringExpressionPrime(void);
void primaryStringExpression(void);
void precondition(void);
void conditionalExpression(void);
void logicalORExpression(void);
void logicalORExpressionPrime(void);
void logicalANDExpression(void);
void logicalANDExpressionPrime(void);
void relationalExpression(void);
void primaryArithmeticRelationalExpression(void);
void primaryArithmeticRelationalExpressionPrime(void);
void primaryStringRelationalExpression(void);
void primaryStringRelationalExpressionPrime(void);
void variableList(void);
void variableListPrime(void);
void opt_variableList(void);
void variableIdentifier(void);
