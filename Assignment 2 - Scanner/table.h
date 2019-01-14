/***********************************************************************************************************************************************
 *	Filename: table.h
 *	Compiler: MS Visual Studio 2015 / 2017
 *	Author: John Dobie, 040659609 & Ahnaf Faiz, 040802394
 *	Course: CST8152 - Compilers, Lab Sections [12, 13]
 *	Assignment: 2
 *	Date: Thursday, November 8th 2018
 *	Professor: Sv. Ranev
 *	Purpose: TABLE.H:	To provide the structure of dataflow for our Scanner to process input.
 *						as required for CST8152, Assignment #2
 *	
 *  Provided by: Svillen Ranev
 *  Version: 1.0
 *	Function list: 
 *		aa_func02(), aa_func03(), aa_func05(), aa_func08(), aa_func10(), aa_func11(), aa_func12();
 ***********************************************************************************************************************************************/

#ifndef  TABLE_H_
#define  TABLE_H_ 

#ifndef BUFFER_H_
#include "buffer.h"
#endif

#ifndef NULL
#include <_null.h> /* NULL pointer constant is defined there */
#endif

/* ADDITIONAL_DEFINES */
/* Source end-of-file (SEOF) sentinel symbol: '\0' or one of 255,0xFF,EOF */
#define SEOF 255
#define SEOF_2 EOF
/* Numbers */
#define ZERO 0
#define ONE 1
#define NEGATIVE_ONE -1
#define TWO 2
#define THREE 3

/* Boolean constants */
#define TRUE 1
#define FALSE 0
/* States */
#define AVID_STATE 2
#define SVID_STATE 3
#define DIL_STATE 5
#define FPL_STATE 8
#define STRL_STATE 10
#define ERRNR_STATE 11
#define ERRWR_STATE 12
/* Columns */
#define LETTER_COLUMN 0
#define ZERO_COLUMN 1
#define NZD_COLUMN 2
#define DOT_COLUMN 3
#define DOLLAR_SIGN_COLUMN 4
#define OTHER_COLUMN 5
#define DBL_QUOTES_COLUMN 6
#define SEOF_COLUMN 7

#define ES 11	/* Error state */
#define ER 12	/* Error retracting state */
#define IS -1   /* Inavalid state */

/* State transition table definition */
#define TABLE_COLUMNS 8 //Letters, 0, 1-9, ., $, other, "string literal", EOF
/*transition table - type of states defined in separate table */
int  st_table[][TABLE_COLUMNS] = {
	/*				L   0   NZD  .   $  oth  "" EOF*/
	/* State 0  */ {1,  6,   4, ES, ES, ES,  9, IS}, /* Starting State */
	/* State 1  */ {1,  1,   1,  2,  3,  2,  2,  2}, /* Letter State */
	/* State 2  */ {IS, IS, IS, IS, IS, IS, IS, IS}, /* Letter Accepting State */
	/* State 3  */ {IS, IS, IS, IS, IS, IS, IS, IS}, /* String Variable Accepting State */
	/* State 4  */ {ES,  4,  4,  7,  5,  5,  5,  5}, /* Non-Zero Digit State */
	/* State 5  */ {IS, IS, IS, IS, IS, IS, IS, IS}, /* Integer Literal Accepting State */
	/* State 6  */ {ES,  6, ES,  7, ES,  5,  5,  5}, /* Zero Starting Digit State */
	/* State 7  */ { 8,  7,  7,  8,  8,  8,  8,  8}, /* Decimal State */
	/* State 8  */ {IS, IS, IS, IS, IS, IS, IS, IS}, /* Floating Point Literal Accepting State */
	/* State 9  */ { 9,  9,  9,  9,  9,  9, 10, ER}, /* String Literal State */
	/* State 10 */ {IS, IS, IS, IS, IS, IS, IS, IS}, /* String Literal Accepting State */
	/* State 11 */ {IS, IS, IS, IS, IS, IS, IS, IS}, /* Error Accepting State */
	/* State 12 */ {IS, IS, IS, IS, IS, IS, IS, IS}, /* Retracting Error Accepting State */
};

/* Accepting state table definition */
#define ASWR     317  /* accepting state with retract */ /*ASCII code sum*/
#define ASNR     308  /* accepting state with no retract */ /*ASCII code sum*/
#define NOAS     305  /* not accepting state */ /*ASCII code sum*/

int as_table[] = {
	NOAS,	/*State 0*/
	NOAS,	/*State 1*/
	ASWR,	/*State 2*/
	ASNR,	/*State 3*/
	NOAS,	/*State 4*/
	ASWR,	/*State 5*/
	NOAS,	/*State 6*/
	NOAS,	/*State 7*/
	ASWR,	/*State 8*/
	NOAS,	/*State 9*/
	ASNR,	/*State 10*/
	ASNR,	/*State 11*/
	ASWR,	/*State 12*/
	ASNR,	/*State 13*/
};

/* Accepting action function declarations
FOR EACH OF YOUR ACCEPTING STATES YOU MUST PROVIDE
ONE FUNCTION PROTOTYPE.THEY ALL RETURN Token AND TAKE
ONE ARGUMENT : A string REPRESENTING A TOKEN LEXEME.
Token aa_funcXX(char *lexeme); Replace XX with the number of the accepting state : 02, 03 and so on.
*/

Token aa_func02(char lexeme[]); /*aswr*/
Token aa_func03(char lexeme[]); /*asnr*/
Token aa_func05(char lexeme[]); /*aswr*/
Token aa_func08(char lexeme[]); /*aswr*/
Token aa_func10(char lexeme[]); /*asnr*/
Token aa_func11(char lexeme[]); /*asnr*/
Token aa_func12(char lexeme[]); /*aswr*/

/* defining a new type: pointer to function (of one char * argument)
   returning Token*/
typedef Token(*PTR_AAF)(char *lexeme);

/* Accepting function (action) callback table (array) definition */
/* If you do not want to use the typedef, the equvalent declaration is:
 * Token (*aa_table[])(char lexeme[]) = {
 */

PTR_AAF aa_table[] =
{
	/* Initialization for an array of pointers to accepting functions. */
	NULL, NULL, aa_func02, aa_func03, NULL, aa_func05, NULL, NULL, aa_func08, NULL, aa_func10, aa_func11, aa_func12
};

/*Language Keyword table*/
#define KWT_SIZE  10
char * kw_table[] =
{
"ELSE",
"FALSE",
"IF",
"PLATYPUS",
"READ",
"REPEAT",
"THEN",
"TRUE",
"WHILE",
"WRITE"
};

#endif
