/***********************************************************************************************************************************************
 *	Filename: scanner.c
 *	Compiler: MS Visual Studio 2015 / 2017
 *	Authors: John Dobie, 040659609 & Ahnaf Faiz, 040802394
 *	Course: CST8152 - Compilers, Lab Sections [12, 13]
 *	Assignment: 2
 *	Date: Thursday, November 8th 2018
 *	Professor: Sv. Ranev
 *	Purpose: SCANNER.C:  Functions implementing a Lexical Analyzer (Scanner)
 *						 as required for CST8152, Assignment #2
 *	
 *  Provided by: Svillen Ranev
 *  Version: 1.19
 *	Function list:
 *		char_class(), get_next_state(), iskeyword(), scanner_init(), malar_next_token(), aa_func02(), 
 *		aa_func03(), aa_func05(), aa_func08(), aa_func10(), aa_func11(), aa_func12()
 *	Warnings:	scanner_init() must be called before using the scanner.
***********************************************************************************************************************************************/

 /* The #define _CRT_SECURE_NO_WARNINGS should be used in MS Visual Studio projects
  * to suppress the warnings about using "unsafe" functions like fopen()
  * and standard sting library functions defined in string.h.
  * The define does not have any effect in Borland compiler projects.
  */
#define _CRT_SECURE_NO_WARNINGS

#include <stdio.h>   /* standard input / output */
#include <ctype.h>   /* conversion functions */
#include <stdlib.h>  /* standard library functions and constants */
#include <string.h>  /* string functions */
#include <limits.h>  /* integer types constants */
#include <float.h>   /* floating-point types constants */

  /*#define NDEBUG        to suppress assert() call */
#include <assert.h>  /* assert() prototype */

/* project header files */
#include "buffer.h"
#include "token.h"
#include "table.h"

#define DEBUG  /* for conditional processing */
#undef  DEBUG

/* Global objects - variables */
/* This buffer is used as a repository for string literals.
   It is defined in platy_st.c */
extern Buffer * str_LTBL; /*String literal table */
int line; /* current line number of the source code */
extern int scerrnum;     /* defined in platy_st.c - run-time error number */

/* Local(file) global objects - variables */
static Buffer *lex_buf;/*pointer to temporary lexeme buffer*/
static pBuffer sc_buf;

/* No other global variable declarations/definitions are allowed */

/* scanner.c static(local) function  prototypes */
static int char_class(char c); /* character class function */
static int get_next_state(int, char, int *); /* state machine function */
static int iskeyword(char * kw_lexeme); /*keywords lookup functuion */

/***********************************************************************************************************************************************
Purpose: To Initialize the setup for our scanner.
Author(s): Sv. Ranev
History/Versions: 1.0
Called functions: b_isempty(), b_rewind(), b_clear()
Parameters:	Pointer to Buffer
Return Value: int EXIT_SUCCESS or EXIT_FAILURE if the buffer isn't already empty.
Algorithm:
***********************************************************************************************************************************************/
int scanner_init(Buffer * psc_buf)
{
	if (b_isempty(psc_buf)) return EXIT_FAILURE;/*1*/
	/* in case the buffer has been read previously  */
	b_rewind(psc_buf);
	b_clear(str_LTBL);
	line = ONE;
	sc_buf = psc_buf;
	return EXIT_SUCCESS;/*0*/
/*   scerrnum = 0;  *//*no need - global ANSI C */
}

/***********************************************************************************************************************************************
Purpose: return an appropriate token for the next character in the buffer.
Author(s): John Dobie, Ahnaf Faiz
History/Versions: 43.0
Called functions: b_getc(), b_retract(), b_mark(), b_getcoffset(), b_reset(),
				  b_allocate(), isalpha(), isdigit(), get_next_state(), strlen()
				  b_addc(), b_compact(), aa_func02(), aa_func03(), aa_func05(), aa_func08(),
				  aa_func10(), aa_func11(), aa_func12(), b_free()
Parameters:	N/A
Return Value: Token with appropriate code and attributes set, see TOKEN.H
Algorithm:	Get the next character from the buffer
			Check if it is a special character, and if so then process it. (returning the completed token)
			If no match is found, send the character to the finite automaton to create a lexeme from the following string of characters.
			Check if the generated lexeme can match one of our states and send it to an accepting function to get the correct token.
			Depending on the state, retract our buffer so that a character can be reread and reprocessed seperately.
			If we cannot create a token, assume that the character is illegal for our compiled language and declare it as an error token.
			return the generated token.
***********************************************************************************************************************************************/
Token malar_next_token(void)
{
	Token t = { ZERO }; /* token to return after pattern recognition. Set all structure members to 0 */
	unsigned char c;	/* input symbol */
	int state = ZERO;	/* initial state of the FSM */
	short lexstart;		/* start offset of a lexeme in the input char buffer (array) */
	short lexend;		/* end   offset of a lexeme in the input char buffer (array)*/
	int accept = NOAS;	/* type of state - initially not accepting */
  
	int i = ZERO;		/* iterator for for loops */
	while (TRUE)		/* endless loop broken by token returns it will generate a warning */
	{
		/*    GET THE NEXT SYMBOL FROM THE INPUT BUFFER */ 
		c = b_getc(sc_buf);

		/*  Special case tokens processed separately one by one
		 *  in the token-driven part of the scanner
		 *  '=' , ' ' , '(' , ')' , '{' , '}' , == , <> , '>' , '<' , ';',
		 *  white space
		 *  !!comment , ',' , '"' , ';' , '-' , '+' , '*' , '/', # ,
		 *  .AND., .OR. , SEOF, 'illigal symbol',
		 */
		/* Check for special token characters and process them to store in the appropriate tokens */
		switch (c)
		{
		case '=':	/* [=], [==] */
		{
			c = b_getc(sc_buf);
			switch (c)
			{
			case '=':	/* double == */
				t.code = REL_OP_T; //relational operators: https://msdn.microsoft.com/en-us/library/hh877828.aspx
				t.attribute.rel_op = EQ;
				return t;
			default:	/* only single =, return our next character back to the buffer */
				b_retract(sc_buf);
				t.code = ASS_OP_T;
				return t;
			}
		}
		case ' ':		/* whitespace      */
		case '\t':		/* tab             */
		case '\f':		/* form feed       */
		case '\v':		/* vertical tab    */
		case '\r':		/* carriage return */
			continue;
		case '\n':	/* line terminator */
			line++;		//TODO: FIX LT, SV
			continue;
		case '(':		/* right parenthesis */
			t.code = LPR_T;
			return t;
		case ')':		/* right parenthesis */
			t.code = RPR_T;
			return t;
		case '{':		/* left squiggly brace */
			t.code = LBR_T;
			return t;
		case '}':		/* right squiggly brace */
			t.code = RBR_T;
			return t;
		case '<':		/* < lessthan, <> notequal */
		{
			c = b_getc(sc_buf);
			switch (c)	/* check the next character to see if it is a different token */
			{
			case '>':	/* <> not equal */
				t.code = REL_OP_T;
				t.attribute.rel_op = NE;
				return t;
			default:	/* only < less than */
				b_retract(sc_buf);	/*return the next character back to the buffer*/
				t.code = REL_OP_T;
				t.attribute.rel_op = LT;
				return t;
			}
		}
		case '>':		/* > greater than */
		{
			t.code = REL_OP_T;
			t.attribute.rel_op = GT;
			return t;
		}
		case ';':		/* semi-colon */
		{
			t.code = EOS_T;
			return t;
		}
		case '!':		/* exclamation, comment processing */
			c = b_getc(sc_buf);
			switch (c)	/* check the next character to see if it is a comment or an error token */
			{
			case '!':	/* comment */
				while (c = b_getc(sc_buf) != '\n')
				{ 
					if (c == SEOF_2 || c == SEOF || c == '\0')
						{ t.code = ERR_T; t.attribute.err_lex[ZERO] = '!'; t.attribute.err_lex[ONE] = c; return t;} }
				line++;
				continue;
			default:	/* error */
				b_retract(sc_buf);
				t.code = ERR_T;
				t.attribute.err_lex[ZERO] = '!';
				if (c == SEOF_2 || c == SEOF || c == '\0')
				{
					t.attribute.err_lex[ONE] = '\0';
					t.code = ERR_T;
					return t;
				}
				c != '\n' ? t.attribute.err_lex[ONE] = c : c;
				while (c = b_getc(sc_buf) != '\n') {}
				line++;
				return t;
			}
		case ',':		/* comma */
		{
			t.code = COM_T;
			return t;
		}
		case '-':		/* minus */
		{
			t.code = ART_OP_T;
			t.attribute.arr_op = MINUS;
			return t;
		}
		case '+':		/* plus */
		{
			t.code = ART_OP_T;
			t.attribute.arr_op = PLUS;
			return t;
		}
		case '*':		/* multiply */
		{
			t.code = ART_OP_T;
			t.attribute.arr_op = MULT;
			return t;
		}
		case '/':		/* divide */
		{
			t.code = ART_OP_T;
			t.attribute.arr_op = DIV;
			return t;
		}
		case '#':		/* string concatenation */
		{
			t.code = SCC_OP_T;
			return t;
		}
		case '.':		/* .AND. , .OR. */
		{
			b_mark(sc_buf, b_getcoffset(sc_buf));	/* set our mark to the start incase we find an error and need to retract */
			int result = ZERO;
			(c = b_getc(sc_buf) != 'A') ? b_retract(sc_buf) :
				(c = b_getc(sc_buf) != 'N') ? b_retract(sc_buf) :
				(c = b_getc(sc_buf) != 'D') ? b_retract(sc_buf) :
				(c = b_getc(sc_buf) != '.') ? b_retract(sc_buf) : (result = ONE);	/* we have the .AND. lexeme */

			(c = b_getc(sc_buf) != 'O') ? b_retract(sc_buf) :
				(c = b_getc(sc_buf) != 'R') ? b_retract(sc_buf) :
				(c = b_getc(sc_buf) != '.') ? b_retract(sc_buf) : (result = TWO);	/* we have the .OR. lexeme */
			if (result == ZERO)
			{			/* error has occurred, store the error throwing char into the token's array */
				t.code = ERR_T;
				b_reset(sc_buf);
				t.attribute.err_lex[ZERO] = '.';
				t.attribute.err_lex[ONE] = '\0';
				return t;
			}
			if (result == ONE)
			{
				t.code = LOG_OP_T;
				t.attribute.log_op = AND;
				return t;
			}
			if (result == TWO)
			{
				t.code = LOG_OP_T;
				t.attribute.log_op = OR;
				return t;
			}
		}
		case SEOF:
		case SEOF_2:						/* END OF FILE token */
			t.attribute.seof = SEOF2;
			t.code = SEOF_T;
			return t;
		case '\0':
			t.attribute.seof = SEOF1;
			t.code = SEOF_T;
			return t;
		default:		/* symbol needs to be checked in the finite automaton to see if it is a proper token or an illegal character. */
			if (isalpha(c) || isdigit(c) || c == '"')	/* not illegal character, we can process it in the finite automaton */
			{
				lexstart = b_mark(sc_buf, b_getcoffset(sc_buf) - ONE);	/* retract from step one and set the mark */
				state = get_next_state(state, c, &accept);				/* get the initial state */

				while (accept == NOAS)
				{								/* while not accepting, get the next state */
					c = b_getc(sc_buf);
					state = get_next_state(state, c, &accept);
				}
				if (accept == ASWR)
				{								/* accepting state found, retract the last character for procesing in the next loop */
					b_retract(sc_buf);
				}
				lexend = b_getcoffset(sc_buf);

				if (!(lex_buf = b_allocate(lexend - lexstart, ZERO, 'f')))
				{								/* create a temporary buffer to store our lexeme */
					t.code = ERR_T;
					scerrnum = ONE;
					i = ZERO;
					for (; i < (int)strlen("RUN TIME ERROR: "); ++i)
						t.attribute.err_lex[i] = "RUN TIME ERROR: "[i];
					return t;
				}
				b_reset(sc_buf);
				i = ZERO;						/* iterate through the buffer and copy the lexeme into the temporary buffer */
				for (; i < lexend - lexstart; ++i)
					b_addc(lex_buf, b_getc(sc_buf));
				b_compact(lex_buf, '\0');
				t = aa_table[state](b_location(lex_buf, ZERO));
				b_free(lex_buf);
				return t;		/* return the generated token */
			}
			break;
		}
		///* if nothing else hits, it is an illegal character */
		t.code = ERR_T;
		t.attribute.err_lex[ZERO] = c;
		t.attribute.err_lex[ONE] = '\0';
		return t;
	} /* end while(TRUE) */
}

/***********************************************************************************************************************************************
Purpose: To select the next state in relation to our state table and where our current symbol resides in one of the columns.
Author(s): Sv. Ranev
History/Versions: 1.0
Called functions: assert()
Parameters:	current finite automaton state, current symbol we are about to process, accepting state value of our next state
Return Value: int next state for our state machine to go to
Algorithm:	lookup the corresponding column for our symbol
			lookup the next state based on our current state and the derived column
			set the accepting state in relation to our next state
			return the next state
***********************************************************************************************************************************************/
#pragma region CHECK WHAT OUR NEXT STATE SHOULD BE IN THE STATE MACHINE DEPENDING ON CURRENT STATE AND COLUMN OF OUR CHARACTER
#pragma endregion
int get_next_state(int state, char c, int *accept)
{
	int col;
	int next;
	col = char_class(c);
	next = st_table[state][col];
#ifdef DEBUG
	printf("Input symbol: %c Row: %d Column: %d Next: %d \n", c, state, col, next);
#endif
	assert(next != IS);
#ifdef DEBUG
	if (next == IS)
	{
		printf("Scanner Error: Illegal state:\n");
		printf("Input symbol: %c Row: %d Column: %d\n", c, state, col);
		exit(1);
	}
#endif
	*accept = as_table[next];
	return next;
}

/***********************************************************************************************************************************************
Purpose: To return the column of our state table depending where the character fits
Author(s): John Dobie
History/Versions: 2.0
Called functions: 
Parameters:	character symbol
Return Value: int appropriate column for the symbol
Algorithm: N/A
***********************************************************************************************************************************************/
#pragma region CHECK WHAT COLUMN OUR CURRENT CHARACTER BELONGS TO
#pragma endregion
int char_class(char c)
{
	int val;	/* determine which column our character belongs to in our state machine */
	if ((c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z'))	{ val = LETTER_COLUMN; }	  
	else if (c == '0')										{ val = ZERO_COLUMN; }		  
	else if (c >= '1' && c <= '9')							{ val = NZD_COLUMN; }		  
	else if (c == '.')										{ val = DOT_COLUMN; }		  
	else if (c == '$')										{ val = DOLLAR_SIGN_COLUMN; } 
	else if (c == '"')										{ val = DBL_QUOTES_COLUMN; }  
	else if (c == SEOF_2 || c == SEOF || c == '\0')			{ val = SEOF_COLUMN; }
	else													{ val = OTHER_COLUMN; }		  
	return val;
}

/***********************************************************************************************************************************************
Purpose: To determine if the lexeme is a valid (keyword or an arithmetic variable identifier) and return the appropriate token.
Author(s): John Dobie
History/Versions: 1.0
Called functions: iskeyword(), strlen()
Parameters:	char[] lexeme to process
Return Value: the generated token for the lexeme
Algorithm:	Check if we have a keyword, if so then return the keyword token
			If not then we have a valid AVID, store the characters into the VID lexeme attribute of the token
			return the generated token
***********************************************************************************************************************************************/
#pragma region ACCEPTING FUNCTION FOR AVID - ARITHMETIC VARIABLE IDENTIFIER
#pragma endregion
Token aa_func02(char lexeme[])
{
	Token t = { ZERO };
	int i = ZERO;
	int l = ZERO;
	
	for (; i < KWT_SIZE; ++i)
	{	/* check if our lexeme is a keyword in the keyword table, return the token if so. */
		if ((t.attribute.kwt_idx = iskeyword(lexeme)) != NEGATIVE_ONE)
		{
			t.code = KW_T;
			return t;
		}
	}

	t.code = AVID_T;
	l = (int)strlen(lexeme);
	i = ZERO;
	if (l > VID_LEN)	/* store the AVID lexeme into the vid-lex attribute up until a maximum length */
	{
		for (; i < VID_LEN; ++i)
			t.attribute.vid_lex[i] = lexeme[i];
		t.attribute.vid_lex[VID_LEN] = '\0';
	}
	else
	{
		for (; i < l; ++i)
			t.attribute.vid_lex[i] = lexeme[i];
		t.attribute.vid_lex[l] = '\0';
	}
	return t;
}

/***********************************************************************************************************************************************
Purpose: To generate the token for a valid string variable identifier.
Author(s): John Dobie
History/Versions: 1.0
Called functions: strlen()
Parameters:	char[] lexeme to process
Return Value: the generated token for the lexeme
Algorithm:	we have a valid SVID, store the characters into the VID lexeme attribute of the token
			return the generated token
***********************************************************************************************************************************************/
#pragma region ACCEPTING FUNCTION FOR SVID - STRING VARIABLE IDENTIFIER
#pragma endregion
Token aa_func03(char lexeme[])
{
	Token t = { ZERO };
	int i = ZERO;
	int l = ZERO;

	t.code = SVID_T;
	l = (int)strlen(lexeme);
	if (l > VID_LEN)	/* store the SVID lexeme into the vid-lex attribute up until a maximum length */
	{					/* append the string VID discerning character '$' to the end since it is too long*/
		for (; i < VID_LEN - ONE; ++i)
			t.attribute.vid_lex[i] = lexeme[i];
		t.attribute.vid_lex[VID_LEN - 1] = '$';
		t.attribute.vid_lex[VID_LEN] = '\0';
	}
	else
	{
		for (; i < l; ++i)
			t.attribute.vid_lex[i] = lexeme[i];
		t.attribute.vid_lex[l] = '\0';
	}
	return t;
}

/***********************************************************************************************************************************************
Purpose: To determine if the lexeme is a valid (Integer Literal / Decimal Constant) and return the appropriate token.
Author(s): Ahnaf Faiz
History/Versions: 2.0
Called functions: atol(), aa_func11()
Parameters:	char[] lexeme to process
Return Value: the generated token for the lexeme
Algorithm:	Check if we have a valid IL/DIL, store the numbers into the int_value lexeme attribute of the token
			return the generated token
***********************************************************************************************************************************************/
#pragma region ACCEPTING FUNCTION FOR THE integer literal(IL) - decimal constant (DIL)
#pragma endregion
Token aa_func05(char lexeme[])
{
	Token t = { ZERO };
	long number = atol(lexeme);	/* parse the lexeme into a long integer in case of big numbers overflowing*/
	if ((short)number > SHRT_MAX || (short)number < SHRT_MIN)
	{	/* the number is too big, go to error state*/
		t = aa_table[ES](lexeme);
		return t;
	}
	t.code = INL_T;	/* the number is an integer literal, store it into the short int variable */
	t.attribute.int_value = (short)number;	
	return t;
}

/***********************************************************************************************************************************************
Purpose: To determine if the lexeme is a valid Floating Point Literal and return the appropriate token.
Author(s): Ahnaf Faiz
History/Versions: 2.0
Called functions: atof(), aa_func11()
Parameters:	char[] lexeme to process
Return Value: the generated token for the lexeme
Algorithm:	Check if we have a valid FPL, store the numbers into the flt_value lexeme attribute of the token
			return the generated token
***********************************************************************************************************************************************/
#pragma region ACCEPTING FUNCTION FOR THE floating-point literal (FPL)
#pragma endregion
Token aa_func08(char lexeme[])
{
	Token t = { ZERO };
	double number = atof(lexeme);	/* parse the lexeme into a double in case of big numbers overflowing */
	if (number > FLT_MAX || (number > ZERO && number < FLT_MIN))
	{	/* if the number is too big, go to error state */
		t = aa_table[ES](lexeme);
		return t;
	}
	t.code = FPL_T;	/* store the number into a floating point value */
	t.attribute.flt_value = (float)number;
	return t;
}

/***********************************************************************************************************************************************
Purpose: To generate a valid token for the processed String Literal from the char[] lexeme.
Author(s): John Dobie
History/Versions: 2.0
Called functions: strlen(), b_addc(), b_limit()
Parameters:	char[] lexeme to process
Return Value: the generated token for the lexeme
Algorithm:	Store the char[] lexeme into the string literal table, and set the string offset attribute of the token
			return the generated string literal token
***********************************************************************************************************************************************/
#pragma region ACCEPTING FUNCTION FOR THE string literal(SL)
#pragma endregion
Token aa_func10(char lexeme[]) 
{
	Token t = { ZERO };
	int i = ONE;	
	t.attribute.str_offset = b_limit(str_LTBL);	/* set the string offset to the next character location in the string literal table */
	for (; i < (int)strlen(lexeme) - ONE; i++)
	{
		if (lexeme[i] == '\n') line++;		/* increment newline if needed */
		b_addc(str_LTBL, lexeme[i]);			/* copy the lexeme into the string literal table buffer */	//TODO: FIX MAYBE
	}
	b_addc(str_LTBL, '\0');				/* append the null terminator character to the end of the string */	//TODO: FIX MAYBE
	t.code = STR_T;
	return t;
}

/***********************************************************************************************************************************************
Purpose: To handle the generation of an error token
Author(s): Ahnaf Faiz
History/Versions: 1.0
Called functions: strlen(), strcpy(), strncat()
Parameters:	char[] lexeme to process
Return Value: the generated token for the lexeme
Algorithm:	Store the erratic lexeme into the err_lex attribute of the token
			If it is too long, then truncate and have it contain "..." for indication
			return the generated token
***********************************************************************************************************************************************/
#pragma region ACCEPTING FUNCTION FOR THE ERROR TOKEN 
#pragma endregion
Token aa_func11(char lexeme[])
{
	Token t = { ZERO };
	int i = ZERO;
	int lexeme_size = (int)strlen(lexeme);

	for (; i < lexeme_size; ++i)			/* increment newline counter if needed */
		if (lexeme[i] == '\n') line++;

	if (lexeme_size <= ERR_LEN - THREE)		/* store the erratic lexeme into the err_lex token attribute */
	{
		strcpy(t.attribute.err_lex, lexeme);
		t.attribute.err_lex[ERR_LEN] = '\0';
	}
	else									/* the erratic lexeme is too long, so append the [...] to the end to notify */
	{
		strncpy(t.attribute.err_lex, lexeme, ERR_LEN - THREE);
		strncat(t.attribute.err_lex, "...", (THREE + ONE));
	}
	t.code = ERR_T;
	return t;
}

/***********************************************************************************************************************************************
Purpose: To handle erratic lexemes generated by a non retracting state and return the appropriate token.
Author(s): Ahnaf Faiz
History/Versions: 1.0
Called functions: aa_func11()
Parameters:	char[] lexeme to process
Return Value: the generated token for the lexeme
Algorithm:	Store the erratic lexeme into the err_lex attribute of the token
			If it is too long, then truncate and have it contain "..." for indication
			return the generated token, using accepting function 11 to generate the token
***********************************************************************************************************************************************/
#pragma region ACCEPTING FUNCTION FOR ERROR WITH RETRACT
#pragma endregion
Token aa_func12(char lexeme[])
{
	Token t = { ZERO };
	t = aa_table[ES](lexeme);	/* call error state, when we return the token we will go to retract via the as_table specifications */
	return t;
}

/***********************************************************************************************************************************************
Purpose: To return the correct index in the keyword table if the lexeme matches any of the keywords of the language.
Author(s): John Dobie
History/Versions: 1.0
Called functions: strncmp(), strlen()
Parameters:	char* kw_lexeme to check against
Return Value: the index of the matched keyword in the keyword table, or -1 on failing to match any keyword
Algorithm:	Check against each entry of the keyword table and return the index if found of the keyword in the array.
***********************************************************************************************************************************************/
#pragma region CHECK IF WE HAVE A KEYWORD
#pragma endregion
int iskeyword(char * kw_lexeme)
{
	/* keywords, if there is a match, return the keyword's index.
	PLATYPUS | IF | THEN | ELSE | WHILE | REPEAT | READ | WRITE | TRUE | FALSE */
	int i = ZERO;
	for (; i < KWT_SIZE; ++i)
	{
		if (strncmp(kw_lexeme, kw_table[i], strlen(kw_table[i])) == ZERO) return i;
	}
	return NEGATIVE_ONE;	/* does not match a keyword */
}