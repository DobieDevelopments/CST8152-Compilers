/***********************************************************************************************************************************************
*	Filename: parser.c
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
#include "token.h"
#include "buffer.h"
#include "parser.h"
#include <stdlib.h>		//included for the use of exit() in syn_eh()

/***********************************************************************************************************************************************
Purpose:
Author(s): Sv. Ranev
History/Versions: 1.0
Called functions: malar_next_token(), program(), match(), gen_incode()
Parameters:	void
Return Value: void
Algorithm:
***********************************************************************************************************************************************/
void parser(void)
{
	lookahead = malar_next_token();
	program();
	match(SEOF_T, NO_ATTR);
	gen_incode("PLATY: Source file parsed");
}

/***********************************************************************************************************************************************
Purpose: To match two tokens: the current input token (lookahead) and the token required by the parser.
Author(s): John Dobie, 040659609
History/Versions: 1.0
Called functions: syn_eh(), malar_next_token(), syn_printe()
Parameters: Token Code, Token Attribute
Return Value: void
Algorithm:
***********************************************************************************************************************************************/
void match(int pr_token_code, int pr_token_attribute)
{														
	if (lookahead.code == pr_token_code)	/*match is successful.*/
	{
		switch (pr_token_code)				/*do we need to match attribute?*/
		{
		case KW_T:
		case LOG_OP_T:
		case ART_OP_T:
		case REL_OP_T:
			if (lookahead.attribute.get_int != pr_token_attribute)
			{
				syn_eh(pr_token_code);
				return;
			}
			break;
		default:
			if (lookahead.code == SEOF_T)
				return;
		}
		lookahead = malar_next_token();
		if (lookahead.code == ERR_T)
		{
			syn_printe();
			lookahead = malar_next_token();
			synerrno++;
			return;
		}
	}
	else
	{
		syn_eh(pr_token_code);
		return;
	}
	return;
}

/***********************************************************************************************************************************************
Purpose: To advance to the next input token by calling malar_next_token() again, increment the error counter synerrno, and returns.
Author(s): John Dobie, 040659609
History/Versions: 1.0
Called functions: syn_printe(), exit(), malar_next_token(),
Parameters:	Token code
Return Value: void
Algorithm:
***********************************************************************************************************************************************/
void syn_eh(int sync_token_code)
{
	syn_printe();
	synerrno++;
	do
	{
		lookahead = malar_next_token();

		if (lookahead.code == SEOF_T)
			if(sync_token_code != SEOF_T)
			exit(synerrno);
			else return;
	} while (lookahead.code != sync_token_code);
	lookahead = malar_next_token();
}

/***********************************************************************************************************************************************
Purpose: To call syn_printe() and implement a panic mode error recovery which advances the input token (lookahead) until it finds a
		 token code matching the one required by the parser.
Author(s): Sv. Ranev
History/Versions: 1.0
Called functions: printf(), b_location()
Parameters:	N/A
Return Value: void
Algorithm:
***********************************************************************************************************************************************/
/* error printing function for Assignment 3 (Parser), F18 */
void syn_printe()
{
	Token t = lookahead;

	printf("PLATY: Syntax error:  Line:%3d\n", line);
	printf("*****  Token code:%3d Attribute: ", t.code);
	switch (t.code)
	{
		case  ERR_T: /* 0 - Error token */
			printf("%s\n", t.attribute.err_lex);
			break;
		case  SEOF_T: /* 1 - Source end-of-file token */
			printf("SEOF_T\t\t%d\t\n", t.attribute.seof);
			break;
		case  AVID_T: /* 2 - Arithmetic Variable identifier token */
		case  SVID_T:/* 3 - String Variable identifier token */
			printf("%s\n", t.attribute.vid_lex);
			break;
		case  FPL_T: /* 4 - Floating point literal token */
			printf("%5.1f\n", t.attribute.flt_value);
			break;
		case INL_T: /* 5 - Integer literal token */
			printf("%d\n", t.attribute.get_int);
			break;
		case STR_T:/* 6 - String literal token */
			printf("%s\n", b_location(str_LTBL, t.attribute.str_offset));
			break;

		case SCC_OP_T: /* 7 - String concatenation operator token */
			printf("NA\n");
			break;

		case  ASS_OP_T:/* 8 - Assignment operator token */
			printf("NA\n");
			break;
		case  ART_OP_T:/* 9 - Arithmetic operator token */
			printf("%d\n", t.attribute.get_int);
			break;
		case  REL_OP_T: /* 10 - Relational operator token */
			printf("%d\n", t.attribute.get_int);
			break;
		case  LOG_OP_T:/* 11 - Logical operator token */
			printf("%d\n", t.attribute.get_int);
			break;

		case  LPR_T: /* 12 - Left parenthesis token */
			printf("NA\n");
			break;
		case  RPR_T: /* 13 -  Right parenthesis token */
			printf("NA\n");
			break;
		case LBR_T: /* 14 - Left brace token */
			printf("NA\n");
			break;
		case RBR_T: /* 15 - Right brace token */
			printf("NA\n");
			break;

		case KW_T: /* 16 - Keyword token */
			printf("%s\n", kw_table[t.attribute.get_int]);
			break;

		case COM_T: /* 17 - Comma token */
			printf("NA\n");
			break;
		case EOS_T: /* 18 - End of statement *(semi - colon) */
			printf("NA\n");
			break;
		default:
			printf("PLATY: Scanner error: invalid token code: %d\n", t.code);
	}/*end switch*/
}/* end syn_printe()*/

 /***********************************************************************************************************************************************
 Purpose: Error printing by accepting a string as an argument, then prints it and may be may be called any time a production is recognized.
 Author(s): John Dobie, 040659609
 History/Versions: 1.0
 Called functions: printf()
 Parameters: String Argument to be printed.
 Return Value: void
 Algorithm:
 ***********************************************************************************************************************************************/
 /* function for Assignment 3 (Parser), F18 */
void gen_incode(char * argument)
{
	printf("%s\n", argument);
}

/***********************************************************************************************************************************************
Purpose: <program> -> PLATYPUS {<opt_statements>}
		 FIRST(<program>) = { KW_T(PLATYPUS) }
Author(s): John Dobie, 040 659 609
History/Versions: 1.0
Called functions: match(), opt_statements(), gen_incode()
Parameters: String Argument to be printed.
Return Value: void
Algorithm:
***********************************************************************************************************************************************/
void program(void)
{
	match(KW_T, PLATYPUS);
	match(LBR_T, NO_ATTR);
	opt_statements();
	match(RBR_T, NO_ATTR);
	gen_incode("PLATY: Program parsed");
}

/***********************************************************************************************************************************************
Purpose: <opt_statements> -> <statements> | e
		 FIRST(<opt_statements>) = {<statements>, e}
Author(s): John Dobie, 040659609
History/Versions: 1.0
Called functions: statements(), gen_incode()
Parameters:	void
Return Value: void
Algorithm:
***********************************************************************************************************************************************/
void opt_statements(void)
{
	/* FIRST set: {AVID_T,SVID_T,KW_T(but not … see above),e} */
	switch (lookahead.code)
	{
		case AVID_T:
		case SVID_T: statements(); break;
		case KW_T:
			/* check for PLATYPUS, ELSE, THEN, REPEAT, TRUE, FALSE here
			and in statements_p()*/
			if (lookahead.attribute.get_int != PLATYPUS
				&& lookahead.attribute.get_int != ELSE
				&& lookahead.attribute.get_int != THEN
				&& lookahead.attribute.get_int != REPEAT
				&& lookahead.attribute.get_int != TRUE
				&& lookahead.attribute.get_int != FALSE)
			{ statements(); break; }
		default: /*empty string – optional statements*/;
			gen_incode("PLATY: Opt_statements parsed");
	}
}

/***********************************************************************************************************************************************
Purpose: <statements’> -> <statement>, <statements’> | e
		 FIRST(<statements>) = {AVID, SVID, KW_T(WHILE), KW_T(REPEAT), KW_T(READ), KW_T(WRITE), e}
Author(s): John Dobie, 040659609
History/Versions: 1.0
Called functions: statementPrime, statementsPrime()
Parameters:	void
Return Value: void
Algorithm:
***********************************************************************************************************************************************/
void statements(void)
{
	statement();
	statementsPrime();
}

/***********************************************************************************************************************************************
Purpose: <statements’> -> <statement>, <statements’> | e
Author(s): John Dobie, 040659609
History/Versions: 1.0
Called functions: statementsPrime(), statement()
Parameters:	void
Return Value: void
Algorithm:
***********************************************************************************************************************************************/
void statementsPrime(void)
{
	switch (lookahead.code)
	{
		case AVID_T:
		case SVID_T: statement();
			statementsPrime(); break;
		case KW_T:
			if (lookahead.attribute.get_int != PLATYPUS
				&& lookahead.attribute.get_int != ELSE
				&& lookahead.attribute.get_int != THEN
				&& lookahead.attribute.get_int != REPEAT
				&& lookahead.attribute.get_int != TRUE
				&& lookahead.attribute.get_int != FALSE)
			{
				statement();
				statementsPrime();
				break;
			}
	}
}

/***********************************************************************************************************************************************
Purpose: <statement> -> <assignment statement> | <selection statement> | <iteration statement>   | <input statement> | <output statement>
		 FIRST(<statement>) = {AVID, SVID, KW_T(WHILE), KW_T(REPEAT), KW_T(READ), KW_T(WRITE), e}
Author(s): John Dobie, 040659609
History/Versions: 1.0
Called functions: assignmentStatement(), selectionStatement(), iterationStatement(), inputStatement(), outputStatement(), syn_printe()
Parameters:	void
Return Value: void
Algorithm:
***********************************************************************************************************************************************/
void statement(void)
{
	switch (lookahead.code)
	{
		case AVID_T:
		case SVID_T:
			assignmentStatement(); break;
		case KW_T:
			switch (lookahead.attribute.get_int)
			{
				case IF: selectionStatement(); break;
				case WHILE: iterationStatement(); break;
				case READ: inputStatement(); break;
				case WRITE: outputStatement(); break;
				default: syn_printe(); break;
			} break;
		default: syn_printe(); break;
	}
}


/***********************************************************************************************************************************************
Purpose: <assignment statement> -> <assignment expression>
		 FIRST(<assignment statement>) = FIRST(<assignment expression>)
Author(s): John Dobie, 040659609
History/Versions: 1.0
Called functions: assignmentExpression(), match()
Parameters:	void
Return Value: void
Algorithm:
***********************************************************************************************************************************************/
void assignmentStatement(void)
{
	assignmentExpression();
	match(EOS_T, NO_ATTR);
	gen_incode("PLATY: Assignment statement parsed");
}

/***********************************************************************************************************************************************
Purpose: < assignment expression> -> AVID = <arithmetic expression> | SVID = <string expression>
		 FIRST(<assignment expression>) = {AVID, SVID}
Author(s): John Dobie, 040659609
History/Versions: 1.0
Called functions: match(), arithmeticExpression(), gen_incode, stringExpression(), syn_printe()
Parameters:	void
Return Value: void
Algorithm:
***********************************************************************************************************************************************/
void assignmentExpression(void)
{
	switch (lookahead.code)
	{
	case AVID_T: match(AVID_T, NO_ATTR);
			match(ASS_OP_T, EQ);
			arithmeticExpression();
			gen_incode("PLATY: Assignment expression (arithmetic) parsed");
		break;
	case SVID_T: match(SVID_T, NO_ATTR);
				match(ASS_OP_T, EQ);
				stringExpression();
				gen_incode("PLATY: Assignment expression (string) parsed");
		break;
	default: syn_printe(); break;

	}
}

/***********************************************************************************************************************************************
Purpose: <selection statement> -> IF <pre-condition> (<conditional expression>) THEN { <opt_statements> } ELSE { <opt_statements> } ;
		 FIRST(<selection expression>) = {KW_T(IF) }
FIRST(<pre condition>) = {KW_T(TRUE), KW_T(FALSE)}
Author(s): John Dobie, 040659609
History/Versions: 1.0
Called functions: match(), syn_printe()
Parameters:	void
Return Value: void
Algorithm:
***********************************************************************************************************************************************/
void selectionStatement(void)
{
	match(KW_T, IF); 
	precondition(); 
	match(LPR_T, NO_ATTR);
	conditionalExpression();
	match(RPR_T, NO_ATTR);
	match(KW_T, THEN); 
	match(LBR_T, NO_ATTR);
	opt_statements(); 
	match(RBR_T, NO_ATTR);
	match(KW_T, ELSE);
	match(LBR_T, NO_ATTR);
	opt_statements();
	match(RBR_T, NO_ATTR);
	match(EOS_T, NO_ATTR);
	gen_incode("PLATY: Selection statement parsed");
}

/***********************************************************************************************************************************************
Purpose: <pre-condition> ->  TRUE | FALSE
		 FIRST(<pre condition>) = {KW_T(TRUE), KW_T(FALSE)}
Author(s): John Dobie, 040659609
History/Versions: 1.0
Called functions: match(), syn_printe()
Parameters:	void
Return Value: void
Algorithm:
***********************************************************************************************************************************************/
void precondition(void)
{
	if (lookahead.code == KW_T)
	{
		switch (lookahead.attribute.get_int)
		{
		case TRUE: 
		case FALSE: 
			match(KW_T, lookahead.attribute.get_int); break;
		default: syn_printe(); break;
		}
	}
	else syn_printe();
}

/***********************************************************************************************************************************************
Purpose: <conditional expression> ->  <logical OR expression>
		 FIRST(<conditional expression>) = { .OR. , .AND. }
Author(s): John Dobie, 040659609
History/Versions: 1.0
Called functions: logicalORExpression()
Parameters:	void
Return Value: void
Algorithm:
***********************************************************************************************************************************************/
void conditionalExpression(void)
{
	logicalORExpression();
	gen_incode("PLATY: Conditional expression parsed");
}

/***********************************************************************************************************************************************
Purpose: <logical OR expression> -><logical OR expression> .OR. <logical AND expression> (relational) | <logical AND expression>
		 FIRST(<logical OR expression>) = { AVID_T, SVID_T, FPL_T, INL_T, STR_T }
Author(s): John Dobie, 040659609
History/Versions: 1.0
Called functions: logicalANDExpression(), logicalORExpressionPrime()
Parameters:	void
Return Value: void
Algorithm:
***********************************************************************************************************************************************/
void logicalORExpression(void)
{
	logicalANDExpression();
	logicalORExpressionPrime();
}

/***********************************************************************************************************************************************
Purpose: <logical OR expression’> -> <logical AND expression> <.OR.> <logical OR expression’> | e
		 FIRST(<logical OR expression’>) = { .OR. , e }
Author(s): John Dobie, 040659609
History/Versions: 1.0
Called functions: match(), logicalORExpression()
Parameters:	void
Return Value: void
Algorithm:
***********************************************************************************************************************************************/
void logicalORExpressionPrime(void)
{
	switch (lookahead.code)
	{
	case LOG_OP_T:
		switch (lookahead.attribute.log_op)
		{
		case OR:
			match(LOG_OP_T, OR);
			logicalANDExpression();
			logicalORExpressionPrime();
			gen_incode("PLATY: Logical OR expression parsed");
			break;
		} break;
	}
}

/***********************************************************************************************************************************************
Purpose: <logical AND expression> -> <relational expression> .AND. <logical AND expression> | <logical AND expression>
		 FIRST(<logical AND expression>) = { AVID_T, SVID_T, FPL_T, INL_T, STR_T }
Author(s): John Dobie, 040659609
History/Versions: 1.0
Called functions: relationalExpression(), logicalANDExpression()
Parameters:	void
Return Value: void
Algorithm:
***********************************************************************************************************************************************/
void logicalANDExpression(void)
{
	relationalExpression();
	logicalANDExpressionPrime();	
}

/***********************************************************************************************************************************************
Purpose: <logical AND expression’> -> <.AND.> <relational expression> <logical AND expression’>
		 FIRST(<logical AND expression’>) = { .AND. , e }
Author(s): John Dobie, 040659609
History/Versions: 1.0
Called functions: match(), logicalORExpression()
Parameters:	void
Return Value: void
Algorithm:
***********************************************************************************************************************************************/
void logicalANDExpressionPrime(void)
{
	switch (lookahead.code)
	{
	case LOG_OP_T:
		switch (lookahead.attribute.log_op)
		{
		case AND:
			match(LOG_OP_T, AND);
			relationalExpression();
			logicalANDExpressionPrime();
			gen_incode("PLATY: Logical AND expression parsed");
			break;
		} break;
	}
}

/***********************************************************************************************************************************************
Purpose: <relational expression> ->	<primary a relational expression> <primary a relational expression’>
		 | <primary s relational expression> <primary s relational expression’>
		 FIRST(<relational expression>) = { AVID_T, SVID_T, FPL_T, INL_T, STR_T }
Author(s): John Dobie, 040659609
History/Versions: 1.0
Called functions: primaryArithmeticRelationalExpression(), primaryArithmeticRelationalExpressionPrime(),
				  primaryStringRelationalExpression(), primaryStringRelationalExpressionPrime(), syn_printe()
Parameters:	void
Return Value: void
Algorithm:
***********************************************************************************************************************************************/
void relationalExpression(void)
{
	switch(lookahead.code)
	{ 
	case AVID_T:
	case FPL_T:
	case INL_T:
		primaryArithmeticRelationalExpression();
		primaryArithmeticRelationalExpressionPrime();
		break;
	case SVID_T:
	case STR_T:
		primaryStringRelationalExpression();
		primaryStringRelationalExpressionPrime();
		break;
	default: syn_printe(); break;
	}
	gen_incode("PLATY: Relational expression parsed");
}

/***********************************************************************************************************************************************
Purpose: <primary a_relational expression> ->  AVID_T | FPL_T | INL_T
		 FIRST(<primary a relational expression>) == { AVID_T, FPL_T, INL_T }
Author(s): Ahnaf Faiz, 040802394
History/Versions: 1.0
Called functions: match(), syn_printe()
Parameters:	void
Return Value: void
Algorithm:
***********************************************************************************************************************************************/
void primaryArithmeticRelationalExpression(void)
{
	switch (lookahead.code)
	{
	case AVID_T: 
	case FPL_T: 
	case INL_T: 
		match(lookahead.code, NO_ATTR); break;
	default: syn_printe(); break;
	}
	gen_incode("PLATY: Primary a_relational expression parsed");
}

/***********************************************************************************************************************************************
Purpose: <primary a relational expression’> -> == <primary a relational expression> | < <primary a relational expression> | > <primary a relational expression> | <> <primary a relational expression>
		 FIRST(<primary a relational expression’>) = { ==, >, <, <> }
Author(s): Ahnaf Faiz, 040802394
History/Versions: 1.0
Called functions: match(), primaryArithmeticRelationalExpression(), syn_printe()
Parameters:	void
Return Value: void
Algorithm:
***********************************************************************************************************************************************/
void primaryArithmeticRelationalExpressionPrime(void)
{
	switch (lookahead.code)
	{
	case REL_OP_T:
		switch (lookahead.attribute.rel_op)
		{
		case EQ:
		case GT:
		case LT:
		case NE:
			match(REL_OP_T, lookahead.attribute.rel_op);
			primaryArithmeticRelationalExpression();
		} break;
	default: syn_printe(); break;
		break;
	}
}

/***********************************************************************************************************************************************
Purpose: <primary s_relational expression> -> <primary string expression>
		 FIRST(<primary s relational expression>) == { SVID_T, STR_T }
Author(s): Ahnaf Faiz, 040802394
History/Versions: 1.0
Called functions: primaryStringExpression(), syn_printe()
Parameters:	void
Return Value: void
Algorithm:
***********************************************************************************************************************************************/
void primaryStringRelationalExpression(void)
{
	primaryStringExpression();
	gen_incode("PLATY: Primary s_relational expression parsed");
}

/***********************************************************************************************************************************************
Purpose: <primary s relational expression’ > -> == <primary s relational expression> | < <primary s relational expression> | > <primary s relational expression> | <> <primary s relational expression>
		 FIRST(<primary s relational expression’>) = { ==, >, <, <> }
Author(s): Ahnaf Faiz, 040802394
History/Versions: 1.0
Called functions: match(), primaryStringRelationalExpression(), syn_printe()
Parameters:	void
Return Value: void
Algorithm:
***********************************************************************************************************************************************/
void primaryStringRelationalExpressionPrime(void)
{
	switch (lookahead.code)
	{
	case REL_OP_T:
		switch (lookahead.attribute.rel_op)
		{
		case EQ:
		case GT:
		case LT:
		case NE:
			match(REL_OP_T, lookahead.attribute.rel_op);
			primaryStringRelationalExpression();
		} break;
	default: syn_printe(); break;
		break;
	}
}

/***********************************************************************************************************************************************
Purpose: <iteration statement> -> WHILE <pre-condition> (<conditional expression>)
								  REPEAT { <statements>};
		 FIRST(<iteration statement>) = {KW_T(WHILE)}
Author(s): Ahnaf Faiz, 040802394
History/Versions: 1.0
Called functions: match(), precondition(), statements()
Parameters:	void
Return Value: void
Algorithm:
***********************************************************************************************************************************************/
void iterationStatement(void)
{
	match(KW_T, WHILE);
	precondition();
	match(LPR_T, NO_ATTR);
	conditionalExpression();
	match(RPR_T, NO_ATTR);
	match(KW_T, REPEAT);
	match(LBR_T, NO_ATTR);
	statements();
	match(RBR_T, NO_ATTR);
	match(EOS_T, NO_ATTR);
	gen_incode("PLATY: Iteration statement parsed");
}

/***********************************************************************************************************************************************
Purpose: <input statement> -> READ (<variable list>);
		 FIRST(<input statement>) = {KW_T(READ)}
FIRST(<variable list>) = {AVID, SVID}
Author(s): Ahnaf Faiz, 040802394
History/Versions: 1.0
Called functions: match(), variableList(), gen_incode()
Parameters:	void
Return Value: void
Algorithm:
***********************************************************************************************************************************************/
void inputStatement(void)
{
	match(KW_T, READ);
	match(LPR_T, NO_ATTR);
	variableList();
	match(RPR_T, NO_ATTR);
	match(EOS_T, NO_ATTR);
	gen_incode("PLATY: Input statement parsed");
}

/***********************************************************************************************************************************************
Purpose: <variable list> -> <variable identifier> <variable list’>
		 FIRST(<variable list>) = {AVID, SVID}
Author(s): Ahnaf Faiz, 040802394
History/Versions: 1.0
Called functions: variableIdentifier(), variableListPrime(), syn_printe()
Parameters:	void
Return Value: void
Algorithm:
***********************************************************************************************************************************************/
void variableList(void)
{
	variableIdentifier();
	variableListPrime();
	gen_incode("PLATY: Variable list parsed");
}

/***********************************************************************************************************************************************
Purpose: <variable identifier> -> {AVID | SVID}
		 FIRST(<variable identifier>) = {AVID, SVID}
Author(s): Ahnaf Faiz, 040802394
History/Versions: 1.0
Called functions: match(), syn_printe()
Parameters:	void
Return Value: void
Algorithm:
***********************************************************************************************************************************************/
void variableIdentifier(void)
{
	switch (lookahead.code)
	{
	case AVID_T:
	case SVID_T:
		match(lookahead.code, NO_ATTR); break;
	default: syn_printe(); break;
	}
}

/***********************************************************************************************************************************************
Purpose: <variable list’> -> <,><variable identifier> <variable list’> | e
		 FIRST(<variable list’>) = {COM_T, e}
Author(s): Ahnaf Faiz, 040802394
History/Versions: 1.0
Called functions: match()
Parameters:	void
Return Value: void
Algorithm:
***********************************************************************************************************************************************/
void variableListPrime(void)
{
	switch (lookahead.code)
	{
	case COM_T: match(COM_T, NO_ATTR); variableIdentifier(); variableListPrime(); break;
	default: return;
	}
}

/***********************************************************************************************************************************************
Purpose: <output statement> -> WRITE(<opt variable list>)
		 FIRST(<output statement>) = {KW_T(WRITE)}
Author(s): Ahnaf Faiz, 040802394
History/Versions: 1.0
Called functions: match(), opt_variableList()
Parameters:	void
Return Value: void
Algorithm:
***********************************************************************************************************************************************/
void outputStatement(void)
{
	match(KW_T, WRITE);
	match(LPR_T, NO_ATTR);
	opt_variableList();
	match(RPR_T, NO_ATTR);
	match(EOS_T, NO_ATTR);
	gen_incode("PLATY: Output statement parsed");
}

/***********************************************************************************************************************************************
Purpose: <opt variable list> -> <opt variable list> | <string literal [STR_T]>
		 FIRST(<opt variable list>) = {AVID, SVID, STR_T, e}
Author(s): Ahnaf Faiz, 040802394
History/Versions: 1.0
Called functions: variableList(), match()
Parameters:	void
Return Value: void
Algorithm:
***********************************************************************************************************************************************/
void opt_variableList(void)
{
	switch (lookahead.code)
	{
	case AVID_T:
	case SVID_T:
		variableList();
		break;
	case STR_T: match(STR_T, NO_ATTR);
		gen_incode("PLATY: Output list (string literal) parsed");
		break;
	default: 
		gen_incode("PLATY: Output list (empty) parsed");
		return;
	}
}
/***********************************************************************************************************************************************
Purpose: <arithmetic expression> -> <unary arithmetic expression> | <additive arithmetic expression>
		 FIRST(<arithmetic expression>) = {FIRST(unary arithmetic expression), FIRST(additive arithmetic expression)}
Author(s): Ahnaf Faiz, 040802394
History/Versions: 1.0
Called functions: unaryArithmeticExpression(), syn_printe(), additiveArithmeticExpression(), arithmeticExpression(), match();
Parameters:	void
Return Value: void
Algorithm:
***********************************************************************************************************************************************/
void arithmeticExpression(void)
{
	switch (lookahead.code)
	{
		case ART_OP_T:
			switch (lookahead.attribute.get_int)
			{
				case PLUS:
				case MINUS:
					unaryArithmeticExpression(); break;
				default: syn_printe(); break;
			}
			break;
		case AVID_T:
		case FPL_T:
		case INL_T:
			additiveArithmeticExpression(); break;
		case LPR_T: additiveArithmeticExpression(); break;
		default: syn_printe(); break;
	}
	gen_incode("PLATY: Arithmetic expression parsed");
}

/***********************************************************************************************************************************************
Purpose: <unary arithmetic expression> -> - <primary arithmetic expression> | + <primary arithmetic expression>
		 FIRST(<unary arithmetic expression>) = { +, -, FIRST(primary arithmetic expression)}
Author(s): Ahnaf Faiz, 040802394
History/Versions: 1.0
Called functions: match(), primaryArithmeticExpression(), syn_printe()
}
Parameters:	void
Return Value: void
Algorithm:
***********************************************************************************************************************************************/
void unaryArithmeticExpression(void)
{
	switch (lookahead.code)
	{
	case ART_OP_T:
		switch (lookahead.attribute.get_int)
		{
		case PLUS:
		case MINUS:
			match(lookahead.code, lookahead.attribute.get_int);
			primaryArithmeticExpression();
			break;
		default: syn_printe(); break;
		} break;
	}
	gen_incode("PLATY: Unary arithmetic expression parsed");
}

/***********************************************************************************************************************************************
Purpose: <primary arithmetic expression> -> AVID_T | FPL_T  (<arithmetic expression>)
		 FIRST(<additive arithmetic expression>) = { AVID_T, FPL_T, INL_T, ( }
Author(s): Ahnaf Faiz, 040802394
History/Versions: 1.0
Called functions: match(), arithmeticExpression(), syn_printe()
Parameters:	void
Return Value: void
Algorithm:
***********************************************************************************************************************************************/
void primaryArithmeticExpression(void)
{
	switch (lookahead.code)
	{
	case AVID_T: 
	case FPL_T: 
	case INL_T:
		match(lookahead.code, NO_ATTR);
		break;
	case LPR_T: 
		match(LPR_T, NO_ATTR);
		arithmeticExpression();
		match(RPR_T, NO_ATTR);
		break;
	default: syn_printe(); break;
	}
	gen_incode("PLATY: Primary arithmetic expression parsed");
}

/***********************************************************************************************************************************************
Purpose: <additive arithmetic expression> -> <multiplicative expression> <additive expression’>
		 FIRST(<additive arithmetic expression>) = { AVID_T, FPL_T, INL_T, ( }
Author(s): Ahnaf Faiz, 040802394
History/Versions: 1.0
Called functions: multiplicativeArithmeticExpression(), additiveArithmeticExpressionPrime()
}
Parameters:	void
Return Value: void
Algorithm:
***********************************************************************************************************************************************/
void additiveArithmeticExpression(void)
{
	multiplicativeArithmeticExpression();
	additiveArithmeticExpressionPrime();
}

/***********************************************************************************************************************************************
Purpose: <additive expression’> -> + <multiplicative expression> <additive expression’> |
		 - <multiplicative expression> <additive expression’> | e
		 FIRST(<additive arithmetic expression’>) = { +, -, e}
Author(s): Ahnaf Faiz, 040802394
History/Versions: 1.0
Called functions: match(), multiplicativeArithmeticExpression(), additiveArithmeticExpressionPrime(), gen_incode()
Parameters:	void
Return Value: void
Algorithm:
***********************************************************************************************************************************************/
void additiveArithmeticExpressionPrime(void)
{
	switch (lookahead.code)
	{
	case ART_OP_T:
		switch (lookahead.attribute.arr_op)
		{
		case PLUS:
		case MINUS:
			match(ART_OP_T, lookahead.attribute.arr_op);
			multiplicativeArithmeticExpression();
			additiveArithmeticExpressionPrime();
			gen_incode("PLATY: Additive arithmetic expression parsed");
			break;
		default: return;
		} break;
	}
}

/***********************************************************************************************************************************************
Purpose: <multiplicative arithmetic expression> -> <primary expression> <multiplicative arithmetic expression’>
		 FIRST(<multiplicative arithmetic expression>) = { AVID_T, FPL_T, INL_T, ( }
Author(s): Ahnaf Faiz, 040802394
History/Versions: 1.0
Called functions: primaryArithmeticExpression(), multiplicativeArithmeticExpressionPrime()
Parameters:	void
Return Value: void
Algorithm:
***********************************************************************************************************************************************/
void multiplicativeArithmeticExpression(void)
{
	primaryArithmeticExpression();
	multiplicativeArithmeticExpressionPrime();
}

/***********************************************************************************************************************************************
Purpose: <multiplicative expression’> -> * <primary expression> <multiplicative expression’> |
		 / <primary expression> <multiplicative expression’> | e
		 FIRST(<multiplicative arithmetic expression’>) = {*, /,  e}
Author(s): Ahnaf Faiz, 040802394
History/Versions: 1.0
Called functions: match(), primaryArithmeticExpression(), multiplicativeArithmeticExpressionPrime()
Parameters:	void
Return Value: void
Algorithm:
***********************************************************************************************************************************************/
void multiplicativeArithmeticExpressionPrime(void)
{
	switch (lookahead.code)
	{
	case ART_OP_T:
		switch (lookahead.attribute.get_int)
		{
		case MULT:
		case DIV:
			match(lookahead.code, lookahead.attribute.get_int);
			primaryArithmeticExpression();
			multiplicativeArithmeticExpressionPrime();
			gen_incode("PLATY: Multiplicative arithmetic expression parsed");
			break;
		default: return;
		} break;
	}
}

/***********************************************************************************************************************************************
Purpose: <string expression> -> <primary string expression> <string expression’>
		 FIRST(<string expression>) = {SVID_T, STR_T}
Author(s): Ahnaf Faiz, 040802394
History/Versions: 1.0
Called functions: primaryStringExpression(), stringExpressionPrime(), gen_code()
Parameters:	void
Return Value: void
Algorithm:
***********************************************************************************************************************************************/
void stringExpression(void)
{
	primaryStringExpression();
	stringExpressionPrime();
	gen_incode("PLATY: String expression parsed");
}

/***********************************************************************************************************************************************
Purpose: <string expression’> ->  # <primary string expression> <string expression’> | e
Author(s): Ahnaf Faiz, 040802394
History/Versions: 1.0
Called functions: match(), primaryStringExpression(), stringExpressionPrime()
Parameters:	void
Return Value: void
Algorithm:
***********************************************************************************************************************************************/
void stringExpressionPrime(void)
{
	switch (lookahead.code)
	{
	case SCC_OP_T: match(SCC_OP_T, NO_ATTR);
		primaryStringExpression();
		stringExpressionPrime();
		break;
	default: return;
	}
}

/***********************************************************************************************************************************************
Purpose: <primary string expression> -> SVID_T | STR_T
		 FIRST(<primary string expression>) = {#,  e}
Author(s): Ahnaf Faiz, 040802394
History/Versions: 1.0
Called functions: match(), syn_printe(), gen_incode()
Parameters:	void
Return Value: void
Algorithm:
***********************************************************************************************************************************************/
void primaryStringExpression(void)
{
	switch (lookahead.code)
	{
	case SVID_T: 
	case STR_T: match(lookahead.code, NO_ATTR); break;
	default: syn_printe(); break;
	}
	gen_incode("PLATY: Primary string expression parsed");
}
/***********************************************************************************************************************************************
* END OF FILE *
***********************************************************************************************************************************************/
