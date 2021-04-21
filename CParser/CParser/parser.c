/*************************************************************************************
File name :		Parser.c
Compiler :		MS Visual Studio 15
Author :		Sharusshan Sinnadurai / Sharusshan Sinnadurai
Course :		CST 8152 – Compilers, Lab Section : 10
Assignment :	3 - Parser
Date :			December 07, 2018
Professor :		Sv.Ranev
Function List: void syn_printe(); void parser(void);void match(int, int);void gen_incode(char*);
void program(void); void opt_statements(void); void statements(void); void statementsP(void);
void statement(void); void assignmentStatement(void); void assignmentExpression(void);
void selectionStatement(void); void iterationStatement(void); void preCondition(void);
void inputStatement(void); void variableList(void); void variableIdentifier(void);
void variableListP(void); void outputStatement(void); void outputList(void);
void arithmeticExpression(void); void unaryArithmeticExpression(void);
void additiveArithmeticExpression(void); void additiveArithmeticExpressionP(void);
void multiplicativeArithmeticExpression(void); void multiplicativeArithmeticExpressionP(void);
void primaryArithmeticExpression(void); void stringExpression(void);
void stringExpressionP(void); void stringLiteral(void); void primaryStringExpression(void);
void conditionalExpression(void); void logicalORexpression(void); void logicalORexpressionP(void);
void logicalANDexpression(void); void logicalANDexpressionP(void); void relationalExpression(void);
void primarySRelationalExpression(void); void primaryARelationalExpression(void);
void primaryARelationalExpressionP(void); void primarySRelationalExpressionP(void);
*************************************************************************************/
#include "parser.h"

void gen_incode(char* out)
{
	printf("%s\n", out);
}


void parser(void) {
	lookahead = malar_next_token();
	program();
	match(SEOF_T, lookahead.attribute.get_int);
	gen_incode("PLATY: Source file parsed");
}

void match(int pr_token_code, int pr_token_attribute)
{
	if (lookahead.code != pr_token_code) {
		syn_eh(pr_token_code);
		return;
	}

	switch (lookahead.code) {
	case SEOF_T:
		return;
	case KW_T:
	case LOG_OP_T:
	case ART_OP_T:
	case REL_OP_T:
		if (lookahead.attribute.get_int != pr_token_attribute) {
			syn_eh(pr_token_code);
			return;
		}
	default: lookahead = malar_next_token();
		if (lookahead.code == ERR_T) {
			syn_printe();
			lookahead = malar_next_token();
			++synerrno;
		}
		return;
	}
}

void syn_eh(int sync_token_code)
{

	syn_printe();
	++synerrno;
	while (lookahead.code != SEOF_T)
	{
		lookahead = malar_next_token();
		if (lookahead.code == sync_token_code)
		{
			if (lookahead.code != SEOF_T)
				lookahead = malar_next_token();
			return;
		}
	}
	if (sync_token_code != SEOF_T)
		exit(synerrno);
}

/*
<program> ->PLATYPUS {<opt_statements>}

FIRST(program) = {KW_T(PLATYPUS) }
*/
void program(void) {
	match(KW_T, PLATYPUS);
	match(LBR_T, NO_ATTR);
	opt_statements();
	match(RBR_T, NO_ATTR);
	gen_incode("PLATY: Program parsed");
}

/*
<opt_statements>-> <statements> | E

FIRST(opt_statements) = { FIRST(statements), E } = { AVID_T, SVID_T, KW_T(IF), KW_T(WHILE), KW_T(READ), KW_T(WRITE), E}
*/
void  opt_statements(void) {
	switch (lookahead.code) {
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
			&& lookahead.attribute.get_int != FALSE) {
			statements();
			break;
		}
	default: /*empty string – optional statements*/;
		gen_incode("PLATY: Opt_statements parsed");
	}
}

/* <statements>-> <statement><statements’>

FIRST(statements) = { FIRST(statement) } = { AVID_T, SVID_T, KW_T(IF), KW_T(WHILE), KW_T(READ), KW_T(WRITE) }
*/
void statements(void) {
	statement(); statementsP();
}

/*
<statements’>-> <statement><statements’>|E

FIRST(statements’) = { FIRST(statement), E } = { AVID_T, SVID_T, KW_T(IF), KW_T(WHILE), KW_T(READ), KW_T(WRITE), E }
*/
void statementsP(void)
{
	switch (lookahead.code) {
	case AVID_T:
	case SVID_T: statement(); statementsP(); break;
	case KW_T:
		if (lookahead.attribute.get_int != PLATYPUS
			&& lookahead.attribute.get_int != ELSE
			&& lookahead.attribute.get_int != THEN
			&& lookahead.attribute.get_int != REPEAT
			&& lookahead.attribute.get_int != TRUE
			&& lookahead.attribute.get_int != FALSE) {
			statement();
			statementsP();
			break;
		}
	default: /*empty string – optional statements*/;
	}
}

/*
<statement> -> <assignment statement>
| <selection statement>
| <iteration statement>
| <input statement>
| <output statement>

FIRST(statement) = { FIRST(assignment statement), FIRST(selection statement), FIRST(iteration statement), FIRST(input statement), FIRST(output statement) } = { AVID_T, SVID_T, KW_T(IF), KW_T(WHILE), KW_T(READ), KW_T(WRITE) }
*/
void statement(void)
{
	switch (lookahead.code) {
	case AVID_T:
	case SVID_T: assignmentStatement(); break;
	case KW_T:
		switch (lookahead.attribute.get_int) {
		case IF: selectionStatement(); break;
		case WHILE: iterationStatement(); break;
		case READ: inputStatement(); break;
		case WRITE: outputStatement(); break;
		default: syn_printe();
		} break;
	default: syn_printe();
	}
}

/*
<assignment statement> ->
<assignment expression>;

FIRST(assignment statement) = { FIRST(assignment expression) } = { AVID_T, SVID_T }
*/
void assignmentStatement(void)
{
	assignmentExpression();
	match(EOS_T, NO_ATTR);
	gen_incode("PLATY: Assignment statement parsed");
}

/*
< assignment expression> ->
AVID = <arithmetic expression>
| SVID = <string expression>

FIRST(assignment expression) = { AVID_T, SVID_T }
*/
void assignmentExpression(void)
{
	switch (lookahead.code) {
	case AVID_T:match(AVID_T, NO_ATTR); match(ASS_OP_T, NO_ATTR); arithmeticExpression();
		gen_incode("PLATY: Assignment expression (arithmetic) parsed");
		break;
	case SVID_T:match(SVID_T, NO_ATTR); match(ASS_OP_T, NO_ATTR); stringExpression();
		gen_incode("PLATY: Assignment expression (string) parsed");
		break;
	default: syn_printe();
	}
}

/*
<selection statement> ->
IF <pre-condition> (<conditional expression>)
THEN { <opt_statements> } ELSE { <opt_statements> };

FIRST(selection statement) = { KW_T(IF) }
*/
void selectionStatement(void)
{
	match(KW_T, IF);
	preCondition();
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

/*
<iteration statement> ->
WHILE <pre-condition> (<conditional expression>)
REPEAT { <statements>};

FIRST(iteration statement) = { KW_T(WHILE) }
*/
void iterationStatement(void)
{
	match(KW_T, WHILE);
	preCondition();
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

/*
<pre-condition> ->
TRUE
| FALSE

FIRST(pre-condition) = { KW_T(TRUE), KW_T(FALSE) }
*/
void preCondition(void)
{
	switch (lookahead.code) {

	case KW_T:
		if (lookahead.attribute.get_int == TRUE)
			match(KW_T, TRUE);
		else if (lookahead.attribute.get_int == FALSE)
			match(KW_T, FALSE);
		break;
	default: syn_printe();
	}
}

/*
<input statement> ->
READ (<variable list>);

FIRST(input statement) = { KW_T(READ) }
*/
void inputStatement(void)
{
	match(KW_T, READ);
	match(LPR_T, NO_ATTR);
	variableList();
	match(RPR_T, NO_ATTR);
	match(EOS_T, NO_ATTR);
	gen_incode("PLATY: Input statement parsed");
}

/*
<variable list> ->
<variable identifier><variable list’>

FIRST(variable list) = { FIRST(variable identifier)} ={ SVID_T, AVID_T }
*/
void variableList(void)
{
	variableIdentifier();	variableListP();
}

/*
<variable identifier> ->
SVID_T |
AVID_T

FIRST(variable identifier) = { SVID_T, AVID_T }
*/
void variableIdentifier(void) {
	switch (lookahead.code) {
	case AVID_T:
		match(AVID_T, NO_ATTR); break;
	case SVID_T:
		match(SVID_T, NO_ATTR); break;

	default:
		syn_printe();
	}
}

/*
<variable list’> ->
,<variable list>|E

FIRST(variable list’) = { , , E }

*/
void variableListP(void)
{
	switch (lookahead.code) {
	case COM_T: match(COM_T, NO_ATTR); variableList(); break;
	default: /*empty string – optional statements*/;
		gen_incode("PLATY: Variable list parsed");
	}


}

/*
<output statement> ->
WRITE(<output list>);

FIRST(output statement) = { KW_T(WRITE) }
*/
void outputStatement(void)
{
	match(KW_T, WRITE);
	match(LPR_T, NO_ATTR);
	outputList();
	match(RPR_T, NO_ATTR);
	match(EOS_T, NO_ATTR);
	gen_incode("PLATY: Output statement parsed");
}

/*
<output list> ->
<variable list>
| STR_T|E

FIRST(output list) = { SVID_T, AVID_T, STR_T, E }
*/
void outputList(void)
{
	switch (lookahead.code) {
	case SVID_T:
	case AVID_T: variableList();

		break;
	case STR_T: match(STR_T, NO_ATTR);
		gen_incode("PLATY: Output list (string literal) parsed");
		break;

	default:  gen_incode("PLATY: Output list (empty) parsed");/*empty string – optional statements*/;

	}
}

/*
<arithmetic expression> ->
<unary arithmetic expression>
| <additive arithmetic expression>

FIRST(arithmetic expression) = { FIRST(unary arithmetic expression), FIRST(additive arithmetic expression) } = { AVID_T, FPL_T, INL_T, (,+, - }
*/
void arithmeticExpression(void)
{
	switch (lookahead.code) {
	case AVID_T:
	case FPL_T:
	case INL_T:
	case LPR_T: additiveArithmeticExpression(); break;
	case ART_OP_T:
		switch (lookahead.attribute.get_int) {
		case PLUS:
		case MINUS:unaryArithmeticExpression(); break;
		default:syn_printe();
		} break;
	default: /*empty string – optional statements*/ break;
	}
	gen_incode("PLATY: Arithmetic expression parsed");
}

/*
<unary arithmetic expression> ->
- <primary arithmetic expression>
| + <primary arithmetic expression>

FIRST(unary arithmetic expression) = { -, + }
*/
void unaryArithmeticExpression(void)
{
	switch (lookahead.code) {
	case ART_OP_T:
		switch (lookahead.attribute.get_int) {
		case PLUS:
		case MINUS: match(ART_OP_T, lookahead.attribute.get_int);
			primaryArithmeticExpression();
			gen_incode("PLATY: Unary arithmetic expression parsed");
			return;
		}

	default: syn_printe();
	}
}

/*
<additive arithmetic expression> ->
<multiplicative arithmetic expression><additive arithmetic expression’>

FIRST(additive arithmetic expression) = { FIRST(multiplicative arithmetic expression)} = { AVID_T, FPL_T, INL_T, ( }
*/
void additiveArithmeticExpression(void)
{
	multiplicativeArithmeticExpression(); additiveArithmeticExpressionP();
}

/*
<multiplicative arithmetic expression> ->
<primary arithmetic expression> <multiplicative arithmetic expression’>

FIRST(multiplicative arithmetic expression) = { FIRST(primary arithmetic expression)} =  { AVID_T, FPL_T, INL_T, ( }
*/
void additiveArithmeticExpressionP(void)
{
	switch (lookahead.code) {
	case ART_OP_T:
		switch (lookahead.attribute.get_int) {
		case PLUS:
		case MINUS: match(ART_OP_T, lookahead.attribute.get_int);
			multiplicativeArithmeticExpression();
			additiveArithmeticExpressionP();
			gen_incode("PLATY: Additive arithmetic expression parsed");
			break;
		}
	default: /*empty string – optional statements*/; break;
	}
}

/*
<multiplicative arithmetic expression> ->
<primary arithmetic expression> <multiplicative arithmetic expression’>

FIRST(multiplicative arithmetic expression) = { FIRST(primary arithmetic expression)} = { AVID_T, FPL_T, INL_T, ( }
*/
void multiplicativeArithmeticExpression(void)
{
	primaryArithmeticExpression(); multiplicativeArithmeticExpressionP();
}

/*
<multiplicative arithmetic expression’> ->
*<primary arithmetic expression><multiplicative arithmetic expression’>
| /<primary arithmetic expression><multiplicative arithmetic expression’>
| E

FIRST(multiplicative arithmetic expression’) = { *, /, E }
*/
void multiplicativeArithmeticExpressionP(void)
{
	switch (lookahead.code) {
	case ART_OP_T:
		switch (lookahead.attribute.get_int) {
		case MULT:
		case DIV: match(ART_OP_T, lookahead.attribute.get_int);
			primaryArithmeticExpression();
			multiplicativeArithmeticExpressionP();
			gen_incode("PLATY: Multiplicative arithmetic expression parsed");
			return;
		}
	default: /*empty string – optional statements*/;
	}
}

/*
<primary arithmetic expression> ->
<arithmetic variable identifier>
| <floating-point literal>
| <integer literal>
| (<arithmetic expression>)

FIRST(primary arithmetic expression) = { AVID_T, FPL_T, INL_T, ( }
*/
void primaryArithmeticExpression(void)
{
	switch (lookahead.code) {
	case FPL_T:
	case INL_T:
	case AVID_T: match(lookahead.code, NO_ATTR); gen_incode("PLATY: Primary arithmetic expression parsed"); break;
	case LPR_T:
		match(LPR_T, NO_ATTR); arithmeticExpression(); match(RPR_T, NO_ATTR);
		gen_incode("PLATY: Primary arithmetic expression parsed");
		break;
	}
}

/*
<string expression> ->
<primary string expression><string expression’>

FIRST(string expression) -> { FIRST(primary string expression) } = { SVID_T, STR_T }
*/
void stringExpression(void)
{
	primaryStringExpression(); stringExpressionP();

}

/*
<string expression’> ->
#<primary string expression><string expression’>
|E

FIRST(string expression’) -> { #, SVID_T, STR_T, E }
*/
void stringExpressionP(void)
{
	switch (lookahead.code) {
	case SCC_OP_T: match(SCC_OP_T, NO_ATTR); primaryStringExpression(); stringExpressionP(); break;
	case SVID_T:
	case STR_T: match(lookahead.code, NO_ATTR); break;
	default: /*empty string – optional statements*/; gen_incode("PLATY: String expression parsed"); break;
	}
}
/*
<primary string expression> ->
<string variable identifier>
| <string literal>

FIRST(primary string expression) = { SVID_T, STR_T }
*/
void primaryStringExpression(void)
{
	switch (lookahead.code) {
	case SVID_T: match(SVID_T, NO_ATTR); break;
	case STR_T: match(STR_T, NO_ATTR); break;
	}
	gen_incode("PLATY: Primary string expression parsed");
}

/*
<conditional expression> ->
<logical OR expression>

FIRST(conditional expression) = { FIRST(logical OR expression) } = { AVID_T, FPL_T, INL_T, SVID_T, STR_T }
*/
void conditionalExpression(void)
{
	logicalORexpression();
	gen_incode("PLATY: Conditional expression parsed");
}

/*
<logical OR expression> ->
<logical AND expression><logical OR expression’>

FIRST(logical OR expression) = { FIRST(logical AND expression) } = { AVID_T, FPL_T, INL_T, SVID_T, STR_T }
*/
void logicalORexpression(void)
{
	logicalANDexpression(); logicalORexpressionP();

}

/*
<logical OR expression’> ->
.OR.<logical AND expression><logical OR expression’>
|E

FIRST(logical OR expression’) = { LOG_OP_T(OR), E }
*/
void logicalORexpressionP(void)
{
	switch (lookahead.code) {
	case LOG_OP_T:
		switch (lookahead.attribute.get_int)
		{
		case OR:
			match(LOG_OP_T, OR);
			logicalANDexpression();
			logicalORexpressionP();
			gen_incode("PLATY: Logical OR expression parsed");
			return;
		};
	default: /*empty string – optional statements*/;  break;
	}


}

/*
<logical AND expression> ->
<relational expression><logical AND expression’>

FIRST(logical AND expression) = { FIRST(relational expression) } = { AVID_T, FPL_T, INL_T, SVID_T, STR_T }
*/
void logicalANDexpression(void)
{
	relationalExpression(); logicalANDexpressionP();

}

/*
<logical AND expression’> ->
.AND.<relational expression><logical AND expression’>
|E

FIRST(logical AND expression’) = { LOG_OP_T(AND), E }
*/
void logicalANDexpressionP(void)
{
	switch (lookahead.code) {
	case LOG_OP_T:
		switch (lookahead.attribute.get_int)
		{
		case AND:
			match(LOG_OP_T, AND);
			relationalExpression();
			logicalANDexpressionP();
			gen_incode("PLATY: Logical AND expression parsed");
			return;
		}
	default: /*empty string – optional statements*/;
	}

}

/*
<relational expression> ->
<primary a_relational expression> < primary a_relational expression’>
| <primary s_relational expression> < primary s_relational expression’>

FIRST (relational expression) = {FIRST (primary a_relational expression), FRIST (primary s_relational expression)} = AVID_T, FPL_T, INL_T, SVID_T, STR_T
*/
void relationalExpression(void)
{
	switch (lookahead.code) {
	case AVID_T:
	case FPL_T:
	case INL_T: primaryARelationalExpression(); primaryARelationalExpressionP();

		break;
	case SVID_T:
	case STR_T: primarySRelationalExpression(); primarySRelationalExpressionP(); break;
	default: syn_printe();

	}
	gen_incode("PLATY: Relational expression parsed");
}

/*
<primary s_relational expression> ->
<primary string expression>

FIRST(primary s_relational expression) = SVID_T, STR_T
*/
void primarySRelationalExpression(void)
{
	primaryStringExpression();
	gen_incode("PLATY: Primary s_relational expression parsed");
}

/*
<primary a_relational expression> ->
<floating-point literal>
| <integer literal>
| <arithmetic variable identifier>

FIRST(primary a_relational expression) = AVID_T, FPL_T, INL_T
*/
void primaryARelationalExpression(void)
{
	switch (lookahead.code) {
	case AVID_T:
	case FPL_T:
	case INL_T:
		match(lookahead.code, NO_ATTR);
		gen_incode("PLATY: Primary a_relational expression parsed");
		break;
	default: syn_printe(); gen_incode("PLATY: Primary a_relational expression parsed");
	}
}

/*
<primary a_relational expression‘> ->
== <primary a_relational expression>
| <> <primary a_relational expression>
| > <primary a_relational expression>
| < <primary a_relational expression>

FIRST (primary a_relational expression’) = { REL_OP_T(EQ), REL_OP_T (NE), REL_OP_T (GT), REL_OP_T (LT) }
*/
void primaryARelationalExpressionP(void)
{
	if (lookahead.code == REL_OP_T)
	{
		switch (lookahead.attribute.get_int) {
		case EQ:
		case NE:
		case GT:
		case LT:
			match(REL_OP_T, lookahead.attribute.get_int);
			primaryARelationalExpression();
			break;
		default: syn_printe();

		}
	}
	else
		syn_printe();
}

/*
<primary s_relational expression‘> ->
== <primary s_relational expression>
| <> <primary s_relational expression>
| > <primary s_relational expression>
| < <primary s_relational expression>

FIRST (primary s_relational expression’) = REL_OP_T {EQ, NE, GT, LT}
*/
void primarySRelationalExpressionP(void)
{

	if (lookahead.code == REL_OP_T)
	{
		match(REL_OP_T, lookahead.attribute.rel_op);
		primarySRelationalExpression();
	}
	else
		syn_printe();
}



/* error printing function for Assignment 3 (Parser), F18 */
void syn_printe() {
	Token t = lookahead;

	printf("PLATY: Syntax error:  Line:%3d\n", line);
	printf("*****  Token code:%3d Attribute: ", t.code);
	switch (t.code) {
	case  ERR_T: /* ERR_T     0   Error token */
		printf("%s\n", t.attribute.err_lex);
		break;
	case  SEOF_T: /*SEOF_T    1   Source end-of-file token */
		printf("SEOF_T\t\t%d\t\n", t.attribute.seof);
		break;
	case  AVID_T: /* AVID_T    2   Arithmetic Variable identifier token */
	case  SVID_T:/* SVID_T    3  String Variable identifier token */
		printf("%s\n", t.attribute.vid_lex);
		break;
	case  FPL_T: /* FPL_T     4  Floating point literal token */
		printf("%5.1f\n", t.attribute.flt_value);
		break;
	case INL_T: /* INL_T      5   Integer literal token */
		printf("%d\n", t.attribute.get_int);
		break;
	case STR_T:/* STR_T     6   String literal token */
		printf("%s\n", b_location(str_LTBL, t.attribute.str_offset));
		break;

	case SCC_OP_T: /* 7   String concatenation operator token */
		printf("NA\n");
		break;

	case  ASS_OP_T:/* ASS_OP_T  8   Assignment operator token */
		printf("NA\n");
		break;
	case  ART_OP_T:/* ART_OP_T  9   Arithmetic operator token */
		printf("%d\n", t.attribute.get_int);
		break;
	case  REL_OP_T: /*REL_OP_T  10   Relational operator token */
		printf("%d\n", t.attribute.get_int);
		break;
	case  LOG_OP_T:/*LOG_OP_T 11  Logical operator token */
		printf("%d\n", t.attribute.get_int);
		break;

	case  LPR_T: /*LPR_T    12  Left parenthesis token */
		printf("NA\n");
		break;
	case  RPR_T: /*RPR_T    13  Right parenthesis token */
		printf("NA\n");
		break;
	case LBR_T: /*    14   Left brace token */
		printf("NA\n");
		break;
	case RBR_T: /*    15  Right brace token */
		printf("NA\n");
		break;

	case KW_T: /*     16   Keyword token */
		printf("%s\n", kw_table[t.attribute.get_int]);
		break;

	case COM_T: /* 17   Comma token */
		printf("NA\n");
		break;
	case EOS_T: /*    18  End of statement *(semi - colon) */
		printf("NA\n");
		break;
	default:
		printf("PLATY: Scanner error: invalid token code: %d\n", t.code);
	}/*end switch*/
}/* end syn_printe()*/