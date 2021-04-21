/*************************************************************************************
File name :		Scanner.c
Compiler :		MS Visual Studio 15
Author :		Sharusshan Sinnadurai
Course :		CST 8152 – Compilers, Lab Section : 10
Assignment :	2 - Scanner
Date :			November 22, 2018
Professor :		Sv.Ranev
Function List: 	Token aa_func02(), Token aa_func03(), Token aa_func05(),Token aa_func08(),
Token aa_func10(), Token aa_func12(),int scanner_init(), Token malar_next_token(),
int iskeyword(), int get_next_state(), int char_class()
*************************************************************************************/

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
						 /* scerrnum is loaded with a unsigned number if an error is found. Whereever the error is found it loads
						 scerrnum with the func number.  Ex: aa_func03, if runtime error occurs it will return scerrnum = 3*/

						 /* Local(file) global objects - variables */
static Buffer *lex_buf;/*pointer to temporary lexeme buffer*/
static pBuffer sc_buf; /*pointer to input source buffer*/
					   /* No other global variable declarations/definitions are allowed */

					   /* scanner.c static(local) function  prototypes */
static int char_class(char c); /* character class function */
static int get_next_state(int, char, int *); /* state machine function */
static int iskeyword(char * kw_lexeme); /*keywords lookup functuion */


										/*Initializes scanner */
int scanner_init(Buffer * psc_buf) {
	if (b_isempty(psc_buf)) return EXIT_FAILURE;/*1*/
												/* in case the buffer has been read previously  */
	b_rewind(psc_buf);
	b_clear(str_LTBL);
	line = 1;
	sc_buf = psc_buf;
	return EXIT_SUCCESS;/*0*/
						/*   scerrnum = 0;  *//*no need - global ANSI C */
}


/**********************************************************************************************************
* Purpose:			Performs token recognition by reading lexeme from the input stream (sc_buf)
* Author:			Sharusshan Sinnadurai
* History/Versions:	2.0
* Called Function:	b_getc(), b_retract(), b_getcoffset(), b_mark(), b_addc(), b_free(), b_reset();

* Parameters:		sc_buf		 Buffer*	    Points to a valid buffer structure in the heap
* Return Value:		Token containing attribute value and code
* Algorithm:		Performs token recognition by
*					reading one char at a time from the input buffer (sc_buf)
*					if the character read is equal to SEOF, breaks out of the while loop
*					else loops through, processing tokens one by one (for special cases or exceptions)
*					or using transition table (for SVID, AVID, DIL, FIL, OIL, Comment)
*					returns a token structure if a token pattern (as defined in the lexical grammar)
*					matches with a lexeme found in the stream of input symbols.
*					else returns a Error Token if a error occurs
**********************************************************************************************************/
Token malar_next_token(void) {

	Token t = { 0 }; /* token to return after pattern recognition. Set all structure members to 0 */
	unsigned char c; /* input symbol */
	int state = 0; /* initial state of the FSM */
	short lexstart;  /*start offset of a lexeme in the input char buffer (array) */
	short lexend;    /*end offset of a lexeme in the input char buffer (array)*/
	int accept = NOAS; /* type of state - initially not accepting */


	while (1) { /* endless loop broken by token returns it will generate a warning */


				/* GET THE NEXT SYMBOL FROM THE INPUT BUFFER */

		c = b_getc(sc_buf);

		if (c == SEOF_1) {
			t.code = SEOF_T;
			t.attribute.seof = SEOF1;
			return t;
		}
		else if (c == SEOF_2)
		{
			t.code = SEOF_T;
			t.attribute.seof = SEOF2;
			return t;
		}


		switch (c) {

		case '\n':
			++line;
			continue;


			/* IF(c == '!') TRY TO PROCESS COMMENT
			IF THE FOLLOWING CHAR IS NOT !REPORT AN ERROR
			ELSE IN A LOOP SKIP CHARACTERS UNTIL line terminator is found THEN continue; */
		case '!':
			b_mark(sc_buf, b_getcoffset(sc_buf));
			c = b_getc(sc_buf);
			if (c == '!')
			{
				while (1)
				{
					c = b_getc(sc_buf);
					if (c == '\n')
					{
						++line;
						break;
					}

					continue;
				}
			}
			else
			{
				t.code = ERR_T;
				t.attribute.err_lex[0] = '!';
				t.attribute.err_lex[1] = c;

				while (c != '\n')
				{
					c = b_getc(sc_buf);
					if (c == '\n')
						++line;
				}
				return t;
			}
			continue;
		case '#':
			t.code = SCC_OP_T;
			return t;
		case ';':
			t.code = EOS_T;
			return t;
		case '+':
			t.code = ART_OP_T;
			t.attribute.arr_op = PLUS;
			return t;
		case '-':
			t.code = ART_OP_T;
			t.attribute.arr_op = MINUS;
			return t;
		case '/':
			t.code = ART_OP_T;
			t.attribute.arr_op = DIV;
			return t;
		case '*':
			t.code = ART_OP_T;
			t.attribute.arr_op = MULT;
			return t;

		case '{':
			t.code = LBR_T;
			return t;

		case '}':
			t.code = RBR_T;
			return t;

		case '(':
			t.code = LPR_T;
			return t;
		case ')':
			t.code = RPR_T;
			return t;

		case ' ':
		case '\t': /*horizontal tab*/
		case '\v': /*vertical tab*/
		case '\f': /*formfeed*/
			continue;

			/* if an = sign is found check to see if the following char is also an = */
		case '=':
			if (b_getc(sc_buf) == '=') {
				/* produce token for == */
				t.code = REL_OP_T;
				t.attribute.rel_op = EQ;
				return t;
			}
			b_retract(sc_buf);
			/* else produce assignment token and throw back the char to be read again */
			t.code = ASS_OP_T;

			return t;

		case '>':
			t.code = REL_OP_T;
			t.attribute.rel_op = GT;
			return t;
		case '.':
			b_mark(sc_buf, b_getcoffset(sc_buf));
			if (b_getc(sc_buf) == 'A' && b_getc(sc_buf) == 'N' && b_getc(sc_buf) == 'D' && b_getc(sc_buf) == '.')
			{
				t.code = LOG_OP_T;
				t.attribute.log_op = AND;
				return t;
			}
			else
				b_reset(sc_buf);
			if (b_getc(sc_buf) == 'O' && b_getc(sc_buf) == 'R' && b_getc(sc_buf) == '.')
			{
				t.code = LOG_OP_T;
				t.attribute.log_op = OR;
				return t;
			}
			else
				b_reset(sc_buf);
			break;

		case '<':
			if (b_getc(sc_buf) == '>') {
				t.code = REL_OP_T;
				t.attribute.rel_op = NE;

			}
			else {
				b_retract(sc_buf);
				t.code = REL_OP_T;
				t.attribute.rel_op = LT;
			}
			return t;

		case ',':
			t.code = COM_T;
			return t;

		default:
			break;
		}


		lexstart = b_mark(sc_buf, b_getcoffset(sc_buf) - 1);

		state = get_next_state(state, c, &accept);

		/* continuously loop until state has changed from NOAS */

		while (accept == NOAS) {

			state = get_next_state(state, b_getc(sc_buf), &accept);
		}
		if (accept == ASWR)
			b_retract(sc_buf);

		lexend = b_getcoffset(sc_buf);
		lex_buf = b_allocate(lexend - lexstart, 15, 'f');

		if (!lex_buf) {
			scerrnum = 1;
			aa_func12("RUN TIME ERROR!");
		}
		/* reset to marked spot */
		b_reset(sc_buf);

		while (lexstart < lexend) {
			/* while lexend is greater than the offset add the chars to the lex_buf */
			c = b_getc(sc_buf);
			b_addc(lex_buf, c);
			++lexstart;
		}
		b_compact(lex_buf, '\0');
		//printf("lexemeMAIN |%s|\n", b_location(lex_buf, 0));

		t = aa_table[state](b_location(lex_buf, 0));
		b_free(lex_buf);
		return t;
	}/* end while */
}/* end function */


 /**********************************************************************************************************
 * Purpose:				gets the next state depeneding on the curr state and input buffer
 * Author:				Svillen Ranev
 * History/Versions:	2.9v
 * Called Function:		char_class()
 * Parameters:			curr state / char c / int pointer to states(NOAS, ASWR ...)
 * Return Value:		The next state
 * Algorithm:			None
 **********************************************************************************************************/

int get_next_state(int state, char c, int *accept) {
	int col;
	int next;
	col = char_class(c);
	next = st_table[state][col];
#ifdef DEBUG
	printf("Input symbol: %c Row: %d Column: %d Next: %d \n", c, state, col, next);
#endif

	assert(next != IS);


#ifdef DEBUG
	if (next == IS) {
		printf("Scanner Error: Illegal state:\n");
		printf("Input symbol: %c Row: %d Column: %d\n", c, state, col);
		exit(1);
	}
#endif
	*accept = as_table[next];
	return next;
}

/**********************************************************************************************************
* Purpose:			Defines which column each char goes into
* Author:			Sharusshan Sinnadurai
* History/Versions:	1.0
* Called Function:	isaplpha(), isdigit()
* Parameters:		character c
* Return Value:		int val
* Algorithm:		None
**********************************************************************************************************/

int char_class(char c)
{
	int val;

	/*THIS FUNCTION RETURNS THE COLUMN NUMBER IN THE TRANSITION
	TABLE st_table FOR THE INPUT CHARACTER c.
	SOME COLUMNS MAY REPRESENT A CHARACTER CLASS .
	FOR EXAMPLE IF COLUMN 2 REPRESENTS[A - Za - z]
	THE FUNCTION RETURNS 2 EVERY TIME c IS ONE
	OF THE LETTERS A, B, ..., Z, a, b...z.
	PAY ATTENTION THAT THE FIRST COLOMN IN THE TT IS 0 (has index 0)*/
	if (isalpha((unsigned char)c))
	{
		val = 0;
	}
	else if (isdigit((unsigned char)c)) {
		if (c == '0') {
			val = 1;
		}
		else
			val = 2;
	}
	else if (c == '.') {
		val = 3;
	}
	else if (c == '$') {
		val = 4;
	}
	else if (c == '"') {
		val = 6;
	}
	else if (c == SEOF_1 || c == (signed)SEOF_2) {
		val = 7;
	}
	else
		val = 5;

	return val;
}



/*	ACCEPTING FUNCTION FOR THE arithmentic variable identifier AND keywords(VID - AVID / KW)
REPLACE XX WITH THE CORRESPONDING ACCEPTING STATE NUMBER */

/**********************************************************************************************************
* Purpose:			ACCEPTING FUNCTION FOR THE arithmentic variable identifier AND keywords(VID - AVID / KW)
* Author:			Sharusshan Sinnadurai
* History/Versions:	1.0
* Called Function:  strcmp(), strlen(), isKeyword()
* Parameters:		char array lexeme
* Return Value:		Keyword Token or AVID Token
* Algorithm:		Checks if the lexeme is a keyword or AVID and creates a token accordingly
**********************************************************************************************************/
Token aa_func02(char lexeme[]) {

	Token t;
	/* sets the location of the keyword into value.*/

	if (iskeyword(lexeme) != -1) {
		t.code = KW_T;
		t.attribute.kwt_idx = iskeyword(lexeme);
		/*WHEN CALLED THE FUNCTION MUST
		1. CHECK IF THE LEXEME IS A KEYWORD.
		IF YES, IT MUST RETURN A TOKEN WITH THE CORRESPONDING ATTRIBUTE
		FOR THE KEYWORD.THE ATTRIBUTE CODE FOR THE KEYWORD
		IS ITS INDEX IN THE KEYWORD LOOKUP TABLE(kw_table in table.h).
		IF THE LEXEME IS NOT A KEYWORD, GO TO STEP 2. */
	}
	else
	{
		/* memset so we can initialize the vidlex array to \0 */
		memset(t.attribute.vid_lex, '\0', VID_LEN + 1);

		if (!t.attribute.vid_lex) {
			scerrnum = 2;
			aa_func12("RUN TIME ERROR!");
		}


		if (strlen(lexeme) >= VID_LEN)
		{
			t.code = AVID_T;
			strncpy(t.attribute.vid_lex, lexeme, VID_LEN);

		}
		else {
			t.code = AVID_T;
			strncpy(t.attribute.vid_lex, lexeme, strlen(lexeme) + 1);
		}
		/* 2. SET a AVID TOKEN.
		IF THE lexeme IS LONGER than VID_LEN(see token.h) CHARACTERS,
		ONLY FIRST VID_LEN CHARACTERS ARE STORED
		INTO THE VARIABLE ATTRIBUTE ARRAY vid_lex[](see token.h) .
		ADD \0 AT THE END TO MAKE A C - type STRING. */
	}

	return t;

}



/*ACCEPTING FUNCTION FOR THE string variable identifier(VID - SVID)*/


/**********************************************************************************************************
* Purpose:			ACCEPTING FUNCTION FOR THE string variable identifier(VID - SVID)
* Author:			Sharusshan Sinnadurai
* History/Versions:	1.0
* Called Function:  strncpy(), strlen(), memset(),
* Parameters:		char array lexeme
* Return Value:		SVID Token
* Algorithm:		checks to see if lexeme is larger than VIDLEN or not and produces a Token accordingly
terminating with a dollar signe symbol
**********************************************************************************************************/
Token aa_func03(char lexeme[]) {

	/*	WHEN CALLED THE FUNCTION MUST
	1. SET a SVID TOKEN.
	IF THE lexeme IS LONGER than VID_LEN characters,
	ONLY FIRST VID_LEN - 1 CHARACTERS ARE STORED
	INTO THE VARIABLE ATTRIBUTE ARRAY vid_lex[],
	AND THEN THE $ CHARACTER IS APPENDED TO THE NAME.
	ADD \0 AT THE END TO MAKE A C - type STRING. */
	Token t;

	t.code = SVID_T;

	memset(t.attribute.vid_lex, '\0', VID_LEN + 1);

	if (!t.attribute.vid_lex) {
		scerrnum = 3;
		aa_func12("RUN TIME ERROR!");
	}

	if (strlen(lexeme) > VID_LEN) {

		strncpy(t.attribute.vid_lex, lexeme, VID_LEN - 1);
		t.attribute.vid_lex[VID_LEN - 1] = '$';
	}
	else
	{
		strncpy(t.attribute.vid_lex, lexeme, strlen(lexeme));
		t.attribute.vid_lex[strlen(lexeme) + 1] = '$';
	}


	return t;
}

/*		ACCEPTING FUNCTION FOR THE floating - point literal (FPL) */


/**********************************************************************************************************
* Purpose:			ACCEPTING FUNCTION FOR THE floating - point literal (FPL)
* Author:			Sharusshan Sinnadurai
* History/Versions:	1.0
* Called Function:  strncpy(), strlen(), memset(),
* Parameters:		char array lexeme
* Return Value:		SVID Token
* Algorithm:		convert the string to a double and check to see the size. If larger than limit return an
error token if not load a floating point token
**********************************************************************************************************/
Token aa_func08(char lexeme[]) {

	/*	THE FUNCTION MUST CONVERT THE LEXEME TO A FLOATING POINT VALUE,
	WHICH IS THE ATTRIBUTE FOR THE TOKEN.
	THE VALUE MUST BE IN THE SAME RANGE AS the value of 4 - byte float in C.
	IN CASE OF ERROR(OUT OF RANGE) THE FUNCTION MUST RETURN ERROR TOKEN
	THE ERROR TOKEN ATTRIBUTE IS  lexeme.IF THE ERROR lexeme IS LONGER
	than ERR_LEN characters, ONLY THE FIRST ERR_LEN - 3 characters ARE
	STORED IN err_lex.THEN THREE DOTS ... ARE ADDED TO THE END OF THE
	err_lex C - type string.
	BEFORE RETURNING THE FUNCTION MUST SET THE APROPRIATE TOKEN CODE */
	Token t;
	memset(t.attribute.err_lex, '\0', (ERR_LEN + 1));

	if (!t.attribute.vid_lex) {
		scerrnum = 8;
		aa_func12("RUN TIME ERROR!");
	}

	double fp = strtod(lexeme, NULL);

	if (fp == 0 || fp > FLT_MIN && fp < FLT_MAX) {
		t.code = FPL_T;
		t.attribute.flt_value = (float)fp;
	}
	else {
		t.code = ERR_T;
		if (strlen(lexeme) > ERR_LEN)
		{
			strncpy(t.attribute.err_lex, lexeme, ERR_LEN - 3);
			for (int i = ERR_LEN - 3; i < ERR_LEN; ++i)
				t.attribute.err_lex[i] = '.';
		}
	}
	return t;
}


/**********************************************************************************************************
* Purpose:			ACCEPTING FUNCTION FOR THE integer literal(IL)-decimal constant(DIL)
* Author:			Sharusshan Sinnadurai
* History/Versions:	1.0
* Called Function:  strncpy(), strlen(), memset(),atol(),
* Parameters:		char array lexeme
* Return Value:		SVID Token
* Algorithm:		convert the string to a double and check to see the size. If larger than limit return an
error token if not load a floating point token
**********************************************************************************************************/
Token aa_func05(char lexeme[]) {

	/*	THE FUNCTION MUST CONVERT THE LEXEME REPRESENTING A DECIMAL CONSTANT
	TO A DECIMAL INTEGER VALUE, WHICH IS THE ATTRIBUTE FOR THE TOKEN.
	THE VALUE MUST BE IN THE SAME RANGE AS the value of 2 - byte integer in C.
	IN CASE OF ERROR(OUT OF RANGE) THE FUNCTION MUST RETURN ERROR TOKEN
	THE ERROR TOKEN ATTRIBUTE IS  lexeme.IF THE ERROR lexeme IS LONGER
	than ERR_LEN characters, ONLY THE FIRST ERR_LEN - 3 characters ARE
	STORED IN err_lex.THEN THREE DOTS ... ARE ADDED TO THE END OF THE
	err_lex C - type string.
	BEFORE RETURNING THE FUNCTION MUST SET THE APROPRIATE TOKEN CODE */
	Token t;

	long iL = atol(lexeme);
	memset(t.attribute.vid_lex, '\0', ERR_LEN + 1);

	if (!t.attribute.vid_lex) {
		scerrnum = 5;
		aa_func12("RUN TIME ERROR!");
	}

	if (iL >= 0 && iL <= 32767) {
		t.code = INL_T;
		t.attribute.int_value = iL;
	}
	else {
		t.code = ERR_T;
		if (strlen(lexeme) > ERR_LEN)
		{
			strncpy(t.attribute.err_lex, lexeme, ERR_LEN - 3);
			for (int i = 3; i > 0; i--)
				t.attribute.err_lex[ERR_LEN - i] = '.';
		}
		else
			strcpy(t.attribute.err_lex, lexeme);
	}
	return t;
}



/**********************************************************************************************************
* Purpose:			ACCEPTING FUNCTION FOR THE string literal(SL)
* Author:			Sharusshan Sinnadurai
* History/Versions:	1.0
* Called Function:  strncpy(), strlen(), b_addc(),
* Parameters:		char array lexeme
* Return Value:		STR_T token
* Algorithm:		keep reading the chars until a closed quotation is read and also increment any new lines
**********************************************************************************************************/
Token aa_func10(char lexeme[]) {

	/*	THE FUNCTION MUST STORE THE lexeme PARAMETER CONTENT INTO THE STRING LITERAL TABLE(str_LTBL)
	FIRST THE ATTRIBUTE FOR THE TOKEN MUST BE SET.
	THE ATTRIBUTE OF THE STRING TOKEN IS THE OFFSET FROM
	THE BEGINNING OF THE str_LTBL char buffer TO THE LOCATION
	WHERE THE FIRST CHAR OF THE lexeme CONTENT WILL BE ADDED TO THE BUFFER.
	USING b_addc(..)COPY THE lexeme content INTO str_LTBL.
	THE OPENING AND CLOSING " MUST BE IGNORED DURING THE COPING PROCESS.
	ADD '\0' AT THE END MAKE THE STRING C - type string
	IF THE STING lexeme CONTAINS line terminators THE line COUNTER MUST BE INCTREMENTED.
	SET THE STRING TOKEN CODE. */
	Token t;

	t.code = STR_T;

	t.attribute.str_offset = b_limit(str_LTBL);
	for (unsigned int i = 0; i < strlen(lexeme); i++)
	{
		if (lexeme[i] != '"')
			b_addc(str_LTBL, lexeme[i]);
		if (lexeme[i] == '\n')
			++line;
	}

	b_addc(str_LTBL, '\0');

	return t;
}

/**********************************************************************************************************
* Purpose:			ACCEPTING FUNCTION FOR THE ERROR TOKEN
* Author:			Sharusshan Sinnadurai
* History/Versions:	1.0
* Called Function:  strncpy(), strlen(), b_addc(), memset(),
* Parameters:		char array lexeme
* Return Value:		ERR_T Token
* Algorithm:		display the where the error happened and if the string is too long cut it short and display
also return a token
**********************************************************************************************************/

Token aa_func12(char lexeme[]) {

	/*	THE FUNCTION SETS THE ERROR TOKEN.lexeme[] CONTAINS THE ERROR
	THE ATTRIBUTE OF THE ERROR TOKEN IS THE lexeme CONTENT ITSELF
	AND IT MUST BE STORED in err_lex.IF THE ERROR lexeme IS LONGER
	than ERR_LEN characters, ONLY THE FIRST ERR_LEN - 3 characters ARE
	STORED IN err_lex.THEN THREE DOTS ... ARE ADDED TO THE END OF THE
	err_lex C - type string.
	IF THE ERROR lexeme CONTAINS line terminators THE line COUNTER MUST BE INCTREMENTED.
	BEFORE RETURNING THE FUNCTION MUST SET THE APROPRIATE TOKEN CODE*/
	Token t;

	t.code = ERR_T;
	int i;
	if (strlen(lexeme) > ERR_LEN)
	{	/* We use memset to make the entire errlex array with \0 */
		memset(t.attribute.err_lex, '\0', (ERR_LEN + 1));

		if (!t.attribute.vid_lex) {
			scerrnum = 12;
			aa_func12("RUN TIME ERROR!");
		}

		strncpy(t.attribute.err_lex, lexeme, (ERR_LEN - 3));

		for (i = ERR_LEN - 3; i < ERR_LEN; ++i)
			t.attribute.err_lex[i] = '.';

	}
	else
		strcpy(t.attribute.vid_lex, lexeme);

	/* used unsigned here because the compiler always gave warnings if not used */
	for (unsigned int k = 0; k < strlen(lexeme); k++)
	{
		if (lexeme[k] == '\n')
			++line;
	}

	return t;
}



/**********************************************************************************************************
* Purpose:			Checking for any keyword matches in the kw_table
* Author:			Sharusshan Sinnadurai
* History/Versions:	1.0
* Called Function:  strcmp()
* Parameters:		char array lexeme
* Return Value:		index value
* Algorithm:		iterate through the table and check for matches
**********************************************************************************************************/

int iskeyword(char * kw_lexeme)
{
	if (kw_lexeme == NULL)
		return RT_FAIL_1;
	for (int i = 0; i < KWT_SIZE; i++)
		if (strcmp(kw_table[i], kw_lexeme) == 0)
			return i;
	return RT_FAIL_1;
}