#pragma once
/*************************************************************************************
File name :		table.c
Compiler :		MS Visual Studio 15
Author :		Ababiya Abajobir / Thipichanth Sellathamby
Course :		CST 8152 – Compilers, Lab Section : 10
Assignment :	2 - Scanner
Date :			November 22, 2018
Professor :		Sv.Ranev
Functions :		None
*************************************************************************************/

#ifndef  TABLE_H_
#define  TABLE_H_ 

#ifndef BUFFER_H_
#include "buffer.h"
#endif

#ifndef NULL
#include <_null.h> /* NULL pointer constant is defined there */
#endif

/*   Source end-of-file (SEOF) sentinel symbol
*    '\0' or one of 255,0xFF,EOF
*/

#define SEOF_1	(255)
#define SEOF_2	('\0')



/*  Special case tokens processed separately one by one
*  in the token-driven part of the scanner
*  '=' , ' ' , '(' , ')' , '{' , '}' , == , <> , '>' , '<' , ';',
*  white space
*  !!comment , ',' , ';' , '-' , '+' , '*' , '/', # ,
*  .AND., .OR. , SEOF, 'illegal symbol',
*/


/*REPLACE *ESN* and *ESR* WITH YOUR ERROR STATE NUMBER*/
#define ES  11 /* Error state  with no retract */
#define ER  12 /* Error state  with retract */
#define IS -1    /* Inavalid state */

/* State transition table definition */

/* REPLACE *CN* WITH YOUR COLUMN NUMBER */

#define TABLE_COLUMNS 8
/*transition table - type of states defined in separate table */
int  st_table[][TABLE_COLUMNS] = {
	/* State 0 */{ 1 , 6 , 4 , ES , ES , ES , 9, ER },
	/* State 1 */{ 1 , 1 , 1 , 2 , 3 , 2 , 2, 2 },					/* ES/AS2 */
	/* State 2 */{ IS, IS , IS , IS , IS , IS , IS, IS },
	/* State 3 */{ IS, IS , IS , IS , IS , IS , IS, IS },
	/* State 4 */{ ES, 4 , 4 , 7 , ES , 5 , 5, 5 },					/* ES/AS5 */
	/* State 5 */{ IS, IS , IS , IS , IS , IS , IS, IS },
	/* State 6 */{ ES, 6 , ES , 7 , ES , 5 , 5, 8 },				/* ES/AS5 */
	/* State 7 */{ 8, 7 , 7 , 8 , ES , 8 , 8, IS },				/* ES/AS8 */
	/* State 8 */{ IS, IS , IS , IS , IS , IS , IS, IS },
	/* State 9 */{ 9, 9 , 9 , 9 , 9 , 9 , 10, ES },
	/* State 10 */{ IS, IS , IS , IS , IS , IS , IS },
	/* State 11 */{ IS, IS , IS , IS , IS , IS , IS },
	/* State 12 */{ IS, IS , IS , IS , IS , IS , IS }

};

/* Accepting state table definition */
/* REPLACE *N1*, *N2*, and *N3* WITH YOUR NUMBERS */
#define ASWR     14  /* accepting state with retract */
#define ASNR     15  /* accepting state with no retract */
#define NOAS     16  /* not accepting state */
/* Each state is defined in the table as an ASWR / NOAS / ASNR */
int as_table[] = {

	/* State 0 */{ NOAS },
	/* State 1 */{ NOAS },
	/* State 2 */{ ASWR },
	/* State 3 */{ ASNR },
	/* State 4 */{ NOAS },
	/* State 5 */{ ASWR },
	/* State 6 */{ NOAS },
	/* State 7 */{ NOAS },
	/* State 8 */{ ASWR },
	/* State 9 */{ NOAS },
	/* State 10 */{ ASNR },
	/* State 11 */{ ASNR },
	/* State 12 */{ ASWR }

};



/* Accepting action function declarations */

/*FOR EACH OF YOUR ACCEPTING STATES YOU MUST PROVIDE
ONE FUNCTION PROTOTYPE.THEY ALL RETURN Token AND TAKE
ONE ARGUMENT : A string REPRESENTING A TOKEN LEXEME.*/



Token aa_func02(char *lexeme); /* VID AVID/KW */
Token aa_func03(char *lexeme); /* VID SVID */
Token aa_func05(char *lexeme); /* DIL */
Token aa_func08(char *lexeme); /* FPL */
Token aa_func10(char *lexeme); /* IS */
Token aa_func12(char *lexeme); /* ER */

							   /* defining a new type: pointer to function (of one char * argument)
							   returning Token
							   */

typedef Token(*PTR_AAF)(char *lexeme);


/* Accepting function (action) callback table (array) definition */
/* If you do not want to use the typedef, the equvalent declaration is:
* Token (*aa_table[])(char lexeme[]) = {
*/

PTR_AAF aa_table[] = {
	NULL,
	NULL,
	aa_func02,
	aa_func03,
	NULL,
	aa_func05,
	NULL,
	NULL,
	aa_func08,
	NULL,
	aa_func10,
	aa_func12,
	aa_func12,

	/*
	HERE YOU MUST PROVIDE AN INITIALIZATION FOR AN ARRAY OF POINTERS
	TO ACCEPTING FUNCTIONS.THE ARRAY HAS THE SAME SIZE AS as_table[].
	YOU MUST INITIALIZE THE ARRAY ELEMENTS WITH THE CORRESPONDING
	ACCEPTING FUNCTIONS(FOR THE STATES MARKED AS ACCEPTING IN as_table[]).
	THE REST OF THE ELEMENTS MUST BE SET TO NULL.
	*/
};

/* Keyword lookup table (.AND. and .OR. are not keywords) */

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
