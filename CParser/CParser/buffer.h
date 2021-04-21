#pragma once
/***************************************************************************
File name:		buffer.h
Compiler:		MS Visual Studio 2015
Author:			Sharusshan Sinnadurai
Course:			CST 8152 – Compilers, Lab Section: 10
Assignment:		1 - BUFFER
Date:			October 25, 2018
Professor:		Sv. Ranev

Purpose:		This is the header file which holds all my definitions and prototype functions.

Function list:	Buffer * b_allocate(); pBuffer b_addc();int b_clear();void b_free();int b_isfull();
short b_limit(); short b_capacity(); short b_mark(); int b_mode(); size_t b_incfactor();
int b_load(); int b_isempty(); char b_getc(); int b_eob(); Buffer * b_compact(); char b_rflag();
short b_retract(); short b_reset(); short b_getcoffset(); int b_rewind(); char * b_location();

*****************************************************************************/

#ifndef BUFFER_H_
#define BUFFER_H_

/*#pragma warning(1:4001) *//*to enforce C89 type comments  - to make //comments an warning */

							/*#pragma warning(error:4001)*//* to enforce C89 comments - to make // comments an error */

														   /* standard header files */
#include <stdio.h>							/* standard input/output */
#include <malloc.h>							/* for dynamic memory allocation*/
#include <limits.h>							/* implementation-defined data type ranges and limits */

														   /* Constant Definitions */
#define RT_FAIL_1		   -1				/* fail return value */
#define RT_FAIL_2		   -2				/* fail return value */
#define LOAD_FAIL		   -2				/* load fail return value */
#define NO_SUCCESS			1				/* No success */
#define IS_SUCCESS			0				/* Is successful */

														   /* Modes */
#define	ADD_MODE			1				/* Additive Mode */
#define MULTI_MODE		   -1				/* Multiplicative Mode */						   
#define FIXED_MODE			0				/* Fixed Mode */

														   /* Bit Masks */
#define DEFAULT_FALGS		0xFFFC			/* default flags value */
#define SET_EOB				0x0001			/* set eob mask */
#define RESET_EOB			0xFFFE			/* reset eob mask */
#define CHECK_EOB			0x0001			/* check eob mask */
#define SET_R_FLAG			0x0002			/* set r_flag mask - 14 - */
#define RESET_R_FLAG		0xFFFD			/* reset r_flag mask - 13 - */
#define CHECK_R_FLAG		0x0002			/* check r_flag mask - 14 - */

#define MAX_ALLOWED			(SHRT_MAX-1)
#define CONST_INC_ADD		256
#define CONST_INC_MULTI		101
#define CONST_ZERO			0

														   /* Buffer Structure */
typedef struct BufferDescriptor {
	char *cb_head;							/* pointer to the beginning of character array (character buffer) */
	short capacity;							/* current dynamic memory size (in bytes) allocated to character buffer */
	short addc_offset;						/* the offset (in chars) to the add-character location */
	short getc_offset;						/* the offset (in chars) to the get-character location */
	short markc_offset;						/* the offset (in chars) to the mark location */
	char  inc_factor;						/* character array increment factor */
	char  mode;								/* operational mode indicator*/
	unsigned short flags;					/* contains character array reallocation flag and end-of-buffer flag */
} Buffer, *pBuffer;


/* Function Declarations */
Buffer * b_allocate(short init_capacity, char inc_factor, char o_mode);
pBuffer b_addc(pBuffer const pBD, char symbol);
int b_clear(Buffer * const pBD);
void b_free(Buffer * const pBD);
int b_isfull(Buffer * const pBD);
short b_limit(Buffer * const pBD);
short b_capacity(Buffer * const pBD);
short b_mark(pBuffer const pBD, short mark);
int b_mode(Buffer * const pBD);
size_t b_incfactor(Buffer * const pBD);
int b_load(FILE * const fi, Buffer * const pBD);
int b_isempty(Buffer * const pBD);
char b_getc(Buffer * const pBD);
int b_eob(Buffer * const pBD);
int b_print(Buffer * const pBD);
Buffer * b_compact(Buffer * const pBD, char symbol);
char b_rflag(Buffer * const pBD);
short b_retract(Buffer * const pBD);
short b_reset(Buffer * const pBD);
short b_getcoffset(Buffer * const pBD);
int b_rewind(Buffer * const pBD);
char * b_location(Buffer * const pBD, short loc_offset);

#endif

#pragma once
