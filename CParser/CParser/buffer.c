/*************************************************************************************
File name :		Buffer.c
Compiler :		MS Visual Studio 15
Author :		Thipichanth Sellathamby
Course :		CST 8152 – Compilers, Lab Section : 10
Assignment :	1 - Buffer
Date :			October 25, 2018
Professor :		Sv.Ranev
Function List: 	Buffer * b_allocate(); pBuffer b_addc();int b_clear();void b_free();int b_isfull();
short b_limit(); short b_capacity(); short b_mark(); int b_mode(); size_t b_incfactor();
int b_load(); int b_isempty(); char b_getc(); int b_eob(); Buffer * b_compact(); char b_rflag();
short b_retract(); short b_reset(); short b_getcoffset(); int b_rewind(); char * b_location();
*************************************************************************************/


/* Include statements */
#include <stdio.h>			/* standard input/output */
#include <stdlib.h>			/* standard library */
#include "buffer.h"			/* imports the buffer.h file components */




/*************************************************************************************
Purpose:		This functions creates the initial buffer and allocates memory based on the
selected option
Author:			Thipichanth Sellathamby
Versions:		v9.2
Called f()'s:	None

Parameters:		initialize capacity is of type short and determines how big the buffer will be.
increment factor is of type char and determines how much to increment the buffer
by for instance when it gets full.
o_mode is of type char and determines which mode is the program running in

Return value:	Returns a pointer to the Buffer Structure

Algorithm:		Allocate memory for the structure based on the mode option it will give it
specified attributes

*************************************************************************************/

Buffer * b_allocate(short init_capacity, char inc_factor, char o_mode)
{
	Buffer *pBD = (Buffer*)calloc(1, sizeof(Buffer));				/* Allocating memory for buffer */

	if (!pBD)
	{																/* If Allocation fails free memory and return NULL*/
		return NULL;
	}

	if (init_capacity < 0 || init_capacity > MAX_ALLOWED)
	{
		free(pBD);
		return NULL;
	}

	pBD->cb_head = (char*)malloc(init_capacity);				/* buffer head is pointing to character buffer array which is the size of init_capacity */

	if (pBD->cb_head == NULL)
	{
		free(pBD);
		return NULL;
	}



	if (o_mode == 'f')												/* setting mode/increment factor if the option selected is f and inc factor is equal to 0 */
	{
		pBD->mode = FIXED_MODE;
		pBD->inc_factor = 0;
	}

	else if (o_mode == 'a') 										/* setting mode/increment factor if the option selected is a and inc factor is less than to 0x100 */
	{
		pBD->mode = ADD_MODE;
		pBD->inc_factor = (unsigned)inc_factor;
	}

	else if (o_mode == 'm' && inc_factor > CONST_ZERO && inc_factor < CONST_INC_MULTI)			/* setting mode/increment factor if the option selected is m and inc factor is less then 100 */
	{
		pBD->mode = MULTI_MODE;
		pBD->inc_factor = inc_factor;
	}
	else
	{
		b_free(pBD);
		return NULL;
	}
	pBD->capacity = init_capacity;									/*  setting the capacity based on which option took place before in the function*/
	pBD->flags = DEFAULT_FALGS;

	return pBD;

}




/*************************************************************************************
Purpose:		Decides where the character should be added in the buffer
Author:			Thipichanth Sellathamby
Versions:		v16.4
Called f()'s:	b_isfull()

Parameters:		pointer to the buffer struct & a character which is being entered in the buffer

Return value:	Returns a pointer to the Buffer Structure

Algorithm:		check for next available spot to add the character if none availble try and
increase the size and add if anything isn't possible at any point return NULL.
Also set the FLAGS based on which operation took place.

*************************************************************************************/

pBuffer b_addc(pBuffer const pBD, char symbol)
{
	short availSpace = 0, newInc = 0, newCap = 0;			/* variables created for calculation later to be done */

	if (!pBD)
		return NULL;

	pBD->flags &= RESET_R_FLAG;

	if (pBD->addc_offset >= MAX_ALLOWED)					/* If it is already at the maximum size return */
		return NULL;

	if ((short)((sizeof(char))*(pBD->addc_offset)) != pBD->capacity)		/* If the buffer is not full, add the character to the buffer */
	{
		pBD->cb_head[pBD->addc_offset++] = symbol;
		return pBD;
	}

	if (pBD->mode == FIXED_MODE)							/* If fixed mode, we cannot increase the buffer so return */
	{
		return NULL;
	}

	else if (pBD->mode == ADD_MODE)										/* If additive mode do the following */
	{
		newCap = pBD->capacity + (unsigned char)pBD->inc_factor;		/* assign the (current cap + increment factor) to newCap */

		if (newCap < 0)													/* if newCap somehow ends up below 0 return NULL */
			return NULL;

		if (newCap > 0 && newCap > MAX_ALLOWED)							/* if newCap is greater than zero and also greater than Maximum allowed then */
			newCap = MAX_ALLOWED;										/* assign the maximum allowed value to newCap*/

																		/* Once the calculations have been done assign it back to the curr capacity */
	}

	else if (pBD->mode == MULTI_MODE)									/* If it is at Multiplicative mode do the following */
	{

		if (pBD->capacity >= (MAX_ALLOWED))								/* If greater than or equal to Maximum allowed return */
			return NULL;

		availSpace = (MAX_ALLOWED)-pBD->capacity;						/* Perform the following calculations */
		newInc = (short)((availSpace * pBD->inc_factor) / 100.0f);
		newCap = pBD->capacity + newInc;

		if (newInc == 0 && (unsigned short)newCap < MAX_ALLOWED)		/* After previous calculations, if there is no change to the newCap and it is still less than Max allowed */
			newCap = MAX_ALLOWED;										/* then assign Max allowed to newCap */



	}
	else
		return NULL;

	if (pBD->mode == ADD_MODE || pBD->mode == MULTI_MODE)				/* If the mode is Additive or Multiplicative do the following */
	{
		char * temp = realloc(pBD->cb_head, newCap);				/* Reallocate memory with the new capacity */

		if (!temp)														/* If the allocation fails return NULL */
			return NULL;


		if (pBD->cb_head != temp)										/* Because of realloc, if the temp is not pointing to the same as pBD head */
		{																/* set flag on and make pBD head point to the new location that temp holds */
			pBD->flags |= SET_R_FLAG;
			pBD->cb_head = temp;
		}

		pBD->capacity = newCap;											/* finally newCap is assigned to the curr/buffer capacity */
		pBD->cb_head[pBD->addc_offset++] = symbol;						/* else just go ahead and add the symbol to the next empty spot and increment addc */

	}

	return pBD;															/* return pointer to Buffer */

}








/*************************************************************************************
Purpose:		Clear the buffer and reset required attributes
Author:			Thipichanth Sellathamby
Versions:		v1.4
Called f()'s:	None
Parameters:		pBD which is of type pointer to the Buffer struct

Return value:	Returns an int which indicates whether the op failed or succeeded

Algorithm:		Check if Buffer is valid if not return Error
Else reset capacity / addc offset / getc offset / markc offset

*************************************************************************************/

int b_clear(Buffer * const pBD)
{
	if (!pBD)
		return RT_FAIL_1;

	/* Reset all necessary values to zero and reset flags to default */
	pBD->addc_offset = 0;
	pBD->getc_offset = 0;
	pBD->markc_offset = 0;
	pBD->flags &= DEFAULT_FALGS;
	return IS_SUCCESS;
}






/*************************************************************************************
Purpose:		Free's the memory space
Author:			Thipichanth Sellathamby
Versions:		v1.0
Called f()'s:	None
Parameters:		Pointer to the Buffer Structure
Return value:	Void / Return nothing
Algorithm:		Check if pointer to buffer is valid if not return
ELSE free the contents of pBD then pBD itself

*************************************************************************************/

void b_free(Buffer * const pBD)
{
	if (!pBD)
		return;

	free(pBD->cb_head);		/* Free the pointer to the head then the pointer to buffer */
	free(pBD);

}




/*************************************************************************************
Purpose:		Verifies if the buffer is full
Author:			Thipichanth Sellathamby
Versions:		v1.0
Called f()'s:	None
Parameters:		Constant pointer to Buffer structure
Algorithm:		Check if the pointer to struct is valid if not return ERROR
ELSE check if the capacity and addc offset are equal which indicates if it is
full. If yes, then return 1 else just return 0;

*************************************************************************************/

int b_isfull(Buffer * const pBD)
{
	if (!pBD)
		return RT_FAIL_1;
	/* addc_offset is measured in elements like an array */
	if (pBD->capacity == (short)((sizeof(char))*pBD->addc_offset))			/* if the capacity and addc_offset are equal, buffer is full */
		return NO_SUCCESS;

	return IS_SUCCESS;

}




/*************************************************************************************
Purpose:		This gets the last character in buffer
Author:			Thipichanth Sellathamby
Versions:		v1.2
Called f()'s:	None
Parameters:		Pointer to the Buffer Struct
Return value:	Returns the (short) value of getc_offset
Algorithm:		Check to see if pointer to Buffer is valid if not Error
ELSE return the getc_offset value

*************************************************************************************/

short b_limit(Buffer * const pBD)
{
	if (!pBD)
		return RT_FAIL_1;

	return pBD->addc_offset;
}




/*************************************************************************************
Purpose:		This function lets us know the current capacity
Author:			Thipichanth Sellathamby
Versions:		v1.4
Called f()'s:	None
Parameters:		Pointer to the Buffer Structure
Return value:	Returns (short) capacity within struct
Algorithm:		Simply check to see if pointer to Buffer is valid if not return ERROR
ELSE return the current capacity of buffer

*************************************************************************************/

short b_capacity(Buffer * const pBD)
{
	if (!pBD)
		return RT_FAIL_1;

	return pBD->capacity;
}




/*************************************************************************************
Purpose:		Marks a given location in the buffer
Author:			Thipichanth Sellathamby
Versions:		v1.6
Called f()'s:	None
Parameters:		A pointer to the buffer structure and a mark of type short to indicate what
spot in the buffer
Return value:	Returns the mark that was made if an error occured just returns an ERROR
Algorithm:		If the mark is between 0 and addc_offset then mark is assigned to the
buffer mark.

*************************************************************************************/

short b_mark(pBuffer const pBD, short mark)
{
	if (!pBD)
		return RT_FAIL_1;

	if (mark >= 0 && mark <= pBD->addc_offset)					/* If mark is a valid number set the pBD mark */
		return pBD->markc_offset = mark;

	return RT_FAIL_1;
}



/*************************************************************************************
Purpose:		Indicate the mode
Author:			Thipichanth Sellathamby
Versions:		v1.1
Called f()'s:	None
Parameters:		Pointer to the Buffer Structure
Return value:	Returns the value of mode of type int if any ERROR occurs it returns ERROR
Algorithm:		Return mode if no ERRORS

*************************************************************************************/

int b_mode(Buffer * const pBD)
{
	if (!pBD)
		return RT_FAIL_2;

	return pBD->mode;
}



/*************************************************************************************
Purpose:		How much to increase the buffer by
Author:			Thipichanth Sellathamby
Versions:		v2.5
Called f()'s:	None
Parameters:		pointer to the Buffer Structure
Return value:	returns a unsigned int increment value if not it returns 0x100
Algorithm:		Verify if the pointer to Buffer is valid, if not it returns the increment factor
if any Error occurs it returns 0x100

*************************************************************************************/

size_t b_incfactor(Buffer * const pBD)
{
	if (!pBD)
		return 0x100;

	return (unsigned int)(unsigned char)pBD->inc_factor;

}




/*************************************************************************************
Purpose:		File reading operator
Author:			Thipichanth Sellathamby
Versions:		v10.4
Called f()'s:	None
Parameters:		fi which is of file type and also a pointer to the buffer structure
Return value:	returns an int which is count of chars in this function
Algorithm:		Read the file given in and count the amount of chars and exit once the
last character has been reached.

*************************************************************************************/

int b_load(FILE * const fi, Buffer * const pBD)
{
	char currChar;								/* variable to load current character */
	int count = 0;								/* variable to keep count of characters */

	if (!pBD || !fi)
		return RT_FAIL_1;

	currChar = (char)fgetc(fi);					/* load up currChar with the first letter from loaded file */

	while (!feof(fi))							/* While it is not the end of file do the following */
	{

		if (!b_addc(pBD, currChar))				/* if addc cannot add the char to buffer due to being full do the following */
		{
			//currChar = (char)fgetc(fi);			/* unget the char that made the buffer full and return it back to the file */
			ungetc(currChar, fi);					/* unload from stream and print that unloaded character */
			printf("The last character read from the file is: %c %d\n", currChar, currChar);

			return LOAD_FAIL;					/* return int indicating load has failed */
		}

		currChar = (char)fgetc(fi);				/* load characters to currChar */
		++count;								/* Keep count of chars */
	}

	return count;								/* return char count */
}



/*************************************************************************************
Purpose:		Checks to see if the buffer is empty
Author:			Thipichanth Sellathamby
Versions:		v1.2
Called f()'s:	None
Parameters:		pointer to the buffer structure
Return value:	return an int indicating if the operation was successfull or not
Algorithm:		if the addc_offset is equal to 0 then it is full

*************************************************************************************/

int b_isempty(Buffer * const pBD)
{
	if (!pBD)
		return RT_FAIL_1;

	if (pBD->addc_offset == 0)				/* If addc is 0 it means the buffer is empty */
		return NO_SUCCESS;

	return IS_SUCCESS;
}



/*************************************************************************************
Purpose:		This is used to read through the buffer character by character
Author:			Thipichanth Sellathamby
Versions:		v8.4
Called f()'s:	None
Parameters:		Pointer to buffer structure
Return value:	Returns a character
Algorithm:		if getc offset and addc offset are equal then set the FLAG and return
ELSE return the character at position and increment getc offset

*************************************************************************************/

char b_getc(Buffer * const pBD)
{
	char charAtPos;											/* Variable to indicate char at position */

	if (!pBD)
		return RT_FAIL_2;

	pBD->flags &= RESET_EOB;								/* Reset the EOB flag */

	if (pBD->getc_offset == pBD->addc_offset)				/* If getc equals addc this means the buffer is full */
	{

		pBD->flags |= SET_EOB;								/* setting EOB flag with binary OR and return*/
		return IS_SUCCESS;
	}

	pBD->flags &= RESET_EOB;								/* reseting EOB flag with binary AND */
	return charAtPos = pBD->cb_head[pBD->getc_offset++];	/* Return the character at the position of the getc_offset and then increment getc */

}




/*************************************************************************************
Purpose:		Checks the end of bit value
Author:			Thipichanth Sellathamby
Versions:		v1.2
Called f()'s:	None
Parameters:		A pointer the buffer Strucutre
Return value:	Returns an int indicating whether it has reached the end of buffer
Algorithm:		None

*************************************************************************************/

int b_eob(Buffer * const pBD)
{
	if (!pBD)
		return RT_FAIL_1;

	return (char)pBD->flags & CHECK_EOB;			/* Check EOB flag */

}




/*************************************************************************************
Purpose:		Prints out everything character by character until the end of bit is flagged
Author:			Thipichanth Sellathamby
Versions:		v8.8
Called f()'s:	b_isempty()
Parameters:		Pointer to the Buffer Structure
Return value:	Returns in int of char count
Algorithm:		If it is empty display message, if not run through the structure and print
out each character until end of bit is flagged. Also keep a count of all the
characters that are read.

*************************************************************************************/

int b_print(Buffer * const pBD)
{
	int charCount = 0;								/* variable to keep count of chars */
	char c;											/* variable for character */

	if (!pBD)
		return RT_FAIL_1;

	if (b_isempty(pBD))								/* if the buffer is empty then print and return char count */
	{
		printf("Empty buffer!\n");
		return charCount;
	}

	c = b_getc(pBD);								/* let var c hold the first character of pBD */

	while (!b_eob(pBD))								/* while the EOB is not set keep printing/iterating through the characters */
	{
		printf("%c", c);
		++charCount;
		c = b_getc(pBD);
	}

	printf("\n");									/* print a new line and return count of characters */
	return charCount;
}



/*************************************************************************************
Purpose:		To use the buffer efficiently, will either shrink or expand the buffer
based on usage.
Author:			Thipichanth Sellathamby
Versions:		v7.2
Called f()'s:	None
Parameters:		Pointer to the Buffer Structure and also a character
Return value:	Returns a pointer to the buffer

Algorithm:		For all operational modes of the buffer the function shrinks (or in some cases may expand) the
buffer to a new capacity. The new capacity is the current limit plus a space for one more character.
In other words the new capacity is addc_offset + 1 converted to bytes. The function uses realloc()
to adjust the new capacity, and then updates all the necessary members of the buffer descriptor
structure. Before returning a pointer to Buffer, the function adds the symbol to the end of the
character buffer (do not use b_addc(), use addc_offset to add the symbol) and increments
CST8152 – Compilers, MMXVIII Page 7 of 9 addc_offset. The function must return NULL if for some
reason it cannot to perform the required operation. It must set the r_flag bit appropriately.

*************************************************************************************/

Buffer * b_compact(Buffer * const pBD, char symbol)
{
	char *temp;											/* character pointer which will later be assigned to the buffer */
	short newCap;										/* var newCap to do the calculations */

	if (!pBD)
		return NULL;

	pBD->flags &= RESET_R_FLAG;							/* Reset the R_FLAG with binary AND */

	if (pBD->addc_offset >= MAX_ALLOWED)				/* if the buffer size has already reached the max size then we cannot increase any further */
		return NULL;

	newCap = (short)((pBD->addc_offset + 1) * sizeof(char));		/* newCap is now assigned the total vaule of addc + 1 char */

	temp = (char*)realloc(pBD->cb_head, newCap);						/* reallocation with the new capacity */

	if (!temp)
		return NULL;

	if (temp != pBD->cb_head)							/* if temp and pBD head are not pointing to the same place */
	{													/* make pBD head point to temp's new location and SET the R_FLAG */
		pBD->cb_head = temp;
		pBD->flags |= SET_R_FLAG;
	}

	pBD->cb_head = temp;								/* make the pBD head point to what temp is pointing to */
	pBD->cb_head[pBD->addc_offset++] = symbol;			/* load the symbol in the next empty spot and increment addc_offset */
	pBD->capacity = newCap;

	return pBD;											/* return a pointer to the buffer */
}



/*************************************************************************************
Purpose:		Indicates the R_FLAG BIT value
Author:			Thipichanth Sellathamby
Versions:		v1.2
Called f()'s:	None
Parameters:		pointer to the Buffer Structure
Return value:	Returns a char value of the current R_FLAG
Algorithm:		None

*************************************************************************************/

char b_rflag(Buffer * const pBD)
{
	if (!pBD)
		return RT_FAIL_1;

	return (char)pBD->flags & CHECK_R_FLAG;

}




/*************************************************************************************
Purpose:		Reduces the Buffer size by one
Author:			Thipichanth Sellathamby
Versions:		v1.2
Called f()'s:	None
Parameters:		Pointer to the buffer structure
Return value:	Returns a short value of the getc_offset
Algorithm:		None

*************************************************************************************/

short b_retract(Buffer * const pBD)
{
	if (!pBD)
		return RT_FAIL_1;

	if (pBD->getc_offset > 0)							/* if getc_offset is greater than 0 then reduce getc_offset by 1 */
		return --pBD->getc_offset;

	return RT_FAIL_1;
}




/*************************************************************************************
Purpose:		Resets the getc_offset value
Author:			Thipichanth Sellathamby
Versions:		v1.2
Called f()'s:	None
Parameters:		Pointer to the buffer structure
Return value:	Returns the short value of getc_offset
Algorithm:		None

*************************************************************************************/

short b_reset(Buffer * const pBD)
{
	if (!pBD)
		return RT_FAIL_1;

	return pBD->getc_offset = pBD->markc_offset;			/* set the value of getc to the value of markc */
}




/*************************************************************************************
Purpose:		Gets the current value of getc_offset
Author:			Thipichanth Sellathamby
Versions:		v1.2
Called f()'s:	None
Parameters:		Pointer to the Buffer Structure
Return value:	Returns the current value of getc_offset
Algorithm:		None

*************************************************************************************/

short b_getcoffset(Buffer * const pBD)
{
	if (!pBD)
		return RT_FAIL_1;

	return pBD->getc_offset;
}




/*************************************************************************************
Purpose:		Goes back in the array so it can reread the buffer again
Author:			Thipichanth Sellathamby
Versions:		v1.2
Called f()'s:	None
Parameters:		Pointer to the Buffer Structure
Return value:	Returns an int which indicates if the procedure was successful or not
Algorithm:		None

*************************************************************************************/

int b_rewind(Buffer * const pBD)
{
	if (!pBD)
		return RT_FAIL_1;

	pBD->getc_offset = 0;
	pBD->markc_offset = 0;

	return IS_SUCCESS;
}



/*************************************************************************************
Purpose:		Find a location within the character buffer
Author:			Thipichanth Sellathamby
Versions:		v1.2
Called f()'s:	None
Parameters:		Pointer to the buffer structure and location of type short
Return value:	Returns a char pointer to a location in the buffer structure.

Algorithm:		The function returns a pointer to a location of the character buffer indicated by loc_offset.
loc_offset is the distance (measured in chars) from the beginning of the character array
(cb_head). If a run-time error is possible, it should return NULL.

*************************************************************************************/

char * b_location(Buffer * const pBD, short loc_offset)
{
	if (!pBD)
		return NULL;

	if (loc_offset > pBD->addc_offset || loc_offset < 0)	/* if location is invalid return NULL */
		return NULL;

	return &pBD->cb_head[loc_offset];		/* return the address of loc offset measured from the head */

}

