/* --------------------------------------------------------------------------------------
* File name				: buffer.c
* Compiler				: MS Visual Studio 2015 / 2017
* Author                : John Dobie, 040 659 609
* Course				: CST 8152 - Compilers, Lab Section: 12
* Assignment Number:	: 1
* Date: 				: 10-03-2018
* Professor				: Sv. Ranev
* Purpose				: To allocate memory for a certain capacity of data that may be resizable depending on the mode used.
*						| this class handles different utilities for the buffer capability such as if it is full, debugging to print out charaacters
*						| loading in from a file, resizing the buffer, and resetting or clearing it's memory when needed.
*
* Functions: ---------------------------------------------------------------------------
* 			b_allocate() , b_addc() , b_clear() , b_free() , b_isfull() , b_limit() , b_capacity() , b_mark()			 
*			b_mode() , b_incfactor() , b_load() , b_isempty() , b_getc() , b_eob() , b_print() , b_compact()			 
*			b_rflag() , b_retract() , b_reset()	, b_getcoffset() , b_rewind() , b_location()		 
*/

#include "buffer.h"

/* Function Header : b_allocate() -----------------------------------------------------
* Purpose			: Allocates memory and sets default values for a BufferDescriptor object and returns a pointer to it. 
* Author			: John Dobie, 040 659 609
* Version			: 1.0
* Called functions	: calloc(), malloc(), free().
* Parameters		: short init_capacity	: the starting size of our buffer.	[ 0 to SHRT_MAX bytes]
*					| char inc_factor		: the size to increment the buffer when capacity is reached. [0 to UCHAR_MAX bytes] 
*					| char o_mode			: designates the operating mode of the buffer. ['f' 'a' 'm' in bytes]
* Return value		: returns a pointer to a BufferDescriptor object, or NULL if the buffer could not be created.
* Algorithm			: 1. allocate memory for the buffer descriptor and buffer.
*					| 2. set defaults depending on which mode is selected or return NULL if an error occurs.
--------------------------------------------------------------------------------------*/
Buffer * b_allocate(short init_capacity, char inc_factor, char o_mode)
{
	pBuffer buffer = (Buffer*)calloc(ONE, sizeof(Buffer));

	if (!buffer)
	{
		return NULL;
	}

	/*We allow our character buffer to be allocated as long as the initial capacity is within the range
	* of [0 to Short Max Minus 1] (our maximum allowed buffer capacity) */
	if (init_capacity >= ZERO && init_capacity <= (SHRT_MAX - ONE))
	{
		buffer->cb_head = malloc(sizeof(char) * init_capacity);
		if (!(buffer->cb_head))
		{
			return NULL;
		}
	}
	else
	{
		free(buffer);
		return NULL;
	}

	/*FIXED mode, we set our increment factor to 0 as this mode will never resize the buffer.
	* ADDITIVE mode, we allow our increment factor to be anywhere in the range from [1 to 255] bytes.
	* MULTIPLICATIVE mode, we allow our increment factor to start anywhere from [1 to 100] bytes.*/
	if ((o_mode == BUFFERMODE_FIXED_CHAR || (unsigned char)inc_factor == ZERO) || (o_mode == BUFFERMODE_FIXED_CHAR && (unsigned char)inc_factor != ZERO))
	{
		if (init_capacity == ZERO)	/*cannot [will not] create the buffer if initial size is 0 and we will not be able to increment*/
		{
			if (!(buffer->cb_head))
			free(buffer->cb_head);
			free(buffer);
			return NULL;
		}

		buffer->mode = buffer->inc_factor = ZERO;
	}
	else if ((o_mode == BUFFERMODE_ADDITIVE_CHAR) && ((unsigned char)inc_factor > ZERO && (unsigned char)inc_factor <= UCHAR_MAX))
	{
		buffer->mode = ONE;
		buffer->inc_factor = inc_factor;
	}
	else if ((o_mode == BUFFERMODE_MULTIPLICATIVE_CHAR) && (inc_factor > ZERO && inc_factor <= ONEHUNDRED))
	{
		buffer->mode = NEGATIVE_ONE;
		buffer->inc_factor = inc_factor;
	}
	else
	{
		free(buffer->cb_head);
		free(buffer);
		return NULL;
	}

	buffer->capacity = init_capacity;
	buffer->flags = DEFAULT_FALGS;	/*default flags value 0xFFFC*/
	return buffer;
}

/*  Function Header : b_addc() ---------------------------------------------------------
* Purpose			: To add a symbol (character) into the buffer, and resize the capacity with realloc() if neccessary and if the mode allows us to do so. 
*					| Returns NULL on catching an error.
* Author			: John Dobie, 040 659 609
* Version			: 1.0
* Called functions	: realloc().
* Parameters		: pBuffer const pBD		: a pointer to our BufferDescriptor object.
*					| char symbol			: the current character that we want to add into our buffer.	[ 0 to UCHAR_MAX]
* Return value		: returns a pointer to a BufferDescriptor object
*					| or returns NULL if:	pBD is not initialized.
*					|						buffer is at max capacity, but the buffer mode is set to FIXED.
*					|						buffer is in ADDITIVE mode, but the capacity is already out of range.
*					|						buffer is in ADDITIVE mode, and the newCapacity value has overflowed.
*					|						buffer is in MULTIPLICATIVE mode, and the buffer is already at maximum  
*					|						| capacity before calculating a new increment.
*					|						buffer mode is an illegal mode.
*					|						realloc() method failed to allocate memory.
*					|						there is no space to put the symbol into the buffer after reallocation 
*					|						| more memory.
* Algorithm			: 1. reset the reallocation flag R_FLAG.
*					| 2. if buffer is not at max capacity, insert the symbol and do not change the R_FLAG.
*					| 3. if buffer is full, resize it depending on the buffer operation mode.
*					| 4. realloc if needed, and modify the R_FLAG before inserting the symbol now that we have space.
*					| 5. return the buffer pointer.
-------------------------------------------------------------------------------------- */
pBuffer b_addc(pBuffer const pBD, char symbol)
{
	short new_inc_factor = 0, newCapacity = 0;	/*temporary variables to hold our modified increment factor (used in multiplicative buffer mode) 
												* and capacity (used in additive buffer mode) values.*/
	if (!pBD) return NULL;

	pBD->flags &= RESET_R_FLAG;
	
	/*We insert our symbol into the buffer as long as there is room, 
	* otherwise we check if we can resize the buffer according to the buffer mode.*/
	if(b_isfull(pBD))		/*commented for testing for B_FULL macro define, but it is faster if B_FULL is undefined to compare two variables in scope.*/
	//if((pBD->addc_offset * sizeof(char)) < pBD->capacity)	/*buffer is not yet full*/
	{
		pBD->cb_head[pBD->addc_offset++] = symbol;
		return pBD;
	}
	else									/*buffer is full*/
	{
		/*if buffer mode is non-resizeable, we can leave this function.
		* if the buffer is resizable, perform checks and resize the buffer if applicable. */
		if (pBD->mode == BUFFERMODE_FIXED) { return NULL; }						
		else if (pBD->mode == BUFFERMODE_ADDITIVE)	
		{
			if (pBD->capacity >= (SHRT_MAX - ONE))		/*if capacity is already out of bounds, return NULL an error occured.*/
				return NULL;
			
			newCapacity = pBD->capacity + (unsigned char)pBD->inc_factor;		/*temporarily hold capacity + increment to perform our checks to handle errors*/

			if (newCapacity < ZERO) return NULL;
			else if (newCapacity > (SHRT_MAX - ONE)) { newCapacity = SHRT_MAX - ONE; }	
			//else { pBD->capacity = newCapacity; }								/*if the capacity didn't overflow then we can set our value to our incremented capacity*/
			//	/*how is this destroyed buffer*/
		}
		else if (pBD->mode == BUFFERMODE_MULTIPLICATIVE)
		{
			if (pBD->capacity >= (SHRT_MAX - ONE)) { return NULL; }				/*already at maximum capacity, return null*/

			new_inc_factor														/*new increment formula, (available space x new increment) */
				= (SHRT_MAX - ONE - pBD->capacity) * (unsigned long)pBD->inc_factor / ONEHUNDRED;

			if (((pBD->capacity + new_inc_factor) < (SHRT_MAX - ONE)) && (new_inc_factor != ZERO))	/*if our new capacity wouldn't overflow, set our capacity's value added with the new increment*/
			{																				
				newCapacity = pBD->capacity + new_inc_factor;
			}
			else if (pBD->capacity < (SHRT_MAX - ONE))							/*if capacity with new increment together would cause an overflow, set capacity to maximum capacity value**/
			{																				
				newCapacity = SHRT_MAX - ONE;
			}
		}
		else 
			return NULL;

		char* temp = (char*)realloc(pBD->cb_head, newCapacity * sizeof(char));				/*once capacity value is adjusted, resize our buffer*/
		/*if realloc failed return NULL error*/
		if (temp == NULL)
			return NULL;
		
		/*if realloc was successful set the buffer to it's new memory location if it moved.*/
		if (temp != (pBD->cb_head))
		{
			pBD->flags |= SET_R_FLAG;
			pBD->cb_head = temp;
		}

		pBD->capacity = newCapacity;
			
			/*if no overflow is about to happen, we can add the symbol into the buffer*/
		if (pBD->addc_offset < pBD->capacity) 
			{ pBD->cb_head[pBD->addc_offset++] = symbol; }
		else 
			{ return NULL; }
		
		return pBD;
	}
}

/* Function Header : b_clear() --------------------------------------------------------
* Purpose			: reset some of the buffer's values to defaults so that it can be reused.
* Author			: John Dobie, 040 659 609
* Version			: 1.0
* Called functions	: N/A.
* Parameters		: Buffer* const pBD		: a pointer to our BufferDescriptor object.
* Return value		: returns an int to show if the clear operation has succeeded or failed.
* Algorithm			: 1. return fail if there is a problem.
*					| 2. apply certain default values of the buffer.
-------------------------------------------------------------------------------------- */
int b_clear(Buffer * const pBD)
{
	if (!pBD) return RT_FAIL_1;

	pBD->flags &= DEFAULT_FALGS;
	pBD->addc_offset = ZERO;
	pBD->getc_offset = ZERO;
	pBD->markc_offset = ZERO;

	return ZERO;
}

/* Function Header : b_free() ---------------------------------------------------------
* Purpose			: de-allocate and free the memory held by our BufferDescriptor object.
* Author			: John Dobie, 040 659 609
* Version			: 1.0
* Called functions	: free()
* Parameters		: Buffer* const pBD		: a pointer to our BufferDescriptor object.
* Return value		: no return from the void type
* Algorithm			: 1. if the buffer descriptor isn't already NULL we free it's memory.
--------------------------------------------------------------------------------------- */
void b_free(Buffer * const pBD)
{
	if(pBD)
	if (pBD->cb_head)
		free(pBD->cb_head);

	free(pBD);
}

/* Function Header : b_isfull() --------------------------------------------------------
* Purpose			: return an int to show if the buffer capacity is at maximum or not, return RT_FAIL_1 (-1) if the buffer descriptor is not initialized.
* Author			: John Dobie, 040 659 609
* Version			: 1.0
* Called functions	: N/A
* Parameters		: Buffer* const pBD		: a pointer to our BufferDescriptor object.
* Return value		: an integer, 0 if the capacity is not full, 1 if it is, RT_FAIL_1 (-1) on error.
* Algorithm			: N/A.
--------------------------------------------------------------------------------------- */
#ifndef B_FULL
int b_isfull(Buffer * const pBD)
{
	return (!pBD) ? RT_FAIL_1 : (((pBD->addc_offset * sizeof(char)) < pBD->capacity) ? ONE : ZERO);
}
#endif

/* Function Header : b_limit() --------------------------------------------------------
* Purpose			: To return the current limit or amount of characters in the buffer.
* Author			: John Dobie, 040 659 609
* Version			: 1.0
* Called functions	: N/A
* Parameters		: Buffer* const pBD		: a pointer to our BufferDescriptor object.
* Return value		: a short, the value of addc_offset for the BufferDescriptor, RT_FAIL_1 (-1) on error.
* Algorithm			: N/A.
--------------------------------------------------------------------------------------- */
short b_limit(Buffer * const pBD)
{
	return (!pBD) ? RT_FAIL_1 : pBD->addc_offset;
}

/* Function Header : b_capacity() -----------------------------------------------------
* Purpose			: To return the current capacity of the buffer.
* Author			: John Dobie, 040 659 609
* Version			: 1.0
* Called functions	: N/A
* Parameters		: Buffer* const pBD		: a pointer to our BufferDescriptor object.
* Return value		: a short, the value of capacity for the BufferDescriptor, RT_FAIL_1 (-1) on error if the Buffer is not initialized.
* Algorithm			: N/A.
--------------------------------------------------------------------------------------- */
short b_capacity(Buffer * const pBD)
{
	return (!pBD) ? RT_FAIL_1 : pBD->capacity;
}

/* Function Header : b_mark() ---------------------------------------------------------
* Purpose			: To return the distance in characters from the start of the character array in the buffer. 
*					| Setting the value if it is within an applicable boundary of [0-addc_offset]
* Author			: John Dobie, 040 659 609
* Version			: 1.0
* Called functions	: N/A
* Parameters		: Buffer* const pBD		: a pointer to our BufferDescriptor object.
*					| short mark			: a value passed in to be set as a new markc_offset value if it fits inside the buffer's boundary. [ 0 to SHRT_MAX]
* Return value		: a short, the value of markc_offset for the BufferDescriptor, RT_FAIL_1 (-1) on error.
* Algorithm			: N/A.
--------------------------------------------------------------------------------------- */
short b_mark(pBuffer const pBD, short mark)
{
	return (!pBD) ? RT_FAIL_1 : (mark >= ZERO && mark <= pBD->addc_offset) ? pBD->markc_offset = mark : RT_FAIL_1;
}

/* Function Header : b_mode() ---------------------------------------------------------
* Purpose			: To return the current operational mode of the buffer.
* Author			: John Dobie, 040 659 609
* Version			: 1.0
* Called functions	: N/A
* Parameters		: Buffer* const pBD		: a pointer to our BufferDescriptor object.
* Return value		: an int, the value of mode for the BufferDescriptor, RT_FAIL_1 (-1) on error.
* Algorithm			: N/A.
--------------------------------------------------------------------------------------- */
int b_mode(Buffer * const pBD)
{
	return (!pBD) ? RT_FAIL_2 : pBD->mode;
}

/* Function Header : b_incfactor() ----------------------------------------------------
* Purpose			: To return the current increment factor that our buffer is using.
* Author			: John Dobie, 040 659 609
* Version			: 1.0
* Called functions	: N/A
* Parameters		: Buffer* const pBD		: a pointer to our BufferDescriptor object.
* Return value		: a long int (size_t) variable, the value of inc_factor for the BufferDescriptor, 0x100 int(256) on error.
* Algorithm			: N/A.
--------------------------------------------------------------------------------------- */
size_t b_incfactor(Buffer * const pBD)
{
	return (!pBD) ? HEXADECIMAL_TWO_FIFTY_SIX : (size_t)(unsigned char)pBD->inc_factor;
}

/* Function Header : b_load() ---------------------------------------------------------
* Purpose			: 
* Author			: John Dobie, 040 659 609
* Version			: 1.0
* Called functions	: fgetc(), feof(), b_addc(), ungetc(), printf()
* Parameters		: FILE* const fi		: a pointer to the file we want to load into our buffer.
*					| Buffer* const pBD		: a pointer to our BufferDescriptor object.
* Return value		: an int for our new(updated) addc_offset of our buffer after a new character has been appended. 
					| RT_FAIL_1 (-1) on an initialization runtime error, or LOAD_FAIL (-2) if the load operation fails.
* Algorithm			: 1. check for null exception.
* 					| 2. loop as long as we haven't hit feof() yet. if we have, we break out of the loop.
*					| 3. try to add a character from the file into our buffer, returning it if we cannot fit it into the buffer.
*					| 4. return the new/current amount of characters in the buffer.
---------------------------------------------------------------------------------------- */
int b_load(FILE * const fi, Buffer * const pBD)
{
	char c = NULL_TERMINATOR;	/*temporary character to hold the value we get from loading our file*/

	if (!fi || !pBD)
	{ return RT_FAIL_1; }
		
	for(;;)
	{
		c = (char)fgetc(fi);	/*read check R flag and read again. check if read flag is set or not.*/
		if (feof(fi))			/*if we meet the end of the file we can leave as we don't need to add any more characters to our buffer.*/
			break;
		else if (b_addc(pBD, c) == NULL)	/*otherwise try and add the character into the buffer, if the method fails,*/
		{									/*we put the character back and don't store it in the buffer.*/
				ungetc(c, fi);
				printf("The last character read from the file is: %c %d\n", c, (int)c);
				return LOAD_FAIL;
		}
	}
	return pBD->addc_offset;
}

/* Function Header : b_isempty() ------------------------------------------------------
* Purpose			: To let us know if our buffer is currently empty.
* Author			: John Dobie, 040 659 609
* Version			: 1.0
* Called functions	: N/A
* Parameters		: Buffer* const pBD		: a pointer to our BufferDescriptor object.
* Return value		: an int variable, the value of addc_offset for the BufferDescriptor, or RT_FAIL_1 (-1) on error.
* Algorithm			: N/A.
--------------------------------------------------------------------------------------- */
int b_isempty(Buffer * const pBD)
{
	return (!pBD) ? RT_FAIL_1 : ((pBD->addc_offset == ZERO) ? ONE : ZERO);
}

/* Function Header : b_getc() ---------------------------------------------------------
* Purpose			: To read and return the current character at our index from the buffer as well as setting appropriate EOB and Memory Reallocation 
*					| flags along the way.
* Author			: John Dobie, 040 659 609
* Version			: 1.0
* Called functions	: N/A
* Parameters		: Buffer* const pBD		: a pointer to our BufferDescriptor object.
* Return value		: the current character at our mark in the buffer, or RT_FAIL_2 (-2) on error.
* Algorithm			: 1. check if anything is NULL that shouldn't be.
*					| 2. determine if we are at the end of the buffer and if we are flip the flag on and exit the method.
*					| 3. if we aren't at the end, make sure to reset the end of buffer flag, just incase.
*					| 4. return our current character and increment our get character offset.
--------------------------------------------------------------------------------------- */
char b_getc(Buffer * const pBD)
{
	if (!pBD || !(pBD->cb_head)) return RT_FAIL_2;	/*check for validity? if the character array or buffer descriptor are null*/

	if (pBD->getc_offset == pBD->addc_offset)		/*set the end of buffer flag or reset it depending if the buffer is full.*/
	{
		pBD->flags |= SET_EOB;
		return ZERO;
	}
	
	if (pBD->flags & CHECK_EOB)
	{
		pBD->flags &= RESET_EOB;
	}

	return pBD->cb_head[pBD->getc_offset++];
}

/* Function Header : b_eob() -----------------------------------------------------------
* Purpose			: To let us know if we have reached the end of the buffer.
* Author			: John Dobie, 040 659 609
* Version			: 1.0
* Called functions	: N/A
* Parameters		: Buffer* const pBD		: a pointer to our BufferDescriptor object.
* Return value		: an int value, the EOB or End of Buffer flag from our bitmask, or (-1) on an uninitialized error.
* Algorithm			: N/A.
---------------------------------------------------------------------------------------- */
int b_eob(Buffer * const pBD)
{
	return (!pBD) ? RT_FAIL_1 : (pBD->flags & CHECK_EOB);
}

/* Function Header : b_print() ----------------------------------------------------------
* Purpose			: For debugging purposes, to print out the current contents of the buffer and return how many characters were printed.
* Author			: John Dobie, 040 659 609
* Version			: 1.0
* Called functions	: b_getc(), printf()
* Parameters		: Buffer* const pBD		: a pointer to our BufferDescriptor object.
* Return value		: an int value, number of characters printed, or RT_FAIL_1 (-1) on an uninitialized buffer error.
* Algorithm			: 1. check for a NULL buffer (precautionary)
*					| 2. see if the buffer is empty
*					| 3. if it isn't empty, loop through all characters in the buffer and print them.
*					| 4. return once we have hit the EOB (end of buffer) flag
----------------------------------------------------------------------------------------- */
int b_print(Buffer * const pBD)
{
	char c = NULL_TERMINATOR;	/*temporary character to hold what we get from our buffer and print out*/

	if (!pBD) return RT_FAIL_1;
	if (!(pBD->addc_offset))
	{
		printf("Empty buffer!"NEWLINE);
		return ZERO;
	}
	
	do	/*as long as we have characters in our buffer, get a character from our buffer and print it.*/
	{
		c = b_getc(pBD);

		if (b_eob(pBD) != ONE)		/*if(b_eob(pBD) != 1 )catch end of buffer before we print, this way we will not print the NUL character.*/
		{ printf(CHARACTER, c); }
		else { break; }
	} while (b_eob(pBD) != ONE);	/*check variable instead of calling function for performance inside of buffer.*/

	printf(NEWLINE);
	return pBD->getc_offset;
}

/* Function Header : b_compact() -------------------------------------------------------------
* Purpose			: Used to resize our buffer at runtime, cannot be used while the buffer is running in [fixed] mode.
* Author			: John Dobie, 040 659 609
* Version			: 1.0
* Called functions	: realloc()
* Parameters		: Buffer* const pBD		: a pointer to our BufferDescriptor object.
*					: char symbol			: a character passed in to be inserted into the buffer once resizing has made space [ 0 to UCHAR_MAX bytes]
* Return value		: a pointer to our buffer descriptor, or NULL on error
* Algorithm			: 1. check for a NULL buffer (precautionary), check for buffer capacity overflow, check the mode of the buffer
*					| 2. call realloc() for the buffer and resize it if possible
*					| 3. set appropriate values and put in the symbol passed in at the end of the buffer.
*					| 4. return our pointer to the buffer.
---------------------------------------------------------------------------------------------- */
Buffer * b_compact(Buffer * const pBD, char symbol)
{
	char* temp;
	if (pBD == NULL) { return NULL; }
	
	pBD->flags &= RESET_R_FLAG;
	temp = (char*)realloc(pBD->cb_head, (pBD->addc_offset + ONE)); 
	/*realloc failed*/
	if (temp == NULL)
	{ return NULL; }
	
	/*realloc was successful, resize our buffer and add in a character to our buffer*/
	if (temp != (pBD->cb_head))
	{
		pBD->cb_head = temp;
		pBD->flags |= SET_R_FLAG;
	}
	pBD->capacity = pBD->addc_offset + ONE;
	pBD->cb_head[pBD->addc_offset++] = symbol;
	
	return pBD;
}

/* Function Header : b_rflag() -----------------------------------------------------------------
* Purpose			: To return the current value of our Reallocation flag for us to see.
* Author			: John Dobie, 040 659 609
* Version			: 1.0
* Called functions	: N/A
* Parameters		: Buffer* const pBD		: a pointer to our BufferDescriptor object.
* Return value		: a char value, the R_FLAG or Reallocation flag from our bitmask, or RT_FAIL_1 (-1) on an uninitialized error.
* Algorithm			: N/A.
------------------------------------------------------------------------------------------------ */
char b_rflag(Buffer * const pBD)
{
	return (!pBD) ? RT_FAIL_1 : (pBD->flags & CHECK_R_FLAG);
}

/* Function Header : b_retract() ---------------------------------------------------------------
* Purpose			: To decrement and return our current getc_offset value.
* Author			: John Dobie, 040 659 609
* Version			: 1.0
* Called functions	: N/A
* Parameters		: Buffer* const pBD		: a pointer to our BufferDescriptor object.
* Return value		: a short value, the decremented value of getc_offset from our buffer or RT_FAIL_1 (-1) on a runtime error.
* Algorithm			: N/A.
------------------------------------------------------------------------------------------------ */
short b_retract(Buffer * const pBD)
{
	return (!pBD) ? RT_FAIL_1 : ((pBD->getc_offset > ZERO) ? --(pBD->getc_offset) : RT_FAIL_1);
}

/* Function Header : b_reset() -----------------------------------------------------------------
* Purpose			: To 'reset' our getc_offset value to the same as that of our markc_offset.
* Author			: John Dobie, 040 659 609
* Version			: 1.0
* Called functions	: N/A
* Parameters		: Buffer* const pBD		: a pointer to our BufferDescriptor object.
* Return value		: a short value, for our new getc_offset or our current markc_offset, RT_FAIL_1 (-1) on a runtime error.
* Algorithm			: N/A.
----------------------------------------------------------------------------------------------- */
short b_reset(Buffer * const pBD)
{
	return (pBD) ? pBD->getc_offset = pBD->markc_offset : RT_FAIL_1;
}

/* Function Header : b_getcoffset() -----------------------------------------------------------
* Purpose			: To return the getc_offset value from our buffer.
* Author			: John Dobie, 040 659 609
* Version			: 1.0
* Called functions	: N/A
* Parameters		: Buffer* const pBD		: a pointer to our BufferDescriptor object.
* Return value		: a short value, for our getc_offset from our buffer, RT_FAIL_1 (-1) on a runtime error.
* Algorithm			: N/A.
------------------------------------------------------------------------------------------------ */
short b_getcoffset(Buffer * const pBD)
{
	return (!pBD) ? RT_FAIL_1 : pBD->getc_offset;
}

/* Function Header : b_rewind() ----------------------------------------------------------------
* Purpose			: To 'rewind' and set our mark and getc_offset value from our buffer back to zero (0) so the buffer can be re-read from.
* Author			: John Dobie, 040 659 609
* Version			: 1.0
* Called functions	: N/A
* Parameters		: Buffer* const pBD		: a pointer to our BufferDescriptor object.
* Return value		: an int value of zero(0) or RT_FAIL_1 (-1) on an uninitialized runtime error.
* Algorithm			: N/A.
------------------------------------------------------------------------------------------------ */
int b_rewind(Buffer * const pBD)
{
	return (pBD) ? pBD->getc_offset = pBD->markc_offset = ZERO : RT_FAIL_1;
}
/*TODO location purpose*/
/* Function Header : b_location() --------------------------------------------------------------
* Purpose			: To return the location of a character in our array.
* Author			: John Dobie, 040 659 609
* Version			: 1.0
* Called functions	: N/A
* Parameters		: Buffer* const pBD		: a pointer to our BufferDescriptor object.
*					| short loc_offset		: a number, determining which character's location to return. (the distane from the beginning of our buffer's array) [ 0 to addc_offset in bytes]
* Return value		: a char pointer value, the location in memory of the selected(numbered) character in our array, NULL on a runtime error.
* Algorithm			: N/A.
------------------------------------------------------------------------------------------------ */
char * b_location(Buffer * const pBD, short loc_offset)
{
	return (!pBD) ? NULL : (loc_offset >= 0 && loc_offset < pBD->addc_offset) ? (pBD->cb_head + loc_offset) : NULL;
}  