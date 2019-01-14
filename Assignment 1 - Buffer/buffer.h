/* --------------------------------------------------------------------------------------
* File name				: buffer.h
* Compiler				: MS Visual Studio 2015 / 2017
* Author                : John Dobie, 040 659 609
* Course				: CST 8152 - Compilers, Lab Section: 12
* Assignment Number:	: 1
* Date: 				: 09-28-2018
* Professor				: Sv. Ranev
* Purpose				: To allocate memory for a certain capacity of data that may be resizable depending on the mode used.
*						| this class handles different utilities for the buffer capability such as if it is full, debugging to print out charaacters
*						| loading in from a file, resizing the buffer, and resetting or clearing it's memory when needed.
*
* Functions: -----------------------------------------------------------------------------
* 			b_allocate() , b_addc() , b_clear() , b_free() , b_isfull() , b_limit() , b_capacity() , b_mark()
*			b_mode() , b_incfactor() , b_load() , b_isempty() , b_getc() , b_eob() , b_print() , b_compact()
*			b_rflag() , b_retract() , b_reset()	, b_getcoffset() , b_rewind() , b_location() , B_FULL (b_isfull) macro function
*/
#ifndef BUFFER_H_
#define BUFFER_H_

/*#pragma warning(1:4001) *//*to enforce C89 type comments  - to make //comments an warning */
#pragma warning(1:4001)
/*#pragma warning(error:4001)*//* to enforce C89 comments - to make // comments an error */

/* standard header files */
#include <stdio.h>  /* standard input/output */
#include <malloc.h> /* for dynamic memory allocation*/
#include <limits.h> /* implementation-defined data type ranges and limits */

/* constant definitions */
#define RT_FAIL_1 -1         /* fail return value */
#define RT_FAIL_2 -2         /* fail return value */
#define LOAD_FAIL -2       /* load fail return value */

#define B_FULL
#define BUFFERMODE_FIXED 0
#define BUFFERMODE_ADDITIVE 1
#define BUFFERMODE_MULTIPLICATIVE -1
#define ONE 1
#define ZERO 0
#define NEGATIVE_ONE -1
#define ONEHUNDRED 100
#define HEXADECIMAL_TWO_FIFTY_SIX 0x100
#define NULL_TERMINATOR '\0'
#define BUFFERMODE_FIXED_CHAR 'f'
#define BUFFERMODE_ADDITIVE_CHAR 'a'
#define BUFFERMODE_MULTIPLICATIVE_CHAR 'm'
#define CHARACTER "%c"
#define NEWLINE "\n"

/* Enter your bit-masks constant definitions here */
#define DEFAULT_FALGS  0xFFFC /*default flags value*/
#define SET_EOB        0x0001 /*set eob mask*/
#define RESET_EOB      0xFFFE /*reset eob mask*/
#define CHECK_EOB      0x0001 /*check eob mask*/
#define SET_R_FLAG     0x0002 /*set r_flag mask*/
#define RESET_R_FLAG   0xFFFD /*reset r_flag mask*/
#define CHECK_R_FLAG   0x0002 /*check r_flag mask*/

/* user data type declarations */
typedef struct BufferDescriptor {
    char *cb_head;   /* pointer to the beginning of character array (character buffer) */
    short capacity;    /* current dynamic memory size (in bytes) allocated to character buffer */
    short addc_offset;  /* the offset (in chars) to the add-character location */
    short getc_offset;  /* the offset (in chars) to the get-character location */
    short markc_offset; /* the offset (in chars) to the mark location */
    char  inc_factor; /* character array increment factor */
    char  mode;       /* operational mode indicator*/
    unsigned short flags; /* contains character array reallocation flag and end-of-buffer flag */
} Buffer, *pBuffer;


/* function declarations */
/*
Place your function declarations here.
Do not include the function header comments here.
Place them in the buffer.c file
*/
Buffer * b_allocate(short init_capacity, char inc_factor, char o_mode);
pBuffer b_addc(pBuffer const pBD, char symbol);
int b_clear(Buffer * const pBD);
void b_free(Buffer * const pBD);
#ifndef B_FULL
int b_isfull(Buffer * const pBD);
#endif
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

/* constant macro definitions */
#ifdef B_FULL		/* b_isfull macro alternative definition, this will be used if B_FULL was defined.*/
#define STRUCT		
#define b_isfull(STRUCT) ((!STRUCT) ? RT_FAIL_1 : ((((STRUCT)->addc_offset /* * sizeof(char)*/) < (STRUCT)->capacity) ? 1 : 0))
#endif
#endif
