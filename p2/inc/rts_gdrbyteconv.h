/**  Copyright (c) 1995, California Institute of Technology		**/
/**  U. S. Government sponsorship under NASA contract is acknowledged	**/

/***************************************************************/
/*                                                             */
/* This header file contains macros to convert between         */
/* character arrays containing GDR formatted data and integral */
/* data types.  GDR formatted data is defined in byte order,   */
/* with the  most significant bytes first in multi-byte fields */
/* and most significant bits first within bytes.  These macros */
/* have been defined to handle signed and unsigned data, 16    */
/* and 32 bit integers, and contiguous bit strings smaller     */
/* than integers.  They portably solve problems of pointer     */
/* alignment, byte order, and bit order.  This file uses       */
/* conditional compilation - the most efficient method of      */
/* of conversion is used for tested compilers such as Sun and  */
/* Masscomp, and portable C for all other machines.            */
/*                                                             */
/*                                                             */
/* Some examples of where these macros would be useful:        */
/*                                                             */
/* 1) Read SFDU's as a byte stream into a character array,     */
/*    then extract integer (16 & 32 bit) or sub-integer        */
/*    length fields by offset and size.  This might be         */
/*    done when the type of the SFDU is unknown.               */
/*                                                             */
/* 2) Manipulate any GDR formatted data by offset and size     */
/*    without regard to its structure or content (as in        */
/*    generic data dump routines.)                             */
/*                                                             */
/* 3) Conversion routines or macros involving SFOC defined     */
/*    times in GDR format.                                     */
/*                                                             */
/*                                                             */
/* Design Notes						       */
/* ------------						       */
/*							       */
/* The shift routines used in the machine independent code     */
/* portion are faster than memcpy() (for up to 7 bytes), as    */
/* measured on Sun 3 & 4.  memcpy() is slightly faster for     */
/* 8 bytes, and is used for copying type 'double'.  It is      */
/* also used for type 'float' because the casting rules for    */
/* floating point make the macro more obscure when integer     */
/* shifts are used, and for consistency with the type          */
/* 'double' conversion macro.                                  */
/*                                                             */
/***************************************************************/

#ifndef GDRBYTECONV_H
#define GDRBYTECONV_H


#ifndef U8 
#define U8 unsigned char
#endif
#ifndef I16
#define I16 short
#endif
#ifndef U16
#define U16 unsigned short
#endif
#ifndef I32
#define I32 long
#endif
#ifndef U32
#define U32 unsigned long
#endif


/***************************************************************/
/*                                                             */
/* Machine specific definitions                                */
/*                                                             */
/* Motorola 68000 based machines handle casts of char pointers */
/* to other pointer types without alignment errors, so the     */
/* following definitions can be used.                          */
/*                                                             */
/***************************************************************/

#if (mc68020 || mc68030 || mc) && !lint



/* Convert a two byte char array of GDR data to a signed 16    */
/* bit integer.  'cp' is of type 'char *'.                     */

#define C16_TO_I16(cp)       (*((I16 *) (cp)))


/* Convert a two byte char array of GDR data to an unsigned 16 */
/* bit integer.  'cp' is of type 'char *'.                     */

#define C16_TO_U16(cp)       (*((U16 *) (cp)))


/* Convert a three byte char array containing GDR format data  */
/* to a signed 32 bit integer.  'cp' is of type 'char *'.      */

#define C24_TO_I32(cp)    ( ((((I32) (cp)[0]) & 0xFF) << 16) \
                          | ((((I32) (cp)[1]) & 0xFF) <<  8) \
                          | ((((I32) (cp)[2]) & 0xFF)      ) )


/* Convert a three byte char array containing GDR format data  */
/* to an unsigned 32 bit integer.  'cp' is of type 'char *'.   */

#define C24_TO_U32(cp)    ( ((((U32) (cp)[0]) & 0xFF) << 16) \
                          | ((((U32) (cp)[1]) & 0xFF) <<  8) \
                          | ((((U32) (cp)[2]) & 0xFF)      ) )


/* Convert a four byte char array of GDR data to a signed      */
/* 32 bit integer.  'cp' is of type 'char *'.                  */

#define C32_TO_I32(cp)    (*((I32 *) (cp)))


/* Convert a four byte char array of GDR data to an unsigned   */
/* 32 bit integer.  'cp' is of type 'char *'.                  */

#define C32_TO_U32(cp)    (*((U32 *) (cp)))


/* Convert a six byte char array of GDR data to an unsigned    */
/* 64 bit integer (that is, an array of two 32 bit unsigned    */
/* integers, with the MSW lowest in memory).  'cp' is of type  */
/* 'char *'.  i is an array of two 32 bit unsigned ints.       */

#define C48_TO_U64(cp, i)						\
{									\
	i[0] =	(U32)(*((U16 *)(&((cp)[0]))));				\
	i[1] =	(*((U32 *)(&((cp)[2]))));				\
}


/* Copy a signed 16 bit integer into a two byte character      */
/* array, in GDR format.  'cp' is of type 'char *', 'i' is a   */
/* 16 bit integer.                                             */

#define I16_TO_C16(cp, i) ((void) (*((I16 *) (cp)) = (I16) (i)))


/* Copy an unsigned 16 bit integer into a two byte character   */
/* array, in GDR format.   'cp' is of type 'char *', 'u' is a  */
/* 16 bit integer.                                             */

#define U16_TO_C16(cp, u) ((void) (*((U16 *) (cp)) = (U16) (u)))


/* Copy a signed 32 bit integer into a three byte character    */
/* array, in GDR format.  'cp' is of type 'char *', 'i' is a   */
/* 32 bit unsigned integer.                                    */

#define I32_TO_C24(cp, i)  \
                       ((void) ((cp)[0] = (((i) >> 16) & 0xFF), \
                                (cp)[1] = (((i) >>  8) & 0xFF), \
                                (cp)[2] = ((i) & 0xFF)))
                               

/* Copy an unsigned 32 bit integer into a three byte character */
/* array, in GDR format.  'cp' is of type 'char *', 'u' is a   */
/* 32 bit unsigned integer.                                    */

#define U32_TO_C24(cp, u)  \
                       ((void) ((cp)[0] = (((u) >> 16) & 0xFF), \
                                (cp)[1] = (((u) >>  8) & 0xFF), \
                                (cp)[2] = ((u) & 0xFF)))


/* Copy a signed 32 bit integer into a two byte character      */
/* array, in GDR format.  'cp' is of type 'char *', 'i' is a   */
/* 32 bit integer.                                             */

#define I32_TO_C32(cp, i)  ((void) (*((I32 *) (cp)) = (I32) (i)))



/* Copy an unsigned 32 bit integer into a two byte character   */
/* array, in GDR format.  'cp' is of type 'char *', 'u' is a   */
/* 32 bit integer.                                             */

#define U32_TO_C32(cp, u)  ((void) (*((U32 *) (cp)) = (U32) (u)))


/* Convert a four byte char array containing GDR formatted     */
/* data to a 32 bit IEEE float.  'cp' is of type 'char *', 'f' */
/* is of type 'float'.  The value of the expression is the     */
/* float value placed in 'f'.                                  */

#define C32_TO_F32(cp, f)  ((void) ((f) = *((float *) (cp))))
            

/* Copy a 32 bit IEEE float value into a four byte character   */
/* array, in GDR format.  'cp' is of type 'char *', 'f' is a   */
/* 32 bit IEEE float.  The value of the expression is 'f'.     */

#define F32_TO_C32(cp, f)  \
                       ((void) (*((float *) (cp)) = (float) (f)))


/* Convert an eight byte char array containing GDR formatted   */
/* data to a 64 bit IEEE float.  'cp' is of type 'char *', 'd' */
/* is of type 'double'.  The value of the expression is the    */
/* double value placed in 'd'.                                 */

#define C64_TO_F64(cp, d)  ((void) ((d) = *((double *) (cp))))


/* Copy a 64 bit IEEE float value into an eight byte character */
/* array, in GDR format.  'cp' is of type 'char *', 'd' is a   */
/* 64 bit IEEE float.  The value of the expression is 'd'.     */

#define F64_TO_C64(cp, d)  \
                     ((void) (*((double *) (cp)) = (double) (d)))



/***************************************************************/
/*                                                             */
/* End of machine specific definitions                         */
/*                                                             */
/***************************************************************/


#else     


/***************************************************************/
/*                                                             */
/* Portable C definitions.  This section is guaranteed to work */
/* if the compiler conforms to the K&R definition of C, and if */
/* the machine supports the ANSI/System V function 'memcpy()'. */
/* Also, these macros assume that type 'int' is at least 32    */
/* bits.                                                       */
/*                                                             */
/***************************************************************/

/* Convert a two byte char array containing GDR format data    */
/* to a signed 16 bit integer.  'cp' is of type 'char *'.      */

#define C16_TO_I16(cp)    ((I16) ((((cp)[0] & 0xFF) << 8) \
                                | (((cp)[1] & 0xFF))))


/* Convert a two byte char array containing GDR format data    */
/* to an unsigned 16 bit integer.  'cp' is of type 'char *'.   */

#define C16_TO_U16(cp)    ((U16) ((((cp)[0] & 0xFF) << 8) \
                                | (((cp)[1] & 0xFF))))


/* Convert a three byte char array containing GDR format data  */
/* to a signed 32 bit integer.  'cp' is of type 'char *'.      */

#define C24_TO_I32(cp)    ((I32) ((((cp)[0] & 0xFF) << 16) \
                                | (((cp)[1] & 0xFF) <<  8) \
                                | (((cp)[2] & 0xFF)      )))


/* Convert a three byte char array containing GDR format data  */
/* to an unsigned 32 bit integer.  'cp' is of type 'char *'.   */

#define C24_TO_U32(cp)    ((U32) ((((cp)[0] & 0xFF) << 16) \
                                | (((cp)[1] & 0xFF) <<  8) \
                                | (((cp)[2] & 0xFF)      )))


/* Convert a four byte char array containing GDR format data   */
/* to a signed 32 bit integer.  'cp' is of type 'char *'.      */

#define C32_TO_I32(cp)    ((I32) (((cp)[0] << 24) \
                               | (((cp)[1] & 0xFF) << 16) \
                               | (((cp)[2] & 0xFF) <<  8) \
                               | (((cp)[3] & 0xFF)      )))


/* Convert a four byte char array containing GDR format data   */
/* to an unsigned 32 bit integer.  'cp' is of type 'char *'.   */

#define C32_TO_U32(cp)    ((U32) (((cp)[0] << 24) \
                               | (((cp)[1] & 0xFF) << 16) \
                               | (((cp)[2] & 0xFF) <<  8) \
                               | (((cp)[3] & 0xFF)      )))


/* Convert a six byte char array of GDR data to an unsigned    */
/* 64 bit integer (that is, an array of two 32 bit unsigned    */
/* integers, with the MSW lowest in memory).  'cp' is of type  */
/* 'char *'.  i is an array of two 32 bit unsigned ints.       */

#define C48_TO_U64(cp, i)						\
{									\
	i[0] =	(U32)(((((cp)[0]) & 0xFF) << 8 ) |			\
		(((cp)[1]) & 0xFF));					\
	i[1] =	(U32)(((((cp)[2]) & 0xFF) << 24 ) |			\
		((((cp)[3]) & 0xFF) << 16 ) |				\
		((((cp)[4]) & 0xFF) << 8 )  |				\
		(((cp)[5]) & 0xFF));					\
}

/* Copy a signed 16 bit integer into a two byte character      */
/* array, in GDR format.  'cp' is of type 'char *', 'i' is a   */
/* 16 bit integer.                                             */

#define I16_TO_C16(cp, i) \
                        ((void) ((cp)[0] = (((i) >> 8) & 0xFF), \
                                 (cp)[1] = ((i) & 0xFF)))


/* Copy an unsigned 16 bit integer into a two byte character   */
/* array, in GDR format.  'cp' is of type 'char *', 'u' is a   */
/* 16 bit integer.                                             */

#define U16_TO_C16(cp, u) \
                        ((void) ((cp)[0] = (((u) >> 8) & 0xFF), \
                                 (cp)[1] = ((u) & 0xFF)))


/* Copy a signed 32 bit integer into a three byte character    */
/* array, in GDR format.  'cp' is of type 'char *', 'i' is a   */
/* 32 bit unsigned integer.                                    */

#define I32_TO_C24(cp, i) \
                       ((void) ((cp)[0] = (((i) >> 16) & 0xFF), \
                                (cp)[1] = (((i) >>  8) & 0xFF), \
                                (cp)[2] = ((i) & 0xFF)))


/* Copy an unsigned 32 bit integer into a three byte character */
/* array, in GDR format.  'cp' is of type 'char *', 'u' is a   */
/* 32 bit unsigned integer.                                    */

#define U32_TO_C24(cp, u) \
                       ((void) ((cp)[0] = (((u) >> 16) & 0xFF), \
                                (cp)[1] = (((u) >>  8) & 0xFF), \
                                (cp)[2] = ((u) & 0xFF)))


/* Copy a signed 32 bit integer into a four byte character     */
/* array, in GDR format.  'cp' is of type 'char *', 'i' is     */
/* a 32 bit integer.                                           */

#define I32_TO_C32(cp, i) \
                      ((void) ((cp)[0] = (((i) >> 24) & 0xFF), \
                               (cp)[1] = (((i) >> 16) & 0xFF), \
                               (cp)[2] = (((i) >>  8) & 0xFF), \
                               (cp)[3] = ((i) & 0xFF)))


/* Copy an unsigned 32 bit integer into a four byte character  */
/* array, in GDR format.  'cp' is of type 'char *', 'u' is a   */
/* 32 bit unsigned integer.                                    */

#define U32_TO_C32(cp, u) \
                       ((void) ((cp)[0] = (((u) >> 24) & 0xFF), \
                                (cp)[1] = (((u) >> 16) & 0xFF), \
                                (cp)[2] = (((u) >>  8) & 0xFF), \
                                (cp)[3] = ((u) & 0xFF)))


#if !lint



/* Convert a four byte char array containing GDR formatted     */
/* data to a 32 bit IEEE float.  'cp' is of type 'char *', 'f' */
/* is of type 'float'.  The value of the expression is the     */
/* float value placed in 'f'.                                  */

#define C32_TO_F32(cp, f) \
            (*((float *) memcpy((char *) &(f), (cp), 4)))


/* Copy a 32 bit IEEE float value into a four byte character   */
/* array, in GDR format.  'cp' is of type 'char *', 'f' is a   */
/* 32 bit IEEE float.  The value of the expression is 'f'.     */

#define F32_TO_C32(cp, f) \
            (memcpy((cp), (char *) &(f), 4), (f))


/* Convert an eight byte char array containing GDR formatted   */
/* data to a 64 bit IEEE float.  'cp' is of type 'char *', 'd' */
/* is of type 'double'.  The value of the expression is the    */
/* double value placed in 'd'.                                 */

#define C64_TO_F64(cp, d) \
            (*((double *) memcpy((char *) &(d), (cp), 8)))


/* Copy a 64 bit IEEE float value into an eight byte character */
/* array, in GDR format.  'cp' is of type 'char *', 'd' is a   */
/* 64 bit IEEE float.  The value of the expression is the      */
/* double value in 'd'.                                        */

#define F64_TO_C64(cp, d) \
            (memcpy((cp), (char *) &(d), 8), (d))



#else  /* lint definitions */

#define C32_TO_F32(cp, f)  (void) 0.0
#define F32_TO_C32(cp, f)  (void) 0.0
#define C64_TO_F64(cp, d)  (void) 0.0
#define F64_TO_C64(cp, d)  (void) 0.0

#endif /* float macros */


/***************************************************************/
/*                                                             */
/* End of portable C definitions.                              */
/*                                                             */
/***************************************************************/


#endif


/***************************************************************/
/*                                                             */
/* Start of bit manipulation section.  None of these macros    */
/* are machine specific.                                       */
/*                                                             */
/***************************************************************/


/* Create an unsigned 32 bit integer mask, given the starting  */
/* bit and length of the mask.  'bitoffset' and 'bitlen' are   */
/* integers.  A method is used which does not depend on the    */
/* the size of the intermediate register to truncate left      */
/* shifts.                                                     */

#define U32MASK(bitoffset, bitlen) \
    ((U32) ((U32) (0xFFFFFFFF & (0xFFFFFFFF << (bitoffset))) \
    >> (32 - (bitlen))) \
    << (32 - ((bitoffset) + (bitlen))))


/* Create an unsigned 16 bit integer mask, given the starting  */
/* bit and length of the mask.  'bitoffset' and 'bitlen' are   */
/* integers.  A method is used which does not depend on the    */
/* the size of the intermediate register to truncate left      */
/* shifts.                                                     */

#define U16MASK(bitoffset, bitlen) \
    ((U16) ((U16) (0xFFFF & (0xFFFF << (bitoffset))) \
    >> (16 - (bitlen))) \
    << (16 - ((bitoffset) + (bitlen))))

/* Create an unsigned 8 bit integer mask, given the starting  */
/* bit and length of the mask.  'bitoffset' and 'bitlen' are   */
/* integers.  A method is used which does not depend on the    */
/* the size of the intermediate register to truncate left      */
/* shifts.                                                     */

#define U8MASK(bitoffset, bitlen) \
    ((U8) ((U8) (0xFF & (0xFF << (bitoffset))) \
    >> (8 - (bitlen))) \
    << (8 - ((bitoffset) + (bitlen))))


/* Extract a bit field from a GDR character array, returning   */
/* an unsigned 32 bit integer containing the bit field right   */
/* justified.  'cp' is of type 'char *', 'bitoffset' and       */
/* 'bitlen' are integers.                                      */

#define CBITS_TO_U32(cp, bitoffset, bitlen) \
    ((U32) ((U32)(0xFFFFFFFF & (C32_TO_U32(cp) << (bitoffset))) \
    >> (32 - (bitlen))))


/* Extract a bit field from a GDR character array, returning   */
/* a signed 32 bit integer containing the bit field right      */
/* justified.  'cp' is of type 'char *', 'bitoffset' and       */
/* 'bitlen' are integers.                                      */

#define CBITS_TO_I32(cp, bitoffset, bitlen) \
    ((I32) CBITS_TO_U32(cp, bitoffset, bitlen))


/* Extract a bit field from a GDR character array, returning   */
/* an unsigned 16 bit integer containing the bit field right   */
/* justified.  'cp' is of type 'char *', 'bitoffset' and       */
/* 'bitlen' are integers.                                      */

#define CBITS_TO_U16(cp, bitoffset, bitlen) \
    ((U16) ((U16)(0xFFFF & (C16_TO_U16(cp) << (bitoffset))) \
    >> (16 - (bitlen))))


/* Extract a bit field from a GDR character array, returning   */
/* a signed 16 bit integer containing the bit field right      */
/* justified.  'cp' is of type 'char *', 'bitoffset' and       */
/* 'bitlen' are integers.                                      */

#define CBITS_TO_I16(cp, bitoffset, bitlen) \
    ((I16) CBITS_TO_U16(cp, bitoffset, bitlen))


#define CBITS_TO_U8(cp, bitoffset, bitlen) \
    ((U8) ((U8)(0xFF & (*(cp) << (bitoffset))) \
    >> (8 - (bitlen))))

/* The following three macros reposition the value	*/
/* into its original position using the given		*/
/* bit offset and bit length.				*/

#define U8_RESTORE_OFFSET(u, bitoffset, bitlen) \
    ((U8) ((0xFF & (((U8)(u)) << (8 - bitlen))) \
    >> (bitoffset)))

#define U16_RESTORE_OFFSET(u, bitoffset, bitlen) \
    ((U16) ((0xFFFF & (((U16)(u)) << (16 - bitlen))) \
    >> (bitoffset)))

#define U32_RESTORE_OFFSET(u, bitoffset, bitlen) \
    ((U32) ((0xFFFFFFFF & (((U32)(u)) << (32 - bitlen))) \
    >> (bitoffset)))


/*
        The following is an explanation of the conversion of ATAC
        floating point numbers to IEEE double floating point numbers.

        The format for these numbers is:
  
         1                     23 bits                           8 bits
        -----------------------------------------------------------------
 ATAC   |s|                    mantissa                       |   exp   |
        -----------------------------------------------------------------
  
  
         1    11 bits                    52 bits                     
        -----------------------------------------------------------------
 IEEE   |s|     exp     |                mantissa                       |
        -----------------------------------------------------------------
  

        The ATAC mantissa is a 2's complement number normalized in the
        a range of x < 1.0 (base 2) and x >= -1.0 (base 2), while the IEEE
        mantissa is a sign and magnitude number normalized into the range
        where |x| >= 1.0 (base 2) and |x| < 10.0 (base 2).  

        The set of numbers represented by a 2's complement number with
        a given number of bits contains one more element than the set of
        numbers represented by a sign and magnitude number in the same
        number of bits, since the sign and magnitude number contains
        both 0 and -0.

        Illustrating the ranges on a number line, (using square brackets
        to denote a closed interval and angle brackets to denote an
        open interval):


                 -2        -1  -0.5   0   0.5   1         2
        <---------|---------|----|----|----|----|---------|--------->

        ATAC                [---->    ^    [---->
                           /     /    |    \     \
                                                    
                         /      /     |     \      \
                                                      
                       /       /      |      \       \
                                                        
                     /        /       |       \        \ 
                                                         
                   /         /        |        \         \
        IEEE      <---------]         v         [--------->

        <---------|---------|---------|---------|---------|--------->
                 -2        -1         0         1         2

        This mapping is accomplished by adjusting the exponent by
        '1' when going from ATAC to IEEE.

        One exception to this is the mapping from an ATAC '-1';
        this is an open point on the ATAC interval but not on the
        IEEE place it would map to.  In this instance, the ATAC
        and IEEE contain the same mantissa and the same exponent
        (i.e. -1 maps to -1).  This exception is handled in the
        following macro by setting the variable 'expadjust'.

        The second exception to this is the mapping between '0'
        in both systems.  Because IEEE has a special representation
        for this the algorithm doesn't work directly, and it must
        be checked for explictly.

        The exponent for ATAC is biased from the value 128; the
        exponent for an IEEE 64 bit float is biased from the value
        1023.  1023 - 128 = 895, which is the magic number used in
        the macro below for computing the exponent.

*/

/*
        In the following macro, atacfloat should be a pointer to
        an unsigned 32 bit number, and ieeedouble should be a 
        pointer to a double.  Type (char *) will work for both, as
        well, if it is properly aligned.

        This macro assumes that the ATAC number is in GDR format,
        i.e. most significant bits & bytes are on the 'left' side.
        If this is not the case, then atacfloat needs to be converted
        with the macro C32_TO_U32.
*/


#if !lint
#define ATAC_TO_IEEE(atacfloat, ieeedouble)                               \
{                                                                         \
    U32 expadjust = 1;                                                    \
    U32 mantissa;                                                         \
                                                                          \
    mantissa = (*((U32 *) atacfloat) >> 8);                               \
    switch (mantissa)                                                     \
    {                                                                     \
    case 0:                                                               \
        *((double *) ieeedouble) = 0.0;                                   \
        break;                                                            \
                                                                          \
    case 0x800000:                                                        \
        expadjust = 0;                                                    \
        /* fallthrough */                                                 \
                                                                          \
    default:                                                              \
        mantissa = ((*((U32 *) atacfloat) & 0x80000000) ?                 \
            (~(mantissa - 1)) : mantissa) & 0x3fffff;                     \
        *((U32 *) ieeedouble) =                                           \
            (*((U32 *) atacfloat) & 0x80000000)  |                        \
            (((*((U32 *) atacfloat) & 0xff) + 895 - expadjust) << 20) |   \
            (mantissa >> 2);                                              \
        *(((U32 *) ieeedouble) + 1) = mantissa << 30;                     \
        break;                                                            \
    }                                                                     \
}
#else     /* lint version */
#define ATAC_TO_IEEE(atacfloat, ieeedouble)                               \
    {*((double *) ieeedouble) = 0.0;}
#endif


/*
        The following section describes how a conversion from a Modcomp
        32 bit floating point number to an IEEE 64 bit floating point
        number.

        The format for the two numbers is:

          1   9 bits                       22 bits
         -----------------------------------------------------------------
MODCOMP  |s|   exp   |                     mantissa                      |
         -----------------------------------------------------------------
   
   
          1    11 bits                       52  bits                         
         -----------------------------------------------------------------
   IEEE  |s|     exp     |                   mantissa                    |
         -----------------------------------------------------------------

        Both Modcomp and IEEE floats are represented as sign and magnitude
        quantities.

        If the Modcomp number is negative (the 's' bit is set), then the
        entire number is in 2's complement, including the exponent.

        Modcomp exponents are biased from 256, while IEEE exponents are
        biased from 1023.  Modcomp is normalized between .1 (base 2) and
        1.0 (base 2), while IEEE is normalized between 1.0 (base 2) and
        10.0 (base 2), so there is an additional adjustment of 1 made
        in the exponent to compensate for this difference.  The magic
        number '0x2fe00000' is derived by: 1023 - 256 - 1, left
        shifted by 20 bits to make it align with the IEEE exponent
        bits 1 - 12.
*/


/*
        In the following macro, modcfloat should be a pointer to
        an unsigned 32 bit number, and ieeedouble should be a 
        pointer to a double.  Type (char *) will work for both, as
        well, if it is properly aligned.
*/

#if !lint
#define MODCOMP_TO_IEEE(modcfloat, ieeedouble)                            \
{                                                                         \
    U32 temp;                                                             \
                                                                          \
    if (*((U32 *) modcfloat) == 0)                                        \
        *((double *) ieeedouble) = 0.0;                                   \
    else                                                                  \
    {                                                                     \
        temp = *((U32 *) modcfloat) & 0x80000000 ?                        \
                ~((*((U32 *) modcfloat)) - 1) : *((U32 *) modcfloat);     \
        *((U32 *) ieeedouble) =                                           \
            (*((U32 *) modcfloat) & 0x80000000) |                         \
            (((temp & 0x7fc00000) >> 2) + 0x2fe00000) |                   \
            ((temp >> 1) & 0xfffff);                                      \
        *(((U32 *) ieeedouble) + 1) = temp << 31;                         \
    }                                                                     \
}
#else     /* lint version */
#define MODCOMP_TO_IEEE(modcfloat, ieeedouble)                            \
    {*((double *) ieeedouble) = 0.0;}
#endif


/*
        The following is an explanation of the conversion of MIL-STD-1750A
	32 bit floating point numbers to IEEE double floating point numbers.

        The format for these numbers is:
  
         1                     23 bits                           8 bits
        -----------------------------------------------------------------
 MS1750 |s|                    mantissa                       |   exp   |
        -----------------------------------------------------------------
  
  
         1    11 bits                    52 bits                     
        -----------------------------------------------------------------
 IEEE   |s|     exp     |                mantissa                       |
        -----------------------------------------------------------------
  

        The MS1750A mantissa is a 2's complement number normalized in the
        a range of x < 1.0 (base 2) and x >= -1.0 (base 2), while the IEEE
        mantissa is a sign and magnitude number normalized into the range
        where |x| >= 1.0 (base 2) and |x| < 10.0 (base 2).  

        The set of numbers represented by a 2's complement number with
        a given number of bits contains one more element than the set of
        numbers represented by a sign and magnitude number in the same
        number of bits, since the sign and magnitude number contains
        both 0 and -0.

        Illustrating the ranges on a number line, (using square brackets
        to denote a closed interval and angle brackets to denote an
        open interval):


                 -2        -1  -0.5   0   0.5   1         2
        <---------|---------|----|----|----|----|---------|--------->

        MS1750A             [---->    ^    [---->
                           /     /    |    \     \
                                                    
                         /      /     |     \      \
                                                      
                       /       /      |      \       \
                                                        
                     /        /       |       \        \ 
                                                         
                   /         /        |        \         \
        IEEE      <---------]         v         [--------->

        <---------|---------|---------|---------|---------|--------->
                 -2        -1         0         1         2

        This mapping is accomplished by adjusting the exponent by
        '1' when going from MS1750A to IEEE.

        One exception to this is the mapping from an MS1750A '-1';
        this is an open point on the MS1750A interval but not on the
        IEEE place it would map to.  In this instance, the MS1750A
        and IEEE contain the same mantissa and the same exponent
        (i.e. -1 maps to -1).  This exception is handled in the
        following macro by setting the variable 'expadjust'.

        The second exception to this is the mapping between '0'
        in both systems.  Because IEEE has a special representation
        for this the algorithm doesn't work directly, and it must
        be checked for explictly.

        The exponent for an IEEE 64 bit float is biased from the value
        1023, which is the magic number used in the macro below for 
	computing the exponent.

*/

/*
        In the following macro, msfloat should be a pointer to
        an unsigned 32 bit number, and ieeedouble should be a 
        pointer to a double.  Type (char *) will work for both, as
        well, if it is properly aligned.

        This macro assumes that the MS1750A number is in GDR format,
        i.e. most significant bits & bytes are on the 'left' side.
        If this is not the case, then msfloat needs to be converted
        with the macro C32_TO_U32.
*/


#if !lint
#define MS175032_TO_IEEE( msfloat, ieeedouble )				  \
{									  \
    U32 expadjust = 1;                                                    \
    U32 mantissa;                                                         \
                                                                          \
    mantissa = (*((U32 *) msfloat) >> 8);                                 \
    switch (mantissa)                                                     \
    {                                                                     \
    case 0:                                                               \
        *((double *) ieeedouble) = 0.0;                                   \
        break;                                                            \
                                                                          \
    case 0x800000:                                                        \
        expadjust = 0;                                                    \
        /* fallthrough */                                                 \
                                                                          \
    default:                                                              \
        mantissa = ((*((U32 *) msfloat) & 0x80000000) ?                   \
            (~(mantissa - 1)) : mantissa) & 0x3fffff;                     \
        *((U32 *) ieeedouble) =                                           \
            (*((U32 *) msfloat) & 0x80000000)  |                          \
            (((*((U32 *) msfloat) & 0xff) + 1023 - expadjust) << 20) |    \
            (mantissa >> 2);                                              \
        *(((U32 *) ieeedouble) + 1) = mantissa << 30;                     \
        break;                                                            \
    }                                                                     \
}
#else     /* lint version */
#define MS175032_TO_IEEE( msfloat, ieeedouble )				  \
	{*((double *) ieeedouble) = 0.0;}
#endif


/*
        The following is an explanation of the conversion of MIL-STD-1750A
	48 bit floating point numbers to IEEE double floating point numbers.

        The format for these numbers is:
  
         1         23 bits            8 bits            16 bits
        -----------------------------------------------------------------
 MS1750 |s|       mantissa_ms       |   exp   |      mantissa_ls        |
        -----------------------------------------------------------------
  
  
         1    11 bits                    52 bits                     
        -----------------------------------------------------------------
 IEEE   |s|     exp     |                mantissa                       |
        -----------------------------------------------------------------
  

        The MS1750A mantissa is a 2's complement number normalized in the
        a range of x < 1.0 (base 2) and x >= -1.0 (base 2), while the IEEE
        mantissa is a sign and magnitude number normalized into the range
        where |x| >= 1.0 (base 2) and |x| < 10.0 (base 2).  

        The set of numbers represented by a 2's complement number with
        a given number of bits contains one more element than the set of
        numbers represented by a sign and magnitude number in the same
        number of bits, since the sign and magnitude number contains
        both 0 and -0.

        Illustrating the ranges on a number line, (using square brackets
        to denote a closed interval and angle brackets to denote an
        open interval):


                 -2        -1  -0.5   0   0.5   1         2
        <---------|---------|----|----|----|----|---------|--------->

        MS1750A             [---->    ^    [---->
                           /     /    |    \     \
                                                    
                         /      /     |     \      \
                                                      
                       /       /      |      \       \
                                                        
                     /        /       |       \        \ 
                                                         
                   /         /        |        \         \
        IEEE      <---------]         v         [--------->

        <---------|---------|---------|---------|---------|--------->
                 -2        -1         0         1         2

        This mapping is accomplished by adjusting the exponent by
        '1' when going from MS1750A to IEEE.

        One exception to this is the mapping from an MS1750A '-1';
        this is an open point on the MS1750A interval but not on the
        IEEE place it would map to.  In this instance, the MS1750A
        and IEEE contain the same mantissa and the same exponent
        (i.e. -1 maps to -1).  This exception is handled in the
        following macro by setting the variable 'expadjust'.

        The second exception to this is the mapping between '0'
        in both systems.  Because IEEE has a special representation
        for this the algorithm doesn't work directly, and it must
        be checked for explictly.

        The exponent for an IEEE 64 bit float is biased from the value
        1023, which is the magic number used in the macro below for 
	computing the exponent.

*/

/*
	The following macro expects msfloat to be stored in an array
	of two unsigned 32 bit integers, with the 16 bit least significant
	part of the mantissa left justified in the second element of the
	array of unsigned 32 bit integers.  Also, ieeedouble should be a 
        pointer to a double.  Type (char *) will work for both, as
        well, if it is properly aligned.

        This macro assumes that the MS1750A number is in GDR format,
        i.e. most significant bits & bytes are on the 'left' side.
        If this is not the case, then msfloat needs to be converted
        with the macro C32_TO_U32.
*/

#if !lint
#define MS175048_TO_IEEE( msfloat, ieeedouble )				  \
{									  \
    U32 carry = 0;							  \
    U32 ieee_zero = 0;							  \
    U32 expadjust = 1;							  \
    U32 mantissa_ms;							  \
    U32 mantissa_ls;							  \
									  \
    mantissa_ms = (*((U32 *)msfloat) >> 8);				  \
    mantissa_ls = (*(((U32 *)msfloat)+1) >> 16);			  \
									  \
    if ( mantissa_ms == 0 && mantissa_ls == 0 )				  \
    {									  \
        *((double *)ieeedouble) = 0.0;					  \
        ieee_zero = 1;							  \
    }									  \
    else if (mantissa_ms == 0x800000 && mantissa_ls == 0 )		  \
    {									  \
        expadjust = 0;							  \
    }									  \
									  \
    if ( ieee_zero == 0 )						  \
    {									  \
        if ( mantissa_ms & 0x800000 ) /* sign bit is set */		  \
        {								  \
                if ( mantissa_ls != 0 )					  \
                        carry = 1;					  \
									  \
                mantissa_ls -= 1;					  \
                mantissa_ls = ~mantissa_ls & 0xffff;			  \
                mantissa_ms = ~(mantissa_ms - 1 + carry) & 0x3fffff;	  \
        }								  \
									  \
        *(U32 *)ieeedouble =						  \
                (((*(U32 *)msfloat) & 0x80000000) |			  \
                ((((*(U32 *)msfloat) & 0xff) + 1023 - expadjust) << 20) | \
                (mantissa_ms >> 2) );					  \
        *(((U32 *)ieeedouble) + 1) = (mantissa_ms << 30) |		  \
                (mantissa_ls << 14);					  \
    }							  		  \
}
#else     /* lint version */
#define MS175048_TO_IEEE( msfloat, ieeedouble )				  \
	{*((double *) ieeedouble) = 0.0;}
#endif

#endif    /* gdrbyteconv.h */

