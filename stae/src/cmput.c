/******************************************************************************
 *	Copyright (c) 1990, 1991, 1992,
 *	National Aeronautics and Space Administration
 *	ALL RIGHTS RESERVED
 *
 *	The software (programs, data bases and/or documentation) on or in
 *	any media can not be reproduced, disclosed or used except under
 *	terms of the license between COSMIC and your organization.
 *****************************************************************************/


#include	"stdh.inp"
#include	"taeconf.inp"
#include "taeintproto.h"

/*
 * 	CHANGE LOG:
 *
 *	18-dec-86	Add p_msgout as it ts also called from
 *			TAE library modules under v1.4...dm
 */

/*
 *	p_mput. write message to standard output.
 *
 *	NOTE. The c application programs directly invoke the 
 *	function m_msg() to send message to standard output device,
 *	(equivalent of XMPUT call from FORTRAN programs).
 *	However, library routines such as p_ and q_ functions
 *	use p_mput call (via x_error ) to report error messages.
 *	Since the existing p_mput function, used by FORTRAN 
 *	applications invoke a fortran module 'wrtstd', that version
 *	should not be used from C applications. Hence, we create
 *	this new entry point, which invokes m_msg().
 *
 * 	The same also applies to p_msgout().
 *
 */
     FUNCTION  CODE  p_mput(message, key)

    TEXT	message[];		/* in: message to write */
    TEXT	key[];			/* message key		*/

    {
    CODE	code;

    code = m_msg(message, key);
    return(code);
    }



     FUNCTION  CODE  p_msgout(message, key)

    TEXT	message[];		/* in: message to write */
    TEXT	key[];			/* message key		*/

    {
    CODE	code;

    code = m_msgout(message, key);
    return(code);
    }
