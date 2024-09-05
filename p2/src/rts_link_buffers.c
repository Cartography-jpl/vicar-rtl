/**  Copyright (c) 1995, California Institute of Technology		**/
/**  U. S. Government sponsorship under NASA contract is acknowledged	**/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
/***
#include "rts_errors.h"
 **/
#include "rts_typedefs.h"
#include "rts_link_buffers.h"

/***  Function prototypes  ***/
static	link_buf_typ	*initialize_buffer_link( void );

/*******************************************************************************
 *				APPEND_BUFFER_LINK
 *
 *	Creates and appends a link after the passed buffer link.  It returns
 *  NULL if it could not allocate the memory for the link.  The forward and
 *  backward links are assigned, and the previous and next buffers' links are
 *  also modified.  The data pointer is set to NULL.  This routine will create
 *  the first link of a linked buffer if a NULL link is passed in.  For this
 *  first call, the pointer to the link MUST be a NULL.
 ******************************************************************************/
link_buf_typ	*append_buffer_link(
  link_buf_typ	*current)
{ link_buf_typ	*new;

  if (current == NULL)
     return(initialize_buffer_link());

  new = (link_buf_typ *)malloc(sizeof(link_buf_typ));
  if (new == NULL) return (NULL);

  memset((void *)new,0,sizeof(link_buf_typ));
  new->previous = current;
  new->next = current->next;
  ((link_buf_typ *)current->next)->previous = new;
  current->next = new;
  
  return (new);
}

/*******************************************************************************
 *				DELETE_BUFFER_LINK
 *
 *	Removes a link and frees the memory associated with the link, INCLUDING
 *  the data portion of the link.  It returns the link previous to the link
 *  deleted.  If the last link is deleted, then NULL is returned.
 ******************************************************************************/
link_buf_typ	*delete_buffer_link(
  link_buf_typ	*link)
{ void	*temp;

  if (link == NULL) return (NULL);

  ((link_buf_typ *)link->previous)->next = link->next;
  ((link_buf_typ *)link->next)->previous = link->previous;
  temp = link->previous;
  if ((void *)link == temp) temp = NULL;
  free(link->data);
  free((void *)link);

  return ((link_buf_typ *)temp);
}

/*******************************************************************************
 *				INITIALIZE_BUFFER_LINK
 *
 *	Defines the first buffer to point to itself for both the forward and
 *  backward links.  It returns NULL if it could not allocate the memory for
 *  the link, otheriwse it returns the address of the link.  This routine is
 *  only used by the INSERT_BUFFER_LINK, no application program should need
 *  to call this routine (hence, the static declaration).
 ******************************************************************************/
static link_buf_typ	*initialize_buffer_link( void )
{ link_buf_typ	*first;

  first = (link_buf_typ *)malloc(sizeof(link_buf_typ));
  if (first == NULL) return (NULL);

  memset((void *)first,0,sizeof(link_buf_typ));
  first->next = (void *)first;
  first->previous = (void *)first;

  return (first);
}

/*******************************************************************************
 *				INSERT_BUFFER_LINK
 *
 *	Creates and inserts a link before the passed buffer link.  It returns
 *  NULL if it could not allocate the memory for the link.  The forward and
 *  backward links are assigned, and the previous and next buffers' links are
 *  also modified.  The data pointer is set to NULL.  This routine will create
 *  the first link of a linked buffer if a NULL link is passed in.  For this
 *  first call, the pointer to the link MUST be a NULL.
 ******************************************************************************/
link_buf_typ	*insert_buffer_link(
  link_buf_typ	*current)
{ link_buf_typ	*new;

  if (current == NULL)
     return(initialize_buffer_link());

  new = (link_buf_typ *)malloc(sizeof(link_buf_typ));
  if (new == NULL) return (NULL);

  memset((void *)new,0,sizeof(link_buf_typ));
  new->previous = current->previous;
  new->next = current;
  ((link_buf_typ *)current->previous)->next = new;
  current->previous = new;

  return (new);
}

/*******************************************************************************
 *				PRINT_LINKED_LIST
 *
 *	Prints the data contents of a linked list structure.  Assumes the data
 *  is of type char *.
 ******************************************************************************/
void	print_linked_list (
  link_buf_typ	*List,
  long		Options )
{ int	LinkCount = 0;
  char	PrefixBuffer[32] = "",
	AddressBuffer[32] = "",
	Buffer[256] = "",
	*Ptr = "";
  link_buf_typ	*LocalLink = List;

  if (!List)
  { printf("No List Present\n");
    return;
  }

  do
  { if (Options & RLB_INDEX_PREFIX) sprintf(PrefixBuffer,"%4d: ",++LinkCount);
    if (Options & (RLB_ADDRESS_ALSO | RLB_ADDRESS_ONLY))
       sprintf(AddressBuffer,"(%08X) ",(int)LocalLink->data);
    if (!(Options & RLB_ADDRESS_ONLY))
    { Ptr = LocalLink->data;
      if (!Ptr) Ptr = "(null)";
    }
    printf("%s%s%s\n",PrefixBuffer,AddressBuffer,Ptr);
    LocalLink = LocalLink->next;
  } while (LocalLink != List);

  return;
}
