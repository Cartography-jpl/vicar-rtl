#ifndef RTS_LINK_BUFFERS
#define  RTS_LINK_BUFFERS 1

/**  Copyright (c) 1995, California Institute of Technology		**/
/**  U. S. Government sponsorship under NASA contract is acknowledged	**/

#ifdef __cplusplus
extern "C" {
#endif

/***  Print_Linked_List Options  ***/
#define  RLB_PRINT_TO_LOG		0x00000001
#define  RLB_INDEX_PREFIX		0x00000002
#define  RLB_ADDRESS_ALSO		0x00000004
#define  RLB_ADDRESS_ONLY		0x00000008

/***  Variable Type Definitions  ***/
typedef	struct	{
		void	*previous;
		void	*next;
		void	*data;
		} link_buf_typ;

/***  Function Prototypes  ***/
	link_buf_typ	*append_buffer_link( link_buf_typ *);
	link_buf_typ	*delete_buffer_link( link_buf_typ *);
	link_buf_typ	*insert_buffer_link( link_buf_typ *);
	void		print_linked_list ( link_buf_typ *, long );

#ifdef __cplusplus
}
#endif

#endif
