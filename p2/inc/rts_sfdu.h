#ifndef RTS_SFDU_TYPEDEFS
#define  RTS_SFDU_TYPEDEFS	1

#ifdef __cplusplus
extern "C" {
#endif

/**  Copyright (c) 1995, California Institute of Technology		**/
/**  U. S. Government sponsorship under NASA contract is acknowledged	**/

#include "rts_cntrl.h"
#include "rts_typedefs.h"
#include "rts_chdo_types.h"

#define  MAX_SFDU_LTH		5000
#define  MAX_SFDU_STACK		  10
#define  MAX_SFDU_HDRS		   5
#define  MAX_CHDO_HDRS		   5
#define  SFDU_HDR_LTH		  20
#define  CHDO_HDR_LTH	 	   4

typedef union	{
		chdo_002_typ	*chdo_002;
		chdo_010_typ	*chdo_010;
		chdo_027_typ	*chdo_027;
		chdo_028_typ	*chdo_028;
		chdo_032_typ	*chdo_032;
		chdo_061_typ	*chdo_061;
		chdo_062_typ	*chdo_062;
		chdo_063_typ	*chdo_063;
		chdo_081_typ	*chdo_081;
		chdo_082_typ	*chdo_082;
		chdo_128_typ	*chdo_128;
		Ubyte		*buffer;
		} chdo_typ;

typedef struct	{
		Uword		Type;
		Uword		Length;		/* Length of 'data' portion */
		chdo_typ	Data;
		} chdo_hdr_typ;

typedef	struct	{
		char		CntrlAuth[6];
		char		Version;
		char		ClassId;
		char		Delimiter;
		char		spare;
		char		DataDesc[6];
		char		Marker[10];	/* Includes NULL terminator */
		int		Length;		/* Includes Header lth (20) */
						/* Can be 64-bit integer */
		} sfdu_hdr_typ;

typedef	struct	{
		sfdu_hdr_typ	Hdr;
		chdo_hdr_typ	Chdo[MAX_CHDO_HDRS];
		int		DataIdx;	/* Index of Data CHDO */
		int		ChdoCode;
		int		ChdoHdrLth;
		void		*Buffer;
		int		BufferLth;
		} sfdu_record_typ;

/**  Old version  **/
typedef	struct	{
		char		cntrl_auth[6];
		Ubyte		version;
		Ubyte		class;
		char		delimiter;
		char		spare;
		char		ddp[6];
		Uint		length;
		Uword		aggr_exists;
		Uword		aggr_length;
		chdo_hdr_typ	chdo[MAX_SFDU_HDRS];
		Ubyte		data_idx;	/* Index of element in chdo */
						/* array that contains data */
		} sfdu_typ;
/**/

void    free_chdo_buffer( chdo_hdr_typ *);
int     load_chdo_002(chdo_002_typ *, Ubyte *);
int     load_chdo_010(chdo_010_typ *, Ubyte *);
int     load_chdo_027(chdo_027_typ *, Ubyte *);
int     load_chdo_028(chdo_028_typ *, Ubyte *);
int     load_chdo_062(chdo_062_typ *, Ubyte *);
int     load_chdo_063(chdo_063_typ *, Ubyte *);
int     load_chdo_081(chdo_081_typ *, Ubyte *);
int     load_chdo_082(chdo_082_typ *, Ubyte *);
int     load_chdo_128(chdo_128_typ *, Ubyte *);
int	rts_get_sfdu( telemproc_typ *, sfdu_record_typ *);

#ifdef __cplusplus
}
#endif
#endif
