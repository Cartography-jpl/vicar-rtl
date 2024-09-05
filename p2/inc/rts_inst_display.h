#ifndef  RTS_INST_DISPLAY_INCLUDED
#define  RTS_INST_DISPLAY_INCLUDED		1

#ifdef __cplusplus
extern "C" {
#endif

#include "DisplayInstrumentDefs.h"

#define  RAW_STRTCH	0
#define  MAN_STRTCH	1
#define  AUTO_STRTCH	2
#define  LOG_STRTCH	3
#define  TBL_STRTCH	4

#define  MAX_STRTCH_TBL		 16
#define  FULL_FILENAME		256

static	char	*dsp_strtch_names[] = {	"RAW",	"MANUAL",	"AUTO",
					"LOG",	"TABLE",	0};

typedef	struct	{
		int		Type;		/* 0 - Raw	*/
						/* 1 - Manual	*/
						/* 2 - Auto	*/
						/* 3 - Log	*/
						/* 4 - Table	*/
		float		Percent[2];
		int		TblElements;
		unsigned char	Table[MAX_STRTCH_TBL][2];
		unsigned char	InputDn[2];
		unsigned char	OutputDn[2];
		unsigned char	Start[2];
		} RtsStretchCntrl_typ;

typedef	struct	{
		unsigned char	On;		/* 0 - Off,  1 - On	*/
		unsigned char	Addback;
		unsigned char	Length;
		unsigned char	Median;
		unsigned char	Nweight;
		unsigned char	Pweight;
		unsigned char	Thresh;
		} RtsFilterCntrl_typ;

typedef	struct	{
		char		Xdevice[64];
		InstrumentType	Instrument;
		char		ImageFile[FULL_FILENAME];
		RtsStretchCntrl_typ	Stretch[3];
		RtsFilterCntrl_typ	Filter[3];
		} RtsDspCntrl_typ;

/***  Function Prototypes  ***/
void	fillDeviceName( char * );
void	fillImageStretchLut( int * );
void	fillImageInfoBuffer( char * );
void	fillPicnoErtInfoBuffer( char * );
void	fillRGBImageStretchLut( int *, int *, int * );
void	fillStretchInfoBuffer( char * );
void	fillTitleBarInfoBuffer( char * );
void	fillZoomParams( int * X, int * Y, int *SL, int * SS, int * O_E);
int	*getHistInputArray();
int	*getHistOutputArray();
int	getPseudoMode();
int	getBorderMode();
void	init_rts_dsp_globals();
int	loadStretchParams( RtsStretchCntrl_typ *, int );
int	loadFilterParams( RtsFilterCntrl_typ *, int );
void	shrinkHist( int *, int * );

#ifdef __cplusplus
}
#endif

#endif
