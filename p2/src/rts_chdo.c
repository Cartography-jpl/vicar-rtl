/**  Copyright (c) 1995, California Institute of Technology		**/
/**  U. S. Government sponsorship under NASA contract is acknowledged	**/


#include <stdio.h>
#include <string.h>
#include "rts_typedefs.h"
#include "rts_const_defs.h"
#include "rts_chdo_types.h"
#include "rts_sfdu.h"
#include "rts_gdrbyteconv.h"
#include "rts_logging.h"
#include "rts_errors.h"

#define  MODULE_NAME		"CHDO"

static char	log_msg_buf[128];
/*******************************************************************
 *				FREE_CHDO_BUFFER
 ******************************************************************/
void	free_chdo_buffer(
  chdo_hdr_typ	*chdo)
{
  if (chdo->Type == 10)
  { sprintf(log_msg_buf,"Deallocating CHDO 10 Data: %08X",
            chdo->Data.chdo_010->data);
    rts_logger(RTS_LOG_SFDU,RTS_LOG_DEBUG7,MODULE_NAME,log_msg_buf);
    free(chdo->Data.chdo_010->data);
  } else  if (chdo->Type == 28)
  { if (!chdo->Data.chdo_028->lv_flag)
    { sprintf(log_msg_buf,"Deallocating CHDO 28 lc_value: %08X",
              chdo->Data.chdo_028->lc_value);
      rts_logger(RTS_LOG_SFDU,RTS_LOG_DEBUG7,MODULE_NAME,log_msg_buf);
      free(chdo->Data.chdo_028->lc_value);
    }
  }

  sprintf(log_msg_buf,"Deallocating: %08X",chdo->Data.buffer);
  rts_logger(RTS_LOG_SFDU,RTS_LOG_DEBUG7,MODULE_NAME,log_msg_buf);
  free(chdo->Data.buffer);
}

/*******************************************************************
 *				CHDO_002
 ******************************************************************/
int	load_chdo_002(
  chdo_002_typ	*chdo_002,
  Ubyte		*ptr)
{ int	type,
	length;

		type = C16_TO_U16( ( ptr + 0 ) );
		length = C16_TO_U16( ( ptr + 2 ) );
		chdo_002->major_type = *( ptr + 4 );
		chdo_002->minor_type = *( ptr + 5 );
		chdo_002->mission_id = *( ptr + 6 );
		chdo_002->format_id = *( ptr + 7 );

	return RTS_NORMAL;
}

/*******************************************************************
 *				CHDO_010
 ******************************************************************/
int	load_chdo_010(
  chdo_010_typ	*chdo_010,
  Ubyte		*ptr)
{ int	type,
	length;

		type = C16_TO_U16( ( ptr + 0 ) );
		length = C16_TO_U16( ( ptr + 2 ) );

                chdo_010->data = (void *)malloc(length);
                if (!chdo_010->data)
                { rts_logger(RTS_LOG_SFDU,RTS_LOG_ERROR,MODULE_NAME,
                             "Could not allocate memory for CHDO 10 data");
                  return (ALLOCATE_ERROR);
                }

                sprintf(log_msg_buf,"Allocating CHDO 10 Data: %08X",
                        chdo_010->data);
                rts_logger(RTS_LOG_SFDU,RTS_LOG_DEBUG7,MODULE_NAME,log_msg_buf);

                memcpy(chdo_010->data,(ptr+4),length);

        return ( RTS_NORMAL );
}

/*******************************************************************
 *				CHDO_027
 ******************************************************************/
int	load_chdo_027(
  chdo_027_typ	*chdo_027,
  Ubyte		*ptr)
{ int	type,
	length;

		type = C16_TO_U16( ( ptr + 0 ) );
		length = C16_TO_U16( ( ptr + 2 ) );

		chdo_027->decom_flags = *(ptr + 4);
		chdo_027->filler_length = *(ptr + 5);
		chdo_027->number_channels = C16_TO_U16( (ptr + 6) );
		chdo_027->map_id = C16_TO_U16( (ptr + 8) );

        return ( RTS_NORMAL );
}

/*******************************************************************
 *				CHDO_028
 ******************************************************************/
int	load_chdo_028(
  chdo_028_typ	*chdo_028,
  Ubyte		*ptr)
{ int	type,
	length,
	ValueLength;

		type = C16_TO_U16( ( ptr + 0 ) );
		length = C16_TO_U16( ( ptr + 2 ) );

		chdo_028->source = (*(ptr + 4) >> 3);
		chdo_028->lv_flag = (*(ptr + 4) >> 2) & 0x01;
		chdo_028->bad_data = (*(ptr + 4) >> 1) & 0x01;
		chdo_028->spare = (*(ptr + 4)) & 0x01;
		chdo_028->length_value = *(ptr + 5);
		chdo_028->filler_length = (*(ptr + 6) >> 4);
		chdo_028->channel_number = (C16_TO_U16( (ptr + 6) )) & 0x0FFF;

		if (!chdo_028->lv_flag)
		{ ValueLength = sizeof(Uword) * chdo_028->length_value;
		  chdo_028->lc_value = (void *)malloc(ValueLength);
                  if (!chdo_028->lc_value)
                  { rts_logger(RTS_LOG_SFDU,RTS_LOG_ERROR,MODULE_NAME,
                               "Could not allocate memory for CHDO 28 data");
                    return (ALLOCATE_ERROR);
                  }
                  sprintf(log_msg_buf,"Allocating CHDO 28 lc_value: %08X",
                         chdo_028->lc_value);
                  rts_logger(RTS_LOG_SFDU,RTS_LOG_DEBUG7,MODULE_NAME,log_msg_buf);

                  memcpy(chdo_028->lc_value,(ptr+4),length);
		} else chdo_028->lc_value = (Uword *)0;

        return ( RTS_NORMAL );
}

/*******************************************************************
 *				CHDO_062
 ******************************************************************/
int	load_chdo_062(
  chdo_062_typ	*chdo_062,
  Ubyte		*ptr)
{ int	type,
	length;

		type = C16_TO_U16( ( ptr + 0 ) );
		length = C16_TO_U16( ( ptr + 2 ) );

		chdo_062->originator = *( ptr + 4 );
		chdo_062->last_modifier = *( ptr + 5 );
		chdo_062->scft_id = *( ptr + 6 );
		chdo_062->data_source = *( ptr + 7 );
		chdo_062->mode_flags = CBITS_TO_U8( ( ptr + 8 ), 0, 4 );
		chdo_062->status_flags = CBITS_TO_U16( ( ptr + 8 ), 4, 5 );
		chdo_062->spare_flags_1 = CBITS_TO_U8( ( ptr + 9 ), 1, 7 );
		memcpy( chdo_062->ert, ptr + 10, 6 );
		chdo_062->rs_codeword_status = *( ptr + 16 );
		chdo_062->frame_extract_count = *( ptr + 17 );
		chdo_062->dsn_record_seq = C32_TO_U32( ( ptr + 18 ) );
		chdo_062->bet = *( ptr + 22 );
		chdo_062->fly = *( ptr + 23 );
		chdo_062->decode_status = *( ptr + 24 );
		chdo_062->decode_method = *( ptr + 25 );
		chdo_062->sync_flags = *( ptr + 26 );
		chdo_062->pn_errors = *( ptr + 27 );
		chdo_062->virtual_channel_id = *( ptr + 28 );
		chdo_062->virtual_frame_count = *( ptr + 29 );
		chdo_062->frame_hdr_error_flag = *( ptr + 30 );
		chdo_062->spare_2 = *( ptr + 31 );
		chdo_062->spare_3 = *( ptr + 32 );
		chdo_062->frequency_band = *( ptr + 33 );
		chdo_062->bit_rate = C32_TO_U32( ( ptr + 34 ) );
		chdo_062->spare_4 = C16_TO_U16( ( ptr + 38 ) );
		chdo_062->snt = C32_TO_U32( ( ptr + 40 ) );
		chdo_062->ssnr = C32_TO_U32( ( ptr + 44 ) );
		chdo_062->signal_level = C32_TO_U32( ( ptr + 48 ) );
		chdo_062->antennas = *( ptr + 52 );
		chdo_062->receivers = *( ptr + 53 );
		chdo_062->master_antenna = *( ptr + 54 );
		chdo_062->master_receiver = *( ptr + 55 );
		chdo_062->dtm_group = *( ptr + 56 );
		chdo_062->tlm_channel = *( ptr + 57 );
		chdo_062->lock_status = C16_TO_U16( ( ptr + 58 ) );
		chdo_062->version = *( ptr + 60 );
		chdo_062->build = *( ptr + 61 );
		chdo_062->orig_source = *( ptr + 62 );
		chdo_062->curr_source = *( ptr + 63 );
		memcpy( chdo_062->rct, ptr + 64, 6 );
		chdo_062->anomaly_flags = C16_TO_U16( ( ptr + 70 ) );
		chdo_062->lock_count = C16_TO_U16( ( ptr + 72 ) );
		chdo_062->lrn = C16_TO_U16( ( ptr + 74 ) );
		memset( ( chdo_062->pub ), (int)( ptr + 76 ), 6 );
		chdo_062->frame_type = C16_TO_U16( ( ptr + 82 ) );

	return( RTS_NORMAL );
}

/*******************************************************************
 *				CHDO_063
 ******************************************************************/
int	load_chdo_063(
  chdo_063_typ	*chdo_063,
  Ubyte		*ptr)
{ int	type,
	length;

		type = C16_TO_U16( ( ptr + 0 ) );
		length = C16_TO_U16( ( ptr + 2 ) );

		chdo_063->packet_quality = CBITS_TO_U8( ( ptr + 4 ), 0, 2 );
		chdo_063->pds_error_flag = CBITS_TO_U8( ( ptr + 4 ), 2, 3 );
		chdo_063->spare_flag_1 = CBITS_TO_U8( ( ptr + 4 ), 5, 1 );
		chdo_063->pkt_fill_flag = CBITS_TO_U8( ( ptr + 4 ), 6, 1 );
		chdo_063->sclk_scet_cor_flag = CBITS_TO_U8( ( ptr + 4 ), 7, 1 );
		chdo_063->packet_type = *( ptr + 5 );
		chdo_063->source_pkt_seq_count = C16_TO_U16( ( ptr + 6 ) );
		chdo_063->non_fill_length = C16_TO_U16( ( ptr + 8 ) );
/***  MO CHDO 063 ***
		chdo_063->orbit_phase = *( ptr + 10 );
		chdo_063->spare_1 = *( ptr + 11 );
		chdo_063->orbit_number = C16_TO_U16( ( ptr + 12 ) );
		chdo_063->mo_sclk_sec = C32_TO_U32( ( ptr + 14 ) );
		chdo_063->mo_sclk_spare = *( ptr + 18 );
		chdo_063->mo_sclk_fine = *( ptr + 19 );
		memcpy( chdo_063->scet, ptr + 20, 6 );
		chdo_063->spare_2 = *( ptr + 26 );
		chdo_063->segment_subcount = *( ptr + 27 );
		chdo_063->spare_3 = C16_TO_U16( ( ptr + 28 ) );
****  MPF CHDO 063  ***/
		chdo_063->spare_1 = *( ptr + 10 );
		chdo_063->spare_2 = *( ptr + 11 );
		chdo_063->rover_retransmit_seq_count = C16_TO_U16( ( ptr + 12 ) );
		chdo_063->sclk_sec = C32_TO_U32( ( ptr + 14 ) );
		chdo_063->spare_3 = *( ptr + 18 );
		chdo_063->sclk_fine = *( ptr + 19 );
		memcpy( chdo_063->scet, ptr + 20, 6 );
		chdo_063->wrapped_sclk_sec = C32_TO_U32( ( ptr + 28 ) );

	return ( RTS_NORMAL );
}

/*******************************************************************
 *				CHDO_081
 ******************************************************************/
int	load_chdo_081(
  chdo_081_typ	*chdo_081,
  Ubyte		*ptr)
{ int	type,
	length;

		type = C16_TO_U16( ( ptr + 0 ) );
		length = C16_TO_U16( ( ptr + 2 ) );

		chdo_081->originator = *( ptr + 4 );
		chdo_081->last_modifier = *( ptr + 5 );
		chdo_081->scft_id = *( ptr + 6 );
		chdo_081->data_source = *( ptr + 7 );
		chdo_081->error_flags = C16_TO_U16( ( ptr + 8 ) );
		memcpy( chdo_081->ert, ptr + 10, 6 );
		chdo_081->spare_1 = C16_TO_U16( ( ptr + 16 ) );
		chdo_081->dsn_record_seq = C32_TO_U32( ( ptr + 18 ) );
		chdo_081->acq_bet = *( ptr + 22 );
		chdo_081->maint_bet = *( ptr + 23 );
		chdo_081->verify_cnt = *( ptr + 24 );
		chdo_081->flywheel_cnt = *( ptr + 25 );
		chdo_081->data_length = C16_TO_U16( ( ptr + 26 ) );
		chdo_081->sync_mode = *( ptr + 28 );
		chdo_081->sync_status = CBITS_TO_U8( ( ptr + 29 ), 0, 5 );
		chdo_081->bit_slip = CBITS_TO_U8( ( ptr + 29 ), 5, 3 );
		chdo_081->rs_decode_status = *( ptr + 30 );
		chdo_081->rs_codeword_status = *( ptr + 31 );
		chdo_081->bit_errors = *( ptr + 32 );
		chdo_081->frequency_band = *( ptr + 33 );
		chdo_081->bit_rate = C32_TO_U32( ( ptr + 34 ) );
		chdo_081->spare_2 = C16_TO_U16( ( ptr + 38 ) );
		chdo_081->snt = C32_TO_U32( ( ptr + 40 ) );
		chdo_081->ssnr = C32_TO_U32( ( ptr + 44 ) );
		chdo_081->signal_level = C32_TO_U32( ( ptr + 48 ) );
		chdo_081->antennas = *( ptr + 52 );
		chdo_081->receivers = *( ptr + 53 );
		chdo_081->master_antenna = *( ptr + 54 );
		chdo_081->master_receiver = *( ptr + 55 );
		chdo_081->dtm_group = *( ptr + 56 );
		chdo_081->tlm_channel = *( ptr + 57 );
		chdo_081->lock_status = C16_TO_U16( ( ptr + 58 ) );
		chdo_081->version = *( ptr + 60 );
		chdo_081->build = *( ptr + 61 );
		chdo_081->orig_source = *( ptr + 62 );
		chdo_081->curr_source = *( ptr + 63 );
		memcpy( chdo_081->rct, ptr + 64, 6 );
		chdo_081->anomaly_flags = C16_TO_U16( ( ptr + 70 ) );
		chdo_081->lock_count = C16_TO_U16( ( ptr + 72 ) );
		chdo_081->lrn = C16_TO_U16( ( ptr + 74 ) );

	return( RTS_NORMAL );
}

/*******************************************************************
 *				CHDO_082
 ******************************************************************/
int	load_chdo_082(
  chdo_082_typ	*chdo_082,
  Ubyte		*ptr)
{ int	type,
	length;

		type = C16_TO_U16( ( ptr + 0 ) );
		length = C16_TO_U16( ( ptr + 2 ) );

		chdo_082->originator = *( ptr + 4 );
		chdo_082->last_modifier = *( ptr + 5 );
		chdo_082->scft_id = *( ptr + 6 );
		chdo_082->data_source = *( ptr + 7 );
		chdo_082->mode_flags = CBITS_TO_U8( ( ptr + 8 ), 0, 4 );
		chdo_082->status_flags = CBITS_TO_U16( ( ptr + 8 ), 4, 5 );
		chdo_082->spare_flags_1 = CBITS_TO_U8( ( ptr + 9 ), 1, 7 );
		memcpy( chdo_082->ert, ptr + 10, 6 );
		chdo_082->rs_codeword_status = *( ptr + 16 );
		chdo_082->frame_extract_count = *( ptr + 17 );
		chdo_082->dsn_record_seq = C32_TO_U32( ( ptr + 18 ) );
		chdo_082->bet = *( ptr + 22 );
		chdo_082->fly = *( ptr + 23 );
		chdo_082->decode_status = *( ptr + 24 );
		chdo_082->decode_method = *( ptr + 25 );
		chdo_082->sync_flags = *( ptr + 26 );
		chdo_082->pn_errors = *( ptr + 27 );
		chdo_082->virtual_channel_id = *( ptr + 28 );
		chdo_082->virtual_frame_count = *( ptr + 29 );
		chdo_082->frame_hdr_error_flag = *( ptr + 30 );
		chdo_082->spare_01 = *( ptr + 31 );
		chdo_082->spare_3 = *( ptr + 32 );
		chdo_082->frequency_band = *( ptr + 33 );
		chdo_082->bit_rate = C32_TO_U32( ( ptr + 34 ) );
		chdo_082->spare_4 = C16_TO_U16( ( ptr + 38 ) );
		chdo_082->snt = C32_TO_U32( ( ptr + 40 ) );
		chdo_082->ssnr = C32_TO_U32( ( ptr + 44 ) );
		chdo_082->signal_level = C32_TO_U32( ( ptr + 48 ) );
		chdo_082->antennas = *( ptr + 52 );
		chdo_082->receivers = *( ptr + 53 );
		chdo_082->master_antenna = *( ptr + 54 );
		chdo_082->master_receiver = *( ptr + 55 );
		chdo_082->dtm_group = *( ptr + 56 );
		chdo_082->tlm_channel = *( ptr + 57 );
		chdo_082->lock_status = C16_TO_U16( ( ptr + 58 ) );
		chdo_082->version = *( ptr + 60 );
		chdo_082->build = *( ptr + 61 );
		chdo_082->orig_source = *( ptr + 62 );
		chdo_082->curr_source = *( ptr + 63 );
		memcpy( chdo_082->rct, ptr + 64, 6 );
		chdo_082->anomaly_flags = C16_TO_U16( ( ptr + 70 ) );
		chdo_082->lock_count = C16_TO_U16( ( ptr + 72 ) );
		chdo_082->lrn = C16_TO_U16( ( ptr + 74 ) );
		memset( ( chdo_082->pub ), (int)( ptr + 76 ), 6 );
		chdo_082->spare_41 = C16_TO_U16( ( ptr + 82 ) );

	return( RTS_NORMAL );
}

/*******************************************************************
 *				CHDO_128
 ******************************************************************/
int	load_chdo_128(
  chdo_128_typ	*chdo_128,
  Ubyte		*ptr)
{ int	type,
	length;

		type = C16_TO_U16( ( ptr + 0 ) );
		length = C16_TO_U16( ( ptr + 2 ) );

		chdo_128->pkt_fill_flag = (*(ptr + 3)) >> 7;
		chdo_128->apid = C16_TO_I16( ptr + 4 ) & 0x07FF;
		chdo_128->pkt_seq_count = C16_TO_I16( (ptr + 6) );
		chdo_128->non_fill_length = C16_TO_I16( (ptr + 8) );
        	chdo_128->extract_flags = *(ptr + 10);
		chdo_128->parent_sc_id = *(ptr + 11);
		chdo_128->spare_2 = C16_TO_I16( (ptr + 12) );
		memcpy(chdo_128->sclk, (ptr+14), 6);
		memcpy(chdo_128->scet, (ptr+20), 6);
		chdo_128->spare_3 = *(ptr + 26);
		chdo_128->scet_flags = *(ptr + 27);
		chdo_128->spare_4 = C16_TO_I16( (ptr + 28) );

	return ( RTS_NORMAL );
}
