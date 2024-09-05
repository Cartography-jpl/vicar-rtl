#ifndef RTS_CHDO_TYPES
#define  RTS_CHDO_TYPES		1

#ifdef __cplusplus
extern "C" {
#endif

/**  Copyright (c) 1995, California Institute of Technology		**/
/**  U. S. Government sponsorship under NASA contract is acknowledged	**/

#include "rts_typedefs.h"

typedef struct	{
		Ubyte		major_type;
		Ubyte		minor_type;
		Ubyte		mission_id;
		Ubyte		format_id;
		} chdo_002_typ;

typedef struct	{
		Ubyte		*data;
		} chdo_010_typ;

typedef struct chdo_027 {
		Ubyte		decom_flags;
		Ubyte		filler_length;
		Uword		number_channels;
		Uword		map_id;
		} chdo_027_typ;

typedef struct	{
		Byte		source;
		Byte		lv_flag;
		Byte		bad_data;
		Byte		spare;
		Ubyte		length_value;
		Ubyte		filler_length;
		Uword		channel_number;
		Uword		*lc_value;
		} chdo_028_typ;

typedef struct	{
		Uword		num_items;
		Uword		spare16;
		} chdo_032_typ;

typedef struct	{
		Ubyte		originator;
		Ubyte		last_modifier;
		Ubyte		scft_id;
		Ubyte		data_source;
		Uword		error_flags;
		char		ert[6];
		Uword		spare_1;
		Uint		dsn_record_seq;
		Ubyte		acq_bet;
		Ubyte		maint_bet;
		Ubyte		verify_cnt;
		Ubyte		flywheel_cnt;
		Uword		data_length;
		Ubyte		sync_mode;
		Ubyte		sync_status;
		Ubyte		bit_slip;
		Ubyte		rs_decode_status;
		Ubyte		rs_codeword_status;
		Ubyte		bit_errors;
		Byte		frequency_band;
		Uint		bit_rate;
		Uword		spare_2;
		Uint		snt;
		Uint		ssnr;
		Uint		signal_level;
		Ubyte		antennas;
		Ubyte		receivers;
		Ubyte		master_antenna;
		Ubyte		master_receiver;
		Ubyte		dtm_group;
		Ubyte		tlm_channel;
		Ubyte		lock_status;
		Ubyte		lock_config;
		Ubyte		version;
		Ubyte		build;
		Ubyte		orig_source;
		Ubyte		curr_source;
		char		rct[6];
		Uword		anomaly_flags;
		Uword		lock_count;
		Uword		lrn;
		} chdo_061_typ;

typedef struct	{
		Ubyte		originator;
		Ubyte		last_modifier;
		Ubyte		scft_id;
		Ubyte		data_source;
		Ubyte		mode_flags;
				/*  0	pb_mode     (0-RT; 1-PB)	*/
				/*  1	data_mode   (0-Real; 1-Sim)	*/
				/*  2	test_mode   (0-Test; 1-Flight)	*/
				/*  3	replay_flag (0-Real; 1-Replay)	*/
		Ubyte		status_flags;
				/*  4	data_val    (0-Good; 1-Anomaly)	*/
				/*  5	dsn_val      (spare)		*/
				/*  6	spare_flag_1 (spare)		*/
				/*  7	ert_val     (0-Valid; 1-Bad)	*/
				/*  8	dsn_dodr     (spare)		*/
		Ubyte		spare_flags_1;
		char		ert[6];
		Ubyte		rs_codeword_status;
		Ubyte		frame_extract_count;
		Uint		dsn_record_seq;
		Ubyte		bet;
		Ubyte		fly;
		Ubyte		decode_status;
		Ubyte		decode_method;
		Ubyte		sync_flags;
				/*  0	phase		*/
				/*  1	soft_sync	*/
				/*  2	scid_force	*/
				/*  3	sclk_ref	*/
				/*  4	sclk_cor	*/
				/*  5	spare_flag_3	*/
				/*  6	spare_flag_4	*/
				/*  7	sclk_suspect	*/
		Ubyte		pn_errors;
		Ubyte		virtual_channel_id;
		Ubyte		virtual_frame_count;
		Ubyte		frame_hdr_error_flag;
				/*  0	valid_codeword		*/
				/*  1	valid_frame_version	*/
				/*  2	scid_correct		*/
				/*  3	mstr_incr_correct	*/
				/*  4	virt_incr_correct	*/
				/*  5	sec_hdr_flag		*/
				/*  6	sync_flag_set		*/
				/*  7	seg_length_id		*/
		Ubyte		spare_2;
		Ubyte		spare_3;
		Byte		frequency_band;
		Uint		bit_rate;
		Uword		spare_4;
		Uint		snt;
		Uint		ssnr;
		Uint		signal_level;
		Ubyte		antennas;
		Ubyte		receivers;
		Ubyte		master_antenna;
		Ubyte		master_receiver;
		Ubyte		dtm_group;
		Ubyte		tlm_channel;
		Uword		lock_status;
		Ubyte		version;
		Ubyte		build;
		Ubyte		orig_source;
		Ubyte		curr_source;
		char		rct[6];
		Uword		anomaly_flags;
		Uword		lock_count;
		Uword		lrn;
		char		pub [ 6 ];
		Uword		frame_type;
		} chdo_062_typ;

typedef struct	{
		Ubyte		packet_quality;
		Ubyte		pds_error_flag;
		Ubyte		spare_flag_1;
		Ubyte		pkt_fill_flag;
		Ubyte		sclk_scet_cor_flag;
		Ubyte		packet_type;
		Uword		source_pkt_seq_count;
		Uword		non_fill_length;
		Ubyte		orbit_phase;
		Ubyte		spare_1;
		Uword		orbit_number;
		Uint		mo_sclk_sec;
		Ubyte		mo_sclk_spare;
		Ubyte		mo_sclk_fine;
		Ubyte		scet[6];
		Ubyte		spare_2;
		Ubyte		segment_subcount;
		Uword		spare_3;
		} mo_chdo_063_typ;

typedef struct	{
		Ubyte		packet_quality;
		Ubyte		pds_error_flag;
		Ubyte		spare_flag_1;
		Ubyte		pkt_fill_flag;
		Ubyte		sclk_scet_cor_flag;
		Ubyte		packet_type;
		Uword		source_pkt_seq_count;
		Uword		non_fill_length;
		Ubyte		spare_1;
		Ubyte		spare_2;
		Uword		rover_retransmit_seq_count;
		Uint		sclk_sec;
		Ubyte		spare_3;
		Ubyte		sclk_fine;
		Ubyte		scet[6];
		Uint		wrapped_sclk_sec;
		} mpf_chdo_063_typ;

typedef mpf_chdo_063_typ	chdo_063_typ;

typedef struct	{
		Ubyte		originator;
		Ubyte		last_modifier;
		Ubyte		scft_id;
		Ubyte		data_source;
		Uword		error_flags;
		Ubyte		ert[6];
		Uword		spare_1;
				/*  0	rs_codeword_1	*/
				/*  1	rs_codeword_2	*/
				/*  2	rs_codeword_3	*/
				/*  3	rs_codeword_4	*/
				/*  4	rs_codeword_5	*/
				/*  5-15	spare	*/
		Uint		dsn_record_seq;
		Ubyte		acq_bet;
		Ubyte		maint_bet;
		Ubyte		verify_cnt;
		Ubyte		flywheel_cnt;
		Uword		data_length;
		Ubyte		sync_mode;
		Ubyte		sync_status;
		Ubyte		bit_slip;
		Ubyte		rs_decode_status;
		Ubyte		rs_codeword_status;
		Ubyte		bit_errors;
		Byte		frequency_band;
		Uint		bit_rate;
		Uword		spare_2;
		Uint		snt;
		Uint		ssnr;
		Uint		signal_level;
		Ubyte		antennas;
		Ubyte		receivers;
		Ubyte		master_antenna;
		Ubyte		master_receiver;
		Ubyte		dtm_group;
		Ubyte		tlm_channel;
		Uword		lock_status;
		Ubyte		version;
		Ubyte		build;
		Ubyte		orig_source;
		Ubyte		curr_source;
		Ubyte		rct[6];
		Uword		anomaly_flags;
		Uword		lock_count;
		Uword		lrn;
		} chdo_081_typ;

typedef struct	{
		Ubyte		originator;
		Ubyte		last_modifier;
		Ubyte		scft_id;
		Ubyte		data_source;
		Ubyte		mode_flags;
		Ubyte		status_flags;
		Ubyte		spare_flags_1;
		char		ert[6];
		Ubyte		rs_codeword_status;
		Ubyte		frame_extract_count;
		Uint		dsn_record_seq;
		Ubyte		bet;
		Ubyte		fly;
		Ubyte		decode_status;
		Ubyte		decode_method;
		Ubyte		sync_flags;
		Ubyte		pn_errors;
		Ubyte		virtual_channel_id;
		Ubyte		virtual_frame_count;
		Ubyte		frame_hdr_error_flag;
		Ubyte		spare_01;
		Ubyte		spare_3;
		Byte		frequency_band;
		Uint		bit_rate;
		Uword		spare_4;
		Uint		snt;
		Uint		ssnr;
		Uint		signal_level;
		Ubyte		antennas;
		Ubyte		receivers;
		Ubyte		master_antenna;
		Ubyte		master_receiver;
		Ubyte		dtm_group;
		Ubyte		tlm_channel;
		Uword		lock_status;
		Ubyte		version;
		Ubyte		build;
		Ubyte		orig_source;
		Ubyte		curr_source;
		char		rct[6];
		Uword		anomaly_flags;
		Uword		lock_count;
		Uword		lrn;
		char		pub [ 6 ];
		Uword		spare_41;
		} chdo_082_typ;

typedef struct	{
		Byte		pkt_fill_flag;
				/* 0  pkt_fill_flag */
				/* 1   spare_b */
				/* 2   spare_c */
				/* 3   spare_d */
				/* 4   spare_e */
		Word		apid;		/* 11 bits valid */
		Word		pkt_seq_count;
		Word		non_fill_length;
		Ubyte		extract_flags;
                    		/* 0   spare_a */
                    		/* 1   spare_b */
                    		/* 2   spare_c */
                    		/* 3   spare_d */
                    		/* 4   spare_e */
                    		/* 5   spare_f */
                    		/* 6   spare_g */
                    		/* 7   ccsds_sc_id */
		Ubyte		parent_sc_id;
		Uword		spare_2;
		char		sclk[6];
		char		scet[6];
		Byte		spare_3;
		Ubyte		scet_flags;
		Uword		spare_4;
		} chdo_128_typ;

#ifdef __cplusplus
}
#endif
#endif
