// These are prototypes that are used internally by the RTL library.
// These shouldn't be called externally from the RTL library.
#ifndef RTLINTPROTO_H
#define RTLINTPROTO_H

#include "defines.h"
#include "declares.h"
#include <strings.h>
#include <string.h>
#include <stdlib.h>
#include <stdarg.h>
#if RTL_USE_TAE
// This gets defined again in taeconf.inp
#undef MAX
#include "taeconf.inp"
#include "parblk.inc"
#endif
void *v2_array_io_location(int unit);
void v2_deassign_value_block(int unit);
void v2_full_init_value_tbl(int unit);
int evaluate_label_item(char *value, int vallen, char *format,int *length,
			int *nelements,int *level,int *strlength);
int v2_obtain_label_values(char *value, int vallen, char *valarray, 
			   int nelements, int maxlength, char *format);
void v2_move(char *to, char *from, int len);
int v2_create_label_item(char *key, char *valarray, int maxlen, 
			 int nelements, char *format, int level, char **text);
int cut_label_string(char *start, char *stop, char *label_item);
int v2_insert_label_string(int unit, char *loc, char *newstr);
int v2_add_complex_label_item(int unit, char *newvalue, char *value, 
			      int vallen, char *start, char *stop,
			      int element, int nelements_to_add, int newlength,
			      int level, char *format, char *key);
int add_str_current_table(char *a,int kopt,VALUE_TABLE *currval_table,
			  VALUE_TABLE *def_table);
char *find_entry(char *s, char *key, char **value, int *vallen, char **place);
char *v2_string(char *s, int len);
void v2_add_label_size_item(char *ps,int lblsize); 
void v2_set_eol(char *p);
void v2_clear_eol(char *p);
int close_os_file(int unit);
int set32BitInt(unsigned int len, unsigned char *ptr);
int set_basic_label_params(int unit);
int substr(char *c_string,char *sub);
int delete_file(int unit);
void close_unit(int unit);
void final_cleanup(int unit);
int v2_deactivate_a_unit(int unit);
int read_in_labels(int unit,char **p, int *size);
int lazy_search(int unit);
int move_to_output_label(int unit,char *buf, int size, char *recbuf);
char *v2_place_cut(char *p, int size);
int write_rec(struct bufstate *state, char *buffer, int rec, int startbyte, 
	      int nbytes, struct trans *trans);
int v2_activate_a_unit(int *unit, int instance, char *name);
int v2_est_primary_environ(int unit);
int io_complete_check(struct devstate *devstate);
int v2_flush_buffer(struct bufstate *state);
int v2_finish_file(int unit);
int free_mapped_sect(struct arraystate *arraystate);
int get_one_string_parm(char *name, int number, char **value);
int basic_precheck(int unit);
int basic_preprocess(int unit);
int basic_close(int unit);
int basic_read_rec(int unit, struct bufstate *state);
int basic2_read_rec(int unit, struct bufstate *state);
int v2_movetrans(char *sbuf, int spos, int slen, char *dbuf, int dpos, 
		 int dlen, struct trans* trans, int *pplen, char *partpix);
int basic_writ_rec(int unit, struct bufstate *state);
int basic2_writ_rec(int unit, struct bufstate *state);
int basic_get_eol_size(int unit);
int basic_read_in_eol(int unit, char *p, int eol_size);
int double_byte_swap(unsigned char from[8], unsigned char to[8]);
int real_byte_swap(unsigned char from[4], unsigned char to[4]);
int copy_value_block_item(int from_unit, int to_unit, int item);
int add_msg_current_table(char *a, int kopt, VALUE_TABLE* currval_table,
			  VALUE_TABLE *def_table);
int convert_to_text(char **text, char *value, char *format);
int v2_build_system_label(int unit,char *recbuf,int *lblsize);
int v2_trans_prim_prop_labels(int unit,int *lblsize,char *recbuf);
int v2_trans_prim_hist_labels(int unit,int *lblsize,char *recbuf);
int v2_trans_curr_hist_item(int unit,int *lblsize,char *recbuf);
int v2_update_label_size_item(int unit,int lblsize);
int v2_find_task(int unit, char *task, int instance, char **start, char **stop);
int v2_find_system(int unit,char **start,char **stop);
int v2_find_property(int unit, char *prop, int instance, char **start, 
		     char **stop, int *inst_out);
int find_first_hist_item(char* buf, char **loc);
int v2_add_label_item(int unit, char *newvalue, char *start, char *stop, 
		      char *key, char *value, int vallen,
		      int element, int nelements, int level, char *format, 
		      int maxlen);
int v2_find_key(char **start, char **stop, char *key, char **value,
		int *vallen);
int delete_label_item(int unit, char *start, char *stop, char *key, 
		      char *value, int vallen,
		      int start_element, int *nelements, char **newstop);
void add_lbl_item_value_tbl(int unit,int kopt, char *value);
int compress_close(int unit);
int v2_check_out_lbl_on_close(int unit);
int v2_close_file(int unit);
int v2_process_output_file(int unit);
int v2_process_input_file(int unit);
int determine_translation(int unit);
int compress_preprocess(int unit);
void error_handler(int unit, int code);
int compress_read_rec(int unit, struct bufstate* state, char *buffer, 
		      struct trans *trans);
int read_rec(struct bufstate* state, char *buffer, int rec, int startbyte, 
	     int nbytes, struct trans *trans);
int v2_write_seq_labels(int unit);
int compress_writ_rec(int unit, struct bufstate* state, char *buffer, 
		      struct trans *trans);
int v2_del_complex_label_item(char *value, int vallen, char *start, 
			      char *stop, int start_element,
			      int* nelements_to_delete, int nelements_present,
			      int length, int level, char *format, 
			      char *key, char **newstop);
int v2_translate_input(char *stype, char *dtype, char *sihost, 
		       char *srhost, struct trans *trans);
int v2_translate_output(char *stype, char *dtype, char *dihost, 
			char *drhost, struct trans *trans);
void sys_msg(int unit, int code);
int check_primary_input(int unit);
int get_label_item(char **value, int *vallen, char **element, int *level);
int parse_label(char *label, int len, char **sk, char **ek, char **sv, 
		char **ev);
char *dequoted(char *s,int len);
int v2_write_blocks(struct devstate *devstate, char *buf, V2_OFFSET block, 
		    V2_OFFSET nblocks, int async_flag);
void v2_get_def_filename(void);
int compress_get_eol_size(int unit);
int get_one_int_parm(char *name, int number, int *value);
void v2_update_bufstate(struct bufstate* bufstate, V2_OFFSET location);
int v2_read_blocks(struct devstate *devstate, char *buf, V2_OFFSET block, 
		   V2_OFFSET nblocks, int async_flag);
int v2_set_file_offset(int unit, V2_OFFSET new_offset);
int v2_extend_disk_file(struct diskstate *diskstate, V2_OFFSET amount);
int v2_write_disk_eof(int unit, struct diskstate *diskstate, 
		      V2_OFFSET last_byte, int recsize);
int v2_convert_from_text(char **converted_item, char *text, int length, 
			 char *format);
int v2_open_disk_input(int unit, struct bufstate* bufstate);
int v2_map_disk_file(int unit);
int v2_open_disk_output(int unit, struct bufstate* bufstate);
int v2_open_input_file(int unit);
void v2_close_down(int unit);
int v2_initialize_from_label(int unit);
int bytes_per_pixel(char *ctype, char *cihost, char *crhost, int *status);
int v2_initialize_buffer(int unit);
void v2_collect_history_info(void);
int v2_get_eol_size(int unit);
int v2_add_hist_task(int unit);
void v2_get_out_size_from_parm(int unit);
int est_primary_input(void);
void v2_copy_primary_input_val(int unit);
int v2_open_output_file(int unit);
int v2_create_output_label(int unit);
void general_initialize(void);
int valid_unit(int unit);
void initialize_value_table(struct UNIT_TABLE* unit_table,
			    int n_unit_table_entries,
			    VALUE_TABLE* current_table,
			    VALUE_TABLE* default_table);
int valid_unit(int unit);
int v2_read_disk(struct diskstate* disk, char *buf, V2_OFFSET block, 
		 V2_OFFSET nblocks, int async_flag, 
		 int* transfer_count, V2_OFFSET file_offset);
int read_nop(void);
int v2_read_cache(struct bufstate* bufstate, int rec);
int compress_read_in_eol(int unit, char* q, int eol_size);
int vax_ieee_r(unsigned char* from, unsigned char *ieee);
int vax_ieee_d(unsigned char* from, unsigned char *ieee);
int ieee_vax_r(unsigned char* ieee, unsigned char *from);
int ieee_vax_d(unsigned char* ieee, unsigned char *from);
void hostmsg(int code, char* msg, int maxlen);
void build_history_label(char* ps);
int v2_write_disk(struct diskstate* disk, char *buf, V2_OFFSET block, 
		  V2_OFFSET nblocks, int async_flag, int *transfer_count,
		  V2_OFFSET file_offset);
int write_nop(void);
int p_xladd(int unit);
int p_xldel(int unit);
int c_xldel(int unit, char *type, char *key);
int get_nopts(int *nopts, int *nargs, int nconst, va_list *params);
int process_optionals_c(int unit, struct UNIT_TABLE* opts_table, 
			int n_entries, VALUE_TABLE* currval_table, 
			VALUE_TABLE *def_table,
			int nopts, va_list *params);
int c_xladd(int unit, char *type, char *key, void *newvalue, int len);
int format_preset_for(int unit, int *nopts, int *nargs, int nconst, 
		      va_list *params);
int process_optionals_for(int unit, struct UNIT_TABLE* opts_table, 
			  int n_entries, VALUE_TABLE* currval_table, 
			  VALUE_TABLE* def_table,
			  int nopts, va_list* params, 
			  char *argptr, int nargs, int argno, int strno,
			  va_list* str_params, int *str_which);
int p_xlget(int unit);
int c_xlget(int unit, char *type, char *key, char **newvalue,
	    int *internal_length, int *start_element, 
	    int *u_nelements, char **outformat);
int p_xlhinfo(int unit);
int c_xlhinfo(int unit, char *tasks, int *instances, int *nhist, int len);
int p_xlinfo(int unit);
int c_xlinfo(int unit, char *type, char *key, char *format, int *maxlength, 
	     int *nelement);
int p_xlninfo(int unit);
int c_xlninfo(int unit, char *key, char *format, int *maxlength, int *nelement);
int p_xlpinfo(int unit);
int c_xlpinfo(int unit, char *props, int *nprop, int len);
int p_xvadd(int unit);
int c_xvadd(int unit);
int p_xvclose(int unit);
int c_xvclose(int unit);
int p_xvget(int unit);
int c_xvget(int unit);
int open_act_preset_c(int unit, int nopts, va_list *params);
int open_act_preset_for(int unit, int *nopts, int *nargs, int nconst, 
			va_list *params);
int p_xvopen(int unit);
int c_xvopen(int unit);
int parm_write(char *addr, int len);
int parm_close(void);
int parm_init(int inunit);
int p_xvread(int unit);
int c_xvread(int unit, char *buffer);
int p_xvunit(int *unit, char *name, int instance);
int c_xvunit(int unit, char *name, int instance);
int p_xvwrit(int unit);
int c_xvwrit(int unit, char *buffer);
void zvtrans(
		struct trans *buf,
		void *source,
		void *dest,
		int npix);
int zvtrans_in(
		struct trans *buf,
		char *stype,
		char *dtype,
		char *sihost,
		char *srhost);
int zvtrans_inb(
		struct trans *buf,
		char *stype,
		char *dtype,
		int unit);
int zvtrans_inu(
	        struct trans *buf,
		char *stype,
		char *dtype,
		int unit);
int zvtrans_out(
		struct trans *buf,
		char *stype,
		char *dtype,
		char *dihist,
		char *drhost);
int zvtrans_set(
		struct trans *buf,
		char *stype,
		char *dtype);
#if RTL_USE_TAE
void get_parm_for(struct PARBLK* parblock,
		  char *name,int *count, int *def, int maxcnt,
		  char *value, char **argptr, int nargs, int argno,
		  int strno, va_list *params, int *which, int double_flag);
int get_parm_c(struct PARBLK* parblock, char *name, char *value, int *count, 
	       int *def, int maxcnt, int length, int double_flag);
int test_keyword(struct PARBLK* parblock, char *value);
int get_pstat(struct PARBLK* parblock, char *key, int *count, int *def, 
	      int *maxlen, char *type);
void pack_xvsptr(char* out, struct VARIABLE *v, int count);
#else
void get_parm_for(void* parblock,
		  char *name,int *count, int *def, int maxcnt,
		  char *value, char **argptr, int nargs, int argno,
		  int strno, va_list *params, int *which, int double_flag);
int get_parm_c(void* parblock, char *name, char *value, int *count, 
	       int *def, int maxcnt, int length, int double_flag);
int test_keyword(void* parblock, char *value);
int get_pstat(void* parblock, char *key, int *count, int *def, 
	      int *maxlen, char *type);
#endif
void *v2_array_io_location(int unit);
int byte2half(void *from, void *to, int len, struct trans *trans);
int byte2full(void *from, void *to, int len, struct trans *trans);
int byte2real(void *from, void *to, int len, struct trans *trans);
int byte2doub(void *from, void *to, int len, struct trans *trans);
int byte2comp(void *from, void *to, int len, struct trans *trans);
int half2byte(void *from, void *to, int len, struct trans *trans);
int half2full(void *from, void *to, int len, struct trans *trans);
int half2real(void *from, void *to, int len, struct trans *trans);
int half2doub(void *from, void *to, int len, struct trans *trans);
int half2comp(void *from, void *to, int len, struct trans *trans);
int full2byte(void *from, void *to, int len, struct trans *trans);
int full2half(void *from, void *to, int len, struct trans *trans);
int full2real(void *from, void *to, int len, struct trans *trans);
int full2doub(void *from, void *to, int len, struct trans *trans);
int full2comp(void *from, void *to, int len, struct trans *trans);
int real2byte(void *from, void *to, int len, struct trans *trans);
int real2half(void *from, void *to, int len, struct trans *trans);
int real2full(void *from, void *to, int len, struct trans *trans);
int real2doub(void *from, void *to, int len, struct trans *trans);
int real2comp(void *from, void *to, int len, struct trans *trans);
int doub2byte(void *from, void *to, int len, struct trans *trans);
int doub2half(void *from, void *to, int len, struct trans *trans);
int doub2full(void *from, void *to, int len, struct trans *trans);
int doub2real(void *from, void *to, int len, struct trans *trans);
int doub2comp(void *from, void *to, int len, struct trans *trans);
int comp2byte(void *from, void *to, int len, struct trans *trans);
int comp2half(void *from, void *to, int len, struct trans *trans);
int comp2full(void *from, void *to, int len, struct trans *trans);
int comp2real(void *from, void *to, int len, struct trans *trans);
int comp2doub(void *from, void *to, int len, struct trans *trans);
int trans_swap2(void *from, void *to, int len, struct trans *trans);
int trans_swap4(void *from, void *to, int len, struct trans *trans);
int trans_swap8(void *from, void *to, int len, struct trans *trans);
int r_vax2ieee(void *from, void *to, int len, struct trans *trans);
int r_vax2rieee(void *from, void *to, int len, struct trans *trans);
int r_ieee2vax(void *from, void *to, int len, struct trans *trans);
int r_rieee2vax(void *from, void *to, int len, struct trans *trans);
int d_vax2ieee(void *from, void *to, int len, struct trans *trans);
int d_vax2rieee(void *from, void *to, int len, struct trans *trans);
int d_ieee2vax(void *from, void *to, int len, struct trans *trans);
int d_rieee2vax(void *from, void *to, int len, struct trans *trans);
int c_vax2ieee(void *from, void *to, int len, struct trans *trans);
int c_vax2rieee(void *from, void *to, int len, struct trans *trans);
int c_ieee2vax(void *from, void *to, int len, struct trans *trans);
int c_rieee2vax(void *from, void *to, int len, struct trans *trans);
int c_ieee2rieee(void *from, void *to, int len, struct trans *trans);
int c_rieee2ieee(void *from, void *to, int len, struct trans *trans);
char *determine_format(char *value);
int v2_dual_translation(void *from, void *to, int len, struct trans *trans);
int v2_align_in_translation(void *from, void *to, int len, 
			    struct trans *trans);
int v2_bad_trans(void *from, void *to, int len, struct trans *trans);
int v2_align_out_translation(void *from, void *to, int len, 
			     struct trans *trans);
char *v2_find_pds_keyword(char *label, char *key);
int v2_line_size(VALUE_TYPE value);
int v2_band_size(VALUE_TYPE value);
int v2_error_action(VALUE_TYPE value);
int v2_error_mess(VALUE_TYPE value);
int v2_image_size(VALUE_TYPE value);
int v2_label_format(VALUE_TYPE value);
int v2_samp_size(VALUE_TYPE value);
int v2_nsamp_size(VALUE_TYPE value);
int v2_binary_size(VALUE_TYPE value);
int v2_instance(VALUE_TYPE value);
int v2_element(VALUE_TYPE value);
int v2_image_org(VALUE_TYPE value);
int v2_method(VALUE_TYPE value);
int v2_op(VALUE_TYPE value);
int v2_format(VALUE_TYPE value);
int v2_type(VALUE_TYPE value);
int v2_dim(VALUE_TYPE value);
int v2_hist_name(VALUE_TYPE value);
int v2_property_name(VALUE_TYPE value);
int v2_str_item(VALUE_TYPE value);
int v2_cond(VALUE_TYPE value);
int v2_closa(VALUE_TYPE value);
int v2_u_file(VALUE_TYPE value);
int v2_ladd_mode(VALUE_TYPE value);
int v2_unavailable(VALUE_TYPE value);
int v2_convert_chk(VALUE_TYPE value);
int v2_intfmt_chk(VALUE_TYPE value);
int v2_realfmt_chk(VALUE_TYPE value);
int v2_host_chk(VALUE_TYPE value);
int v2_bltype_chk(VALUE_TYPE value);
int v2_upd_hist_chk(VALUE_TYPE value);
int v2_compress_chk(VALUE_TYPE value);
V2_OFFSET v2_find_pds_offset(char *label);
char *v2_expand_filename(char *inpath, int makedir, int *status);
void i_crack (char *entry, char *name, char *device);
int parm_read(char addr[], int len);

#ifdef RTL_USE_TAE
void zvpblk(struct LARGE_PARBLK **parblk);
#else
void zvpblk(void **parblk);
#endif

// Functions called from TAE
#if RTL_USE_TAE
FUNCTION struct VARIABLE *p_fvar(struct PARBLK	*block, TEXT name[]);
FUNCTION BOOL s_equal(FAST TEXT *s, FAST TEXT *t);
FUNCTION CODE q_cmd (TEXT* command);
FUNCTION CODE p_inim (struct PARBLK* block, FUNINT blksiz, FUNINT mode);
FUNCTION CODE q_cmdwait (TEXT *command);
FUNCTION VOID q_init(struct PARBLK *p, FUNINT pool_size, FUNINT mode);
FUNCTION CODE q_string(struct PARBLK *p, TEXT name[], FUNINT count, 
		       TEXT* vector[], FUNINT mode);
FUNCTION CODE q_dynp(struct PARBLK* p, TEXT pdfspec[], FUNINT mode);
FUNCTION CODE p_inim(struct PARBLK *block, FUNINT blksiz, FUNINT mode);
FUNCTION CODE m_msg(TEXT message[], TEXT key []);
FUNCTION CODE p_mput(TEXT message[], TEXT key[]);
FUNCTION CODE q_out(struct PARBLK* p);
FUNCTION CODE t_pinit(COUNT* lines, COUNT* columns, CODE* type);
FUNCTION FILE *z_init (struct PARBLK *block, FUNINT mode);
#endif
#if RTL_USE_SHELL_VIC
int zzq_out(struct PARBLK* parblk);
int zzinit(struct PARBLK *parblk,int argc,char *argv[]);
#endif
#endif
