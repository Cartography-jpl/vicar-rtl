#include "file_no_path.h"
#include "ibiserrs.h"
#include "ibisfile.h"
#include "mp_routines.h"

int find_hist_key
(
int unit,		/* in: unit number */
char *key,		/* in: key to search for */
int lastflag,		/* in: TRUE=find last, FALSE=find first */
char *task,		/* out: task name where key found */
int *instance		/* out: instance number of task where key found */
);
int mp_triaxcoef( double radii[3], double cc[MLIMIT][NLIMIT],
                  double cp[MLIMIT][NLIMIT], double ac[KLIMIT+1][MLIMIT+1],
                  double ap[NLIMIT]);
int planetodetic_authalic_trans( MP mp, double *planetodetic_latitude,
				 double *authalic_latitude );
int planetodetic_conformal_trans( MP mp, double *planetodetic_latitude,
				  double *conformal_latitude );
int ortho_obl_c(int m, float *data, float *line,
		float *sample, float *lat, float *longi);
int triaxtran_c(double ain, double bin, double cin,
                double cc[MLIMIT][NLIMIT], double cp[MLIMIT][NLIMIT],
                double ac[KLIMIT+1][MLIMIT+1],double ap[NLIMIT],
                double inlat, double inlon, int infmt,
                double *outlat, double *outlon, int outfmt);
#define SEARCHV3_BUFLEN 40
int searcv3_c( int unit, float data[SEARCHV3_BUFLEN], int idata[SEARCHV3_BUFLEN]);
void momati_c(double oal, double oas, double ssl, double sss,
              double scale, double fl, double sclo, double scla,
              double angn, double range, double a[3][3], double rs[3]);
int zknuth(char* string,int *buf);
void zmve(int dcode, int n, void* a, void* b, int inca, int incb);





