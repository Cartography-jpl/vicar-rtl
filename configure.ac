# Note that the version here should match what is in vicar_rtl_version.m4
# Would be nice to just automatically get this, but like so many things in
# autoconf this is extremely convoluted and I could never figure out how to
# do this. Easier just to have a note here and manually do this.
AC_INIT([vicar-rtl], 1.10)
AC_CONFIG_AUX_DIR([config])
AC_CONFIG_MACRO_DIR([config/m4])
AC_CONFIG_FILES([vicar-rtl.pc])
# This is a number of checks we have in all the afids software, so
# collect into this one routine
AFIDS_COMMON
# We have some code that builds differently if CURSES is available, so
# check for it.
MP_WITH_CURSES
# We have the top directory for vicar_rtl as a variable. This allows the
# full AFIDS system to include this in a subdirectory. For this standalone
# build, this is just "." relative to srcdir.
vicar_rtl_topdir=.
# This set up all the source directories. Source for this is found in
# config/m4/afids_source_directory.m4 so we can include this in multiple
# configuration files.
VICAR_RTL_SOURCE_DIRECTORY

# Always build VICAR
AM_CONDITIONAL([BUILD_VICAR_RTL], [true])
AM_CONDITIONAL([BUILD_AFIDS], [false])

AC_CONFIG_FILES([Makefile])
AC_OUTPUT

#======================================================================
# Give a nice log message describing what has been configured, and which
# optional libraries are going to be used.
#======================================================================

LOC_MSG()
LOC_MSG([VICAR RTL is now configured])
LOC_MSG()
LOC_MSG([  Installation directory:        ${prefix}])
LOC_MSG([  Build debug version:           ${enable_debug}])
LOC_MSG([  Fortran compiler:              ${F77} ${FFLAGS}])
LOC_MSG([  C compiler:                    ${CC} ${CFLAGS}])
