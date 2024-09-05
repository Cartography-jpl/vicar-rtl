$!----------------------------------------------------------------------
$!
$! This utility will build the VICAR RTL (VMS version).
$!
$!----------------------------------------------------------------------
$!
$ set noon
$ curdir = f$environment("default")
$!
$ write sys$output "**********************************************************"
$ write sys$output "* Building Run-Time Library                              *"
$ write sys$output "**********************************************************"
$!
$! Build the error-related routines
$!
$ set def v2$rtltop
$ copy v2share.iif v2$share.iif
$! @errorbld
$ copy v2$inc:vicmain_for_vms.fin v2$inc:vicmain_for.fin
$ copy errdefs.h v2$inc
$ copy errdefs.fin v2$inc
$ copy sys_msg.c v2$source
$ copy vic2fac.msg v2$lib
$!
$ @v2$rtltop:incrtl errdefs.h
$ copy errdefs.fin v2$lib
$!
$! Create the build lists
$!
$ perl v2$util:create_bld.perl imake_src.rtl rtl_source.bld	source
$ perl v2$util:create_bld.perl imake_src.rtl rtl_inc.bld	inc
$!
$! Create new libraries for object code
$!
$ library/create v2$rtl_olb
$ library/create v2$rtl_debug
$!
$! Insert the includes into c$library (for old programs only)
$!
$ set def v2$inc
$ @v2$rtltop:rtl_inc.bld
$!
$! Do funky stuff with the main includes (for old programs only)
$!
$ set def r2lib
$ copy v2$inc:vicmain_c.h main.inc
$ copy v2$inc:vicmain_for.fin main.fin
$ library/text c$library main.inc
$!
$! Pick the appropriate version of xviodefs
$!
$ copy v2$inc:xviodefs_vms.h v2$inc:xviodefs.h
$!
$! Compile the source code
$!
$ set def v2$source
$ @v2$rtltop:rtl_source.bld
$ @v2$rtltop:comprtl sys_msg.c
$!
$! Run MSGBLD on the error message file to make it available for TAE
$!
$ set def v2$lib
$ taetm "$taepdf:msgbld vic2fac"
$!
$! Link the shareable image
$!
$ set def v2$rtltop
$ copy v2share.opt v2$olb
$ copy v2$share.iif v2$olb
$ @v2share
$!
$! Copy miscellaneous files into v2$lib
$!
$ set def v2$lib
$ copy v2$rtltop:request.pdf v2$lib
$ copy v2$rtltop:syntax.pdf v2$lib
$ copy v2$rtltop:vicar.pdf v2$lib
$ copy v2$rtltop:v2version.pdf v2$lib
$ copy v2$rtltop:dtemp.com v2$lib
$ copy v2$rtltop:getpid.com v2$lib
$ copy v2$rtltop:logout.com v2$lib
$!
$! Build the v2param program
$!
$ set def v2$lib
$ @v2$rtltop:v2param.com system
$!
$ set def v2$rtltop
$ purge [...]
$ purge v2$olb
$ purge v2$lib
$!
$ set def 'curdir'
$!
$ exit

