$ if (f$getsyi("cpu").eq.128)
$ then						! Alpha
$   set def [.lib.axp-vms]
$ else						! VAX
$   set def [.lib.vax-vms]
$ endif
$!
$ delete math77.olb;
$!
$loop:
$ spec = f$search("[-.-.fortran]*.f")
$ if spec .eqs. "" then goto loop_done
$ name = f$parse(spec,,,"NAME")
$ if name .eqs. "AMACH" then goto loop
$ if (f$getsyi("cpu").eq.128)
$ then						! Alpha
$   fortran/nodebug/nolist/float=d_float 'spec'
$ else						! VAX
$   fortran/nodebug/nolist 'spec'
$ endif
$ library math77.olb 'name'
$ delete 'name'.obj;
$ goto loop
$!
$loop_done:
$!
$ if (f$getsyi("cpu").eq.1280
$ then						! Alpha
$   fortran/nodebug/nolist/float=d_float [-.-]amach.alpha_d3
$ else						! VAX
$   fortran/nodebug/nolist [-.-]amach.vax
$ endif
$ library math77.olb amach
$ delete amach.obj;
$!
$ exit

