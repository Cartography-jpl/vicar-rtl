$!----------------------------------------------------------------------
$!
$! This utility will compile a single RTL module, both debug and
$! non-debug, and put it in the appropriate libraries.  P1 is the
$! name of the module to compile, with directory if needed.
$!
$!----------------------------------------------------------------------
$!
$ write sys$output "Compiling ''p1'"
$ name = f$parse(p1,,,"NAME")
$ if (f$getsyi("cpu").eq.128)
$ then					! Alpha (it's always VAXC)
$   opt = "/standard=vaxc/tie/float=d_float/warn=(disable=protoscope)"
$ else					! VAX
$   opt = "/standard=portable"
$   if (f$locate("_VMS",name) .ne. f$length(name)) then opt = ""
$ endif
$!
$ cc/nodebug/nolist/include_dir=(v2$inc,tae$inc)'opt' 'p1'
$ library v2$rtl_olb 'name'
$ delete 'name'.obj;*
$!
$ cc/debug/noopt/nolist/include_dir=(v2$inc,tae$inc)'opt' 'p1'
$ library v2$rtl_debug 'name'
$ delete 'name'.obj;*
$!
$ exit
