$!----------------------------------------------------------------------
$!
$! This utility will assemble a single RTL macro module, both debug and
$! non-debug, and put it in the appropriate libraries.  P1 is the
$! name of the module to assemble, with directory if needed.
$!
$!----------------------------------------------------------------------
$!
$ write sys$output "Assembling ''p1'"
$ name = f$parse(p1,,,"NAME")
$!
$ macro/nodebug/nolist 'p1'
$ library v2$rtl_olb 'name'
$ delete 'name'.obj.*
$!
$ macro/debug/nolist 'p1'
$ library v2$rtl_debug 'name'
$ delete 'name'.obj.*
$!
$ exit
