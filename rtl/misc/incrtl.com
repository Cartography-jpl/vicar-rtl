$!----------------------------------------------------------------------
$!
$! This utility will insert a single RTL include into c$library, and copy
$! it to v2$lib.  Note that this is for old compatibility only, and should
$! not be necessary in the future (as the includes should come from v2$inc,
$! not c$library).  P1 is the name of the module to insert, with directory
$! if needed.
$!
$!----------------------------------------------------------------------
$!
$ write sys$output "*** Inserting ''p1'"
$!
$ copy 'p1' v2$lib
$ library/text c$library 'p1'
$!
$ exit
