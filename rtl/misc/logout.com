$!
$! The "logout" command is redefined by the "vicar" command to run this file.
$! If the current process is the top level in the process tree, it will
$! delete the temporary files (dtemp.com).
$!
$ if f$getjpi("", "owner") .eqs. "" then @v2$lib:dtemp
$ delete/sym/glob logout
$ logout
