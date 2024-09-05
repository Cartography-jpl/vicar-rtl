$!
$! Extract process-unique code from last two digits of the PID for use
$! by VICAR temporary files.  The code becomes the "xx" in the ".Zxx"
$! temporary file extension.
$!
$ pcode = f$getjpi( "", "PID")
$ define v2$pidcode 'f$extract(f$length(pcode)-2, 2, pcode)'
$ exit
