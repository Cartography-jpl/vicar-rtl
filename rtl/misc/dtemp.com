$!
$! delete Vicar temporary image files from the scratch disks and the
$! disk the user's login directory is on.
$!
$ mess = f$environment("message")
$ set mess/nofac/noid/nosev/notext	! turn off error messages
$!
$ SPEC = "Z" + F$LOGICAL("V2$PIDCODE")
$ HOMDIR = F$LOGICAL("SYS$LOGIN")
$ FILSPEC = HOMDIR - "]" + "...]*.''SPEC';*"
$!
$ DELETE 'FILSPEC'			! home directory
$!
$ FILSPEC = F$PARSE(HOMDIR,,,"DIRECTORY") - "]" + "...]*.''SPEC';*"
$!
$ DELETE scx1:'FILSPEC'			! scratch disks
$ DELETE scx2:'FILSPEC'
$ DELETE v2$scratch:*.'SPEC';*		! v2$scratch as well
$!
$ set mess 'mess'			! restore error messages
$ exit
