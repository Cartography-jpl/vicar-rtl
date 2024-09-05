$!----------------------------------------------------------------------
$!
$! This utility builds a shareable image for the VICAR run-time library
$!
$! This utility may be run either on a VAX or an Alpha.  If you update
$! the transfer vectors, make sure you change both parts!
$!
$!----------------------------------------------------------------------
$!
$ write sys$output "Building v2$share"
$!
$ curdir = f$environment("default")
$!
$ set def v2$rtltop
$!
$ if (f$getsyi("cpu").eq.128) then goto alpha
$!
$!----------------------------------------------------------------------
$!
$! VAX/VMS version
$!
$!----------------------------------------------------------------------
$!
$ create v2trans.mar
	.title  VICSHAR VICAR RTL transfer vectors
	.ident  /1-01/

	.psect  $v2trans,exe,nowrt,nopic,shr,gbl

	.macro	trans	fname,cname
	.transfer	fname
	.mask   fname
	jmp     fname+2
	.transfer	cname
	.mask	cname
	jmp	cname+2
	.endm   trans

	.macro	trans1	uname
	.transfer	uname
	.mask   uname
	jmp     uname+2
	.endm   trans1

	trans1 xvzinit
	trans1 zvzinit
	trans1 zvpinit
	trans1 zv_rtl_init

	trans1 sc2for
	trans1 sc2for_array
	trans1 sfor2c
	trans1 sfor2c_array
	trans1 sfor2len
	trans1 sfor2ptr

	trans abend,zabend
	trans qprint,zqprint
	trans xladd,zladd
	trans xldel,zldel
	trans xlget,zlget
	trans xlgetlabel,zlgetlabel
	trans xlhinfo,zlhinfo
	trans xlinfo,zlinfo
	trans xlninfo,zlninfo
	trans xmove,zmove
	trans xvadd,zvadd
	trans xvbands,zvbands
	trans xvclose,zvclose
	trans xvcmdout,zvcmdout
	trans xvcommand,zvcommand
	trans xveaction,zveaction
	trans xvend,zvend
	trans xvfilpos,zvfilpos
	trans xvget,zvget
	trans xvintract,zvintract
	trans xvip,zvip
	trans xviparm,zviparm
	trans xviparmd,zviparmd
	trans xvipcnt,zvipcnt
	trans xvipone,zvipone
	trans xvipstat,zvipstat
	trans xviptst,zviptst
	trans xvmessage,zvmessage
	trans xvopen,zvopen
	trans xvp,zvp
	trans xvparm,zvparm
	trans xvparmd,zvparmd
	trans xvpblk,zvpblk
	trans xvpclose,zvpclose
	trans xvpcnt,zvpcnt
	trans xvpixsizeu,zvpixsizeu
	trans xvpixsize,zvpixsize
	trans xvpone,zvpone
	trans xvpopen,zvpopen
	trans xvpout,zvpout
	trans xvpstat,zvpstat
	trans xvptst,zvptst
	trans xvread,zvread
	trans xvselpi,zvselpi
	trans xvsfile,zvsfile
	trans xvsignal,zvsignal
	trans xvsize,zvsize
	trans xvsptr,zvsptr
	trans xvtpinfo,zvtpinfo
	trans xvtpmode,zvtpmode
	trans xvtpset,zvtpset
	trans xvtrans,zvtrans
	trans xvtrans_in,zvtrans_in
	trans xvtrans_inu,zvtrans_inu
	trans xvtrans_out,zvtrans_out
	trans xvtrans_set,zvtrans_set
	trans xvunit,zvunit
	trans xvwrit,zvwrit

; The following have been added since GSMATCH (4,1).  Sort them in to the
; main list at the next major GSMATCH increment.

	trans xvhost,zvhost		; (4,2)
	trans xvpixsizeb,zvpixsizeb	; (4,3)
	trans xvtrans_inb,zvtrans_inb	; (4,3)
	trans xlpinfo,zlpinfo		; (4,3)
	trans xvfilename,zvfilename	; (4,4)
	trans xvqout,zvq_out		; (4,5)
	trans1 v2param_count_elements	; (4,6)
	trans1 v2param_find_entry	; (4,6)
	trans1 v2param_get_file		; (4,6)
	trans1 v2param_get_one_value	; (4,6)
	trans1 v2param_remove_file	; (4,6)
	trans xvselpiu,zvselpiu		; (4,7)
        trans xvplabel,zvplabel         ; (4,8)
        trans xvplabel2,zvplabel2       ; (4,9)

	trans1 v2_sc2for
	trans1 v2_sc2for_array
	trans1 v2_sfor2c
	trans1 v2_sfor2c_array
	trans1 v2_sfor2len
	trans1 v2_sfor2ptr

	.end
$!
$ macro v2trans
$!
$ create v2share_vax_build.opt
v2trans
gsmatch=lequal,4,5		! 1st=major, 2nd=minor
$!
$ link/share=v2$share/map=v2share_vax.map/cross -
	v2share_vax_build/options,-
	v2$rtl_olb/lib,-
	v2$olb:shvic.olb/lib,-
	tae$shr_exe/opt,-
	$taelib:crtl/opt
$!
$ goto done
$!
$!----------------------------------------------------------------------
$!
$! Alpha/VMS version
$!
$! The symbol vectors must match exactly the number and order of the
$! VAX shareable image transfer vectors.  One exception: the first vector
$! must be SYMBOL_VECTOR=(spare), then the rest follow.
$!
$! If the transfer vectors are changed, the IIF file (v2$olb:v2$share.iif)
$! must be regenerated.  To do so, copy the VAX v2$share to the Alpha in
$! a work directory, call it v2share_vax.exe.  Then, do the following:
$! $ vest/sif v2share_vax		! Creates a .SIF file
$! $ vest v2share_vax			! Uses .SIF to create a .IIF file
$! $ rename v2share_vax.iif v2$share.iif
$! Then, edit v2$share.iif and change the third line, which starts:
$!	Image "V2SHARE_VAX", ...
$! to read:
$!	Image "V2$SHARE", ...
$! (where the "..." represents other stuff you don't change).
$! Then, redeliver v2$share.iif and put it in v2$olb.  You can ignore
$! errors from the "vest" runs since we're not using the executable anyway.
$!
$!----------------------------------------------------------------------
$!
$alpha:
$!
$ create v2share_axp_build.opt
gsmatch=lequal,4,4		! 1st=major, 2nd=minor
SYMBOL_VECTOR=(spare)
!
SYMBOL_VECTOR=(xvzinit=PROCEDURE)
SYMBOL_VECTOR=(zvzinit=PROCEDURE)
SYMBOL_VECTOR=(xzsinit=PROCEDURE)
SYMBOL_VECTOR=(zzinit=PROCEDURE)
SYMBOL_VECTOR=(zvpinit=PROCEDURE)
SYMBOL_VECTOR=(zv_rtl_init=PROCEDURE)

SYMBOL_VECTOR=(sc2for=PROCEDURE)
SYMBOL_VECTOR=(sc2for_array=PROCEDURE)
SYMBOL_VECTOR=(sfor2c=PROCEDURE)
SYMBOL_VECTOR=(sfor2c_array=PROCEDURE)
SYMBOL_VECTOR=(sfor2len=PROCEDURE)
SYMBOL_VECTOR=(sfor2ptr=PROCEDURE)

SYMBOL_VECTOR=(abend=PROCEDURE,zabend=PROCEDURE)
SYMBOL_VECTOR=(qprint=PROCEDURE,zqprint=PROCEDURE)
SYMBOL_VECTOR=(xladd=PROCEDURE,zladd=PROCEDURE)
SYMBOL_VECTOR=(xldel=PROCEDURE,zldel=PROCEDURE)
SYMBOL_VECTOR=(xlget=PROCEDURE,zlget=PROCEDURE)
SYMBOL_VECTOR=(xlgetlabel=PROCEDURE,zlgetlabel=PROCEDURE)
SYMBOL_VECTOR=(xlhinfo=PROCEDURE,zlhinfo=PROCEDURE)
SYMBOL_VECTOR=(xlinfo=PROCEDURE,zlinfo=PROCEDURE)
SYMBOL_VECTOR=(xlninfo=PROCEDURE,zlninfo=PROCEDURE)
SYMBOL_VECTOR=(xmove=PROCEDURE,zmove=PROCEDURE)
SYMBOL_VECTOR=(xvadd=PROCEDURE,zvadd=PROCEDURE)
SYMBOL_VECTOR=(xvbands=PROCEDURE,zvbands=PROCEDURE)
SYMBOL_VECTOR=(xvclose=PROCEDURE,zvclose=PROCEDURE)
SYMBOL_VECTOR=(xvcmdout=PROCEDURE,zvcmdout=PROCEDURE)
SYMBOL_VECTOR=(xvcommand=PROCEDURE,zvcommand=PROCEDURE)
SYMBOL_VECTOR=(xveaction=PROCEDURE,zveaction=PROCEDURE)
SYMBOL_VECTOR=(xvend=PROCEDURE,zvend=PROCEDURE)
SYMBOL_VECTOR=(xvfilpos=PROCEDURE,zvfilpos=PROCEDURE)
SYMBOL_VECTOR=(xvget=PROCEDURE,zvget=PROCEDURE)
SYMBOL_VECTOR=(xvintract=PROCEDURE,zvintract=PROCEDURE)
SYMBOL_VECTOR=(xvip=PROCEDURE,zvip=PROCEDURE)
SYMBOL_VECTOR=(xviparm=PROCEDURE,zviparm=PROCEDURE)
SYMBOL_VECTOR=(xviparmd=PROCEDURE,zviparmd=PROCEDURE)
SYMBOL_VECTOR=(xvipcnt=PROCEDURE,zvipcnt=PROCEDURE)
SYMBOL_VECTOR=(xvipone=PROCEDURE,zvipone=PROCEDURE)
SYMBOL_VECTOR=(xvipstat=PROCEDURE,zvipstat=PROCEDURE)
SYMBOL_VECTOR=(xviptst=PROCEDURE,zviptst=PROCEDURE)
SYMBOL_VECTOR=(xvmessage=PROCEDURE,zvmessage=PROCEDURE)
SYMBOL_VECTOR=(xvopen=PROCEDURE,zvopen=PROCEDURE)
SYMBOL_VECTOR=(xvp=PROCEDURE,zvp=PROCEDURE)
SYMBOL_VECTOR=(xvparm=PROCEDURE,zvparm=PROCEDURE)
SYMBOL_VECTOR=(xvparmd=PROCEDURE,zvparmd=PROCEDURE)
SYMBOL_VECTOR=(xvpblk=PROCEDURE,zvpblk=PROCEDURE)
SYMBOL_VECTOR=(xvpclose=PROCEDURE,zvpclose=PROCEDURE)
SYMBOL_VECTOR=(xvpcnt=PROCEDURE,zvpcnt=PROCEDURE)
SYMBOL_VECTOR=(xvpixsizeu=PROCEDURE,zvpixsizeu=PROCEDURE)
SYMBOL_VECTOR=(xvpixsize=PROCEDURE,zvpixsize=PROCEDURE)
SYMBOL_VECTOR=(xvpone=PROCEDURE,zvpone=PROCEDURE)
SYMBOL_VECTOR=(xvpopen=PROCEDURE,zvpopen=PROCEDURE)
SYMBOL_VECTOR=(xvpout=PROCEDURE,zvpout=PROCEDURE)
SYMBOL_VECTOR=(xvpstat=PROCEDURE,zvpstat=PROCEDURE)
SYMBOL_VECTOR=(xvptst=PROCEDURE,zvptst=PROCEDURE)
SYMBOL_VECTOR=(xvread=PROCEDURE,zvread=PROCEDURE)
SYMBOL_VECTOR=(xvselpi=PROCEDURE,zvselpi=PROCEDURE)
SYMBOL_VECTOR=(xvsfile=PROCEDURE,zvsfile=PROCEDURE)
SYMBOL_VECTOR=(xvsignal=PROCEDURE,zvsignal=PROCEDURE)
SYMBOL_VECTOR=(xvsize=PROCEDURE,zvsize=PROCEDURE)
SYMBOL_VECTOR=(xvsptr=PROCEDURE,zvsptr=PROCEDURE)
SYMBOL_VECTOR=(xvtpinfo=PROCEDURE,zvtpinfo=PROCEDURE)
SYMBOL_VECTOR=(xvtpmode=PROCEDURE,zvtpmode=PROCEDURE)
SYMBOL_VECTOR=(xvtpset=PROCEDURE,zvtpset=PROCEDURE)
SYMBOL_VECTOR=(xvtrans=PROCEDURE,zvtrans=PROCEDURE)
SYMBOL_VECTOR=(xvtrans_in=PROCEDURE,zvtrans_in=PROCEDURE)
SYMBOL_VECTOR=(xvtrans_inu=PROCEDURE,zvtrans_inu=PROCEDURE)
SYMBOL_VECTOR=(xvtrans_out=PROCEDURE,zvtrans_out=PROCEDURE)
SYMBOL_VECTOR=(xvtrans_set=PROCEDURE,zvtrans_set=PROCEDURE)
SYMBOL_VECTOR=(xvunit=PROCEDURE,zvunit=PROCEDURE)
SYMBOL_VECTOR=(xvwrit=PROCEDURE,zvwrit=PROCEDURE)

! The following have been added since GSMATCH (4,1).  Sort them in to the
! main list at the next major GSMATCH increment.

SYMBOL_VECTOR=(xvhost=PROCEDURE,zvhost=PROCEDURE)		! (4,2)
SYMBOL_VECTOR=(xvpixsizeb=PROCEDURE,zvpixsizeb=PROCEDURE)	! (4,3)
SYMBOL_VECTOR=(xvtrans_inb=PROCEDURE,zvtrans_inb=PROCEDURE)	! (4,3)
SYMBOL_VECTOR=(xlpinfo=PROCEDURE,zlpinfo=PROCEDURE)		! (4,3)
SYMBOL_VECTOR=(xvfilename=PROCEDURE,zvfilename=PROCEDURE)	! (4,4)
SYMBOL_VECTOR=(xvqout=PROCEDURE,zvq_out=PROCEDURE)		! (4,5)
SYMBOL_VECTOR=(v2param_count_elements=PROCEDURE)		! (4,6)
SYMBOL_VECTOR=(v2param_find_entry=PROCEDURE)			! (4,6)
SYMBOL_VECTOR=(v2param_get_file=PROCEDURE)			! (4,6)
SYMBOL_VECTOR=(v2param_get_one_value=PROCEDURE)			! (4,6)
SYMBOL_VECTOR=(v2param_remove_file=PROCEDURE)			! (4,6)
SYMBOL_VECTOR=(xvselpiu=PROCEDURE,zvselpiu=PROCEDURE)		! (4,7)
SYMBOL_VECTOR=(xvplabel=PROCEDURE,zvplabel=PROCEDURE)           ! (4,8)
SYMBOL_VECTOR=(xvplabel2=PROCEDURE,zvplabel2=PROCEDURE)         ! (4,9)

SYMBOL_VECTOR=(v2_sc2for=PROCEDURE)
SYMBOL_VECTOR=(v2_sc2for_array=PROCEDURE)
SYMBOL_VECTOR=(v2_sfor2c=PROCEDURE)
SYMBOL_VECTOR=(v2_sfor2c_array=PROCEDURE)
SYMBOL_VECTOR=(v2_sfor2len=PROCEDURE)
SYMBOL_VECTOR=(v2_sfor2ptr=PROCEDURE)
$!
$ link/share=v2$share/map=v2share_axp.map/cross/nonative_only -
	v2share_axp_build/options,-
	v2$rtl_olb/lib,-
	v2$olb:shvic.olb/lib,-
	tae$shr_exe/opt
$!
$!----------------------------------------------------------------------
$!
$done:
$!
$ set def 'curdir'
$!
$ exit
