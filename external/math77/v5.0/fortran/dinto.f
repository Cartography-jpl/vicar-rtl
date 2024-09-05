      SUBROUTINE DINTO (JUMP,WORK)
C     .  Copyright (C) 1989, California Institute of Technology.
C     .  All rights reserved.  U. S. Government sponsorship under
C     .  NASA contract NAS7-918 is acknowledged.
C>> 1995-11-10 DINTO  Krogh  Fixed so char. data at col. 72 is not ' '.
C>> 1994-11-17 DINTO  Krogh  Rearranged parameter statments.
C>> 1994-11-14 DINTO  Krogh  Declared all vars.
c>> 1994-10-19 DINTO  Krogh  Changes to use M77CON
c>> 1994-07-07 DINTO  Snyder set up for CHGTYP.
C>> 1994-06-13 DINTO  Krogh -- Fixed value of LLOC3 "+2" not "+3".
C>> 1993-05-18 DINTO  Krogh -- Changed "END" to "END PROGRAM"
c>> 1993-04-29 DINTO  Krogh  Additions for Conversion to C.
C>> 1993-04-13 DINTO  Krogh  Minor changes for new MESS.
C>> 1992-05-28 DINTO  Krogh  Corrected minor problem in error message.
C>> 1992-04-08 DINTO  Krogh unused label 210, FDAT in MESS calls removed
C>> 1992-03-03 DINTO  Krogh converted to use message processor.
C>> 1991-09-20 DINTO  Krogh converted '(1)' dimensioning to '(*)'.
C>> 1988-11-17 DINTO  Snyder  Remove unnecessary specializer comments.
C>> 1988-06-07 DINTO  Snyder  Correct format statement 20.
C>> 1987-11-19 DINTO  Snyder  Initial code.
C
C     PRINT SOMETHING FOR DINTA.
c
c--D replaces "?": ?INT, ?INTA, ?intc, ?intec, ?INTO, ?MESS
C
C     *****     DESCRIPTION OF VARIABLES WITH SPECIAL USE HERE *********
C
C AACUM  First floating point variable in saved common area.
C ACUM   Double precision result accumulated.
C AINIT  First floating point variable in the unsaved common area.
C DISCHK Nonzero if checking for a discontinuity.  This and WORRY are
C        only printed if DISCHK is not zero.
C DMESS  Used when message contains floating point.
C FDAT   Temporary storage for floating point variables to be printed.
C FNSAV  Array equivalenced to AINIT, access to unsaved floating point.
C FSAV   Array equivalenced to AACUM, access to saved floating point.
C I      First integer in the saved common area.
C IDAT   Temporary storage for integer variables to be printed.
C IDFLT1 Parameter, see description of MESSA.
C IDINT1 Parameter, see description of MESSA.
C IDINT2 Parameter, see description of MESSA.
C IF1    Pointer to floating point variable, see MESSA.
C II1    First integer extracted from MESSF, see MESSA.
C II2    Second integer extracted from MESSF, see MESSA.
C INSAV  Array equivalenced to KDIM for access to unsaved integers.
C ISAV   Array equivalenced to I for access to saved integers.
C JUMP   Input variable defining what is to be printed.
C    = 1 Panel boundaries
C    = 2 Header (if needed) and K, ERRI, ERR, EPSMIN, EPS, RE, ...
C    = 3 No action.
C    = 4 Note that direction of accumulation has been reversed.
C    = 5 x's, f's, difference lines, etc.
C    = 6 Estimated errors during a search.
C    = 7 Panel boundaries after a disconinuity found.
C    = 8 New round off level after noise detected.
C    = 9 Indicate a nonintegrable singularity.
C    =10 Note that abscissae have coalesced.
C    =11 Data for an accepted answer.
C    =12 Step size from an initial interval search.
C    =13 Message that there appears to be a discontinuity.
C KDIM   First integer variable in the unsaved common area.
C L??    Many names starting with L identify the locations of variables
C        the common blocks.  The letters following the L serve to
C        identify which variable in the common blocks.  All of these
C        variables are defined in a block of parameter statements
C        separated from others by header and ending comments.
C LENDT  Number of x's, f,s and differences to be printed.
C LIDAT  Current location for saving integers in IDAT.
C LMACT  Current base location for saving data in MACT.
C LMESS  Value taken from MESSL and updated depending on MESSF, see
C        description of MESSA below.  If > 0, provides an index
C        into MESSA for the action.  After getting actions, if this
C        is > 1, it gives the next value of LMESS; else data is printed
C        and a return is made with printout of exterior abscissae
C        following the other data if LMESS is 1.  If LMESS < 0, -LMESS
C        is packed data like that in MESSA, except that in this case
C        II1 gives the next value to be assigned to LMESS, and both the
C        integer and floating point data is to be printed from the
C        unsaved common block.
C LTXTA? All names of this type were generated by running the data in
C        DINTO.ERR through PMESS.  These are all parameters that define
C        where various text starts in MTXTAx.
C MACT   Array used to store actions for calls to MESS.
C MACTAR As for MACT except for printing a vector.
C MACTH  As for MACT except only need pointer to one text entry.
C MACTMA As for MACT except for printing the array containing x's, f's,
C        and differences.
C MECONT Parameter defined in MESS, means return, print is to continue.
C MEFDAT Parameter defined in MESS, means set index for next floating
C        point item to print.
C MEFMAT Parameter defined in MESS, means print a matrix.
C MEFVEC Parameter defined in MESS, means print a vector.
C MENTXT Parameter defined in MESS, as for MEFDAT execpt for text.
C MERET  Parameter defined in MESS, means print buffers and return.
C MESAdd Where dd is one or two digits.  These define parameters that
C        are used in the data statement for MESSA, see comments there.
C MESLd  As for the above, execpt used in assigning value to MESSL, when
C        those value are < 0.
C MESS   Message routine, when no floating point is to be printed.
C MESSA  An array containing packed integers that define the actions to
C        be taken.  Entries in MESSA have the form IDINT1*II1 +
C        IDINT2*II2 + IDFLT1*IF1 + t, where in every case the first
C        multiplyer in a product is a parameter which is a power of 2,
C        and the second defines an action as follows:
C    II1  first integer to be printed.  In most cases, LMESS increases
C         by 1, but if this is > 31, LMESS is set to II1 / 32 - 1 and
C         II1 is replaced by mod(II1,32).  If the result is:
C       31 Print out of final results (some vars. are always double
C          precision).
C       30 Check on NDIM to see if KDIM should print.
C       29 Check on DISCHK to decide if it and WORRY are to print.
C      >12 Subtract 12, and use to get an integer from unsaved common.
C      <12 Use to get an integer from the saved common area.
C    II2  Second integer.  Treated like the first, except always < 25.
C    IF1  Used to get a floating point number from the saved common
C         block.
C      t  Location of text for message in MTXTAA.
C MESSF  Obtained from MESSA or -MESSL(LMESS), see above.
C MESSL  Maps value of JUMP into actions.  If > 0, gives an index into
C        MESSA, else the negative defines the value of MESSF.  Also see
C        LMESS above.
C METEXT Parameter defined in MESS, means print as defined by MTXTAx.
C MTXTAx Character arrays containing text and instructions for the
C        printing of messages by MESS.
C NDIM   Number of dimensions in the total integral.
C NEEDH  .TRUE. if heading needed when JUMP is 2, else is .FALSE.
C DMESS  Used when message contains single precision floating point.
C WORK   Array passed in containing exterior abscissae.
C
C     *****     PROGRAM VARIABLES     **********************************
C
      INTEGER JUMP
      DOUBLE PRECISION             WORK(*)
      INTEGER         IDAT(5), INSAV(7), ISAV(7), MESSL(13), MESSA(15)
      DOUBLE PRECISION             FDAT(4), FNSAV(11), FSAV(169)
      integer LABSC, LDELMN, LDELTA, LDISCX, LEPS, LERRI, LINC,         
     *    LISTOP, LK, LKAIMT, LKDIM, LLOC1, LLOC3, LNFEVA, LNSUB, LPART,
     *    LPHISU, LRNDC, LSEARC, LTPS, LWHERE, LWORRY, LX, LXJ, LXT,    
     *    IDFLT1, IDINT2, IDINT1, MESA1, MESA2, MESA3, MESA4, MESA5,    
     *    MESA6, MESA7, MESA8, MESA9, MESA10, MESA11, MESA12, MESA13,   
     *    MESA14, MESA15, MESA16, MESL3, MESL5, MESL6, MESL8, MESL9,    
     *    MESL13, LMACT, LIDAT, LMESS, MESSF, II1, II2, IF1
C
C     *****     COMMON VARIABLES     ***********************************
C
C     COMMON /DINTNC/ CONTAINS VARIABLES NOT SEPARATELY SAVED FOR
C     EACH DIMENSION OF A MULTIPLE QUADRATURE.  COMMON /DINTC/
C     CONTAINS VARIABLES THAT MUST BE SAVED FOR EACH DIMENSION OF THE
C     QUADRATURE.  THE VARIABLES IN EACH COMMON BLOCK ARE STORED IN THE
C     ORDER - ALWAYS DOUBLE, DOUBLE IF DOUBLE PRECISION PROGRAM, DOUBLE
C     IF DOUBLE PRECISION PROGRAM AND EXPONENT RANGE OF DOUBLE AND
C     SINGLE VERY DIFFERENT, SINGLE, INTEGER, LOGICAL.  A PAD OF LOGICAL
C     VARIABLES IS INCLUDED AT THE END OF /DINTC/.  THE DIMENSION OF
C     THE PAD MAY NEED TO BE VARIED SO THAT NO VARIABLES BEYOND THE END
C     OF THE COMMON BLOCK ARE ALTERED.
C
C     DECLARATIONS OF COMMON /DINTNC/ VARIABLES.
C
      DOUBLE PRECISION AINIT, BINIT, FNCVAL, S, TP
      DOUBLE PRECISION FER, FER1, RELOBT, TPS, XJ, XJP
      INTEGER     FEA,       FEA1,      INC,       INC2,      IPRINT,   
     * ISTOP(2,2),JPRINT,    KDIM,      KK,        KMAXF,     NDIM,     
     * NFINDX,    NFMAX,     NFMAXM,    OUT,       RELTOL,    REVERM,   
     * REVERS,    WHEREM
      LOGICAL NEEDH
C
C     DECLARATIONS OF COMMON /DINTC/ VARIABLES.
C
C     4 DOUBLE PRECISION VARIABLES
c--   Mask next line from code changes
      DOUBLE PRECISION ACUM, PACUM, RESULT(2)
C     139 $.TYPE.$ VARIABLES
      DOUBLE PRECISION                                                  
     * AACUM,     ABSCIS,    DELMIN,    DELTA,     DIFF,      DISCX(2), 
     * END(2),    ERRINA,    ERRINB,    FAT(2),    FSAVE,               
     * FUNCT(24), F1,        F2,        LOCAL(4),  PAACUM,    PF1,      
     * PF2,       PHISUM,    PHTSUM,    PX,        SPACE(6),            
     * STEP(2),   START(2),  SUM,       T,         TA,        TASAVE,   
     * TB,        TEND,      WORRY(2),  X,         X1,                  
     * X2,        XT(17),    FT(17),    PHI(34)
c Note XT, FT, and PHI above are last, because they must be in adjacent
c locations in DINTO.
C     30 $DSTYP$ VARIABLES
      DOUBLE PRECISION                                                  
     * ABSDIF,    COUNT,     EDUE2A,    EDUE2B,    EP,        EPNOIZ,   
     * EPS,       EPSMAX,    EPSMIN,    EPSO,      EPSR,      EPSS,     
     * ERR,       ERRAT(2),  ERRC,      ERRF,      ERRI,      ERRT(2),  
     * ESOLD,     EXTRA,     PEPSMN,    RE,        RELEPS,    REP,      
     * REPROD,    RNDC,      TLEN,      XJUMP
C     29 INTEGER VARIABLES
      INTEGER     DISCF,     DISCHK,    ENDPTS,    I,         INEW,     
     * IOLD,      IP,        IXKDIM,    J,         J1,        J1OLD,    
     * J2,        J2OLD,     K,         KAIMT,     KMAX,      KMIN,     
     * L,         LENDT,     NFEVAL,    NFJUMP,    NSUB,      NSUBSV,   
     * NXKDIM,    PART,      SEARCH,    TALOC,     WHERE,     WHERE2
C     11 TO 18 LOGICALS (7 ARE PADDING).
      LOGICAL     DID1,      FAIL,      FATS(2),   FSAVED,    HAVDIF,   
     * IEND,      INIT,      ROUNDF,    XCDOBT(2), PAD(7)
C
C     THE COMMON BLOCKS.
C
      COMMON /DINTNC/                                                   
     * AINIT,  BINIT,  FNCVAL, S,      TP,     FER,    FER1,   RELOBT,  
     * TPS,    XJ,     XJP,    FEA,    FEA1,   KDIM,    INC,    INC2,   
     * ISTOP,  JPRINT, IPRINT, KK,     KMAXF,  NDIM,   NFINDX, NFMAX,   
     * NFMAXM, OUT,    RELTOL, REVERM, REVERS, WHEREM, NEEDH
      COMMON /DINTC/                                                    
     * ACUM,   PACUM,  RESULT
      COMMON /DINTC/                                                    
     * AACUM,  LOCAL,  ABSCIS, TA,     DELTA,  DELMIN, DIFF,   DISCX,   
     * END,    ERRINA, ERRINB, FAT,    FSAVE,  FUNCT,  F2,              
     * PAACUM, PF1,    PF2,    PHISUM, PHTSUM, PX,     SPACE,           
     * STEP,   START,  SUM,    T,      TASAVE, TB,     TEND,            
     * WORRY,  X1,     X2,     X,      F1,     COUNT,                   
     * XT, FT, PHI
      COMMON /DINTC/                                                    
     * ABSDIF, EDUE2A, EDUE2B, EP,     EPNOIZ, EPSMAX,                  
     * EPSO,   EPSR,   EPSS,   ERRAT,  ERRC,   ERRF,                    
     * ERRT,   ESOLD,  EXTRA,  PEPSMN, RELEPS, REP,                     
     * RNDC,   TLEN,   XJUMP,                                           
     * ERRI,   ERR,    EPSMIN, EPS,    RE,     REPROD
      COMMON /DINTC/                                                    
     * DISCF,  DISCHK, ENDPTS, INEW,   IOLD,   IP,     IXKDIM,          
     * J,      J1,     J1OLD,  J2,     J2OLD,  KMAX,                    
     * KMIN,   L,      LENDT,  NFEVAL, NFJUMP, NSUBSV, NXKDIM,          
     * TALOC,  WHERE2,                                                  
     * I,      K,      KAIMT,  NSUB,   PART,   SEARCH, WHERE
      COMMON /DINTC/                                                    
     * DID1,   FAIL,   FATS,   FSAVED, HAVDIF, IEND,   INIT,   ROUNDF,  
     * XCDOBT, PAD
      SAVE /DINTNC/, /DINTC/
C
C     THE VARIABLES HERE DEFINE THE MACHINE ENVIRONMENT.  ALL ARE SET
C     IN DINTOP.  THE MEANING ATTACHED TO THESE VARIABLES CAN BE
C     FOUND BY LOOKING AT THE DEFINITIONS IN DINTOP.
      DOUBLE PRECISION                                                  
     *  EMEPS,  EEPSM8, EMEPSX, EDELM2, EDELM3, ESQEPS,                 
     *  ERSQEP, ERSQE6, ESQ2EP, EMINF,  ESMALL, ENZER,  EDELM1, ENINF
      COMMON /DINTEC/                                                   
     *  EMEPS,  EEPSM8, EMEPSX, EDELM2, EDELM3, ESQEPS,                 
     *  ERSQEP, ERSQE6, ESQ2EP, EMINF,  ESMALL, ENZER,  EDELM1, ENINF
      SAVE /DINTEC/
C
C     *****     EQUIVALENCE STATEMENTS     *****************************
C
      EQUIVALENCE (INSAV,KDIM), (FNSAV,AINIT), (ISAV,I), (FSAV,AACUM)
C
C     *****    Statements for Processing Messages **********************
C
      INTEGER MENTXT, MEFDAT, MECONT, MERET, MEEMES, METEXT, MEFVEC,    
     *    MEFMAT
      PARAMETER (MENTXT =23)
      PARAMETER (MEFDAT =25)
      PARAMETER (MECONT =50)
      PARAMETER (MERET  =51)
      PARAMETER (MEEMES =52)
      PARAMETER (METEXT =53)
      PARAMETER (MEFVEC =61)
      PARAMETER (MEFMAT =62)
      INTEGER MACT(26), MACTMA(7), MACTAR(6), MACTH(2), MACTER(5)
C Parameters defining locations in the common blocks.
      PARAMETER (LTPS=9)
      PARAMETER (LXJ=10)
c
      PARAMETER (LINC=2)
      PARAMETER (LISTOP=4)
      PARAMETER (LKDIM=1)
c
      PARAMETER (LABSC=6)
      PARAMETER (LDELMN=9)
      PARAMETER (LDELTA=8)
      PARAMETER (LDISCX=11)
      PARAMETER (LEPS=167)
      PARAMETER (LERRI=164)
      PARAMETER (LPHISU=48)
      PARAMETER (LRNDC=161)
      PARAMETER (LWORRY=66)
      PARAMETER (LX=70)
      PARAMETER (LXT=73)
c
      PARAMETER (LK=2)
      PARAMETER (LKAIMT=3)
      PARAMETER (LNSUB=4)
      PARAMETER (LPART=5)
      PARAMETER (LSEARC=6)
      PARAMETER (LWHERE=7)
      PARAMETER (LNFEVA=8)
c
      PARAMETER (LLOC1=2)
      PARAMETER (LLOC3=LLOC1+2)
c
C End of parameters defining locations in the common blocks.
C
c ********* Error message text ***************
c[Last 2 letters of Param. name]  [Text generating message.]
cAA NSUB=$I on ($F, $F)$B
cAB  DEL=$(E12.5) DELMIN=$(E09.3) $B
cAC EPS=$G PART=$I AIM=$I$B
cAD  DISCHK=$I WORRY=$F$B
cAE  KDIM=$I$B
cAF TA=$F$E
cAG $(I2) $(E11.4) $G $G $G $G $G $J$E
cAH **** Reverse Direction ****$E
cAI ISTOP=$I $I $I $I,  XJ=$F XJP=$F$B
cAJ XT$HFT$HPHI$HPHIT$E
cAK $#
cAL INC=$I INC2=$I  E=$F $B
cAM X=$F F1=$F COUNT=$F$E
cAN Round-Off = $F.  $B
cAO Apparent non-integrable singularity near $F.  $B
cAP Absiccae have coalesced.  WHERE=$I, DELMIN=$F, NSUB=$I$E
cAQ DELTA chosen by search = $F.$E
cAR Discontinuity in ($F, $F).$E
cAS Used $I function values -- the maximium.$E
c   $
cAT  K     ERRI        ERR       EPSMIN       EPS  $C
c           RE        REPROD  KDIM$E
c   $
cAU  PHISUM=$F PHTSUM=$F SEARCH=$I$E
c   $
cAV Accept result $I = $F, ACCUM=$F, ERR=$(E10.4), EPSMIN=$G,$C
c    KDIM=$I$E
c   $
cAW Abscissae for dimensions $I to $I: $B
c   $
cAX Really on ($F, $F) DEL=$(E12.5)     TA=$(E10.3) $E
c   $
cAY DINT$B
      integer LTXTAA,LTXTAB,LTXTAC,LTXTAD,LTXTAE,LTXTAF,LTXTAG,LTXTAH,  
     * LTXTAI,LTXTAJ,LTXTAK,LTXTAL,LTXTAM,LTXTAN,LTXTAO,LTXTAP,LTXTAQ,  
     * LTXTAR,LTXTAS,LTXTAT,LTXTAU,LTXTAV,LTXTAW,LTXTAX,LTXTAY
      parameter (LTXTAA=  1,LTXTAB= 22,LTXTAC= 54,LTXTAD= 77,LTXTAE= 98,
     * LTXTAF=108,LTXTAG=115,LTXTAH=149,LTXTAI=178,LTXTAJ=212,          
     * LTXTAK=231,LTXTAL=233,LTXTAM=256,LTXTAN=277,LTXTAO=296,          
     * LTXTAP=344,LTXTAQ=400,LTXTAR=430,LTXTAS=459,LTXTAT=  1,          
     * LTXTAU=  1,LTXTAV=  1,LTXTAW=  1,LTXTAX=  1,LTXTAY=  1)
      character MTXTAA(2) * (250)
      character MTXTAB(1) * (80)
      character MTXTAC(1) * (32)
      character MTXTAD(1) * (67)
      character MTXTAE(1) * (37)
      character MTXTAF(1) * (50)
      character MTXTAG(1) * (6)
      PARAMETER (IDFLT1=1024)
      PARAMETER (IDINT2=IDFLT1*256)
      PARAMETER (IDINT1=IDINT2*32)
      PARAMETER (MESA1=IDINT1*LNSUB+IDFLT1*LLOC1+LTXTAA)
      PARAMETER (MESA2=IDFLT1*LDELTA+LTXTAB)
      PARAMETER (MESA3=IDINT1*LPART+IDINT2*LKAIMT+IDFLT1*LEPS+LTXTAC)
      PARAMETER (MESA4=29*IDINT1+IDFLT1*LWORRY+LTXTAD)
      PARAMETER (MESA5=30*IDINT1+LTXTAE)
      PARAMETER (MESA6=(32+LK)*IDINT1+(12+LKDIM)*IDINT2+IDFLT1*LERRI+   
     *   LTXTAG)
      PARAMETER (MESA7=32*IDINT1+LTXTAH)
      PARAMETER (MESA8=32*IDINT1+IDFLT1*LX+LTXTAM)
      PARAMETER (MESA9=IDINT1*(96+LNSUB)+IDFLT1*LDISCX+LTXTAA)
      PARAMETER (MESA10=64*IDINT1+IDFLT1*LRNDC+LTXTAN)
      PARAMETER (MESA11=64*IDINT1+IDFLT1*LLOC3+LTXTAO)
      PARAMETER (MESA12=IDINT1*(64+LWHERE)+IDINT2*LNSUB+IDFLT1*LDELMN+  
     *   LTXTAP)
      PARAMETER (MESA13=31*IDINT1)
      PARAMETER (MESA14=32*IDINT1+IDFLT1*LDELTA+LTXTAQ)
      PARAMETER (MESA15=64*IDINT1+IDFLT1*LDISCX+LTXTAR)
      PARAMETER (MESA16=IDINT1*(64+LNFEVA)+LTXTAS)
C
      PARAMETER (MESL3=-(16*IDINT1+5*IDINT2+57))
      PARAMETER (MESL5=-(IDINT2*LISTOP+IDFLT1*LXJ+LTXTAI))
      PARAMETER (MESL6=-(8*IDINT1+IDINT2*LINC+IDFLT1*LTPS+LTXTAL))
      PARAMETER (MESL8=-(10*IDINT1+1*IDINT2+24))
      PARAMETER (MESL9=-(11*IDINT1+6*IDINT2+68))
      PARAMETER (MESL13=-(15*IDINT1+3*IDINT2+25))
      data MTXTAA/'NSUB=$I on ($F, $F)$B DEL=$(E12.5) DELMIN=$(E09.3) $B
     *EPS=$G PART=$I AIM=$I$B DISCHK=$I WORRY=$F$B KDIM=$I$BTA=$F$E$(I2)
     * $(E11.4) $G $G $G $G $G $J$E**** Reverse Direction ****$EISTOP=$I
     * $I $I $I,  XJ=$F XJP=$F$BXT$HFT$HPHI$HPHIT$E$#INC=$I INC2=$I  E='
     *,'$F $BX=$F F1=$F COUNT=$F$ERound-Off = $F.  $BApparent non-integr
     *able singularity near $F.  $BAbsiccae have coalesced.  WHERE=$I, D
     *ELMIN=$F, NSUB=$I$EDELTA chosen by search = $F.$EDiscontinuity in$
     * ($F, $F).$EUsed $I function values -- the maximium.$E'/
      data MTXTAB/' K     ERRI        ERR       EPSMIN       EPS       $
     *   RE        REPROD  KDIM$E'/
      data MTXTAC/' PHISUM=$F PHTSUM=$F SEARCH=$I$E'/
      data MTXTAD/'Accept result $I = $F, ACCUM=$F, ERR=$(E10.4), EPSMIN
     *=$G, KDIM=$I$E'/
      data MTXTAE/'Abscissae for dimensions $I to $I: $B'/
      data MTXTAF/'Really on ($F, $F) DEL=$(E12.5)     TA=$(E10.3) $E'/
      data MTXTAG/'DINT$B'/
C           JUMP = 1  2      3  4      5      6  7      8      9
      DATA MESSL / 1, 6, MESL3, 7, MESL5, MESL6, 9, MESL8, MESL9,       
     *    12, 13, 14, MESL13 /
C  JUMP = 10  11  12      13
 
      DATA MESSA / MESA1, MESA2, MESA3, MESA4, MESA5, MESA6, MESA7,     
     *   MESA8, MESA9, MESA10, MESA11, MESA12, MESA13, MESA14, MESA15 /
 
      DATA MACT / MENTXT, 0, MEFDAT, 0, METEXT,                         
     *            MENTXT, 0, MEFDAT, 0, METEXT,                         
     *            MENTXT, 0, MEFDAT, 0, METEXT,                         
     *            MENTXT, 0, MEFDAT, 0, METEXT,                         
     *            MENTXT, 0, MEFDAT, 0, METEXT, MERET/
      DATA MACTMA / MEFMAT, 17, 0, 4, LTXTAK, LTXTAJ, MERET /
      DATA MACTAR / METEXT, MEFDAT, 0, MEFVEC, 0, MERET /
      DATA MACTH / METEXT, MERET /
      DATA MACTER / MEEMES, 0, 0, -1, MECONT /
C
C     *****     EXECUTABLE STATEMENTS     ******************************
C
      IF(JUMP .EQ. 2)THEN
         IF (NEEDH) CALL MESS(MACTH, MTXTAB, IDAT)
         NEEDH = .FALSE.
      ELSE
         NEEDH = .TRUE.
      END IF
      LMACT = 2
      LIDAT = 1
      LMESS = MESSL(JUMP)
      IF(LMESS .LT. 0)THEN
C                         Output data from the unsaved common block
         MESSF = -LMESS
         GO TO 20
      END IF
   10 MESSF = MESSA(LMESS)
   20 II1 = MESSF / IDINT1
      MESSF = MESSF - II1 * IDINT1
      II2 = MESSF / IDINT2
      MESSF = MESSF - II2 * IDINT2
      IF1 = MESSF / IDFLT1
      MACT(LMACT) = MESSF - IF1 * IDFLT1
      MACT(LMACT+2) = IF1
      IF(LMESS .LE. 0)THEN
         LMESS = II1
      IF(IF1 .EQ. 0)THEN
            IF (LMESS .EQ. 0) RETURN
            MACTER(2) = MACT(LMACT)
            MACTER(3) = II2
            CALL MESS(MACTER, MTXTAG, IDAT)
            NDIM = NDIM + 100
            GO TO 10
      END IF
         MACT(6) = MECONT
         CALL DMESS(MACT, MTXTAA, INSAV(II2), FNSAV)
         MACT(6) = MENTXT
         IF (IF1 .NE. LXJ) GO TO 10
         CALL DMESS(MACTH, MTXTAC, ISAV(LSEARC), FSAV(LPHISU))
         MACTMA(3) = LENDT
         CALL DMESS(MACTMA, MTXTAA, IDAT, FSAV(LXT))
         RETURN
      ELSE
      IF(II1 .GE. 32)THEN
            LMESS = II1 / 32
            II1 = II1 - 32*LMESS
            LMESS = LMESS - 1
      ELSE
            LMESS = LMESS + 1
      END IF
      IF(II1 .NE. 0)THEN
      IF(II1 .GT. 12)THEN
      IF(II1 .GE. 28)THEN
                  GO TO (100, 110, 120), II1-28
                  LMESS = 1
                  GO TO 220
  100             IF (DISCHK .EQ. 0) GO TO 10
                     IDAT(LIDAT) = DISCHK
                     MACT(LMACT+2) = IF1 + PART - 1
                     GO TO 200
  110             LMESS = 1
                     IF (NSUB .NE. 0) LMESS = -1
                     IF (NDIM .EQ. 1) GO TO 220
                     IDAT(LIDAT) = KDIM
                     GO TO 200
C Take care of stuff that is double precision in single precision code.
  120             FDAT(1) = RESULT(I)
                     FDAT(2) = ACUM
                     FDAT(3) = ERR
                     FDAT(4) = EPSMIN
                     IDAT(1) = I
                     IDAT(2) = KDIM
                     CALL DMESS(MACTH, MTXTAD, IDAT, FDAT)
                     GO TO 230
      ELSE
                  IDAT(LIDAT) = INSAV(II1-12)
      END IF
      ELSE
               IDAT(LIDAT) = ISAV(II1)
      END IF
      IF(II2 .NE. 0)THEN
               LIDAT = LIDAT + 1
      IF(II2 .GT. 12)THEN
                  IDAT(LIDAT) = INSAV(II2-12)
      ELSE
                  IDAT(LIDAT) = ISAV(II2)
      END IF
      END IF
            LIDAT = LIDAT + 1
      END IF
      END IF
  200 LMACT = LMACT + 5
      IF (LMESS .GT. 1) GO TO 10
  220 MACT(LMACT-1) = MERET
      IF (NDIM .GT. 100) MACT(LMACT-1) = MECONT
      CALL DMESS(MACT, MTXTAA, IDAT, FSAV)
      MACT(LMACT-1) = MEFDAT
      IF (LMESS .EQ. 0) RETURN
      IF(LMESS .LT. 0)THEN
         ABSCIS = abs(LOCAL(4) - LOCAL(3))
         CALL DMESS(MACTH, MTXTAF, IDAT, FSAV(LLOC3))
      END IF
  230 IF (NDIM .LE. KDIM) RETURN
      LMACT = 1
      IF(NDIM .GT. 100)THEN
         NDIM = NDIM - 100
         IF (NDIM .LE. KDIM) LMACT = 2
      END IF
      IDAT(2) = NDIM
      IDAT(1) = KDIM+1
      MACTAR(3) = IDAT(1)
      MACTAR(5) = -NDIM
      CALL DMESS(MACTAR(LMACT), MTXTAE, IDAT, WORK)
      RETURN
      END        
