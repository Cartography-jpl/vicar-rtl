      SUBROUTINE DINTNS (JUMPTO)
C     .  Copyright (C) 1989, California Institute of Technology.
C     .  All rights reserved.  U. S. Government sponsorship under
C     .  NASA contract NAS7-918 is acknowledged.
C>> 1994-11-14 DINTNS  Krogh  Declared all vars.
c>> 1994-10-19 DINTNS  Krogh  Changes to use M77CON
c>> 1994-07-07 DINTNS  Snyder set up for CHGTYP.
C>> 1993-05-18 DINTNS  Krogh -- Changed "END" to "END PROGRAM"
C>> 1987-11-19 DINTNS Snyder  Initial code.
C
C     REDUCE OR INCREASE NSUB, DEPENDING ON JUMPTO.  VALUES OF JUMPTO
C     ARE GIVEN BY THE PARAMETERS NSRA, NSRB, NSIA, NSIB IN DINTA.
C
c--D replaces "?": ?INTA, ?INTC, ?INTEC, ?INTNS, ?INTSM
C
C     *****     EXTERNAL REFERENCES     ********************************
C
C DINTSM  TO CALCULATE THE MINIMUM STEPSIZE.
      DOUBLE PRECISION DINTSM
C
C     *****     LOCAL VARIABLES     ************************************
C
C TDECR   IS AN ARITHMETIC STATEMENT FUNCTION DEFINED BELOW.
      DOUBLE PRECISION TDECR
C TINCR   IS AN ARITHMETIC STATEMENT FUNCTION DEFINED BELOW.
      DOUBLE PRECISION TINCR
C ZL1     IS AN ARGUMENT OF ARITHMETIC STATEMENT FUNCTIONS.
      DOUBLE PRECISION ZL1
      INTEGER JUMPTO
C
C     *****     COMMON VARIABLES     ***********************************
C
C ALOCAL  IS EQUIVALENT TO /DINTC/ LOCAL(1).
C BLOCAL  IS EQUIVALENT TO /DINTC/ LOCAL(2).
      DOUBLE PRECISION ALOCAL, BLOCAL
C FATAS, FATBS
C         ARE EQUIVALENCED TO FATS.
      LOGICAL FATAS, FATBS
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
c--D Next line special: S => D, X => Q, D => D
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
c locations in DINTC.
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
c        1       2       3     4        5       6       7        8
     W AINIT,  BINIT,  FNCVAL, S,      TP,     FER,    FER1,   RELOBT,
c       9      10       11      12      13       1       2        3
     X TPS,    XJ,     XJP,    FEA,    FEA1,   KDIM,    INC,    INC2,
c     4 (2,2)    8       9     10       11      12       13      14
     Y ISTOP,  JPRINT, IPRINT, KK,     KMAXF,  NDIM,   NFINDX, NFMAX,
c        15     16       17      18      19      20     21
     Z NFMAXM, OUT,    RELTOL, REVERM, REVERS, WHEREM, NEEDH
      COMMON /DINTC/                                                    
     * ACUM,   PACUM,  RESULT
      COMMON /DINTC/
c        1     2 (4)     6      7        8       9      10     11 (2)
     1 AACUM,  LOCAL,  ABSCIS, TA,     DELTA,  DELMIN, DIFF,   DISCX,
c     13 (2)     15      16    17 (2)   19     20 (24) 44
     2 END,    ERRINA, ERRINB, FAT,    FSAVE,  FUNCT,  F2,
c       45      46     47       48      49     50      51 (6)
     3 PAACUM, PF1,    PF2,    PHISUM, PHTSUM, PX,     SPACE,
c      57 (2)  59 (2)   61     62        63    64       65
     4 STEP,   START,  SUM,    T,      TASAVE, TB,     TEND,
c      66 (2)  68      69      70      71       72
     5 WORRY,  X1,     X2,     X,      F1,     COUNT,
c      73 (17) 90 (17) 107 (34)
     6 XT,     FT,     PHI
      COMMON /DINTC/
c       141     142    143     144      145     146
     1 ABSDIF, EDUE2A, EDUE2B, EP,     EPNOIZ, EPSMAX,
c       147     148     149    150 (2)  152     153
     2 EPSO,   EPSR,   EPSS,   ERRAT,  ERRC,   ERRF,
c     154 (2)   156     157     158     159    160
     3 ERRT,   ESOLD,  EXTRA,  PEPSMN, RELEPS, REP,
c       161     162     163
     4 RNDC,   TLEN,   XJUMP,
c       164    165      166    167    168       169
     5 ERRI,   ERR,    EPSMIN, EPS,    RE,     REPROD
      COMMON /DINTC/
c       170     171     172
     1 DISCF,  DISCHK, ENDPTS, INEW,   IOLD,   IP,     IXKDIM,          
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
      EQUIVALENCE (LOCAL(1),ALOCAL), (LOCAL(2),BLOCAL)
      EQUIVALENCE (FATS(1),FATAS), (FATS(2),FATBS)
C
C     *****     STATEMENT FUNCTIONS     ********************************
C
C TDECR   IS USED TO TRANSFORM AN ABSCISSA FROM THE CURRENT COORDINATE
C         SYSTEM TO ONE IN WHICH NSUB IS DECREMENTED BY A FACTOR OF 2.
      TDECR(ZL1)=TA+(ZL1-TA)*((ZL1-TA)/TB)
C TINCR   IS USED TO TRANSFORM AN ABSCISSA FROM THE CURRENT COORDINATE
C         SYSTEM TO ONE IN WHICH NSUB IS INCREMENTED BY A FACTOR OF 2.
      TINCR(ZL1)=TA+SIGN(SQRT(ABS(TB*(ZL1-TA))),TB)
C
C     *****     EXECUTABLE STATEMENTS     ******************************
C
      GO TO (2670,2680,2690,2700), JUMPTO
C
C     REDUCE NSUB
C
2670  BLOCAL=TDECR(BLOCAL)
2680  ALOCAL=TDECR(ALOCAL)
      WORRY(PART)=TDECR(WORRY(PART))
      ABSCIS=TDECR(START(PART)+SIGN(STEP(PART),TB))
      START(PART)=TDECR(START(PART))
      NSUB=2*(NSUB/4)
      GO TO 2710
C
C     INCREASE NSUB
C
2690  BLOCAL=TINCR(BLOCAL)
2700  ALOCAL=TINCR(ALOCAL)
      WORRY(PART)=TINCR(WORRY(PART))
      ABSCIS=TINCR(START(PART)+SIGN(STEP(PART),TB))
      START(PART)=TINCR(START(PART))
      NSUB=MAX0(NSUB+NSUB,2)
C
2710  FSAVED=.FALSE.
      FATAS=.FALSE.
      FATBS=.FALSE.
      ENDPTS=1
      STEP(PART)=SIGN(ABSCIS-START(PART),STEP(PART))
      DELTA=ABS(BLOCAL-ALOCAL)
      ABSDIF=0.5d0*DELTA
      TLEN=ABS(END(1)-START(1))
      IF (PART.EQ.2) TLEN=TLEN+ABS(END(2)-START(2))
      IF (DISCHK.EQ.0) DISCHK=-1
      DELMIN=DINTSM(ALOCAL)
C
      RETURN
C
      END        
