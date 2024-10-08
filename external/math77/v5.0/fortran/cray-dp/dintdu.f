      SUBROUTINE DINTDU
C     .  Copyright (C) 1989, California Institute of Technology.
C     .  All rights reserved.  U. S. Government sponsorship under
C     .  NASA contract NAS7-918 is acknowledged.
c>> 1994-10-19 DINTDU  Krogh  Changes to use M77CON
c>> 1994-08-19 DINTDU  Snyder correct "middle" that's really at alocal
c>> 1994-07-07 DINTDU  Snyder set up for CHGTYP.
C>> 1993-05-18 DINTDU  Krogh -- Changed "END" to "END PROGRAM"
C>> 1987-11-20 DINTDU Snyder  Initial code.
C
C     THIS SUBROUTINE UPDATES DIFFERENCE LINES FOR DINTA DURING
C     THE SEARCHES.
c
c--S replaces "?": ?INTA, ?intc, ?INTDU, ?intec
C
C     *****     INTERNAL AND COMMON VARIABLES   ************************
C
C EPSCOR  IS A CORRECTION TO BE ADDED ONTO EPSMIN.
      REAL             EPSCOR
C FATA    THE FUNCTION VALUE AT THE ALOCAL END OF THE INTERVAL.
C FATB    THE FUNCTION VALUE AT THE BLOCAL END OF THE INTERVAL.
      REAL             FATA,FATB
C PHIT    IS THE BACKWARD DIFFERENCE LINE.
      REAL             PHIT(17)
C
C     *****    COMMON STORAGE ******************************************
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
      REAL             AINIT, BINIT, FNCVAL, S, TP
      REAL             FER, FER1, RELOBT, TPS, XJ, XJP
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
      REAL                                                              
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
      REAL                                                              
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
      REAL                                                              
     *  EMEPS,  EEPSM8, EMEPSX, EDELM2, EDELM3, ESQEPS,                 
     *  ERSQEP, ERSQE6, ESQ2EP, EMINF,  ESMALL, ENZER,  EDELM1, ENINF
      COMMON /DINTEC/                                                   
     *  EMEPS,  EEPSM8, EMEPSX, EDELM2, EDELM3, ESQEPS,                 
     *  ERSQEP, ERSQE6, ESQ2EP, EMINF,  ESMALL, ENZER,  EDELM1, ENINF
      SAVE /DINTEC/
C
C     *****    EQUIVALENCE STATEMENTS    *******************************
C
      EQUIVALENCE (PHI(18),PHIT)
      EQUIVALENCE (FAT(1),FATA), (FAT(2),FATB)
C
C     *****    PROCEDURES     ******************************************
C
      IF (WHERE-5) 200,40,10
C
C     UPDATE BY ADDING A FUNCTION VALUE IN THE MIDDLE.
C
10    HAVDIF=.FALSE.
      IF(NFEVAL.GT.NFJUMP+6)THEN
         if (l .le. 0) go to 70
         WHERE=0
         L=MIN(L,LENDT+1)
         IF (L.GE.LENDT+1) GO TO 180
         I=LENDT
         EPSCOR=0.5e0*(-ABS(XT(L-1)*(FT(L)-FT(L-1)))                    
     *   +ABS(XT(L-1)*(FNCVAL-FT(L-1)))+ABS(X*(FT(L)-FNCVAL)))
         EPSCOR=EPSCOR*EMEPS
         EPSCOR=EPSCOR+ABS(FNCVAL*RNDC*(XT(L)-XT(L-1)))
         IF (FEA.NE.0) EPSCOR=EPSCOR+ABS(0.5e0*ERRF*(XT(L)-XT(L-1)))
         EPSMIN=EPSMIN+MAX(EPSCOR,0.0E0)
20004 CONTINUE
            XT(I+1)=XT(I)
            FT(I+1)=FT(I)
            IF (I.EQ.L) GO TO 180
            I=I-1
      GO TO 20004
      END IF
      IF (L-LENDT-1) 80,50,50
C
C     UPDATE BY ADDING A FUNCTION VALUE ON THE BLOCAL END.
C
40    IF (WHERE2.EQ.1) GO TO 70
      FNCVAL=FATB
      L=LENDT+1
C     ADD ONE AT THE BLOCAL END
50    PHIT(L)=FNCVAL
      TP=1.0e0
      PHIT(LENDT)=FNCVAL-PHIT(LENDT)
      I=LENDT
60       TP=TP*((X-XT(I))/(XT(LENDT)-XT(I-1)))
         PHIT(I-1)=PHIT(I)-TP*PHIT(I-1)
         I=I-1
      IF (I.GE.2) GO TO 60
      GO TO 140
C
C     UPDATE BY ADDING A FUNCTION VALUE ON THE ALOCAL END.
C
70    FNCVAL=FATA
      L=1
C     ADD ONE IN THE MIDDLE OR AT THE ALOCAL END.
80    I=LENDT
      TP=PHIT(I)-FNCVAL
      S=XT(I)-X
20006 CONTINUE
         XT(I+1)=XT(I)
         FT(I+1)=FT(I)
         PHIT(I+1)=PHIT(I)
         PHI(I+1)=PHI(I)
         I=I-1
      IF (I.LT.L) GO TO 20007
         TP=TP+(S/(X-XT(I)))*(TP-PHIT(I))
      GO TO 20006
20007 CONTINUE
      PHIT(L)=TP
      IF(L.EQ.1)THEN
C        ADD ONE AT THE ALOCAL END.
         PHI(1)=FNCVAL
         TP=1.0e0
         PHI(2)=FNCVAL-PHI(2)
         I=2
110         TP=TP*((X-XT(I))/(XT(1)-XT(I+1)))
            PHI(I+1)=PHI(I)-TP*PHI(I+1)
            I=I+1
         IF (I.LE.LENDT) GO TO 110
         GO TO 180
      END IF
C     UPDATE PHIT FOR ADDING ONE IN THE INTERIOR.
      I=L-1
130      PHIT(I)=PHIT(I+1)+(S/(X-XT(I)))*(PHIT(I+1)-PHIT(I))
         I=I-1
      IF (I.GT.0) GO TO 130
C     UPDATE PHI FOR ADDING ONE IN THE INTERIOR OR AT THE BLOCAL END.
140   TP=PHI(1)-FNCVAL
      S=XT(1)-X
      I=2
      IF(L.NE.2)THEN
150         TP=TP+(S/(X-XT(I)))*(TP-PHI(I))
            I=I+1
         IF (I.LT.L) GO TO 150
      END IF
      PHI(L)=TP
      IF(L.NE.LENDT+1)THEN
C        I = L AT THIS TIME.
170         PHI(I+1)=PHI(I)+(S/(X-XT(I+1)))*(PHI(I)-PHI(I+1))
            I=I+1
         IF (I.LE.LENDT) GO TO 170
      END IF
180   LENDT=LENDT+1
      XT(L)=X
      FT(L)=FNCVAL
      IF(J1OLD.NE.18)THEN
         IF (J1OLD.GE.L) J1OLD=J1OLD+1
      END IF
      IF (J2OLD.GE.L) J2OLD=J2OLD+1
      IF (WHERE.NE.0) GO TO 230
C
C     REFORM THE DIFFERENCE LINES.
C
200   NFJUMP=NFEVAL
      PHI(1)=FT(1)
      PHIT(1)=FT(2)-FT(1)
      PHI(2)=-PHIT(1)
      PHIT(2)=FT(2)
      DO 20016 J=3,LENDT
         TP=1.0e0
         S=1.0e0
         PHIT(J)=FT(J)
      DO 20019 I=3,J
            PHIT(J-I+2)=PHIT(J-I+3)-TP*PHIT(J-I+2)
            TP=TP*((XT(J)-XT(J-I+2))/(XT(J-1)-XT(J-I+1)))
            S=S*((XT(1)-XT(J-I+2))/(XT(J)-XT(J-I+2)))
20019 CONTINUE
         PHIT(1)=PHIT(2)-TP*PHIT(1)
         PHI(J)=-S*PHIT(1)
20016 CONTINUE
C
230   CONTINUE
      RETURN
C
      END        
