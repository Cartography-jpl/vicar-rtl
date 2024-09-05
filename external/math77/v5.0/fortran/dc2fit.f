      SUBROUTINE DC2FIT(XI,YI,SDI,NXY,B,NB,W,NW,YKNOT,YPKNOT,           
     *         SIGFAC, IERR1)
C     .  Copyright (C) 1989, California Institute of Technology.
C     .  U. S. Government sponsorship under
C     .  NASA contract NAS7-918 is acknowledged.
c>> 1994-10-19 DC2FIT Krogh  Changes to use M77CON
c>> 1994-01-31 DC2FIT CLL Added test for SDI(i) .le. 0 when SDI(1) > 0.
c>> 1990-01-23 CLL Deleted ref to unused variable NX in call to IERM1
C>> 1989-10-20 CLL
C>> 1987-10-22 DC2FIT Lawson  Initial code.
c           Least squares fit to discrete data by a C-2 cubic spline.   00001300
c     ------------------------------------------------------------------
C     Algorithm and program designed by C.L.Lawson and R.J.Hanson.
c     The general approach but not the complete code is given in
C     'SOLVING LEAST SQUARES PROBLEMS', by Lawson and Hanson,
C     publ by Prentice-Hall, 1974.
C     Programming and later changes and corrections by Lawson,Hanson,
C     T.Lang, and D.Campbell, Sept 1968, Nov 1969, and Aug 1970.
C     Modified 1968 Sept 17 to provide C-2 continuity.
C     1974 5/21, C.L.Lawson, Fixed bug that caused ISEG to get too big.
C     Also changed to exit immidiately if B() array is not              00002300
C     strictly increasing.
c     1984 July 10. Modified for improved portability and to conform
c          to Fortran 77.  C. L. Lawson, JPL.
c          Added calls to the error message subrs.
c     7/23/87 CLL.  Added the IERR1 argument.
C     1989-10-20 CLL  Changed code so there are no RETURN statements in
c     Sftran procedures.  Previous code had such a RETURN that led to
c     a warning diagnostic from a Cray compiler due to unreachable
c     CONTINUE statements.  Also introduced "c--" lines for CHGTYP.
c     ------------------------------------------------------------------00003300
c                     SUBROUTINE ARGUMENTS
c
c  (XI(i),i=1,NXY)  [in]  Abcissas of data to be fitted.  Require
c        this data be ordered so X(i) .le. X(i+1).
c
c  (YI(i),i=1,NXY)  [in]  Ordinates of data to be fitted.
c
c  (SDI(i),i=1,NXY) [in]  User may use this array to assign an
c                     a priori standard deviation of error to each
c       YI(i) value.  The weighted fitting algorithm will take          00004300
c       account of these.  Optionally the user may set SDI(1) to
c       a negative value.  Then this subr will use ABS(SDI(1)) as
c       the standard deviation for each YI(i) value.  In this case
c       the SDI() array can be dimensioned SDI(1).
c       If SDI(1) = 0., the subr issues an error message and returns.
c
c  NXY  [in]  No. of data pairs, (XI(i), YI(i)), and no. of elts
c          in SDI() if SDI(1) is positive.  Require NXY .ge. 4.
c
c  (B(j),j=1,NB)  [in]  Breakpoints for the spline function,            00005300
c        including endpoints.  These breakpoints must be
c        strictly increasing:  B(j) .lt. B(j+1).
c        It is required that all abcissas, XI(i), lie in the
c        closed interval, [B(1), B(NB)].
c
c  NB  [in]  No. of breakpoints, including endpoints.
c         The no. of parameters in the least squares problem
c         will be NB + 2.
c         To have a nonsingular problem one must have
c         NXY .ge. NB + 2, and the distribution of the breakpoints      00006300
c         must not be too skewed relative to the data abcissas.
c       If singularity is detected, an error message will be
c       issued by the subr that solves the band matrix.
c
c  W()  [scratch]  Work space dimensioned W(NW,5).
c
c  NW  [in]  First dimension of W().   Must satisfy NW .ge. NB + 4
c          Let KMAX denote the max no. of data abcissas, XI(i),
c          in any one breakpoint interval, i.e. between B(j) and
c          B(j+1) for some j.   The subr will be more efficient         00007300
c          if NW is at least NB + 3 + KMAX.
c
c  (YKNOT(k) and YPKNOT(k),k=1,NB)  [out]  The subr will return
c       values defining the fitted C2 spline curve in these arrays.
c       These values and first derivatives of the fitted curve at the
c       knot abcissae.  YKNOT(j) = f(B(j))   and
c       YPKNOT(j) = fprime(B(j))  for  j = 1,...,NB.
c       The user can then evaluate the fitted curve at any point by
c       Hermite interpolation.  See subrs DHINT or SHINT.
c                                                                       00008300
c  SIGFAC  [out]  The subr sets SIGFAC to RNORM / sqrt(DOF) where
c      RNORM = sqrt( sum over i of [( (yfit(i) - YI(i))/SDI(i))**2])
c       and DOF = max(1, NXY - (NB+2))
C
c  IERR1  [out]  Error status indicator.  Note that IERR2 comes from
c         DBACC and IERR3 comes from DBSOL.
c
c        =    0 means no errors detected.
c        =  100 means  NB .lt. 2   .or.   NXY .lt. NB+2
c        =  200 means  B(I) .ge. B(I+1)                                 00009300
c        =  300 means  NW .lt. NB+4
c        =  400 means  XI(I-1) .gt. XI(I)
c        =  500 means  B(1) .gt. XI(1) .or. B(NB) .lt. XI(NXY)
c        =  600 means  Need larger dimension NW.
c        =  700 + IERR2 means IERR2 .ne. 0
c        =  800 + IERR2 means IERR2 .ne. 0
c        =  900 + IERR2 means IERR2 .ne. 0
c        = 1000 + IERR3 means IERR3 .ne. 0 due to singularity
c                       detected in DBSOL.
c        = 1100 means SDI(1) = zero.                                    00010300
c        = 1200 means SDI(1) > zero and SDI(i) .le. zero for some i.
c     ------------------------------------------------------------------
c             Important internal variables.
c
c     ISEG     Index of current spline segment, starting with 1 for
c              the first segment.
c              Also tells the band matrix subroutine the column index
c              of the least squares matrix with which the first col
c              of the new block of data in G() is to be associated.
C     KSIZE    Size of current block.                                   00011300
C     JPOINT   Current data pointer.
c     ------------------------------------------------------------------
c--D replaces "?": ?C2FIT, ?BACC, ?C2BAS, ?ERM1, ?ERV1, ?BSOL, ?TRC2C
c     Both versions use ERMSG, IERM1, IERV1
c     Lower level subrs needed: (D/S)HTCC, (D/S)NRM2, ERFIN
c     ------------------------------------------------------------------
      integer I, IERR1, IERR2, IERR3, IRNOW, ISEG, J, JPOINT, JTPREV
      integer K, KSIZE, N1, NB, NPARAM, NW, NXY
      integer NBAND, NBAND1
      integer N20006, N20018, N20027                                    00012300
      parameter(NBAND = 4, NBAND1 = NBAND+1)
      double precision XI(NXY), YI(NXY), SDI(NXY), B(NB), W(NW, 5)
      double precision YKNOT(NB), YPKNOT(NB), P(4), ONE, ZERO
      double precision DOF, RNORM, sdijp, SIGFAC, WT, WT1
      logical  NEWSEG, USEWT1
      parameter( ONE = 1.0D0, ZERO = 0.0D0)
c     ------------------------------------------------------------------
      IERR1 = 0
C
C          EXIT IMMEDIATELY IF NB .lt. 2  OR  NXY .LT NB+2  OR  IF THE  00013400
C          BREAKPOINTS ARE NOT STRICTLY INCREASING.
c
      NPARAM = NB+2
      IF (.NOT.( NB .lt. 2   .or.   NXY .lt. NPARAM)) GO TO 20003
         IERR1 = 100
         call IERM1('DC2FIT',IERR1,0,                                   
     *   'Require NB .ge. 2 and NXY .ge. NB+2', 'NB',NB,',')
         call IERV1('NXY',NXY,'.')
      ASSIGN 20004 TO NPR001
      GO TO 30001
20004    return
c                                                                       00014600
20003 N1 = NB-1
      I =1
      N20006=N1
      GO TO 20007
20005 I =I +1
20007 IF ((N20006-I ).LT.0) GO TO 20006
      IF (.NOT.(B(I) .ge. B(I+1))) GO TO 20009
          IERR1 = 200
          call IERM1('DC2FIT',IERR1,0,                                  
     *    'Require knots, B(I), to be strictly increasing.',            
     *    'I',I,',')
          call DERV1('B(I)',B(I),',')
          call DERV1('B(I+1)',B(I+1),'.')
      ASSIGN 20010 TO NPR001
      GO TO 30001
20010     return
20009 GO TO 20005                                                       00015900
C
C     Require NW .ge. NB+4
C
20006 IF (.NOT.(NW .lt. NB+4)) GO TO 20012
        IERR1 = 300
        call IERM1('DC2FIT',IERR1,0,'Require NW .ge. NB+4','NW',NW,',')
        call IERV1('NB',NB,'.')
      ASSIGN 20013 TO NPR001
      GO TO 30001
20013   return
C                                                                       00017000
c     ------------------------------------------------------------------
C                                       TEST SDI(1)
20012 IF(SDI(1) .lt. ZERO)THEN
         WT1 = -ONE/SDI(1)
         USEWT1 = .true.
      ELSEIF( SDI(1) .gt. ZERO)THEN
         USEWT1 = .false.
      ELSE
         IERR1 = 1100
         call ERMSG('DC2FIT',IERR1,0,'Require SD(1) .ne. Zero','.')     00018000
         return
      END IF
C
c                             Test ordering of XI() array.
c
      I=2
      N20018=NXY
      GO TO 20019
20017 I=I+1
20019 IF ((N20018-I).LT.0) GO TO 20018
      IF (.NOT.(XI(I-1) .gt. XI(I))) GO TO 20021
          IERR1 = 400
          call IERM1('DC2FIT',IERR1,0,                                  
     *    'Require abcissas, X(I), to be nondecreasing.',               
     *    'I',I,',')
          call DERV1('X(I-1)',XI(I-1),',')                              00019200
          call DERV1('X(I)',XI(I),'.')
      ASSIGN 20022 TO NPR001
      GO TO 30001
20022     return
20021 GO TO 20017
c
C                             TEST THE FIRST AND LAST BREAKPOINT
C                             FOR BRACKETING THE DATA ABCISSAS.
c
20018 IF (.NOT.(B(1) .gt. XI(1) .or. B(NB) .lt. XI(NXY))) GO TO 20024
        IERR1 = 500                                                     00020300
        call DERM1('DC2FIT',IERR1,0,                                    
     *     'Require B(1) .LE. X(1) and B(NB) .ge. XI(NXY)',             
     *     'B(1)',B(1),',')
        call DERV1('X(1)',XI(1),',')
        call DERV1('B(NB)',B(NB),',')
        call DERV1('X(NXY)',XI(NXY),'.')
      ASSIGN 20025 TO NPR001
      GO TO 30001
20025   return
C
C     BEGIN LOOP TO FORM EQUATIONS FOR C2 LEAST SQUARES FIT.
C
20024 IRNOW = 1                                                         00021600
      K = 1
      KSIZE = 0
      NEWSEG = .TRUE.
      ISEG = 1
      JPOINT =1
      N20027=NXY
      GO TO 20028
20026 JPOINT =JPOINT +1
20028 IF ((N20027-JPOINT ).LT.0) GO TO 20027
      IF (.NOT.( K .gt. NW )) GO TO 20030
            call DBACC(W, NW, NBAND, IRNOW, KSIZE, ISEG, JTPREV, IERR2)
      IF (.NOT.(IERR2 .ne. 0)) GO TO 20032
               IERR1 = 700 + IERR2
      ASSIGN 20033 TO NPR002                                            00022600
      GO TO 30002
20033          return
 
20032 IF (.NOT.(IRNOW .gt. NW)) GO TO 20035
               IERR1 = 600
               call IERM1('DC2FIT',IERR1,0,                             
     *            'Need larger dimension NW.','NW',NW,'.')
      ASSIGN 20036 TO NPR001
      GO TO 30001
20036          return
20035       K = IRNOW
            KSIZE = 0
c                                                                       00024000
20030 CONTINUE
20037 IF (.NOT.( XI(JPOINT) .gt. B(ISEG+1) )) GO TO 20038
            call DBACC(W, NW, NBAND, IRNOW, KSIZE, ISEG, JTPREV, IERR2)
      IF (.NOT.(IERR2 .ne. 0)) GO TO 20040
               IERR1 = 800 + IERR2
      ASSIGN 20041 TO NPR002
      GO TO 30002
20041          return
20040       KSIZE = 0
            K = IRNOW
            ISEG = ISEG + 1
            NEWSEG = .TRUE.                                             00025100
      GO TO 20037
c
20038 GO TO 30003
20042    K = K+1
         KSIZE = KSIZE + 1
c
      GO TO 20026
20027 call DBACC(W, NW, NBAND, IRNOW, KSIZE, ISEG, JTPREV, IERR2)
      IF (.NOT.(IERR2 .ne. 0)) GO TO 20044
               IERR1 = 900 + IERR2                                      00026100
      ASSIGN 20045 TO NPR002
      GO TO 30002
20045          return
C
C     ALL DATA POINTS HAVE BEEN PROCESSED.  CALL FOR SOLUTION.
C
20044 call DBSOL(1,W, NW, NBAND, IRNOW, JTPREV, W(1,NBAND1),            
     *           NPARAM, RNORM, IERR3)
      IF (.NOT.(IERR3 .ne. 0)) GO TO 20047
               IERR1 = 1000 + IERR2
               call ERMSG('DC2FIT',IERR1,0,                             
     *         'Singularity noted in DBSOL.','.')
      ASSIGN 20048 TO NPR001                                            00027400
      GO TO 30001
20048          return
 
20047 DOF = MAX(1, NXY - NPARAM)
      SIGFAC = RNORM / sqrt(DOF)
C
C                  TRANSFORM PARAMETERS TO Y,YPRIME BASIS
C
      call DTRC2C (B,NB,W(1,5),YKNOT,YPKNOT)
      RETURN
c                                                                       00028500
c     ------------------------------------------------------------------
C     PROCEDURE( BUILD ONE EQUATION )
c
30003 call DC2BAS(XI(JPOINT), 0, ISEG,NEWSEG,B,NB,P)
      IF( USEWT1)THEN
         WT = WT1
      ELSE
         sdijp = sdi(jpoint)
      IF(sdijp .gt. ZERO)THEN
            WT=ONE/sdijp                                                00029500
      ELSE
            IERR1 = 1200
            call ERMSG('DC2FIT',IERR1,0,                                
     *         'With SD(1) > 0  require all SD(I) > 0.', ',')
            call DERV1('SD(1)',SDI(1),'.')
            call IERV1('I',jpoint,',')
            call DERV1('SD(I)',sdijp,'.')
            return
      END IF
      END IF
      DO 20053 J = 1,4                                                  00030600
         W(K,J)=P(J)*WT
20053 CONTINUE
      W(K,5)=YI(JPOINT)*WT
      GO TO 20042
c     ------------------------------------------------------------------
C     procedure( ERROR IN _BACC )
 
30002    call ERMSG('DC2FIT',IERR1,0,                                   
     *   'Error detected in subroutine DBACC','.')
      ASSIGN 20056 TO NPR001
      GO TO 30001
20056 GO TO NPR002,(20033,20041,20045)                                  00031700
c     ------------------------------------------------------------------
c
C     PROCEDURE( SET YKNOT() & YPKNOT() TO ZERO )
C
30001 DO 20057 I=1,NB
               YKNOT(I)=ZERO
               YPKNOT(I)=ZERO
20057 CONTINUE
      GO TO NPR001,(20004,20010,20013,20022,20025,20036,20048,20056)
c     ------------------------------------------------------------------00032700
c
      END                     
