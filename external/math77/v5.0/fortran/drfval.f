      SUBROUTINE DRFVAL (X, Y, Z, RF, IERR)
c>>   1994-11-02 DRFVAL Krogh  Changes to use M77CON
c>>   1994-08-15 DRFVAL WV Snyder JPL use 2-arg min and max for C conv.
c>>   1993-02-23 DRFVAL WV Snyder JPL Split DRFVLX into a separate subr.
C>>   1991-10-31 DRFVAL WV Snyder JPL Incorporate changes from Carlson
C>>   1990-11-27 DRFVAL WV Snyder JPL Convert Carlson's code for MATH77
C
C--D replaces "?": ?ERM1, ?ERV1, ?RFVAL, ?RFVLX
C
C     CHECK ARGUMENT RANGES AND CALL DRFVLX TO DO THE COMPUTATION       00001100
C     OF CARLSON'S ELLIPTIC INTEGRAL RF(X,Y,Z).
C
C     INPUT ...
C
C     X, Y, AND Z ARE THE VARIABLES IN THE INTEGRAL RF(X,Y,Z).
C
C     OUTPUT ...
C
C     RF IS THE VALUE OF THE INCOMPLETE ELLIPTIC INTEGRAL.
C                                                                       00002100
C     IERR IS THE RETURN ERROR CODE.
C          IERR = 0  FOR NORMAL COMPLETION OF THE SUBROUTINE.
C          IERR = 1  X, Y, OR Z IS NEGATIVE.
C          IERR = 2  X+Y, X+Z, OR Y+Z IS TOO SMALL.
C          IERR = 3  X, Y, OR Z IS TOO LARGE.
C
      DOUBLE PRECISION X, Y, Z, RF
      INTEGER IERR
C ----------------------------------------------------------------------
      DOUBLE PRECISION D1MACH                                           00003100
      EXTERNAL D1MACH
C ----------------------------------------------------------------------
C
C     MACHINE DEPENDENT PARAMETERS ...
C
C     LOLIM AND UPLIM DETERMINE THE RANGE OF VALID ARGUMENTS.
C     LOLIM IS NOT LESS THAN THE MACHINE MINIMUM MULTIPLIED BY 5.
C     UPLIM IS NOT GREATER THAN THE MACHINE MAXIMUM DIVIDED BY 5.
C
      DOUBLE PRECISION LOLIM, UPLIM                                     00004100
      SAVE LOLIM, UPLIM
      DATA LOLIM /-1.0d0/
C ----------------------------------------------------------------------
C     WARNING. CHANGES IN THE PROGRAM MAY IMPROVE SPEED AT THE
C     EXPENSE OF ROBUSTNESS.
C ----------------------------------------------------------------------
C
      RF = 0.0d0
      IF(MIN(X,MIN(Y,Z)) .LT. 0.0d0)THEN
         IERR = 1                                                       00005200
         CALL DERM1 ('DRFVAL',1,0,'One of X, Y or Z is negative','X',x, 
     *     ',')
         CALL DERV1 ('Y',Y,',')
         CALL DERV1 ('Z',Z,'.')
         RETURN
      END IF
      IF(LOLIM .LT. 0.0d0)THEN
         LOLIM = 5.0d0 * D1MACH(1)
         UPLIM = D1MACH(2) / 5.0d0
      END IF
      IF(MAX(X,MAX(Y,Z)) .GT. UPLIM)THEN                                00006300
         IERR = 3
         CALL DERM1 ('DRFVAL',3,0,'One of X, Y or Z > UPLIM','X',       
     *     x,',')
         CALL DERV1 ('Y',Y,',')
         CALL DERV1 ('Z',Z,',')
         CALL DERV1 ('UPLIM',UPLIM,'.')
         RETURN
      END IF
      IF(MIN(X+Y,MIN(X+Z,Y+Z)) .LT. LOLIM)THEN
         IERR = 2
         CALL DERM1 ('DRFVAL',2,0,'One of X+Y, X+Z or Y+Z < LOLIM','X', 00007400
     *     x,',')
         CALL DERV1 ('Y',Y,',')
         CALL DERV1 ('Z',Z,',')
         CALL DERV1 ('LOLIM',LOLIM,'.')
         RETURN
      END IF
C
      IERR = 0
      CALL DRFVLX (X, Y, Z, RF)
      RETURN
      END                                                               00008500
 
      SUBROUTINE DRFVLX (X, Y, Z, RF)
c>>   1993-02-23 DRFVLX WV Snyder JPL Split DRFVLX into a separate subr.
C     ------------------------------------------------------------------
C     DRFVLX SHOULD ONLY BE CALLED FROM OTHER LIBRARY ROUTINES, WHERE WE
C     KNOW X, Y, Z ARE IN RANGE.  USERS SHOULD NOT CALL DRFVLX DIRECTLY.
C     ------------------------------------------------------------------
C
C     COMPUTE THE INCOMPLETE ELLIPTIC INTEGRAL OF THE FIRST KIND
C                                                                       00009600
C     RF(X,Y,Z) = INTEGRAL FROM ZERO TO INFINITY OF
C
C                           -1/2     -1/2     -1/2
C                 (1/2)(T+X)    (T+Y)    (T+Z)    DT,
C
C     WHERE X, Y, AND Z ARE NONNEGATIVE AND AT MOST ONE OF THEM
C     IS ZERO.  IF ONE OF THEM IS ZERO, THE INTEGRAL IS COMPLETE.
C     THE DUPLICATION THEOREM IS ITERATED UNTIL THE VARIABLES ARE
C     NEARLY EQUAL, AND THE FUNCTION IS THEN EXPANDED IN TAYLOR
C     SERIES TO FIFTH ORDER.                                            00010600
C     REFERENCES: B. C. CARLSON AND E. M. NOTIS, ALGORITHMS FOR
C     INCOMPLETE ELLIPTIC INTEGRALS, ACM TRANSACTIONS ON MATHEMA-
C     TICAL SOFTWARE, 7 (1981), 398-403; B. C. CARLSON, COMPUTING
C     ELLIPTIC INTEGRALS BY DUPLICATION, NUMER. MATH., 33 (1979),
C     1-16.
C     AUTHORS: B. C. CARLSON AND ELAINE M. NOTIS, AMES LABORATORY-
C     DOE, IOWA STATE UNIVERSITY, AMES, IA 50011, AND R. L. PEXTON,
C     LAWRENCE LIVERMORE NATIONAL LABORATORY, LIVERMORE, CA 94550.
C     AUG. 1, 1979, REVISED JAN. 15, 1987.
C                                                                       00011600
C     CHECK VALUE: RF(0,1,2) = 1.31102 87771 46059 90523 24198
C     CHECK BY ADDITION THEOREM: RF(X,X+Z,X+W) + RF(Y,Y+Z,Y+W)
C     = RF(0,Z,W), WHERE X,Y,Z,W ARE POSITIVE AND X * Y = Z * W.
C
      DOUBLE PRECISION X, Y, Z, RF
C     INPUT ...
C
C     X, Y, AND Z ARE THE VARIABLES IN THE INTEGRAL RF(X,Y,Z).
C
C     OUTPUT ...                                                        00012600
C
C     RF IS THE VALUE OF THE INCOMPLETE ELLIPTIC INTEGRAL.
C
C ----------------------------------------------------------------------
      DOUBLE PRECISION D1MACH
      EXTERNAL D1MACH
      DOUBLE PRECISION C1,C2,C3,E2,E3,EPSLON,ERRTOL,LAMDA
      DOUBLE PRECISION MU,S,XN,XNDEV,XNROOT
      DOUBLE PRECISION YN,YNDEV,YNROOT,ZN,ZNDEV,ZNROOT
      PARAMETER (C1 = 10.0d0 / 24.0d0)                                  00013600
      PARAMETER (C2 = 30.0d0 / 44.0d0)
      PARAMETER (C3 = 10.0d0 / 14.0d0)
C ----------------------------------------------------------------------
C     MACHINE DEPENDENT PARAMETER ...
C     ERRTOL IS SET TO THE DESIRED ERROR TOLERANCE.
C     RELATIVE ERROR DUE TO TRUNCATION IS LESS THAN
C     ERRTOL ** 6 / (4 * (1 - ERRTOL)).
C
      SAVE ERRTOL
      DATA ERRTOL /-1.0D0/                                              00014600
C ----------------------------------------------------------------------
      IF (ERRTOL .LT. 0.0d0) ERRTOL = (3.6d0 * D1MACH(4))**(1.0d0/6.0d0)
      XN = X
      YN = Y
      ZN = Z
C
20002 CONTINUE
         MU = (XN + YN + ZN) / 3.0d0
         XNDEV = 2.0d0 - (MU + XN) / MU
         YNDEV = 2.0d0 - (MU + YN) / MU                                 00015700
         ZNDEV = 2.0d0 - (MU + ZN) / MU
         EPSLON = MAX(ABS(XNDEV),MAX(ABS(YNDEV),ABS(ZNDEV)))
      IF (EPSLON .LT. ERRTOL) GO TO 20003
         XNROOT = SQRT(XN)
         YNROOT = SQRT(YN)
         ZNROOT = SQRT(ZN)
         LAMDA = XNROOT * (YNROOT + ZNROOT) + YNROOT * ZNROOT
         XN = (XN + LAMDA) * 0.25d0
         YN = (YN + LAMDA) * 0.25d0
         ZN = (ZN + LAMDA) * 0.25d0                                     00016700
      GO TO 20002
20003 CONTINUE
C
      E3 = XNDEV * YNDEV
      E2 = E3 - ZNDEV * ZNDEV
      E3 = E3 * ZNDEV
      S = 10.0d0 + (C1 * E2 - 1.0d0 - C2 * E3) * E2 + C3 * E3
      RF = S / (10.0d0*SQRT(MU))
      RETURN
C                                                                       00017600
      END        
