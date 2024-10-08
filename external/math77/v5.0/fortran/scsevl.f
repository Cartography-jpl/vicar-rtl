      real             function SCSEVL(X,A,N)
C>> 1994-10-20 SCSEVL Krogh  Changes to use M77CON
C>> 1990-11-29 SCSEVL CLL Changing name of single precision version.
C>> 1989-10-30 CLL
C>> 1985-08-02 SCSEVL Lawson  Initial code.
C
C EVALUATE THE N-TERM CHEBYSHEV SERIES A AT X. ADAPTED FROM
C R. BROUCKE, ALGORITHM 446, C.A.C.M., 16, 254 (1973).
C W. FULLERTON, C-3, LOS ALAMOS SCIENTIFIC LABORATORY.
C INSTALLED ON THE VAX BY DOLORES MONTANO, C-3, 5/80.
C 1989-10-30 CLL. Replaced name CSEVL with SCSEVL in calls to error
c     processing subroutines.  Commented out the test on X being in
c     [-1.0, 1.0].  Was giving unnecessary error msg when X was
c     slightly greater than 1.0 due to imprecise Cray DP arithmetic.
C
C       INPUT ARGUMENTS --
C X  [float,in]  VALUE AT WHICH THE SERIES IS TO BE EVALUATED.
C A  [float,in]  ARRAY OF N TERMS OF A CHEBYSHEV SERIES. IN EVAL-
C      UATING the series, ONLY HALF THE FIRST COEF IS SUMMED.
C N  [integer,in]  NUMBER OF TERMS IN ARRAY A.
C     ------------------------------------------------------------------
c--S replaces "?": ?CSEVL, ?ERM1
c     Also uses IERM1
C     ------------------------------------------------------------------
      integer I, N
      real             A(N),X,TWOX,B0,B1,B2
C     ------------------------------------------------------------------
      IF (N .LT. 1) CALL IERM1('SCSEVL',1,0,
     *'NUMBER OF TERMS .LE. 0','ARG',N,'.')
      IF (N .GT. 1000) CALL IERM1('SCSEVL',2,0,
     *'NUMBER OF TERMS .GT. 1000','ARG',N,'.')
c     IF (X .LT. -1.E0 .OR. X .GT. 1.E0) CALL SERM1(
c    *'SCSEVL',3,0,'X OUTSIDE (-1,+1)','ARG',X,'.')
C
C
       TWOX = 2.0E0*X
       B1 = 0.E0
       B0=0.E0
       DO 10 I=N,1,-1
         B2=B1
         B1=B0
         B0 = TWOX*B1 - B2 + A(I)
 10    CONTINUE
C
       SCSEVL = 0.5E0 * (B0-B2)
C
       RETURN
       END
