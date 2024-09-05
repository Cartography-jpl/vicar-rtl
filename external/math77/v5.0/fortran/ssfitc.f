      subroutine SSFITC(CCODE, XI, YI, SD, KORDER, NCOEF, TKNOTS,       
     *                  BCOEF, RNORM, ISET, INFO, WORK)
c>> 1994-11-16 CLL Add loops to zero debug arrays XT() and RT().
c>> 1994-10-19 SSFITC Krogh  Changes to use M77CON
c>> 1994-01-31 SSFITC CLL Added test for SD(i) .le. 0 when SD(1) > 0.
c>> 1992-12-16 CLL Corrected formula for NEED2 and comments re WORK().
c>> 1992-11-12 C. L. Lawson, JPL  Initializing LEFT, J1, J2.
c>> 1992-10-27 C. L. Lawson, JPL
c>> 1989-03-02 C. L. Lawson, JPL
c>> 1989-02-23 C. L. Lawson, JPL
c>> 1988-04-01 C. L. Lawson, JPL
c                                                                       00001300
c        Weighted least squares fit to discrete data by a polynomial
c     spline function of order KORDER.  The user can specify equality or
c     inequality constraints.  The fitting equations as well as the
c     constraints may involve the value, a derivative of specified
c     order, or a definite integral of the spline function.  A fitting
c     equation or constraint may involve function or derivative values
c     at different points, and relations between derivatives of
c     different orders.
c
c        The order of a polynomial spline function is one               00002300
c     greater than the degree of its polynomial pieces.
c     Example: KORDER = 4 specifies a cubic spline function.
c
c        The "proper fitting interval" is from A = TKNOTS(KORDER) to
c     B = TKNOTS(NT+1-KORDER).  Extrapolation outside this interval
c     is permitted, but one must expect diminished accuracy at
c     extrapolated points.  The given data or constraint
c     abcissas may be outside [A, B], and subsequent evaluations can be
c     done at points X outside [A, B].
c     ------------------------------------------------------------------00003300
c           Specification of fitting and constraint equations.
c
c        Let F denote the polynomial spline to be determined.  Let the
c     quadruple (CCODE(i), X(i), Y(i), SD(i)) be called the ith
c     specification row.  For each desired fitting equation or
c     constraint equation, (either of which we call a relation) the user
c     must specify one or two (consecutive) specification rows.
c
c        CCODE(i) consists of 4 characters, that we call
c     KIND, DERIV, RELOP, and ACTIVE.                                   00004300
c
c        KIND may be '1', '2', '3', or '4'.  KIND determines the kind of
c     relation being specified.  When KIND = '1' or '2' all
c     information for the relation is given in a single specification
c     row.  We call this Row i in the following discussion.
c     When KIND = '3' or '4', two consecutive specification rows are use
c     We call these Rows i and i+1.  We will complete the explanation of
c     KIND after describing DERIV, RELOP, and ACTIVE.
c
c        DERIV may be '0', '1', ..., or '9'.  This selects the order of 00005300
c     derivative of F to appear in the relation.  '0' denotes the value
c     of F itself.
c
c        RELOP may be '~', '=', '<', or '>'.
c     RELOP = '~' means the relation is to be a least-squares fitting
c     equation, '=' means an equality constraint equation, '<' means a
c     less-than-or-equal constraint equation, and '>' means a
c     greater-than-or-equal constraint equation.  When RELOP = '~',
c     SD(i) specifies the a priori standard deviation of the right-side
c     member of the equation.  When RELOP indicates a constraint        00006300
c     equation, SD(i) will be ignored.
c
c        ACTIVE may be 'A', 'N', or '!'.  Lower case 'a' and 'n' are
c     also accepted.  ACTIVE = 'A' means the current specification row
c     is active, i.e., a relation will be generated from these
c     specifications.  ACTIVE = 'N' means the specifications are not
c     active, i.e., no relation will be generated.  ACTIVE = '!' means
c     the current row is inactive and there are no following rows,
c     i.e., this marks the end of the specification data.  The user must
c     provide this termination signal.                                  00007300
c     When KIND = '3', or '4', meaning two specification rows are to
c     be interpreted together, the setting of ACTIVE = 'A' or
c     ACTIVE = 'N' must be consistent in these two rows.
c     Setting ACTIVE = '!' in only the first row is permitted, however,
c     since then the second row will not be accessed anyway.
c
c        The forms of the relations selected by KIND are:
c
c     KIND = 1:      G(X(i)) RELOP Y(i)
c                                                                       00008300
c        where G is the derivative of F selected by DERIV.
c
c     KIND = 2:      G(X(i)) - G(Y(i)) RELOP 0
c
c        where G is the derivative of F selected by DERIV.
c
c     KIND = 3:      G(X(i)) - Y(i+1) * H(X(i+1)) RELOP Y(i)
c
c        where G is the derivative of F selected by DERIV(i) and
c        and H is the derivative of F selected by DERIV(i+1).  In this  00009300
c        case KIND(i+1) and RELOP(i+1) are not used.
c
c     KIND = 4:      Integral from X(i) to X(i+1) of F RELOP Y(i)
c
c        In this case DERIV(i), KIND(i+1), DERIV(i+1), RELOP(i+1), and
c        Y(i+1) are not used.
c     ------------------------------------------------------------------
C     The linear algebra methods were designed by C.L.Lawson and
c     R.J.Hanson.  The method of representing spline functions is due
c     to Carl de Boor.  References:                                     00010300
C     "SOLVING LEAST SQUARES PROBLEMS", by Lawson and Hanson,
C     Prentice-Hall, 1974.
c     "A PRACTICAL GUIDE TO SPLINES" by Carl de Boor,
c     Springer-Verlag, 1978.
c     The functionality and user interface of this subprogram are
c     modeled on the "French Curve" subroutine, FC, developed by Hanson
c     and Lawson at JPL in 1970, and the subsequent version developed by
c     Hanson at Sandia in 1979.
c     March 1988, CLL, JPL.  Revised to conform to the Fortran 77
c     standard.  Intended for inclusion in the JPL MATH77 math library. 00011300
c     Feb 1989, CLL, JPL.  Revised to use KIND = 3 to specify an
c     integral, and added new kind of relation using KIND = 4.
c     ------------------------------------------------------------------
c                     SUBROUTINE ARGUMENTS
c
c  CCODE()  [in, char*4]  CCODE(i) is regarded as consisting of four
c           single-character fields.
c        CCODE(i)(1:1) = KIND  =  '1',  '2', '3', '4'.
c        CCODE(i)(2:2) = DERIV   =  '0',  '1', ..., '9'.
c        CCODE(i)(3:3) = RELOP  =  '~',  '=',  '<',  '>'.               00012300
c        CCODE(i)(4:4) = ACTIVE =  'A',  'N',  '!'
c        Where alphabetic characters are shown, the corresponding
c        lower case character is also acceptable.
c
c  X()  [in]  Abcissas for specification of fitting or
c        constraint equations.
c
c  Y()  [in]  Values or abcissas for specification
c        of fitting or constraint equations.
c                                                                       00013300
c  SD() [in]  Specifies the a priori standard deviation of error in the
c       right-side value in each fitting equation.
c       The weighted fitting algorithm will take account of these.
c       Optionally, the user may set SD(1) to a negative value.
c       Then this subr will use abs(SD(1)) as the standard deviation
c       for the right-side value in each fitting equation.  In this
c       latter case the SD() array can be dimensioned SD(1).
c       Note that a negative value in SD(1) will always be interpreted
c       in this way by this subr, even if the associated RELOP is not
c       '~' or if ACTIVE is not 'A'.                                    00014300
c
c  KORDER  [in]  Order of the spline basis functions.  The
c        polynomial degree of the spline segments is one less than the
c        order.  Example:  the order of a cubic spline is 4.
c        Require KORDER .ge. 1.  Internal arrays in subroutines used put
c        an upper limit of 20 on KORDER.
c
c  NCOEF  [in]  No. of B-spline coefficients to be determined.
c
c  (TKNOTS(j),j=1,NT, where NT = NCOEF+KORDER)  [in]  This is the deBoor00015300
c     knot sequence for definition of the spline basis functions.
c     These values must be nondecreasing.
c     Repeated values are permitted, but values at
c     an index spacing of KORDER must be strictly increasing.
c        The first and last KORDER-1 values in TKNOTS() are needed to
c     support the deBoor method of representing splines.
c        The "proper fitting interval" is from
c     A = TKNOTS(KORDER) to B = TKNOTS(NT+1-KORDER).  One acceptable and
c     convenient way to set the first and last KORDER-1 knots is to set
c     the first KORDER-1 to A and the last KORDER-1 to B.               00016300
c        Continuity of the spline at knots interior to (A, B) will be of
c     order KORDER-2, unless a knot is repeated, in which case the order
c     of continuity will be decreased at that knot.
c
c  BCOEF()  [out]  An array of length NCOEF into which the computed
c        coefficients defining the fitted curve will be stored.  These
c        are coeffients relative to B-spline basis functions.
c        For I = 1, ..., NCOEF, the coefficient BCOEF(I)
c        is associated with the basis function whose support interval
c        runs from TKNOTS(I) to TKNOTS(I+KORDER).                       00017300
c
c  RNORM   [out]  RNORM := sqrt( sum over the indices i for which
c         fitting was requested of [( (yfit(i) - Y(i))/SD(i))**2])
c
c  ISET()  [in integer]  Array of length 3.
c        ISET(1) = NINFO, the dimension of INFO().  A sufficiently
c           large value for NINFO is 7 + 2*(NCOEF + NS).
c        ISET(2) = NWORK, the dimension of WORK().  A sufficiently
c           large value for NWORK can be computed as follows:
c           See definition of NS, M1, and MFIT below under INFO().      00018300
c           NTOT = NCOEF + NS
c           MTOT = M1 + MFIT
c           MINMN = min(MTOT, NTOT)
c           NWORK = MTOT*NTOT + 3*MTOT + 6*NTOT + 3*MINMN + M1
c        ISET(3) = KPRINT, a print flag in the range [0, 4].  It is
c           passed on to SBLSE.  Larger values produce more printing.
c
c  INFO()  [out and scratch integer]  The first 7 elements of INFO()
c        are used to return information about the problem.  The
c        following 2*(NCOEF+NS) locations are used as scratch.          00019300
c        INFO(1) = IERR5, the Error status indicator.
c        Note that IERR4 comes from SBLSE.  Possible values of IERR5 are
c        as follows:
c
c        =    0 means no errors detected.
c        =  100 means  NCOEF .lt. 1
c        =  200 means  TKNOTS(I) .gt. TKNOTS(I+1)
c        =  250 means  TKNOTS(I) .ge. TKNOTS(I+KORDER)
c        =  300 means  NINFO or NWORK is too small.
c        =  500 means  DERIV has bad value.                             00020300
c        =  600 means  RELOP has bad value.
c        =  700 means  KIND has bad value.
c        =  800 means  ACTIVE has bad value.
c        = 1000 + IERR4 means IERR4 .ne. 0 due to error
c                       detected in _BLSE.
c        = 1100 means  SD(1) = zero.
c        = 1200 means  SD(1) > zero and SD(i) .le. zero for some i.
c
c        INFO(2)  = NEED1, the dimension needed for INFO().
c        INFO(3)  = NEED2, the dimension needed for WORK().             00021300
c        INFO(4)  = M1, the number of constraints rows in the matrix
c           representation of the problem.  This will be a count of
c           the number of nonignored instances of CCODE(i) having
c           RELOP = '=', '<', or '>, and ACTIVE = 'A'.
c        INFO(5)  = MFIT, the number of least-squares equations.
c           This will be a count of
c           the number of nonignored instances of CCODE(i) having
c           RELOP = '~' and ACTIVE = 'A'.
c        INFO(6)  = NS, the number of slack variables.  This will be a
c           count of the number of nonignored instances of CCODE(i)     00022300
c           having RELOP = '<' or '>, and ACTIVE = 'A'.
c        INFO(7)  = NSETP, the number of variables in Set P at
c           termination.  These variables are at values determined by
c           solution of a system of equations.  The other NCOEF + NS
c           - NSETP variables will be at fixed values, either at one of
c           their bounds or at zero.
c
c  WORK()  [scratch]  Work space dimensioned NWORK.
c     ------------------------------------------------------------------
c          Important internal variables.                                00023300
c
c  BASIS()  Array in which values of KORDER basis functions or their
c           will be stored.  Dimensioned using the parameter KMAX.
c           This puts an upper limit on permissible KORDER.
c  JCOL     Column of matrix into which first element of current
c           set of basis function values will be placed.
c           JCOL = LEFT - KORDER + 1.
c  KINFO    Parameter specifying the number of locations at the
c           beginning of INFO() used for specific items of returned
c           information.  Space beyond these locations is used for      00024300
c           scratch.
c  KMAX     Intermal dimensioning parameter.  The input value of KORDER
c           must not exceed KMAX.
c  KORDP1   = KORDER+1
C  KSIZE    Number of rows in current block.
c  LEFT     Index of current spline segment.  LEFT will satisfy
c           KORDER .le. LEFT .le. NCOEF.
c           The knot interval associated with index LEFT is from
c           T(LEFT) to T(LEFT+1).
c           Note that the union of these                                00025300
c           segments is the "proper fitting interval".
c  ELIMIT   Limit on number of errors in initial scan of specs before
c           quitting.
c     ------------------------------------------------------------------
c--S replaces "?": ?SFITC, ?BLSE, ?SBASI, ?SBASD, ?SFIND, ?ERV1
c     Both versions use      ERMOR, ERMSG, IERM1, IERV1
c     ------------------------------------------------------------------
      integer ELIMIT, KINFO
      parameter(ELIMIT = 9, KINFO = 7)
      integer ACTIVE, DERIV, FAC                                        00026300
      integer I, IC, IERR4, IERR5
      integer INFO(*), IRCON, IRLS, IROW, ISET(3)
      integer IWBND, IWBVEC, IWCC, IWDIFF, IWINDX, IWJSTA
      integer IWRT, IWSIZ, IWSS, IWTNRM, IWWRK, IWXS, IWXT, IWZ
      integer J, J1, J2, JCOL, JS, KMAX, KORDER, KPRINT, LEFT
      integer M1, MFIT, MINMN, MN, MODE, MTOT
      integer NBADCC, NCOEF, NEED1, NEED2, NINFO, NTOT
      integer NS, NSETP, NT, NWORK, RELOP, KIND
      integer N20009, N20015
      parameter(KMAX=20)                                                00027300
      real             R1MACH
      real             BASIS(KMAX), BCOEF(NCOEF), ONE
      real             RNORM, RTVAL, SD(*), SDIC
      real             TKNOTS(NCOEF+KORDER), TOL, UNBND
      real             WORK(*), WT, WT1
      real             X, XI(*), YI(*), ZERO
      character ATAB*6, CCODE(*)*4, DTAB*10
      character RTAB*4, NVTAB*4
      logical   USEWT1
      parameter( ONE = 1.0E0, UNBND = 99.0E0, ZERO = 0.0E0)             00028300
      parameter(DTAB='0123456789', RTAB='~=<>')
      parameter(NVTAB='1234', ATAB=' AaNn!')
c     ------------------------------------------------------------------
      NINFO  = ISET(1)
      NWORK  = ISET(2)
      KPRINT = ISET(3)
      NT = NCOEF + KORDER
      IERR5 = 0
C
C          Exit immediately if NCOEF .lt. 1  or if                      00029400
C          the knots fail to be nondecreasing.
c
      IF (.NOT.( NCOEF .lt. 1 )) GO TO 20003
         IERR5 = 100
         call IERM1('SSFITC',IERR5,0,                                   
     *   'Require NCOEF .ge. 1', 'NCOEF',NCOEF,'.')
      ASSIGN 20004 TO NPR001
      GO TO 30001
20004 CONTINUE
20003 IF (.NOT.(KORDER .gt. KMAX)) GO TO 20006
         IERR5 = 150
         call IERM1('SSFITC',IERR5,0,'Require KORDER .le. KMAX.',       
     *   'KORDER',KORDER,',')
         call IERV1('KMAX',KMAX,'.')                                    00030700
      ASSIGN 20007 TO NPR001
      GO TO 30001
c
20007 CONTINUE
20006 I =1
      N20009=NT-1
      GO TO 20010
20008 I =I +1
20010 IF ((N20009-I ).LT.0) GO TO 20009
      IF (.NOT.(TKNOTS(I) .gt. TKNOTS(I+1))) GO TO 20012
          IERR5 = 200
          call IERM1('SSFITC',IERR5,0,                                  
     *    'Require knots, TKNOTS(I), to be nondecreasing.',             
     *    'I',I,',')
          call SERV1('TKNOTS(I)',TKNOTS(I),',')
          call SERV1('TKNOTS(I+1)',TKNOTS(I+1),'.')
      ASSIGN 20013 TO NPR001
      GO TO 30001
20013 CONTINUE                                                          00032100
20012 GO TO 20008
c
20009 I =1
      N20015=NCOEF
      GO TO 20016
20014 I =I +1
20016 IF ((N20015-I ).LT.0) GO TO 20015
      IF (.NOT.(TKNOTS(I) .ge. TKNOTS(I+KORDER))) GO TO 20018
          IERR5 = 250
          call IERM1('SSFITC',IERR5,0,                                  
     *    'Require TKNOTS(I) < TKNOTS(I+KORDER).',                      
     *    'I',I,',')
          call SERV1('TKNOTS(I)',TKNOTS(I),',')
          call SERV1('TKNOTS(I+KORDER)',TKNOTS(I+KORDER),'.')
      ASSIGN 20019 TO NPR001
      GO TO 30001
20019 CONTINUE
20018 GO TO 20014
C                                                                       00033400
c     ------------------------------------------------------------------
C                                       TEST SD(1)
20015 IF (.NOT.(SD(1) .lt. ZERO)) GO TO 20021
         WT1 = -ONE/SD(1)
         USEWT1 = .true.
      GO TO 20020
20021 IF (.NOT.( SD(1) .gt. ZERO)) GO TO 20022
         USEWT1 = .false.
      GO TO 20020
20022    IERR5 = 1100
         call ERMSG('SSFITC',IERR5,0,'Require SD(1) .ne. Zero','.')     00034400
      ASSIGN 20023 TO NPR001
      GO TO 30001
c
c     .  Determine M1, MFIT, NS, MTOT, and NTOT.
c     .  M1 = number of non-ignored constraint specifications.
c     .  MFIT = number of non-ignored least-squates equations.
c     .  NS = number of non-ignored constraints that
c     .  are inequality constraints and thus require a slack variable.
c     .  MTOT = M1 + MFIT
c     .  NTOT = NCOEF + NS
c                                                                       00035500
20023 CONTINUE
20020 NBADCC = 0
      M1   = 0
      MFIT = 0
      NS   = 0
      I = 1
20024 CONTINUE
         KIND = index(NVTAB, CCODE(I)(1:1))
         DERIV  = index(DTAB, CCODE(I)(2:2)) - 1
         RELOP = index(RTAB, CCODE(I)(3:3))
         ACTIVE = index(ATAB, CCODE(I)(4:4))/2                          00036500
      IF (ACTIVE .eq. 3) GO TO 20025
c
c        .  CCODE(I)(1:1) =  1  2  3  4
c        .           KIND =  1  2  3  4
c
c        .  CCODE(I)(2:2) =  0  1  2  3  4  5  6  7  8  9
c        .          DERIV =  0  1  2  3  4  5  6  7  8  9
c
c        .  CCODE(I)(3:3) =  ~  =  <  >
c        .          RELOP =  1  2  3  4                                 00037500
c
c        .  CCODE(I)(4:4) =  A  a  N  n  !
c        .         ACTIVE =  1  1  2  2  3
c
      IF(ACTIVE .eq. 1)THEN
      GO TO (20030,20031,20032,20033), RELOP
      GO TO 20028
20030          MFIT = MFIT+1
      GO TO 20029
20031          M1 = M1+1
      GO TO 20029                                                       00038600
20032          M1 = M1+1
               NS = NS+1
      GO TO 20029
20033          M1 = M1+1
               NS = NS+1
      GO TO 20029
20028          IERR5 = 600
               call IERM1('SSFITC',IERR5,0,                             
     *         'RELOP = CCODE(I)(3:3) has invalid value.','I',I,',')
               call ERMOR('CCODE(I)(3:3) = '// CCODE(I)(3:3), '.')
               NBADCC = NBADCC+1                                        00039700
20029 CONTINUE
      ELSEIF(ACTIVE .ne. 2)THEN
               IERR5 = 800
               call IERM1('SSFITC',IERR5,0,                             
     *         'ACTIVE = CCODE(I)(4:4) has invalid value.','I',I,',')
               call ERMOR('CCODE(I)(4:4) = '// CCODE(I)(4:4), '.')
               NBADCC = NBADCC+1
      END IF
c
      IF(KIND .eq. 1 .or. KIND .eq. 2)THEN
            I = I+1
      ELSEIF(KIND .eq. 3 .or. KIND .eq. 4)THEN                          00040900
            I = I+2
      ELSE
            IERR5 = 700
            call IERM1('SSFITC',IERR5,0,                                
     *      'KIND = CCODE(I)(1:1) has invalid value.','I',I,',')
            call ERMOR('CCODE(I)(1:1) = '// CCODE(I)(1:1), '.')
            NBADCC = ELIMIT+1
      END IF
      IF (.NOT.(NBADCC .gt. ELIMIT)) GO TO 20039
            call ERMSG('SSFITC',IERR5,0,                                
     *      'Quitting on bad values in CCODE()','.')
      ASSIGN 20040 TO NPR001                                            00042100
      GO TO 30001
20040 CONTINUE
20039 GO TO 20024
20025 CONTINUE
c
      IF (.NOT.(NBADCC .ne. 0)) GO TO 20042
         call ERMSG('SSFITC',IERR5,0,                                   
     *    'Quitting on bad values in CCODE()','.')
      ASSIGN 20043 TO NPR001
      GO TO 30001
20043 CONTINUE
20042 MTOT = M1 + MFIT
      NTOT = NCOEF + NS
*     print*,'SSFITC.. M1, MFIT, NCOEF, NS =',M1, MFIT, NCOEF, NS
      MINMN = min(MTOT, NTOT)                                           00043300
      INFO(4) = M1
      INFO(5) = MFIT
      INFO(6) = NS
c
c     .  Set indices to partition the work arrays INFO() and WORK().
c
      IWINDX = 1+KINFO
      IWJSTA = IWINDX+NTOT
      NEED1  = IWJSTA+NTOT - 1
      INFO(2) = NEED1                                                   00044300
c
      MN = MTOT*NTOT
      IWBVEC = MN+1
      IWBND  = IWBVEC+MTOT
      IWXS   = IWBND +2*NTOT
      IWWRK  = IWXS+NTOT
      IWSIZ  = IWWRK+NTOT
      IWTNRM = IWSIZ+M1
      IWZ    = IWTNRM+NTOT
      IWCC   = IWZ+MINMN                                                00045300
      IWSS   = IWCC+MINMN
      IWXT   = IWSS+MINMN
      IWRT   = IWXT+NTOT
      IWDIFF = IWRT+MTOT
      NEED2  = IWDIFF+MTOT - 1
      INFO(3) = NEED2
c     .                                     Check NINFO and NWORK.
c
      IF (.NOT.(NINFO .lt. NEED1 .or. NWORK .lt. NEED2)) GO TO 20045
              IERR5 = 300                                               00046300
         call IERM1('SSFITC',IERR5,0,                                   
     *   'Require NINFO .ge. NEED1 and NWORK .ge. NEED2.',              
     *   'NINFO',NINFO,',')
         call IERV1('NEED1',NEED1,',')
         call IERV1('NWORK',NWORK,',')
         call IERV1('NEED2',NEED2,'.')
      ASSIGN 20046 TO NPR001
      GO TO 30001
c     .                       Zero an MTOT x NTOT space for the matrix.
20046 CONTINUE
20045 DO 20047 I = 1, MN
         WORK(I) = ZERO
20047 CONTINUE
c     .                       Zero debug arrays XT() and RT() in WORK().
      do 10 j = 1, ntot
         work(iwxt + j - 1) = ZERO
   10 continue
      do 20 j = 1, mtot
         work(iwrt + j - 1) = ZERO
   20 continue
c                                                                       00047600
c     .  Set bounds for variables.  Storing into a 2 x NTOT space.
c
      DO 20050 J = 1,2*NTOT
         WORK(IWBND-1+J) = UNBND
20050 CONTINUE
      DO 20053 J = 1,NS
         WORK(IWBND+(NCOEF+J-1)*2) = ZERO
20053 CONTINUE
c     -- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
c     Begin loop to form equations, both constraints and least-squares. 00048600
c
c     Initialize JS = column index of previous slack variable.
c             IRCON = row index of previous constraint equation.
c             IRLS  = row index of previous least-squares equation.
c               IC  = index of current specification data.
c             LEFT  = Arbitrary starting value for use by SSFIND.
c             J1,J2 = Arbitrary starting values for use by SSBASI.
c
      JS = NCOEF
      IRCON = 0                                                         00049600
      IRLS  = M1
      IC    = 1
      J1    = 1
      J2    = 1
      LEFT  = 1
20056 CONTINUE
         ACTIVE = index(ATAB, CCODE(IC)(4:4))/2
      IF (ACTIVE .eq. 3) GO TO 20057
      IF(ACTIVE .eq. 2)THEN
            IC = IC+1                                                   00050600
      GO TO 20056
      END IF
         KIND= index(NVTAB, CCODE(IC)(1:1))
         RELOP = index(RTAB, CCODE(IC)(3:3))
c
c        .  CCODE(I)(1:1) =  1  2  3  4
c        .           KIND =  1  2  3  4
c
c        .  CCODE(I)(2:2) =  0  1  2  3  4  5  6  7  8  9
c        .          DERIV =  0  1  2  3  4  5  6  7  8  9               00051600
c
c        .  CCODE(I)(3:3) =  ~  =  <  >
c        .          RELOP =  1  2  3  4
c
c        .  CCODE(I)(4:4) =  A  a  N  n  !
c        .         ACTIVE =  1  1  2  2  3
c
c        .  Set matrix row index, IROW.
c        .  Set weight, WT, if RELOP = 1.
c        .  Store coefficient of +1 or -1 for slack variable if         00052600
c        .  RELOP is 3 or 4.
c
      IF (.NOT.(RELOP .eq. 1)) GO TO 20061
            IRLS = IRLS+1
            IROW = IRLS
      IF (.NOT.( USEWT1)) GO TO 20063
               WT = WT1
      GO TO 20062
20063          SDIC = SD(IC)
      IF (.NOT.(SDIC .gt. ZERO)) GO TO 20065                            00053600
                  WT=ONE/SDIC
      GO TO 20064
20065             IERR5 = 1200
                  call ERMSG('SSFITC',IERR5,0,                          
     *            'With SD(1) > 0  require all SD(I) > 0.', ',')
                  call SERV1('SD(1)',SD(1),',')
                  call IERV1('I',IC,',')
                  call SERV1('SD(I)',SDIC,'.')
      ASSIGN 20066 TO NPR001
      GO TO 30001
20066 CONTINUE
20064 CONTINUE
20062 GO TO 20060                                                       00054800
20061       IRCON = IRCON+1
            IROW = IRCON
      IF(RELOP .eq. 3)THEN
               JS = JS+1
               WORK(IROW+(JS-1)*MTOT) = ONE
      ELSEIF(RELOP .eq. 4)THEN
               JS = JS+1
               WORK(IROW+(JS-1)*MTOT) = -ONE
      END IF
c                                                                       00055900
20060 IF (.NOT.(KIND .eq. 4)) GO TO 20071
      GO TO 30002
20072       IC = IC+2
      GO TO 20070
20071 GO TO 30003
20073       IC = IC+1
            if(KIND .eq. 3) IC = IC+1
20070 GO TO 20056
20057 CONTINUE
c     .                          End loop to form equations.            00056900
c     -- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
c     print'(1x/1x,a,i5,a,i5)','SSFITC..  MTOT =',MTOT,',   NTOT =',NTOT
c     print'(1x/1x,a/1x)','SSFITC.. Matrix going to SBLSE:'
c     do for I = 1,MTOT
c        print'(1x/1x,i5,3x,5g13.5/(9x,5g13.5))',
c    *    I,(WORK(I+(J-1)*MTOT),J=1,NTOT+1)
c     end for ! I
c
c     .          All points have been processed.  Call for the solution.
c                                                                       00057900
      TOL = R1MACH(4)**(0.75e0)
      call SBLSE(WORK, MTOT, MTOT, NTOT, M1, WORK(MN+1),                
     *     WORK(IWBND), UNBND, KPRINT, TOL, IERR4, WORK(IWXS),          
     *     RNORM, NSETP,                                                
     *     WORK(IWWRK), WORK(IWSIZ), WORK(IWTNRM), WORK(IWZ),           
     *     WORK(IWCC), WORK(IWSS), INFO(IWINDX), INFO(IWJSTA),          
     *     WORK(IWXT), WORK(IWRT), WORK(IWDIFF))
c
c     -- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      INFO(7) = NSETP
      IF (.NOT.(IERR4 .ne. 0)) GO TO 20075
         IERR5 = 1000 + IERR4
         call ERMSG('SSFITC',IERR5,0, 'Error noted in SBLSE.','.')
      ASSIGN 20076 TO NPR001
      GO TO 30001
c                                                                       00059500
20076 CONTINUE
20075 DO 20077 I=1,NCOEF
         BCOEF(I) = WORK(IWXS-1+I)
20077 CONTINUE
      INFO(1) = IERR5
      return
c     ------------------------------------------------------------------
C     procedure( SETUP FOR INTEGRAL )
 
30002 call SSBASI(KORDER, NCOEF, TKNOTS, XI(IC), XI(IC+1),              
     *            J1, J2,  WORK(IWXS))
      DO 20080 J = J1, J2                                               00060600
      IF(RELOP .eq. 1)THEN
            WORK(IROW+(J-1)*MTOT) = WORK(IWXS-1+J) * WT
      ELSE
            WORK(IROW+(J-1)*MTOT) = WORK(IWXS-1+J)
      END IF
20080 CONTINUE
c
      IF(RELOP .eq. 1)THEN
         WORK(MN+IROW) = YI(IC) * WT
      ELSE                                                              00061600
         WORK(MN+IROW) = YI(IC)
      END IF
      GO TO 20072
c     ------------------------------------------------------------------
C     procedure( SETUP FOR VALUE OR DERIVATIVE )
c
c        .  DERIV = CCODE()(2:2) =  0  1  2  3  4  5  6  7  8  9
c
30003    DERIV  = index(DTAB, CCODE(IC)(2:2)) - 1
         X = XI(IC)                                                     00062600
         FAC = ONE
      ASSIGN 20087 TO NPR004
      GO TO 30004
20087 IF (.NOT.(KIND .eq. 1)) GO TO 20089
            RTVAL = YI(IC)
      GO TO 20088
c           .                              Here KIND = 2 or 3
20089 IF(KIND .eq. 2)THEN
               FAC = -ONE
               RTVAL = ZERO
               X = YI(IC)                                               00063600
      ELSE
               DERIV  = index(DTAB, CCODE(IC+1)(2:2)) - 1
               FAC = -YI(IC+1)
               RTVAL = YI(IC)
               X = XI(IC+1)
      END IF
      ASSIGN 20092 TO NPR004
      GO TO 30004
c                                   Set right side of relation.
20092 CONTINUE
20088 IF(RELOP .eq. 1)THEN
            WORK(MN+IROW) = RTVAL * WT                                  00064700
      ELSE
            WORK(MN+IROW) = RTVAL
      END IF
      GO TO 20073
c     ------------------------------------------------------------------
C     procedure( ACCUMULATE VALUES INTO MATRIX )
c
30004       call SSFIND(TKNOTS, KORDER, NCOEF+1, X, LEFT, MODE)
            call SSBASD(KORDER, LEFT, TKNOTS, X, DERIV, BASIS)
            JCOL = LEFT-KORDER+1                                        00065700
*           print*,'SSFITC..            X =',X
*           print*,'SSFITC.. IC,LEFT,JCOL =',IC,LEFT,JCOL
            if(RELOP .eq. 1) FAC = FAC * WT
      DO 20095 J = 1,KORDER
               WORK(IROW+(JCOL-1)*MTOT) =                               
     *               WORK(IROW+(JCOL-1)*MTOT) + FAC * BASIS(J)
               JCOL = JCOL + 1
20095 CONTINUE
      GO TO NPR004,(20087,20092)
c     ------------------------------------------------------------------
C     PROCEDURE( ERROR RETURN )                                         00066800
C
30001 DO 20098 I=1,NCOEF
               BCOEF(I)=ZERO
20098 CONTINUE
         INFO(1) = IERR5
         if (.true.) return
      GO TO NPR001,(20004,20007,20013,20019,20023,20040,20043,20046,2006
     *6,20076)
c     ------------------------------------------------------------------
c
      end                                                               00067800
