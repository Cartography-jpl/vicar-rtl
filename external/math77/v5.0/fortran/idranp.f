      integer function IDRANP(XMEAN)
C     .  Copyright (C) 1989, California Institute of Technology.
C     .  All rights reserved.  U. S. Government sponsorship under
C     .  NASA contract NAS7-918 is acknowledged.
c>> 1994-10-19 IDRANP Krogh  Changes to use M77CON
c>> 1994-06-24 IDRANP CLL Changed common to use RANC[D/S]1 & RANC[D/S]2.
c>> 1994-06-23 CLL Changed name to I[D/S]RANP.
c>> 1992-03-16 CLL
c>> 1991-11-26 CLL Reorganized common. Using RANCM[A/D/S].
c>> 1991-11-22 CLL Added call to RAN0, and DGFLAG in common.            00001200
c>> 1991-01-15 CLL Reordered common contents for efficiency.
C>> 1990-01-23 CLL  Making names in common same in all subprogams.
C>> 1987-04-23 IRANP  Lawson  Initial code.
c        Returns one pseudorandom integer from the Poisson distribution
c     with mean and variance = XMEAN.
c        The probability of occurrence of the nonnegative integer k in
c     the Poisson distribution with mean XMEAN is
 
c                P(k) = exp(-XMEAN) * XMEAN**k / k!
                                                                        00002200
c        Let SUM(n) denote the sum for k = 0 to n of P(k).
c     Let U be a random sample from a uniform
c     distribution on [0.0, 1.0].  The returned value of IDRANP will be
c     the smallest integer such that S(IDRANP) .ge. U.
c     This variable has mean and variance = XMEAN.
c     Reference: Richard H. Snow, Algorithm 342, Comm ACM, V. 11,
c     Dec 1968, p. 819.
c     Code based on subprogram written for JPL by Stephen L. Ritchie,
c     Heliodyne Corp. and Wiley R. Bunton, JPL, 1969.
c     Adapted to Fortran 77 for the JPL MATH77 library by C. L. Lawson &00003200
c     S. Y. Chiu, JPL, Apr 1987.
c     ------------------------------------------------------------------
c                    Subprogram argument and result
c
c     XMEAN [in]  Mean value for the desired distribution.
c           Must be positive and not so large that exp(-XMEAN)
c           will underflow.  For example, on a machine with underflow
c           limit 0.14D-38, XMEAN must not exceed 88.
c
c     IDRANP [Returned function value]  Will be set to a nonnegative    00004200
c           integer value if computation is successful.
c           Will be set to -1 if XMEAN is out of range.
c     ------------------------------------------------------------------
c--D replaces "?": I?RANP, ?ERM1, ?ERV1, ?RANUA
c--&                 RANC?1, RANC?2, ?PTR, ?NUMS, ?GFLAG
c     Generic intrinsic functions referenced: EXP, LOG, MIN, NINT
c     Other MATH77 subprogram referenced: RAN0
c     Other MATH77 subprograms needed: ERMSG, ERFIN, I1MACH
c     Common referenced: /RANCD1/, /RANCD2/
c     ------------------------------------------------------------------00005200
      integer M, NMAX
      parameter(M = 97, NMAX = 84)
      double precision ELIMIT, MONE, D1MACH
      double precision S, DNUMS(M), SPREV, SUM(0:NMAX)
      double precision TERM, TLAST, TWO
      double precision U, XMEAN, XMSAVE, ZERO
      integer LAST, N, NMID
      logical FIRST
      parameter(ZERO = 0.0D0, MONE = -1.0D0, TWO = 2.0D0)
      common/RANCD2/DNUMS                                               00006200
c
      integer DPTR
      logical DGFLAG
      common/RANCD1/DPTR, DGFLAG
c
      save   /RANCD1/, /RANCD2/
      save    ELIMIT, FIRST, LAST, NMID, SUM, TLAST, XMSAVE
      data    FIRST / .true. /
      data    ELIMIT / ZERO /
c     ------------------------------------------------------------------00007200
      IF (.NOT.(FIRST)) GO TO 20003
      ASSIGN 20004 TO NPR001
      GO TO 30001
20004    FIRST = .false.
         call RAN0
      GO TO 20002
20003 IF (.NOT.(XMEAN .ne. XMSAVE)) GO TO 20005
      ASSIGN 20006 TO NPR001
      GO TO 30001
20006 CONTINUE
20005 CONTINUE
c
20002 GO TO 30002
c                                                                       00008300
20007 IF (.NOT.(U .gt. SUM(LAST))) GO TO 20009
      GO TO 30003
20010 GO TO 20008
20009 GO TO 30004
20011 CONTINUE
20008 return
c     ------------------------------------------------------------------
C     procedure( SET FOR NEW XMEAN )
c
c     D1MACH(1) gives the underflow limit.
c     ELIMIT gives a limit for arguments to the exponential function    00009400
c     such that if XMEAN .le. ELIMIT,  exp(-XMEAN) should not
c     underflow.
c
30001 if(ELIMIT .eq. ZERO) ELIMIT = -log(D1MACH(1) * TWO)
      IF(XMEAN .le. ZERO  .or.  XMEAN .gt. ELIMIT)THEN
         call DERM1('IDRANP',1, 0,'Require 0 .lt. XMEAN .le. ELIMIT',   
     *      'XMEAN',XMEAN,',')
         call DERV1('ELIMIT',ELIMIT,'.')
         IDRANP = -1
         return
      END IF                                                            00010500
      LAST = 0
      TLAST = exp(-XMEAN)
      SUM(0) = TLAST
      XMSAVE = XMEAN
      NMID = nint(XMEAN)
      GO TO NPR001,(20004,20006)
c     ------------------------------------------------------------------
C     procedure ( SET U = UNIFORM RANDOM NUMBER )
c
30002 DPTR = DPTR - 1                                                   00011500
      IF(DPTR .eq. 0)THEN
         call DRANUA(DNUMS,M)
         DPTR = M
      END IF
      U = DNUMS(DPTR)
      GO TO 20007
c     ------------------------------------------------------------------
C     procedure( COMPUTE MORE SUMS )
c
30003    N = LAST                                                       00012500
         SPREV = SUM(LAST)
         TERM = TLAST
20016 CONTINUE
            N = N + 1
            TERM = TERM * XMEAN / dble(N)
            S = SPREV + TERM
      IF(S .eq. SPREV)THEN
               IDRANP = N
      GO TO 20017
      END IF                                                            00013500
      IF(N .le. NMAX)THEN
               LAST = N
               SUM(LAST) = S
               TLAST = TERM
      END IF
      IF(U .le. S)THEN
               IDRANP = N
      GO TO 20017
      END IF
            SPREV = S                                                   00014500
      GO TO 20016
20017 CONTINUE
      GO TO 20010
c     ------------------------------------------------------------------
C     procedure( SEARCH THROUGH STORED SUMS )
c
c     Here we already know that U .le. SUM(LAST).
c     It is most likely that U will be near SUM(NMID).
c
30004 IF(NMID .lt. LAST)THEN                                            00015400
      IF( U .gt. SUM(NMID) )THEN
      DO 20028 N = NMID+1, LAST
      IF(U .le. SUM(N))THEN
                     IDRANP = N
      GO TO 31004
      END IF
20028 CONTINUE
      END IF
      END IF
c                                                                       00016400
      DO 20033 N = min(NMID, LAST)-1, 0,-1
      IF(U .gt. SUM(N))THEN
               IDRANP = N+1
      GO TO 31004
      END IF
20033 CONTINUE
         IDRANP = 0
31004 GO TO 20011
      end                 
