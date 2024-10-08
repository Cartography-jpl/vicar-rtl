      subroutine SSTAT1(XTAB, NX, STATS, IHIST, NCELLS, X1, X2)
C     .  Copyright (C) 1989, California Institute of Technology.
C     .  U. S. Government sponsorship under
C     .  NASA contract NAS7-918 is acknowledged.
C>> 1994-11-11 SSTAT1 Krogh   Declared all vars.
C>> 1994-10-20 SSTAT1 Krogh  Changes to use M77CON
C>> 1989-10-20 SSTAT1 CLL
C>> 1987-05-01 SSTAT1 Lawson  Initial code.
c--S replaces "?": ?STAT1, ?STAT2
c
c        This subr computes basic statistics for X, storing them is
c     STATS() as follows:
c
c             STATS(1) = Total count
c             STATS(2) = Min
c             STATS(3) = Max
c             STATS(4) = Mean
c             STATS(5) = Standard deviation
c
c     This subr also accumulates counts in IHIST() to develop a
c     histogram of values of X.
c
c        The data to be treated is given in XTAB(1:NX).  If the
c     value of STATS(1) on entry is positive , say = COUNT, it is
c     assumed that COUNT data values have been processed previously
c     and results from that processing are in IHIST() and STATS().
c     These results will be updated to reflect the additional set of
c     NX values.
c
c        Alternatively, if STATS(1) is zero, the initial contents of
c     STATS(2:5) and IHIST() will be ignored and results will be
c     computed just for the present data set XTAB(1:NX).
c
c        The user must specify the range and resolution of the histogram
c     by setting X1, X2, and NCELLS.  The end cells, IHIST(1) and
c     IHIST(NCELLS) will be used to count occurences of X less than X1
c     or greater than X2 respectively.
c     The cells IHIST(2) through IHIST(NCELLS-1) will
c     be used to count occurences of X in NCELLS-2 equal-length
c     subintervals of [X1, X2].
c
c        Define h = (X2 - X1)/(NCELLS-2).  X-intervals will be
c     associated with elements of IHIST() as follows.
c
c          X interval                   Counting cell
c
c          (-Infinity, X1)              IHIST(1)
c          [X1+(i-2)*h, X1+(i-1)*h)     IHIST(i), i = 2,...,NCELLS-2
c          [X2-h, X2]                   IHIST(NCELLS-1)
c          (X2, Infinity)               IHIST(NCELLS)
c
c        After use of this subroutine, the user can call
c     SSTAT2, to produce a printer-plot of the histogram and print the
c     statistics.
c
c        Remark:  It is more efficient to call this subroutine one
c     time giving it N points rather than calling it N times giving it
c     one point each time, but the results will be the same to within
c     arithmetic limitations either way.
c     ------------------------------------------------------------------
c                    Subroutine arguments
c
c     XTAB()  [in]  Array of NX values whose statistics are to be
c           computed.
c     NX     [in]  Number of values given in XTAB().
c           Require NX .ge. 1.
c     STATS()  [inout]  Array of length 5 into which statistics are or
c           will be stored.  Initial value of STATS(1) must be positive
c           if IHIST() and STATS() contain prior results that are to be
c           updated.  Otherwise the initial value of STATS(1) must be
c           zero.
c     IHIST()  [inout]  Integer array of length at least NCELLS into
c           which counts will be accumulated.
c     NCELLS   [in]  Total number of classification regions.
c           Require NCELLS .ge. 3.
c     X1,X2  [in]  Lower and upper boundaries, respectively defining
c           the range of y values to be classified into NCELLS-2 equal
c           intervals.  Require X1 < X2.
c     ------------------------------------------------------------------
c        The value of FAC is not critical.  It should be greater than
c     one.  The program does less computation each time the test
c     (abs(DELTA) .lt. TEST) is satisfied.  It will be true more
c     frequently if FAC is larger.  There is probably not much advantage
c     in setting FAC larger than 4, so 64 is probably more than ample.
c     ------------------------------------------------------------------
c     C. L. Lawson and S. Y. Chiu, JPL, Apr 1987.
C     1989-10-20 CLL Moved integer declaration earlier to avoid warning
c     msg from Cray compiler.
c     ------------------------------------------------------------------
      integer J1, NCELLS, NX
      integer IHIST(NCELLS)
      integer I, INDEX, J
      real             COUNT, DELTA, FAC, ONE, PREV
      real             SCALE, RSCALE, SCLNEW, STATS(5), SUMSCL
      real             TEMP, TEST, X, X1, X2, XMAX, XMEAN, XMIN
      real             XTAB(NX), ZERO
      parameter(ONE = 1.0E0, ZERO = 0.0E0, FAC = 64.0E0)
c     ------------------------------------------------------------------
      if(NX .lt. 1) return
      COUNT = STATS(1)
      if(COUNT .eq. ZERO) then
         do 10 I=1,NCELLS
            IHIST(I) = 0
   10    continue
c
c                    Begin: Special for first point, i.e. COUNT .eq. ONE
c
         COUNT = ONE
         PREV = ZERO
         X = XTAB(1)
         XMIN = X
         XMAX = X
         XMEAN = X
         TEST = -ONE
         SUMSCL = ZERO
         J1 = 2
c        .                             Begin: Tally in histogram.
         if(X .lt. X1) then
            IHIST(1) = IHIST(1) + 1
         elseif(X .gt. X2) then
            IHIST(NCELLS) = IHIST(NCELLS) + 1
         else
c                          Following stmt converts integer to float.
            TEMP = NCELLS-2
c                          Following stmt converts float to integer.
            INDEX = TEMP*(X-X1)/(X2-X1)
            IHIST(INDEX + 2) = IHIST(INDEX + 2) + 1
         endif
c        .                             End: Tally in histogram.
c
c                    End: Special for first point, i.e. COUNT .eq. ONE
c
      else
c                    Here when COUNT is > zero on entry.
         J1 = 1
         XMIN = STATS(2)
         XMAX = STATS(3)
         XMEAN = STATS(4)
c
         if(STATS(5) .eq. ZERO) then
            TEST = -ONE
            SUMSCL = ZERO
         else
c
c              STATS(5) contains the old value of Sigma.  Since it is
c              nonzero (positive) here, COUNT must be at least 2.
c
            SCALE =  STATS(5)
            RSCALE = ONE/SCALE
            TEST = FAC * SCALE
            SUMSCL = COUNT - ONE
         endif
      endif
c
c                                      Here COUNT is at least 1
      do 30 J = J1, NX
         PREV = COUNT
         COUNT = COUNT + ONE
c                                      Here COUNT is at least 2
         X = XTAB(J)
         XMIN = min(X, XMIN)
         XMAX = max(X, XMAX)
c        .                             Begin: Tally in histogram.
         if(X .lt. X1) then
            IHIST(1) = IHIST(1) + 1
         elseif(X .gt. X2) then
            IHIST(NCELLS) = IHIST(NCELLS) + 1
         else
c                          Following stmt converts integer to float.
            TEMP = NCELLS-2
c                          Following stmt converts float to integer.
            INDEX = TEMP*(X-X1)/(X2-X1)
            IHIST(INDEX + 2) = IHIST(INDEX + 2) + 1
         endif
c        .                             End: Tally in histogram.
c
         DELTA = X - XMEAN
c
c              Expect abs(DELTA) .le. TEST most of the time.
c
         if(abs(DELTA) .gt. TEST) then
            if( DELTA .eq. ZERO ) go to 20
c
c                     Here  abs(DELTA) .gt. TEST  and  DELTA .ne. ZERO
c                     Must compute new SCALE, RSCALE and TEST
c                     and update SUMSCL if it is nonzero.
c
            SCLNEW = abs(DELTA)
            RSCALE = ONE / SCLNEW
            TEST = FAC * SCLNEW
            if(SUMSCL .ne. ZERO) SUMSCL = SUMSCL * (SCALE * RSCALE)**2
            SCALE = SCLNEW
         endif
         XMEAN = XMEAN + DELTA / COUNT
         SUMSCL = SUMSCL + (PREV/COUNT) * (DELTA*RSCALE)**2
   20    continue
   30 continue
c
      STATS(1) = COUNT
      STATS(2) = XMIN
      STATS(3) = XMAX
      STATS(4) = XMEAN
      if(PREV .eq. ZERO) then
         STATS(5) = ZERO
      else
         STATS(5) = SCALE * sqrt( SUMSCL / PREV )
      endif
      return
      end
