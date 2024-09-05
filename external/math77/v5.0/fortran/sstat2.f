      subroutine SSTAT2(STATS, IHIST, NCELLS, X1, X2)
C     .  Copyright (C) 1989, California Institute of Technology.
C     .  U. S. Government sponsorship under
C     .  NASA contract NAS7-918 is acknowledged.
c>> 1994-10-20 SSTAT2 Krogh  Changes to use M77CON
c>> 1989-04-26 SSTAT2 CLL  Changed to use SPRPL instead of PRPL.
c>> 1987-11-24 SSTAT2 Lawson  Initial code.
c--S replaces "?": ?STAT2, ?PRPL, ?STAT1
c
c-- Begin mask code changes
c        This subr produces a printer-plot of the histogram from
c     IHIST() and prints the statistics from STATS(1:5).
c     This subr is intended to be used following the computation of
c     statistics and the building of a histogram by subr DSTAT1.
c        On entry STATS(1:5) must contain:
c             STATS(1) = Total count.
c             STATS(2) = Min
c             STATS(3) = Max
c             STATS(4) = Mean
c             STATS(5) = Standard deviation
c-- End mask code changes
c
c     See subr SSTAT1 for description of the other
c     subroutine arguments.
c     C. L. Lawson and S. Y. Chiu, JPL, Apr 1987.
c     ------------------------------------------------------------------
      real             BRKPT, FLMAXC, ONE, STATS(5), STEP, TEMP, X1, X2
      real             ZERO
      integer NCELLS, I, IHIST(NCELLS), MAXCNT, NLEN
      parameter(NLEN=30)
      character*(NLEN) IMAGE
      parameter(ONE = 1.0E0, ZERO = 0.0E0)
c     ------------------------------------------------------------------
C                        Get max count to scale the plot.
      MAXCNT=0
      do 25 I=1,NCELLS
         MAXCNT = max(IHIST(I),MAXCNT)
  25  continue
c                      The following 2 stmts convert integer to float.
      FLMAXC = MAXCNT
      TEMP = NCELLS-2
      STEP = (X2-X1)/TEMP
      write(*,99)
      if(IHIST(1) .ne. 0) then
         call SPRPL(real(IHIST(1)),'*',IMAGE,NLEN,
     *             ZERO,FLMAXC,.TRUE.)
         write(*,101) IHIST(1),IMAGE
      endif
      BRKPT = X1
      do 30 I=2,NCELLS-1
         call SPRPL(real(IHIST(I)),'*',IMAGE,NLEN,
     *             ZERO,FLMAXC,.TRUE.)
         write(*,100) BRKPT
         write(*,101) IHIST(I),IMAGE
         BRKPT = BRKPT + STEP
   30 continue
      write(*,100) BRKPT
      if(IHIST(NCELLS) .ne. 0) then
         call SPRPL(real(IHIST(NCELLS)),'*',IMAGE,NLEN,
     *             ZERO,FLMAXC,.TRUE.)
         write(*,101) IHIST(NCELLS),IMAGE
      endif
c                           Finished with printer-plot of histogram.
c
      write(*,'(1x/1x,5a)')
     * '  Count','   Minimum   ','   Maximum   ',
     * '    Mean     ','Std. Deviation'
      write(*,'(1x/1x,i7,3g13.5,g14.5)')
     * int(STATS(1)),STATS(2),STATS(3),STATS(4),STATS(5)
      return
  99  format(2X,'BREAK PT',5X,'COUNT ',9X,'PLOT OF COUNT')
 100  format(2X,F6.2,1X,
     *              '----------------------------------------------')
 101  format(13X,I7,5X,A)
        end
