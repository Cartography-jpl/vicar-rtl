      subroutine IDSTA2(ISTATS, XSTATS, IHIST, ILOW, NCELLS)
C     .  Copyright (C) 1989, California Institute of Technology.
C     .  U. S. Government sponsorship under
C     .  NASA contract NAS7-918 is acknowledged.
c>> 1994-10-20 IDSTA2 Krogh  Changes to use M77CON
c>> 1994-06-22 IDSTA2 CLL  Changed name to I[D/S]STA2.
c>> 1989-04-26 ISTAT2 CLL  Changed to use DPRPL instead of PRPL.
c>> 1987-11-24 ISTAT2 Lawson  Initial code.
c
c        This subr produces a printer-plot of a histogram and prints
c     statistics given in ISTATS() and XSTATS().  It is
c     intended for use following the building of a histogram and
c     computation of statistics by ISTAT1.
c     See ISTAT1 for description of the subroutine arguments.
c     C. L. Lawson and S. Y. Chiu, JPL, Apr 1987.
c     ------------------------------------------------------------------
c--D replaces "?": I?STA2, ?PRPL
c     ------------------------------------------------------------------
      double precision    FLMAXC, XSTATS(2), ZERO
      integer MAXCNT, NCELLS, NLEN
      integer I, IHIST(NCELLS), ILOW, ISTATS(3), IVAL
      parameter(NLEN=30)
      character*(NLEN) IMAGE
      parameter(ZERO = 0.0D0)
c     ------------------------------------------------------------------
C                        Get max count to scale the plot.
      MAXCNT=0
      do 25 I=1,NCELLS
         MAXCNT = max(IHIST(I),MAXCNT)
  25  continue
c                 The following stmt converts from integer to float.
      FLMAXC = MAXCNT
      write(*,'(2X,a,5X,a,9X,a)')
     *    '   VALUE', 'COUNT ', 'PLOT OF COUNT'
      if(IHIST(1) .ne. 0) then
         call DPRPL(dble(IHIST(1)),'*',IMAGE,NLEN,
     *             ZERO,FLMAXC,.TRUE.)
         write(*,'(1x,a,i8,i10,5x,a)') '<',ILOW, IHIST(1), IMAGE
      endif
      IVAL = ILOW - 1
      do 30 I=2,NCELLS-1
         IVAL = IVAL + 1
         call DPRPL(dble(IHIST(I)),'*',IMAGE,NLEN,
     *             ZERO,FLMAXC,.TRUE.)
         write(*,'(1x,a,i8,i10,5x,a)') ' ', IVAL, IHIST(I), IMAGE
   30 continue
      if(IHIST(NCELLS) .ne. 0) then
         call DPRPL(dble(IHIST(NCELLS)),'*',IMAGE,NLEN,
     *             ZERO,FLMAXC,.TRUE.)
         write(*,'(1x,a,i8,i10,5x,a)') '>', IVAL, IHIST(NCELLS), IMAGE
      endif
c                           Finished with printer-plot of histogram.
c
      write(*,'(1x/1x,5a)')
     * '     Count','   Minimum','   Maximum',
     * '    Mean     ','Std. Deviation'
      write(*,'(1x/1x,3i10,g13.5,g14.5)')
     * ISTATS(1), ISTATS(2), ISTATS(3),XSTATS(1),XSTATS(2)
      return
      end
