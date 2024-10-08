      subroutine RAN1
C     .  Copyright (C) 1989, California Institute of Technology.
C     .  U. S. Government sponsorship under
C     .  NASA contract NAS7-918 is acknowledged.
c                            Program unit: RANPK1
c>> 1994-06-24 CLL Reorganized common. Using RANC[D/S]1 & RANC[D/S]2.
c>> 1992-03-13 CLL Fixed error in RAN0
c>> 1991-11-26 CLL Reorganized common. Using RANCM[A/D/S].
c>> 1991-11-22 CLL Added Entry RAN0 and common variables SGFLAG,DGFLAG
c>> 1991-01-15 CLL Reordered common contents for efficiency.
C>> 1990-01-23 CLL Corrected type stmt for SNUMS in common.
C>> 1987-04-22 RANPK1 Lawson  Initial code.
c
c        This program unit, RANPK1, along with RANPK2,
c     supports random number generation.
c
c        This prog unit has entries RAN1, RAN0, and RANPUT.
c     The library user can call RAN1 to initialize random number
c     generation at a standard initial seed,
c     or call RANPUT(KSEED) to initialize random number generation
c     at a seed value provided by the integer array, KSEED().
c
c     Other higher level random number subrs call RAN0 on their first
c     time flags to be sure the package is initialized.
c
c     As a result of any of these entries this subroutine will
c     set the pointers in the COMMON arrays to 1, indicating to higher
c     level random number subprograms that these buffer arrays are
c     empty.  It also sets SGFLAG and DGFLAG to .false. to indicate to
c     Gaussian generators that they have no internal saved value.
c
c     The user can determine the appropriate dimension for the array,
c     KSEED() by first calling the entry RANSIZ in prog unit RANPK2.
c
c     The user can retrieve the current seed value by calling entry,
c     RANGET in prog unit RANPK2.  This will be the seed that will be
c     used the next time a batch of random numbers are computed.  This
c     is not necessarily the seed associated with the next number that
c     will be returned.
c     C. L. Lawson, F. T. Krogh, & S. Y. Chiu, JPL, Apr 1987.
c     ------------------------------------------------------------------
      logical FIRST

      integer DSIZE
      parameter(DSIZE = 97)
      double precision DNUMS(DSIZE)
      common/RANCD2/DNUMS
      save  /RANCD2/
c
      integer SSIZE
      parameter(SSIZE = 97)
      real SNUMS(SSIZE)
      common/RANCS2/SNUMS
      save  /RANCS2/
c
      integer DPTR, SPTR
      logical DGFLAG, SGFLAG
      common/RANCD1/DPTR, DGFLAG
      common/RANCS1/SPTR, SGFLAG
      save  /RANCD1/, /RANCS1/
      integer KSEED(*)
      save FIRST
      data FIRST / .true. /
c     ------------------------------------------------------------------
c                      For use by library users: CALL RAN1
      call RN1
      go to 10
c     ------------------------------------------------------------------
c                      For use by other library subprograms: CALL RAN0
      entry RAN0
      if(.not. FIRST) return
      FIRST = .false.
      go to 10
c     ------------------------------------------------------------------
c                         For use by library users: CALL RANPUT(KSEED)
      entry RANPUT(KSEED)
      call   RNPUT(KSEED)
10    continue
      DPTR = 1
      SPTR = 1
      DGFLAG = .false.
      SGFLAG = .false.
      return
      end
