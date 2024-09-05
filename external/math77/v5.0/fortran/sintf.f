      SUBROUTINE SINTF (ANSWER, WORK, IOPT)
C     .  Copyright (C) 1989, California Institute of Technology.
C     .  U. S. Government sponsorship under
C     .  NASA contract NAS7-918 is acknowledged.
C>> 1994-10-20 SINTF  Krogh  Changes to use M77CON
C>> 1990-01-23 SINTF  CLL Added type stmts. Avoids msgs from compilers.
C>> 1988-06-07 SINTF  Snyder  Replaced " by '.
C>> 1987-08-14 SINTF  Snyder  Initial code.
c--S replaces "?": ?INTF, ?INT
C
C     Dummy SINTF subprogram that prints
C     'You have neglected either to write the SINTF subprogram'
C     'or to select reverse communication before invoking SINT.'
C     and then STOPs.
C
      real             ANSWER, WORK(*)
      integer IOPT(*)
      PRINT *,'You have neglected either to write the SINTF subprogram'
      PRINT *,'or to select reverse communication before invoking SINT*'
      STOP
      END
