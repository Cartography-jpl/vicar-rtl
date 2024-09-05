      SUBROUTINE DERVN (NVAL,LABELS,VALUES,FLAG)
C     .  Copyright (C) 1989, California Institute of Technology.
C     .  U. S. Government sponsorship under
C     .  NASA contract NAS7-918 is acknowledged.
C>> 1994-10-20 DERVN  Krogh  Changes to use M77CON
C>> 1989-11-10 DERVN  CLL
C>> 1987-10-02 DERVN  Lawson  Initial code.
C
C     ------------------------------------------------------------------
C
C     This subr prints an array of values with labels as part
C     of an error msg. Uses IALPHA from COMMON that must have
C     been previously set by ERMSG.
C
C     ------------------------------------------------------------------
C
C     NVAL     The number of single precision scalars to be printed.
C
C     LABELS() An array of character data to identify the individual
C              scalars.  There must be one character label for each scal
C              The character string length is used to determine how many
C              labelled scalars can be output on a single line (of 132).
C
C     VALUES() An array of single precision scalars to be printed with
C              their identifiers.
C
C     FLAG     A single character, which when set to '.' will
C              call the subroutine ERFIN and will just RETURN
C              when set to any other character.
C
C     ------------------------------------------------------------------
C     1989-11-10 CLL  Changed to keep length of printed line not
c     greater than MAXCOL = 75 characters.
C     ------------------------------------------------------------------
c--D replaces "?": ?ERVN
C     Both versions use ERFIN
C     ------------------------------------------------------------------
      integer I, IALPHA, IDELTA, K, LENIDV, MAXCOL, NUMBER, NVAL
      double precision VALUES(*)
      character FLAG, LABELS(*)*(*)
      parameter(MAXCOL = 75)
      common /M77ERR/IDELTA,IALPHA
      save /M77ERR/
C     ------------------------------------------------------------------
      if (IALPHA .ge. -1) then
        LENIDV = len (LABELS(1))
        NUMBER = MAXCOL/(LENIDV+17)
        do 10 I=1,NVAL,NUMBER
          write(*,'(4(2x,a,''='',g14.7))')
     *    (LABELS(K),VALUES(K),K=I,min(NVAL,I+NUMBER-1))
   10   continue
        if (FLAG .eq. '.') call ERFIN
      endif
      return
      end
