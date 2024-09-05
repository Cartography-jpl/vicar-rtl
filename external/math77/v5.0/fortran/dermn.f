      SUBROUTINE DERMN (SUBNAM,INDIC,LEVEL,MSG,NVAL,
     1                  LABELS,VALUES,FLAG)
C     .  Copyright (C) 1989, California Institute of Technology.
C     .  U. S. Government sponsorship under
C     .  NASA contract NAS7-918 is acknowledged.
C>> 1994-10-20 DERMN  Krogh  Changes to use M77CON
C>> 1987-10-02 DERMN  Lawson  Initial code.
c--D replaces "?": ?ERMN, ?ERVN
C
C     ------------------------------------------------------------------
C
C     This subr calls ERMSG to initiate an error message and then calls
C     DERVN to print an array of values with labels.
C
C     ------------------------------------------------------------------
C     SUBROUTINE ARGUMENTS
C     --------------------
C     SUBNAM   A name that identifies the subprogram in which
C              the error occurs.
C
C     INDIC    An integer printed as part of the mininal error
C              message. It together with SUBNAM can be used to
C              uniquely identify an error.
C
C     LEVEL    The user sets LEVEL=2,0,or -2 to specify the
C              nominal action to be taken.
C              See subr ERMSG for interpretation of LEVEL.
C
C     MSG      Message to be printed to describe the error.
C
C     NVAL     The number of scalars to be printed.
C
C     LABELS() An array of character data to identify the individual
C              scalars.  There must be one character label for each scal
C              The character string length is used to determine how many
C              labelled scalars can be output on a single line (of 132).
C
C     VALUES() An array of scalars to be printed with their identifiers.
C
C     FLAG     A single character, which when set to '.' will
C              call the subroutine ERFIN and will just RETURN
C              when set to any other character.
C
C     ------------------------------------------------------------------
C
C     Kris Stewart, JPL, 1983 July.
C     C.Lawson & S.Chan, JPL, 1983,Nov.
C
C     ------------------------------------------------------------------
C
      CHARACTER*(*)  SUBNAM
      INTEGER  INDIC,LEVEL,NVAL
      CHARACTER*(*) MSG
      CHARACTER*(*) LABELS(*)
      DOUBLE PRECISION  VALUES(*)
      CHARACTER*1 FLAG
C
      CALL ERMSG(SUBNAM,INDIC,LEVEL,MSG,',')
      CALL DERVN (NVAL,LABELS,VALUES,FLAG)
C
      RETURN
C
      END
