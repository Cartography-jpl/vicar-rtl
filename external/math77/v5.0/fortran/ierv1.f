      SUBROUTINE IERV1(LABEL,VALUE,FLAG)
C     .  Copyright (C) 1989, California Institute of Technology.
C     .  U. S. Government sponsorship under
C     .  NASA contract NAS7-918 is acknowledged.
C>> 1985-09-20 IERV1  Lawson  Initial code.
C
C     ------------------------------------------------------------
C     SUBROUTINE ARGUMENTS
C     --------------------
C     LABEL     An identifing name to be printed with VALUE.
C
C     VALUE     A integer to be printed.
C
C     FLAG      See write up for FLAG in ERMSG.
C
C     ------------------------------------------------------------
C
      COMMON/M77ERR/IDELTA,IALPHA
      INTEGER IDELTA,IALPHA,VALUE
      CHARACTER*(*) LABEL
      CHARACTER*1 FLAG
      SAVE /M77ERR/
C
      IF (IALPHA.GE.-1) THEN
        WRITE (*,1002) LABEL,VALUE
        IF (FLAG .EQ. '.') CALL ERFIN
      ENDIF
      RETURN
C
 1002 FORMAT(3X,A,' = ',I5)
      END
