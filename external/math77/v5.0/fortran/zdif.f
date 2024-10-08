      subroutine ZDIF(Z1, Z2, RESULT)
C     .  Copyright (C) 1989, California Institute of Technology.
C     .  U. S. Government sponsorship under
C     .  NASA contract NAS7-918 is acknowledged.
C>> 1995-10-30 ZDIF  Krogh   Fixed so M77CON can get S.P. for C conv.
C>> 1987-12-07 ZDIF  Lawson  Initial code.
c--Z replaces "?": ?DIF
C
C     Computes the difference of two double precision complex numbers.
C     Computes W = U - V where the data is given as
C        Z1(1)   = Real part of U
C        Z1(2)   = Imaginary part of U
C        Z2(1) = Real part of V
C        Z2(2) = Imaginary part of V
C     Result returned as
C        RESULT(1)  = Real part of W
C        RESULT(2)  = Imaginary part of W
C
C     C.L.Lawson & S.Chan, JPL, Feb 17, 1987.
C     ------------------------------------------------------------------
C
      double precision Z1(2),Z2(2),RESULT(2), TEMP
c
      TEMP = Z1(1) - Z2(1)
      RESULT(2) = Z1(2) - Z2(2)
      RESULT(1) = TEMP
      return
      end
