      SUBROUTINE DBESPQ(X,V,P,Q)
C     .  Copyright (C) 1989, California Institute of Technology.
C     .  All rights reserved.  U. S. Government sponsorship under
C     .  NASA contract NAS7-918 is acknowledged.
C>> 1994-10-19 DBESPQ Krogh  Changes to use M77CON
C>> 1994-04-19 DBESPQ CLL  Edited to make DP & SP files similar.
C>> 1992-03-13 DBESPQ FTK  Removed implicit statements.
C>> 1986-03-18 DBESPQ Lawson  Initial code.
c--D replaces "?": ?BESPQ, ?ERM1, ?ERV1
C                                                                       00001200
c     This subr evaluates asymptotic series for P and Q.
c     These can be used to compute Bessel functions by the formulas
c
c          J = sqrt(2/(pi*X)) * (P * cos(chi) - Q * sin(chi))
c
c          Y = sqrt(2/(pi*X)) * (P * sin(chi) + Q * cos(chi))
c     where
c          chi = X - (0.5 * V + 0.25) * pi
c
C          Reference: NBS AMS55 Eqs 9,2.9 and 9,2.10                    00002200
c
c          We assume V is limited to the range [0,2].
c          To compute P with a relative accuracy of at least
c     10**(-s), X must be restricted to be not less than XPQ,
c     where XPQ = 1.1293 * s - 0.59
c     (This formula for XPQ was determined for s in the range from
c     5 to 25, and limiting V to [0,2].)
c          Let s0 = -log10( ?1mach(3) )
c     and let s1 = s0 + .3,  s2 = s0 + .6
c     We will set XPQ using s2, and then sum the series till a          00003200
c     term less than 10**(-s1) is reached.
c     By setting XPQ using s2 we provide some tolerance to assure
c     that the series will contain a term less than 10**(-s1).
c        We add the constant term of each series in last to
c     reduce the amount of accumulated rounding error.
c
c     1984 Apr 2, JPL, C. L. Lawson and S. Chan.
c     ------------------------------------------------------------------
c
c     >     Let the terms of these two series be numbered               00004200
c     1, 3, 5,... in the P series, and 2, 4, 6,... in the Q series.
c     For X >> V these terms decrease in magnitude in the order 1, 2,
c     3, 4,... to some smallest term, say number N, and then
c     following terms increase in magnitude.
c     >     For V = 2 and for given X, let N be the number of the
c     smallest term, and let SIZE be the magitude of this
c     smallest term.  Here are some values of X, N, and -LOG10(SIZE):
c
c               X = 5    10     15     20     25     30     35
c               N =12    22     32     42     52     62     72          00005200
c     -LOG10(SIZE)= 4.79  9.35  13.81  18.23  22.63  27.02  31.40
c     ------------------------------------------------------------------
      DOUBLE PRECISION D1MACH
      DOUBLE PRECISION A0,A1,A2,B,C11293,C59,C9,CP3,EIGHT,EMU
      DOUBLE PRECISION FOUR,HALF,ONE,P,PSUM,Q,Q1,QSUM,SIG,SMALL,TERM
      DOUBLE PRECISION TWO,V,X,X8,XPQ,ZERO
      PARAMETER( ZERO = 0.D0)
      SAVE XPQ, SMALL
c     ----------
c     Subprograms called: LOG10                                         00006300
      EXTERNAL            D1MACH, DERM1, DERV1
c     ----------
C
      DATA ONE,TWO,FOUR,EIGHT,C9 / 1.D0,2.D0,4.D0,8.D0,9.D0 /
      DATA XPQ, C11293, CP3, C59 / ZERO, 1.1293D0, 0.30103D0, 0.59D0/
      DATA HALF / 0.5D0 /
c     ------------------------------------------------------------------
C
      IF( XPQ .EQ. ZERO )THEN
         SMALL = HALF * D1MACH(3)                                       00007300
         XPQ = C11293 * (CP3 - LOG10(SMALL)) - C59
      END IF
c     ------------------------------------------------------------------
      IF( X .LT. XPQ  .OR.  V .LT. ZERO  .OR.  V .GT. TWO)THEN
         CALL DERM1('DBESPQ',1,0,                                       
     * 'Require X .ge. XPQ and V in [0.,2.].',                          
     * 'X', X, ',')
         CALL DERV1( 'V', V, ',')
         CALL DERV1( 'XPQ', XPQ, '.')
         P = ZERO
         Q = ZERO
         RETURN                                                         00008500
      END IF
c     ------------------------------------------------------------------
      EMU = FOUR * (V*V)
      X8 = EIGHT * X
      A0 = C9
      A1 = EIGHT
      A2 = EIGHT
      B = TWO
      SIG = -ONE
      PSUM = ZERO                                                       00009500
      TERM = (EMU - ONE) / X8
      Q1 = TERM
      QSUM = ZERO
C
20006 CONTINUE
        TERM = TERM * (EMU-A0) / (B*X8)
      IF ( ABS(TERM) .LE. SMALL ) GO TO 20007
        PSUM = PSUM + SIG*TERM
        A1 = A1 + A2
        A0 = A0 + A1                                                    00010500
        B = B + 1
c
        TERM = TERM * (EMU-A0) / (B*X8)
      IF ( ABS(TERM) .LE. SMALL ) GO TO 20007
        QSUM = QSUM + SIG*TERM
        A1 = A1 + A2
        A0 = A0 + A1
        B = B + 1
c
        SIG = -SIG                                                      00011500
      GO TO 20006
20007 CONTINUE
C
      P = HALF + ( HALF + PSUM )
      Q = Q1  + QSUM
      RETURN
      END        
