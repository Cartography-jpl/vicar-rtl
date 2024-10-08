      REAL             FUNCTION SSI (X)
C>> 1995-11-03 SSI Krogh  Removed blanks in numbers for C conversion.
C>> 1994-11-11 SSI Krogh   Declared all vars.
c>> 1994-10-20 SSI Krogh  Changes to use M77CON
c>> 1989-03-14 SSI Original W. V. Snyder at JPL
C
C     COMPUTE THE SINE INTEGRAL OF X =
C     INTEGRAL FROM 0 TO X OF (SIN(T)/T DT).
C
C     FOR ABS(X)<16, USE A CHEBYSHEV SERIES WITH ARGUMENT 2*Z*Z-1 WHERE
C     Z=X/16 TO EVALUATE SI(X)/Z, THEN MULTIPLY THE RESULT BY Z.  THIS
C     AVOIDS STORING ZERO COEFFICIENTS FOR EVEN ORDERS, AND PRESERVES
C     ACCURACY FOR SMALL Z.
C
C     FOR 16.LE.ABS(X).LE.100, USE CHEBYCHEV SERIES WITH ARGUMENT
C     2*Z*Z-1, WHERE Z=16/X ARE USED TO COMPUTE F(X)/X AND G(X)/(X*X).
C     THEN SI(X)=0.5*PI*SIGN(X)-F(X)/X*COS(X)-G(X)/(X*X)*SIN(X).
C
C     WHEN X.GT.100, USE ASYMPTOTIC APPROXIMATIONS FOR F(X)/X AND
C     G(X)/(X*X) AND COMPUTE SI(X) AS ABOVE.
C
C     THIS ALGORITHM YIELDS AT MOST 15 DIGITS OF PRECISION.
C
C--S replaces "?": ?SI, ?CPVAL
C
      INTEGER N
      REAL             X
      REAL             PI2,Z,ZW,FZ,GZ,SCPVAL
      REAL             FT,GT
      REAL             S(23),F(13),G(13)
      EXTERNAL SCPVAL
      DATA PI2/1.57079632679489662E0/
      DATA S/
     * + 0.5E0,                 + 0.5E0,
     1 + 4.052926477680623E0, - 4.063980844911986E0,
     2 + 2.778756381742663E0, - 1.926565091150656E0,
     3 + 1.389308771171888E0, - 0.968322236987086E0,
     4 + 0.530148847916522E0, - 0.211263780976555E0,
     5 + 0.062033679432003E0, - 0.013867445589417E0,
     6 + 0.002436221404749E0, - 0.000345469155569E0,
     7 + 0.000040420271419E0, - 0.000003972908746E0,
     8 + 0.000000332988589E0, - 0.000000024100076E0,
     9 + 0.000000001522370E0, - 0.000000000084710E0,
     A + 0.000000000004185E0, - 0.000000000000185E0,
     B + 0.000000000000007E0/
      DATA F/
     * + 0.5E0,                 + 0.5E0,
     1 + 0.062263729028927E0, - 0.000233756041393E0,
     2 + 0.000002453755677E0, - 0.000000058670317E0,
     3 + 0.000000002356196E0, - 0.000000000136096E0,
     4 + 0.000000000010308E0, - 0.000000000000964E0,
     5 + 0.000000000000107E0, - 0.000000000000014E0,
     6 + 0.000000000000002E0/
      DATA G/
     * + 0.5E0,                 + 0.5E0,
     1 + 0.003862856096703E0, - 0.000042644182622E0,
     2 + 0.000000724995950E0, - 0.000000023468225E0,
     3 + 0.000000001169202E0, - 0.000000000079604E0,
     4 + 0.000000000006875E0, - 0.000000000000717E0,
     5 + 0.000000000000087E0, - 0.000000000000012E0,
     6 + 0.000000000000002E0/
C
      IF (ABS(X).LT.16.0) THEN
         Z = X/16.0
         ZW = Z*Z
         Z = Z*SCPVAL(S,20,ZW)
      ELSE
        IF (ABS(X).LE.100.0) THEN
C           16.LE.ABS(X).LE.100
            Z = 16.0/X
            ZW = Z*Z
            FZ = Z*SCPVAL(F,10,ZW)
            GZ = ZW*SCPVAL(G,10,ZW)
        ELSE
C           ABS(X).GT.100
            FZ = 1.0/X
            FT = FZ
            GZ = FZ/X
            GT = GZ
            Z = GZ
            DO 25 N = 2, 16, 2
               FT = -REAL(N*(N-1))*Z*FT
               GT = -REAL(N*(N+1))*Z*GT
               FZ = FZ + FT
               GZ = GZ + GT
25          CONTINUE
         END IF
         Z = SIGN(PI2,X) - FZ*COS(X) - GZ*SIN(X)
      END IF
C
      SSI = Z
      RETURN
      END
