      FUNCTION SASINH(X)
C     .  Copyright (C) 1989, California Institute of Technology.
C     .  All rights reserved.  U. S. Government sponsorship under
C     .  NASA contract NAS7-918 is acknowledged.
C>> 1995-10-24 SASINH Krogh  Removed blanks in numbers for C conversion.
C>> 1994-10-19 SASINH Krogh  Changes to use M77CON
C>> 1994-04-19 SASINH CLL Edited type stmts to make DP & SP similar.
C>> 1992-03-13 SASINH FTK  Removed implicit statements.
C>> 1988-11-17 SASINH CLL  Using ?1MACH instead of ?2MACH.
C>> 1986-03-18 SASINH Lawson  Initial code.                             00001200
C--S replaces "?": ?ACOSH,?ACSCH,?ACTNH,?ASECH,?ASINH,?ATANH
C--   &              ?ERM1
C
C     This program unit has six entry points for computing
C     the six inverse hyperbolic functions.
C     Using the SFTRAN3 preprocessor.
C     Uses R1MACH(3) to obtain machine precision.
C     All critical constants are given correctly rounded
C     to 25 decimal places. Accuracy of the subprogram
C     adapts to the machine precision up to 25 decimal                  00002200
C     places precision.
C
C     -----------------------------------------------------------
C
C--   Begin mask code changes
C     The single precision code was tested in May 1983, on a
C     Univac 1100 using the JPL program ST4 that compares a
C     single precision function with a double precision function.
C     Each function was tested on at least 6000 points. Max
C     relative errors are expressed as a multiple of R1MACH(3),         00003200
C     which for the Univac 1100 is 2**(-27) = 0.745E-8 .
C
C     FUNCTION            RANGE                MAX REL ERROR
C     --------            -----                -------------
C      SASINH       ALL X                           2.4
C
C      SACOSH       1.0 .LE. X .LE. 1.03            LARGE
C                   1.03 .LE. X .LE. 1.21           3.4
C                   1.21 .LE. X                     2.3
C                                                                       00004200
C      SATANH       ABS(X) .LE. 0.44                3.2
C                   0.44 .LE. ABS(X) .LE. 0.92      5.1
C                   0.92 .LE. ABS(X) .LE. 1.0       LARGE
C
C      SACSCH       ABS(X) .NE. 0                   3.4
C
C      SASECH       0.0 .LT. X .LE. 0.24            2.0
C                   0.24 .LE. X .LE. 0.68           3.0
C                   0.68 .LE. X .LE. 0.88           10.0
C                   0.88 .LE. X .LE. 1.0            LARGE               00005200
C
C      SACTNH       1.0 .LE. ABS(X) .LE. 1.16       LARGE
C                   1.16 .LE. ABS(X) .LE. 2.20      7.1
C                   2.20 LE. ABS(X)                 3.7
C
C     -----------------------------------------------------------
C
C     The D.P. and S.P. codes have each been tested using
C     identities such as X - SINH( ASINH(X) ) = 0. The
C     D.P. codes have about the same accuracy relative to               00006200
C     D1MACH(3) as do the S.P. codes relative to R1MACH(3).
C
C--   End mask code changes
C     Error Messages
C     1) Arg .LT. 1                        ( IN SACOSH )
C     2) Arg .LT. or .EQ. to 1             ( IN SATANH )
C     3) ABS(Arg) .LT. or .EQ. to 1        ( IN SACTNH )
C     4) Arg .EQ. 0                        ( IN SACSCH )
C     5) Arg .LT. or .EQ. 0 or Arg .GT. 1  ( IN SASECH )
C                                                                       00007200
C     -----------------------------------------------------------
C
C     C.L.Lawson and Stella Chan,JPL,Feb 23,1983.
C
      SAVE FIRST,NMAX
      LOGICAL FIRST
      INTEGER I,II,INT,NMAX,NPR001,NPR002,NPR003,NPR004,NPR005
      REAL             R1MACH,SACOSH,SACSCH,SACTNH,SASECH,SASINH,SATANH
      REAL             B1,B2,C(0:13),CU,CUT1,CUT2,CV(8)
      REAL             DIF,HALF,HICUT,LOGE2,ONE,P,Q,QSQ,R,RSQ,SU,SV(8)  00008300
      REAL             TERM,U,V(8),X,ZERO
      DATA ONE,ZERO / 1.E0,0.E0 /
      DATA LOGE2 / 0.6931471805599453094172321E0 /
      DATA CUT1,CUT2,HICUT / 0.463E0,1.176E0,1.E16 /
      DATA B1,B2 / 0.52344E0, 4.4306E0 /
      DATA FIRST / .TRUE. /
      DATA HALF / 0.5E0 /
C
C     Coeffs for SASINH series. C(0) is half of its true value.
C     This series will be used only for arguments, R , in the           00009300
C     range 0 .LE. R .LE. SINH(0.125) = 0.125326 .
C
      DATA   C( 0)/0.5E0/
      DATA   C( 1) /-.1666666666666666666666667E+00/
      DATA   C( 2) /+.7500000000000000000000000E-01/
      DATA   C( 3) /-.4464285714285714285714286E-01/
      DATA   C( 4) /+.3038194444444444444444444E-01/
      DATA   C( 5) /-.2237215909090909090909091E-01/
      DATA   C( 6) /+.1735276442307692307692308E-01/
      DATA   C( 7) /-.1396484375000000000000000E-01/                    00010300
      DATA   C( 8) /+.1155180089613970588235294E-01/
      DATA   C( 9) /-.9761609529194078947368421E-02/
      DATA   C(10) /+.8390335809616815476190476E-02/
      DATA   C(11) /-.7312525873598845108695652E-02/
      DATA   C(12) /+.6447210311889648437500000E-02/
      DATA   C(13) /-.5740037670841923466435185E-02/
C
C     SV(I) = SINH(V(I))
C     CV(I) = COSH(V(I))
C                                                                       00011300
      DATA   V( 1) /+.1250000000000000000000000E+00/
      DATA  SV( 1) /+.1253257752411154569820575E+00/
      DATA  CV( 1) /+.1007822677825710859846950E+01/
      DATA   V( 2) /+.2500000000000000000000000E+00/
      DATA  SV( 2) /+.2526123168081683079141252E+00/
      DATA  CV( 2) /+.1031413099879573176159295E+01/
      DATA   V( 3) /+.3750000000000000000000000E+00/
      DATA  SV( 3) /+.3838510679136145687542957E+00/
      DATA  CV( 3) /+.1071140346704586767299498E+01/
      DATA   V( 4) /+.5000000000000000000000000E+00/                    00012300
      DATA  SV( 4) /+.5210953054937473616224256E+00/
      DATA  CV( 4) /+.1127625965206380785226225E+01/
      DATA   V( 5) /+.6250000000000000000000000E+00/
      DATA  SV( 5) /+.6664922644566160822726066E+00/
      DATA  CV( 5) /+.1201753692975606324229229E+01/
      DATA   V( 6) /+.7500000000000000000000000E+00/
      DATA  SV( 6) /+.8223167319358299807036616E+00/
      DATA  CV( 6) /+.1294683284676844687841708E+01/
      DATA   V( 7) /+.8750000000000000000000000E+00/
      DATA  SV( 7) /+.9910066371442947560531743E+00/                    00013300
      DATA  CV( 7) /+.1407868656822803158638471E+01/
      DATA   V( 8) /+.1000000000000000000000000E+01/
      DATA  SV( 8) /+.1175201193643801456882382E+01/
      DATA  CV( 8) /+.1543080634815243778477906E+01/
C
C     Begin computation for SASINH(x)
C     Defined for all x.The value u ranges from negative infinity to
C     positive infinity as x ranges from negative infinity to positive i
 
C                                                                       00014300
      P=ABS(X)
      IF (.NOT.(P .EQ. ZERO)) GO TO 20003
        SASINH=ZERO
      GO TO 20002
20003 ASSIGN 20004 TO NPR001
      GO TO 30001
20004   SASINH=SIGN(U,X)
20002 RETURN
C
C
      ENTRY SACOSH(X)                                                   00015400
C
C     Defined (double-valued) for all X greater than or equal to 1.
C     We compute the nonnegative value U ranging from zero to infinity
C     as X ranges from 1 to infinity.The second value would be -U.
C
      IF (.NOT.(X .LT. ONE)) GO TO 20006
        SACOSH=ZERO
        CALL SERM1('SACOSH',1,0,'ARG.LT.1','ARG',X,'.')
      GO TO 20005
20006   P=X                                                             00016400
      ASSIGN 20007 TO NPR002
      GO TO 30002
20007   SACOSH=U
20005 RETURN
C
C
      ENTRY SATANH(X)
C
C     Defined for -1 < x < +1. The value U ranges from negative
C     infinity to positive infinity.
C                                                                       00017500
      Q=ABS(X)
      IF (.NOT.(Q .GE. ONE)) GO TO 20009
        SATANH=ZERO
        CALL SERM1('SATANH',2,0,'ABS(ARG).GE.1','ARG',X,'.')
      GO TO 20008
20009 IF (.NOT.(Q .EQ. ZERO)) GO TO 20010
        SATANH=ZERO
      GO TO 20008
20010 ASSIGN 20011 TO NPR003
      GO TO 30003
20011   SATANH=SIGN(U,X)
20008 RETURN                                                            00018600
C
C
      ENTRY SACTNH(X)
C
C     Defined for ABS(X) > 1. The value U ranges from zero to negative
C     infinity as X ranges from negative infinity to -1,and from
C     positive infinity to zero as X ranges from 1 to positive
C     infinity.
C
      P=ABS(X)                                                          00019600
      IF (.NOT.(P .LE. ONE)) GO TO 20013
        SACTNH=ZERO
        CALL SERM1('SACTNH',3,0,'ABS(ARG).LE.1','ARG',X,'.')
      GO TO 20012
20013   Q=ONE/P
      ASSIGN 20014 TO NPR003
      GO TO 30003
20014   SACTNH=SIGN(U,X)
20012 RETURN
C
C                                                                       00020700
      ENTRY SACSCH(X)
C
C     Defined for all X not equal to 0. The value U varies from
C     zero to negative infinity as X ranges from negative
C     infinity to zero,jumps from negative infinity to positive
C     infinity at zero, and varies from positive infinity to
C     zero as X ranges from zero to positive infinity.
C
      P=ABS(X)
      IF (.NOT.(P .EQ. ZERO)) GO TO 20016                               00021700
        SACSCH=ZERO
        CALL SERM1('SACSCH',4,0,'ARG.EQ.0','ARG',X,'.')
      GO TO 20015
20016   P=ONE/P
      ASSIGN 20017 TO NPR001
      GO TO 30001
20017   SACSCH=SIGN(U,X)
20015 RETURN
C
C
      ENTRY SASECH(X)                                                   00022800
C
C     Defined  (double valued) for X greater than zero and X less than
C     or equal to one. We compute the nonnegative value U that varies
C     from infinity to zero as X varies from zero to one. The other
C     value would be -U.
C
      IF (.NOT.(X .LE. ZERO .OR. X .GT. ONE)) GO TO 20019
        SASECH=ZERO
        CALL SERM1('SASECH',5,0,'ARG.GT.1 OR ARG.LE.0','ARG',X,'.')
      GO TO 20018                                                       00023800
20019   P=ONE/X
      ASSIGN 20020 TO NPR002
      GO TO 30002
20020   SASECH=U
20018 RETURN
C
C
C     PROCEDURE (U.EQ.SASINH(P))
C
C     Here P > 0.
C     We break the p range into three intervals separated by CUT2       00024900
C     and HICUT. In the middle range between CUT2 and HICUT we use
C           U=LOG(P+SQRT(1+P**2))
C     CUT2 is set to 1.176 to keep the argument of LOG() greater
C     than e = 2.718 to avoid amplification of relative error by LOG().
C       We set HICUT = 10.**16 assuming that all machines on which
C     this code is to run have overflow limit greater than 10.**32
C     and precision not greater than 32 decimal places. Thus we
C     assume that HICUT**2 would not overflow and HICUT**2 plus or
C     minus ONE would evaluate to HICUT**2.
C     The idea of using U = LOG(P) + LOG(2) for P .GT. HICUT is         00025900
C     copied from an ASINH subroutine due to WAYNE FULLERTON.
C
C
30001 IF (.NOT.(P .GT. HICUT)) GO TO 20022
          U = LOG(P) + LOGE2
      GO TO 20021
20022     SU=P
          CU=SQRT(ONE+P**2)
      IF (.NOT.(SU .LT. CUT2)) GO TO 20024
      ASSIGN 20025 TO NPR004                                            00026900
      GO TO 30004
20025 GO TO 20023
20024       U=LOG(SU+CU)
20023 CONTINUE
20021 GO TO NPR001,(20004,20017)
C
C
C     PROCEDURE (U.EQ.SACOSH(P))
C
C     Here P .GE. 1.
C     See comments in procedure (U.EQ.SASINH(P)).                       00028000
C     The mid-range formula for SACOSH is:
C         U=LOG(P+SQRT(P**2-1))
C
30002 IF (.NOT.(P .GT. HICUT)) GO TO 20027
          U = LOG(P) + LOGE2
      GO TO 20026
20027     SU = SQRT( (P-ONE)*(P+ONE) )
          CU=P
      IF (.NOT.(SU .LT. CUT2)) GO TO 20029
      ASSIGN 20030 TO NPR004                                            00029000
      GO TO 30004
20030 GO TO 20028
20029       U=LOG(SU+CU)
20028 CONTINUE
20026 GO TO NPR002,(20007,20020)
C
C
C     PROCEDURE (U.EQ.SATANH(Q))
C
C     Here Q satisfies 0 < Q < 1.
C     When Q exceeds CUT1 = 0.463 then (1+Q)/(1-Q) > e = 2.718,         00030100
C     and this is large enough to be used as an argument
C     to LOG() without amplification of relative error.
C     When Q is less than CUT1 we transform Q to COSH(U)
C     and SINH(U) and use procedure (LOW RANGE).
C
C     The simple formulas for CU and SU would be
C           CU = ONE / SQRT(ONE - Q**2)
C     and   SU = Q / SQRT(ONE - Q**2)
C     The more complicated formulas used here have less
C     accumulation of round-of error since                              00031100
C     0.0 .LT. TERM .LT. 0.1283
C
30003 IF (.NOT.(Q .LT. CUT1)) GO TO 20032
          QSQ = Q * Q
          DIF = ONE - QSQ
          TERM = QSQ / (DIF + SQRT(DIF))
          CU = ONE + TERM
          SU = Q + (Q * TERM)
      ASSIGN 20033 TO NPR004
      GO TO 30004
20033 GO TO 20031                                                       00032100
20032     U=HALF * LOG((ONE+Q)/(ONE-Q))
20031 GO TO NPR003,(20011,20014)
C
C
C     PROCEDURE (LOW RANGE)
C
C     This procedure controls computation of U, given
C     SU=SINH(U) and CU=COSH(U).These will satisfy
C          0 .LE. SU .LE. 1.176
C     AND  0 .LE. CU .LE. 1.5437                                        00033200
C     so the result, U , will satisfy
C          0 .LE. U .LE. 1.00052
C
30004 IF (.NOT.(SU .LE. SV(1))) GO TO 20035
          R = SU
      ASSIGN 20036 TO NPR005
      GO TO 30005
20036 GO TO 20034
20035     II = MIN ( 7,INT(SU/SV(1)) )
          IF (SU .LT. SV(II)) II = II - 1
C                                                                       00034200
C     Here we transform to an argument, R , in the range
C     0 .LE. R .LE. SINH(0.125). The simple formula would
C     be :     R = SU * CV(II) - SV(II) * CU
C     However we transform this to extract the factor
C     (SU - SV(II)) for better control of round-off error.
C
          R = (SU-SV(II))*(CV(II)-(SV(II)+SU)*SV(II)/(CV(II)+CU))
      ASSIGN 20037 TO NPR005
      GO TO 30005
20037     U = V(II) + U
20034 GO TO NPR004,(20025,20030,20033)                                  00035300
C
C
C     PROCEDURE (SASINH SERIES)
C
C     Here the argument R satisfies 0 .LE. R .LE. SINH(0.125) = 0.125326
C
C     ------------------------------------------------------------------
C
C     On the first call to this procedure the value NMAX
C     will be set and saved.                                            00036300
C     FOR MACHINE PRECISION BETWEEN  4.43062 AND  6.45985 USE NMAX =  2
C     FOR MACHINE PRECISION BETWEEN  6.45985 AND  8.43090 USE NMAX =  3
C     FOR MACHINE PRECISION BETWEEN  8.43090 AND 10.36773 USE NMAX =  4
C     FOR MACHINE PRECISION BETWEEN 10.36773 AND 12.28199 USE NMAX =  5
C     FOR MACHINE PRECISION BETWEEN 12.28199 AND 14.18024 USE NMAX =  6
C     FOR MACHINE PRECISION BETWEEN 14.18024 AND 16.06654 USE NMAX =  7
C     FOR MACHINE PRECISION BETWEEN 16.06654 AND 17.94359 USE NMAX =  8
C     FOR MACHINE PRECISION BETWEEN 17.94359 AND 19.81325 USE NMAX =  9
C     FOR MACHINE PRECISION BETWEEN 19.81325 AND 21.67688 USE NMAX = 10
C     FOR MACHINE PRECISION BETWEEN 21.67688 AND 23.53550 USE NMAX = 11 00037300
C     FOR MACHINE PRECISION BETWEEN 23.53550 AND 25.38987 USE NMAX = 12
C
C     The following linear expression approximate the NMAX
C     selection criteria described above. The expression
C     never sets NMAX too small but will set NMAX too
C     large, by one, in boundary cases.
C
C     ------------------------------------------------------------------
C
30005 IF(FIRST)THEN                                                     00038300
        FIRST = .FALSE.
        NMAX = 2 + INT(B1 * (-log10(R1MACH(3)) - B2))
        NMAX = MAX(2,MIN(NMAX,12))
      END IF
C
      RSQ = R*R
      U = C(NMAX)
      DO 20040 I = NMAX-1,0,-1
        U = RSQ * U  + C(I)
20040 CONTINUE                                                          00039300
      U = R * (U + HALF)
      GO TO NPR005,(20036,20037)
C
C
      END        
