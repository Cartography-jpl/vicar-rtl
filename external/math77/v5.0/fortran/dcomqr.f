      SUBROUTINE DCOMQR(NM,N,LOW,IGH,HR,HI,Z,IERR)
C>> 1995-01-03 DCOMQR WVS  Added EXTERNAL ZQUO, ZSQRT so VAX won't gripe
C>> 1992-03-13 DCOMQR FTK  Removed implicit statements.
C>> 1987-11-12 DCOMQR Lawson  Initial code.
c     ------------------------------------------------------------------
c     Double Precision version of the Eispack subr, COMQR, for use in
c     the JPL MATH77 library.  C. L. Lawson, JPL, 1987 Feb 17.
c     ------------------------------------------------------------------
      INTEGER EN,ENM1,I,IERR,IGH,ITS,J,L,LL,LOW,LP1,MIN0,N,NM
      DOUBLE PRECISION D1MACH,DZABS,HI(NM,N),HR(NM,N),MACHEP
      DOUBLE PRECISION NORM,SI,SR,TI,TR,XI,XR,YI,YR,Z(2*N),Z1(2),Z2(2)
      DOUBLE PRECISION ZDENOM(2),ZIN(2),ZNUM(2),ZOUT(2),ZQUOT(2),ZZI,ZZR
      EXTERNAL ZQUO, ZSQRT
C
C     THIS SUBROUTINE IS A TRANSLATION OF A UNITARY ANALOGUE OF THE
C     ALGOL PROCEDURE  COMLR, NUM. MATH. 12, 369-376(1968) BY MARTIN
C     AND WILKINSON.
C     HANDBOOK FOR AUTO. COMP., VOL.II-LINEAR ALGEBRA, 396-403(1971).
C     THE UNITARY ANALOGUE SUBSTITUTES THE QR ALGORITHM OF FRANCIS
C     (COMP. JOUR. 4, 332-345(1962)) FOR THE LR ALGORITHM.
C
C     THIS SUBROUTINE FINDS THE EIGENVALUES OF A COMPLEX
C     UPPER HESSENBERG MATRIX BY THE QR METHOD.
C
C     ON INPUT-
C
C        NM MUST BE SET TO THE ROW DIMENSION OF TWO-DIMENSIONAL
C          ARRAY PARAMETERS AS DECLARED IN THE CALLING PROGRAM
C          DIMENSION STATEMENT,
C
C        N IS THE ORDER OF THE MATRIX,
C
C        LOW AND IGH ARE INTEGERS DETERMINED BY THE BALANCING
C          SUBROUTINE  CBAL.  IF  CBAL  HAS NOT BEEN USED,
C          SET LOW=1, IGH=N,
C
C        HR AND HI CONTAIN THE REAL AND IMAGINARY PARTS,
C          RESPECTIVELY, OF THE COMPLEX UPPER HESSENBERG MATRIX.
C          THEIR LOWER TRIANGLES BELOW THE SUBDIAGONAL CONTAIN
C          INFORMATION ABOUT THE UNITARY TRANSFORMATIONS USED IN
C          THE REDUCTION BY  CORTH, IF PERFORMED.
C
C     ON OUTPUT-
C
C        THE UPPER HESSENBERG PORTIONS OF HR AND HI HAVE BEEN
C          DESTROYED.  THEREFORE, THEY MUST BE SAVED BEFORE
C          CALLING  COMQR  IF SUBSEQUENT CALCULATION OF
C          EIGENVECTORS IS TO BE PERFORMED,
C
C        WR AND WI CONTAIN THE REAL AND IMAGINARY PARTS,
C          RESPECTIVELY, OF THE EIGENVALUES.  IF AN ERROR
C          EXIT IS MADE, THE EIGENVALUES SHOULD BE CORRECT
C          FOR INDICES IERR+1,...,N,
C
C        IERR IS SET TO
C          ZERO       FOR NORMAL RETURN,
C          J          IF THE J-TH EIGENVALUE HAS NOT BEEN
C                     DETERMINED AFTER 30 ITERATIONS.
C
C     ARITHMETIC IS REAL EXCEPT FOR THE REPLACEMENT OF THE ALGOL
C     PROCEDURE CDIV BY COMPLEX DIVISION AND USE OF THE SUBROUTINES
C     CSQRT AND CMPLX IN COMPUTING COMPLEX SQUARE ROOTS.
C
C     QUESTIONS AND COMMENTS SHOULD BE DIRECTED TO B. S. GARBOW,
C     APPLIED MATHEMATICS DIVISION, ARGONNE NATIONAL LABORATORY
C
C     ------------------------------------------------------------------
C
C     ********** MACHEP IS A MACHINE DEPENDENT PARAMETER SPECIFYING
C                THE RELATIVE PRECISION OF FLOATING POINT ARITHMETIC.
C
C                **********
      MACHEP = D1MACH(4)
C
      IERR = 0
      IF (LOW .EQ. IGH) GO TO 180
C     ********** CREATE REAL SUBDIAGONAL ELEMENTS **********
      L = LOW + 1
C
      DO 170 I = L, IGH
         LL = MIN0(I+1,IGH)
         IF (HI(I,I-1) .EQ. 0.0) GO TO 170
C        NORM = CABS(CMPLX(HR(I,I-1),HI(I,I-1)))
      Z1(1) = HR(I,I-1)
      Z1(2) = HR(I,I-1)
      NORM = DZABS(Z1)
         YR = HR(I,I-1) / NORM
         YI = HI(I,I-1) / NORM
         HR(I,I-1) = NORM
         HI(I,I-1) = 0.0
C
         DO 155 J = I, IGH
            SI = YR * HI(I,J) - YI * HR(I,J)
            HR(I,J) = YR * HR(I,J) + YI * HI(I,J)
            HI(I,J) = SI
  155    CONTINUE
C
         DO 160 J = LOW, LL
            SI = YR * HI(J,I) + YI * HR(J,I)
            HR(J,I) = YR * HR(J,I) - YI * HI(J,I)
            HI(J,I) = SI
  160    CONTINUE
C
  170 CONTINUE
C     ********** STORE ROOTS ISOLATED BY CBAL **********
  180 DO 200 I = 1, N
         IF (I .GE. LOW .AND. I .LE. IGH) GO TO 200
         Z(2*I-1) = HR(I,I)
         Z(2*I) = HI(I,I)
  200 CONTINUE
C
      EN = IGH
      TR = 0.0
      TI = 0.0
C     ********** SEARCH FOR NEXT EIGENVALUE **********
  220 IF (EN .LT. LOW) GO TO 1001
      ITS = 0
      ENM1 = EN - 1
C     ********** LOOK FOR SINGLE SMALL SUB-DIAGONAL ELEMENT
C                FOR L=EN STEP -1 UNTIL LOW  -- **********
  240 DO 260 LL = LOW, EN
         L = EN + LOW - LL
         IF (L .EQ. LOW) GO TO 300
         IF (ABS(HR(L,L-1)) .LE.
     X      MACHEP * (ABS(HR(L-1,L-1)) + ABS(HI(L-1,L-1))
     X             + ABS(HR(L,L)) +ABS(HI(L,L)))) GO TO 300
  260 CONTINUE
C     ********** FORM SHIFT **********
  300 IF (L .EQ. EN) GO TO 660
      IF (ITS .EQ. 30) GO TO 1000
      IF (ITS .EQ. 10 .OR. ITS .EQ. 20) GO TO 320
      SR = HR(EN,EN)
      SI = HI(EN,EN)
      XR = HR(ENM1,EN) * HR(EN,ENM1)
      XI = HI(ENM1,EN) * HR(EN,ENM1)
      IF (XR .EQ. 0.0 .AND. XI .EQ. 0.0) GO TO 340
      YR = (HR(ENM1,ENM1) - SR) / 2.0
      YI = (HI(ENM1,ENM1) - SI) / 2.0
      ZIN(1) = YR**2 - YI**2 + XR
      ZIN(2) = 2.D0*YR*YI + XI
      CALL ZSQRT(ZIN,ZOUT)
      ZZR = ZOUT(1)
      ZZI = ZOUT(2)
      IF (YR * ZZR + YI * ZZI .GE. 0.0) GO TO 310
      ZZR = -ZZR
      ZZI = -ZZI
  310 CONTINUE
      ZNUM(1) = XR
      ZNUM(2) = XI
      ZDENOM(1) = YR + ZZR
      ZDENOM(2) = YI + ZZI
c                      ZQUO computes quotient of two DP complex numbers.
      CALL ZQUO(ZNUM,ZDENOM,ZQUOT)
      SR = SR - ZQUOT(1)
      SI = SI - ZQUOT(2)
      GO TO 340
C     ********** FORM EXCEPTIONAL SHIFT **********
  320 SR = ABS(HR(EN,ENM1)) + ABS(HR(ENM1,EN-2))
      SI = 0.0
C
  340 DO 360 I = LOW, EN
         HR(I,I) = HR(I,I) - SR
         HI(I,I) = HI(I,I) - SI
  360 CONTINUE
C
      TR = TR + SR
      TI = TI + SI
      ITS = ITS + 1
C     ********** REDUCE TO TRIANGLE (ROWS) **********
      LP1 = L + 1
C
      DO 500 I = LP1, EN
         SR = HR(I,I-1)
         HR(I,I-1) = 0.0
         NORM = SQRT(HR(I-1,I-1)*HR(I-1,I-1)+HI(I-1,I-1)*HI(I-1,I-1)
     X               +SR*SR)
         XR = HR(I-1,I-1) / NORM
         Z(2*I-3) = XR
         XI = HI(I-1,I-1) / NORM
         Z(2*I-2) = XI
         HR(I-1,I-1) = NORM
         HI(I-1,I-1) = 0.0
         HI(I,I-1) = SR / NORM
C
         DO 490 J = I, EN
            YR = HR(I-1,J)
            YI = HI(I-1,J)
            ZZR = HR(I,J)
            ZZI = HI(I,J)
            HR(I-1,J) = XR * YR + XI * YI + HI(I,I-1) * ZZR
            HI(I-1,J) = XR * YI - XI * YR + HI(I,I-1) * ZZI
            HR(I,J) = XR * ZZR - XI * ZZI - HI(I,I-1) * YR
            HI(I,J) = XR * ZZI + XI * ZZR - HI(I,I-1) * YI
  490    CONTINUE
C
  500 CONTINUE
C
      SI = HI(EN,EN)
      IF (SI .EQ. 0.0) GO TO 540
C     NORM = CABS(CMPLX(HR(EN,EN),SI))
      Z2(1) = HR(EN,EN)
      Z2(2) = SI
      NORM = DZABS(Z2)
      SR = HR(EN,EN) / NORM
      SI = SI / NORM
      HR(EN,EN) = NORM
      HI(EN,EN) = 0.0
C     ********** INVERSE OPERATION (COLUMNS) **********
  540 DO 600 J = LP1, EN
         XR = Z(2*J-3)
         XI = Z(2*J-2)
C
         DO 580 I = L, J
            YR = HR(I,J-1)
            YI = 0.0
            ZZR = HR(I,J)
            ZZI = HI(I,J)
            IF (I .EQ. J) GO TO 560
            YI = HI(I,J-1)
            HI(I,J-1) = XR * YI + XI * YR + HI(J,J-1) * ZZI
  560       HR(I,J-1) = XR * YR - XI * YI + HI(J,J-1) * ZZR
            HR(I,J) = XR * ZZR + XI * ZZI - HI(J,J-1) * YR
            HI(I,J) = XR * ZZI - XI * ZZR - HI(J,J-1) * YI
  580    CONTINUE
C
  600 CONTINUE
C
      IF (SI .EQ. 0.0) GO TO 240
C
      DO 630 I = L, EN
         YR = HR(I,EN)
         YI = HI(I,EN)
         HR(I,EN) = SR * YR - SI * YI
         HI(I,EN) = SR * YI + SI * YR
  630 CONTINUE
C
      GO TO 240
C     ********** A ROOT FOUND **********
  660 Z(2*EN-1) = HR(EN,EN) + TR
      Z(2*EN) = HI(EN,EN) + TI
      EN = ENM1
      GO TO 220
C     ********** SET ERROR -- NO CONVERGENCE TO AN
C                EIGENVALUE AFTER 30 ITERATIONS **********
 1000 IERR = EN
 1001 RETURN
C     ********** LAST CARD OF COMQR **********
      END
