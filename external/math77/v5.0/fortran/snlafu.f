      SUBROUTINE    SNLAFU(N, P, X, SCALCR, IV, LIV, LV, V)
c>> 1994-10-20 SNLAFU Krogh  Changes to use M77CON
c>> 1990-06-29 SNLAFU C. L. Lawson, JPL
c>> 1990-01-31 C. L. Lawson, JPL
C
C  ***  MINIMIZE A NONLINEAR SUM OF SQUARES USING RESIDUAL VALUES ONLY.
C  ***  THIS AMOUNTS TO SNLAGU WITHOUT THE SUBROUTINE PARAMETER SCALCJ.
C
C  ***  PARAMETERS  ***
C
      EXTERNAL SCALCR
      INTEGER N, P, LIV, LV
      INTEGER IV(LIV)
      REAL             X(P), V(LV)
C
C ----------------------------  DISCUSSION  ----------------------------
C
C        THIS AMOUNTS TO SUBROUTINE NL2SNO (REF. 1) MODIFIED TO CALL
C       SRN2G.
C        THE PARAMETERS FOR SNLAFU ARE THE SAME AS THOSE FOR SNLAGU
C     (WHICH SEE), EXCEPT THAT SCALCJ IS OMITTED.  INSTEAD OF CALLING
C     SCALCJ TO OBTAIN THE JACOBIAN MATRIX OF R AT X,    SNLAFU COMPUTES
C     AN APPROXIMATION TO IT BY FINITE (FORWARD) DIFFERENCES -- SEE
C     V(DLTFDJ) BELOW.     SNLAFU USES FUNCTION VALUES ONLY WHEN COMPUT-
C     THE COVARIANCE MATRIX (RATHER THAN THE FUNCTIONS AND GRADIENTS
C     THAT SNLAGU MAY USE).  TO DO SO, SNLAFU SETS IV(COVREQ) TO MINUS
C     ITS ABSOLUTE VALUE.  THUS V(DELTA0) IS NEVER REFERENCED AND ONLY
C     V(DLTFDC) MATTERS -- SEE NL2SOL FOR A DESCRIPTION OF V(DLTFDC).
C        THE NUMBER OF EXTRA CALLS ON SCALCR USED IN COMPUTING THE JACO-
C     BIAN APPROXIMATION ARE NOT INCLUDED IN THE FUNCTION EVALUATION
C     COUNT IV(NFCALL), BUT ARE RECORDED IN IV(NGCALL) INSTEAD.
C
C V(DLTFDJ)... V(43) HELPS CHOOSE THE STEP SIZE USED WHEN COMPUTING THE
C             FINITE-DIFFERENCE JACOBIAN MATRIX.  FOR DIFFERENCES IN-
C             VOLVING X(I), THE STEP SIZE FIRST TRIED IS
C                       V(DLTFDJ) * MAX(ABS(X(I)), 1/D(I)),
C             WHERE D IS THE CURRENT SCALE VECTOR (SEE REF. 1).  (IF
C             THIS STEP IS TOO BIG, I.E., IF SCALCR SETS NF TO 0, THEN
C             SMALLER STEPS ARE TRIED UNTIL THE STEP SIZE IS SHRUNK BE-
C             LOW 1000 * MACHEP, WHERE MACHEP IS THE UNIT ROUNDOFF.
C             DEFAULT = MACHEP**0.5.
C
C  ***  REFERENCE  ***
C
C 1.  DENNIS, J.E., GAY, D.M., AND WELSCH, R.E. (1981), AN ADAPTIVE
C             NONLINEAR LEAST-SQUARES ALGORITHM, ACM TRANS. MATH.
C             SOFTWARE, VOL. 7, NO. 3.
C
C  ***  GENERAL  ***
C
C     CODED BY DAVID M. GAY.
C
C ++++++++++++++++++++++++++  DECLARATIONS  +++++++++++++++++++++++++++
C
C  ***  EXTERNAL SUBROUTINES  ***
C
      EXTERNAL SIVSET, SRN2G, SN2RDP, SV7SCP
C
c--S replaces "?": ?NLAFU, ?NLAGU, ?RN2G, ?IVSET, ?N2RDP, ?V7SCP
c--&                 ?CALCR, ?CALCJ
c
C  SIVSET.... PROVIDES DEFAULT IV AND V INPUT COMPONENTS.
C  SRN2G...   CARRIES OUT OPTIMIZATION ITERATIONS.
C  SN2RDP...  PRINTS REGRESSION DIAGNOSTICS.
C  SV7SCP...  SETS ALL COMPONENTS OF A VECTOR TO A SCALAR.
C
C  ***  LOCAL VARIABLES  ***
C
      INTEGER D1, DK, DR1, I, IV1, J1K, K, N1, N2, NF, NG, RD1, R1, RN
      REAL             H, H0, HLIM, NEGPT5, ONE, XK, ZERO
C
C  ***  IV AND V COMPONENTS  ***
C
      INTEGER COVREQ, D, DINIT, DLTFDJ, J, MODE, NEXTV, NFCALL, NFGCAL,
     1        NGCALL, NGCOV, R, REGD, REGD0, TOOBIG, VNEED
C/6
C     DATA COVREQ/15/, D/27/, DINIT/38/, DLTFDJ/43/, J/70/, MODE/35/,
C    1     NEXTV/47/, NFCALL/6/, NFGCAL/7/, NGCALL/30/, NGCOV/53/,
C    2     R/61/, REGD/67/, REGD0/82/, TOOBIG/2/, VNEED/4/
C/7
      PARAMETER (COVREQ=15, D=27, DINIT=38, DLTFDJ=43, J=70, MODE=35,
     1           NEXTV=47, NFCALL=6, NFGCAL=7, NGCALL=30, NGCOV=53,
     2           R=61, REGD=67, REGD0=82, TOOBIG=2, VNEED=4)
C/
      DATA HLIM/0.1E+0/, NEGPT5/-0.5E+0/, ONE/1.E+0/, ZERO/0.E+0/
C
C --------------------------------  BODY  ------------------------------
C
      IF (IV(1) .EQ. 0) CALL SIVSET(1, IV, LIV, LV, V)
      IV(COVREQ) = -abs(IV(COVREQ))
      IV1 = IV(1)
      IF (IV1 .EQ. 14) GO TO 10
      IF (IV1 .GT. 2 .AND. IV1 .LT. 12) GO TO 10
      IF (IV1 .EQ. 12) IV(1) = 13
      IF (IV(1) .EQ. 13) IV(VNEED) = IV(VNEED) + P + N*(P+2)
      CALL   SRN2G(X, V, IV, LIV, LV, N, N, N1, N2, P, V, V, V, X)
      IF (IV(1) .NE. 14) GO TO 999
C
C  ***  STORAGE ALLOCATION  ***
C
      IV(D) = IV(NEXTV)
      IV(R) = IV(D) + P
      IV(REGD0) = IV(R) + N
      IV(J) = IV(REGD0) + N
      IV(NEXTV) = IV(J) + N*P
      IF (IV1 .EQ. 13) GO TO 999
C
 10   D1 = IV(D)
      DR1 = IV(J)
      R1 = IV(R)
      RN = R1 + N - 1
      RD1 = IV(REGD0)
C
 20   CALL   SRN2G(V(D1), V(DR1), IV, LIV, LV, N, N, N1, N2, P, V(R1),
     1           V(RD1), V, X)
      IF (IV(1)-2) 30, 50, 100
C
C  ***  NEW FUNCTION VALUE (R VALUE) NEEDED  ***
C
 30   NF = IV(NFCALL)
      CALL SCALCR(N, P, X, NF, V(R1))
      IF (NF .GT. 0) GO TO 40
         IV(TOOBIG) = 1
         GO TO 20
 40   IF (IV(1) .GT. 0) GO TO 20
C
C  ***  COMPUTE FINITE-DIFFERENCE APPROXIMATION TO DR = GRAD. OF R  ***
C
C     *** INITIALIZE D IF NECESSARY ***
C
 50   IF (IV(MODE) .LT. 0 .AND. V(DINIT) .EQ. ZERO)
     1        CALL  SV7SCP(P, V(D1), ONE)
C
      J1K = DR1
      DK = D1
      NG = IV(NGCALL) - 1
      IF (IV(1) .EQ. (-1)) IV(NGCOV) = IV(NGCOV) - 1
      DO 90 K = 1, P
         XK = X(K)
         H = V(DLTFDJ) * MAX( ABS(XK), ONE/V(DK))
         H0 = H
         DK = DK + 1
 60      X(K) = XK + H
         NF = IV(NFGCAL)
         CALL SCALCR (N, P, X, NF, V(J1K))
         NG = NG + 1
         IF (NF .GT. 0) GO TO 70
              H = NEGPT5 * H
              IF ( ABS(H/H0) .GE. HLIM) GO TO 60
                   IV(TOOBIG) = 1
                   IV(NGCALL) = NG
                   GO TO 20
 70      X(K) = XK
         IV(NGCALL) = NG
         DO 80 I = R1, RN
              V(J1K) = (V(J1K) - V(I)) / H
              J1K = J1K + 1
 80           CONTINUE
 90      CONTINUE
      GO TO 20
C
 100  IF (IV(REGD) .GT. 0) IV(REGD) = RD1
      CALL  SN2RDP(IV, LIV, N, V(RD1))
C
 999  RETURN
C
C  ***  LAST LINE OF    SNLAFU FOLLOWS  ***
      END
