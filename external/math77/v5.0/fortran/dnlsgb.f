      SUBROUTINE  DNLSGB(N, P, L, ALF, B, C, Y, DCALCA, DCALCB, INC,
     1             IINC, IV, LIV, LV, V)
c>> 1994-10-20 DNLSGB Krogh  Changes to use M77CON
c>> 1990-07-02 DNLSGB CLL @ JPL
c>> 1990-06-12 CLL @ JPL
c>> 1990-02-14 CLL @ JPL
*** from netlib, Fri Feb  9 13:10:09 EST 1990 ***
c--D replaces "?": ?NLSGB ,?CALCA,?CALCB,?IVSET,?RNSGB
C
C  ***  SOLVE SEPARABLE NONLINEAR LEAST SQUARES USING  ***
C  ***  ANALYTICALLY COMPUTED DERIVATIVES.             ***
C
C  ***  PARAMETER DECLARATIONS  ***
C
      INTEGER IINC, L, LIV, LV, N, P
      INTEGER INC(IINC,P), IV(LIV)
      DOUBLE PRECISION ALF(P), B(2,P), C(L), V(LV), Y(N)
      EXTERNAL DCALCA, DCALCB
C
C  ***  PURPOSE  ***
C
C GIVEN A SET OF N OBSERVATIONS Y(1)....Y(N) OF A DEPENDENT VARIABLE
C T(1)...T(N),  DNLSGB ATTEMPTS TO COMPUTE A LEAST SQUARES FIT
C TO A FUNCTION  ETA  (THE MODEL) WHICH IS A LINEAR COMBINATION
C
C                  L
C ETA(C,ALF,T) =  SUM C * PHI(ALF,T) +PHI   (ALF,T)
C                 J=1  J     J           L+1
C
C OF NONLINEAR FUNCTIONS PHI(J) DEPENDENT ON T AND ALF(1),...,ALF(P)
C (.E.G. A SUM OF EXPONENTIALS OR GAUSSIANS).  THAT IS, IT DETERMINES
C NONLINEAR PARAMETERS ALF WHICH MINIMIZE
C
C                   2    N                      2
C     NORM(RESIDUAL)  = SUM  (Y - ETA(C,ALF,T )) ,
C                       I=1    I             I
C
C SUBJECT TO THE SIMPLE BOUND CONSTRAINTS B(1,I) .LE. X(I) .LE. B(2,I),
C I = 1(1)P.
C
C THE (L+1)ST TERM IS OPTIONAL.
C
C--------------------------  PARAMETER USAGE  -------------------------
C
C INPUT PARAMETERS
C
C N     INTEGER        NUMBER OF OBSERVATIONS (MUST BE .GE. MAX(L,P)).
C
C P     INTEGER        NUMBER OF NONLINEAR PARAMETERS (MUST BE .GE. 1).
C
C L     INTEGER        NUMBER OF LINEAR PARAMETERS (MUST BE .GE. 0).
C
C ALF   D.P. ARRAY     P VECTOR = INITIAL ESTIMATE OF THE NONLINEAR
C                      PARAMETERS.
C
C DCALCA SUBROUTINE     USER PROVIDED FUNCTION TO CALCULATE THE MODEL
C                      (I.E., TO CALCULATE PHI) -- SEE THE NOTE BELOW
C                      ON THE CALLING SEQUENCE FOR DCALCA.
C                      DCALCA MUST BE DECLARED EXTERNAL IN THE CALLING
C                      PROGRAM.
C
C DCALCB SUBROUTINE     USER PROVIDED FUNCTION TO CALCULATE THE DERIVA-
C                      TIVE OF THE MODEL (I.E., OF PHI) WITH RESPECT TO
C                      ALF -- SEE THE NOTE BELOW ON THE CALLING
C                      SEQUENCE FOR DCALCB.  DCALCB MUST BE DECLARED
C                      EXTERNAL IN THE CALLING PROGRAM.
C
C Y     D.P. ARRAY     VECTOR OF OBSERVATIONS.
C
C INC   INTEGER ARRAY  A 2 DIM. ARRAY OF DIMENSION AT LEAST (L+1,P)
C                      INDICATING THE POSITION OF THE NONLINEAR PARA-
C                      METERS IN THE MODEL.  SET INC(J,K) = 1 IF ALF(K)
C                      APPEARS IN PHI(J).  OTHERWISE SET INC(J,K) = 0.
C                      IF PHI((L+1)) IS NOT IN THE MODEL, SET THE L+1ST
C                      ROW OF INC TO ALL ZEROS.  EVERY COLUMN OF INC
C                      MUST CONTAIN AT LEAST ONE 1.
C
C IINC   INTEGER       DECLARED ROW DIMENSION OF INC, WHICH MUST BE AT
C                      LEAST L+1.
C
C IV     INTEGER       ARRAY OF LENGTH AT LEAST LIV THAT CONTAINS
C                      VARIOUS PARAMETERS FOR THE SUBROUTINE, SUCH AS
C                      THE ITERATION AND FUNCTION EVALUATION LIMITS AND
C                      SWITCHES THAT CONTROL PRINTING.  THE INPUT COM-
C                      PONENTS OF IV ARE DESCRIBED IN DETAIL IN THE
C                      PORT OPTIMIZATION DOCUMENTATION.
C                         IF IV(1)=0 ON INPUT, THEN DEFAULT PARAMETERS
C                      ARE SUPPLIED TO IV AND V.  THE CALLER MAY SUPPLY
C                      NONDEFAULT PARAMETERS TO IV AND V BY EXECUTING A
C                      CALL DIVSET(1,IV,LIV,LV,V) AND THEN ASSIGNING
C                      NONDEFAULT VALUES TO THE APPROPRIATE COMPONENTS
C                      OF IV AND V BEFORE CALLING  DNLSGB.
C
C LIV     INTEGER      LENGTH OF IV.  MUST BE AT LEAST
C                      115 + 4*P + L + 2*M,
C                      WHERE  M  IS THE NUMBER OF ONES IN INC.
C
C LV      INTEGER      LENGTH OF V.  MUST BE AT LEAST
C                      105 + N*(L+M+P+3) + L*(L+3)/2 + P*(2*P+21),
C                      WHERE  M  IS AS FOR LIV (SEE ABOVE).
c                 If row L+1 of INC() contains only zeros, meaning
c                 the term PHI sub (L+1) is absent from the model,
c                 then LV can be N less than just described.
C
C V       D.P. ARRAY   WORK AND PARAMETER ARRAY OF LENGTH AT LEAST LV
C                      THAT CONTAINS SUCH INPUT COMPONENTS AS THE
C                      CONVERGENCE TOLERANCES.  THE INPUT COMPONENTS OF
C                      V MAY BE SUPPLIED AS FOR IV (SEE ABOVE).  NOTE
C                      THAT V(35) CONTAINS THE INITIAL STEP BOUND,
C                      WHICH, IF TOO LARGE, MAY LEAD TO OVERFLOW.
C
C
C OUTPUT PARAMETERS
C
C ALF    D.P. ARRAY    FINAL NONLINEAR PARAMETERS.
C
C C      D.P. ARRAY    L VECTOR OF LINEAR PARAMETERS -- NOTE THAT NO
C                      INITIAL GUESS FOR C IS REQUIRED.
C
C IV                   IV(1) CONTAINS A RETURN CODE DESCRIBED IN THE
C                      PORT OPTIMIZATION DOCUMENTATION.  IF IV(1) LIES
C                      BETWEEN 3 AND 7, THEN THE ALGORITHM HAS
C                      CONVERGED (BUT IV(1) = 7 INDICATES POSSIBLE
C                      TROUBLE WITH THE MODEL).  IV(1) = 9 OR 10 MEANS
C                      FUNCTION EVALUATION OR ITERATION LIMIT REACHED.
C                      IV(1) = 66 MEANS BAD PARAMETERS (INCLUDING A
C                      COLUMN OF ZEROS IN INC).  NOTE THAT THE
C                      ALGORITHM CAN BE RESTARTED AFTER ANY RETURN WITH
C                      IV(1) .LT. 12 -- SEE THE PORT DOCUMENTATION.
C
C V                    VARIOUS ITEMS OF INTEREST, INCLUDING THE NORM OF
C                      THE GRADIENT(1) AND THE FUNCTION VALUE(10).  SEE
C                      THE PORT DOCUMENTATION FOR A COMPLETE LIST.
C
C
C
C PARAMETERS FOR DCALCA(N,P,L,ALF,NF,PHI)
C
C N,L,P,ALF ARE INPUT PARAMETERS AS DESCRIBED ABOVE
C
C PHI    D.P. ARRAY  N*(L+1) or N*L array.  DCALCA must store into
c                    PHI(i,j) the value of of the nonlinear function
c                    PHI sub j evaluated with the current values of
c                    ALF() and at the ith point of the data set.
c                    Note that if the model does not have the
c                    term PHI sub L+1 then one must not store to the
c                    (L+1)st column of PHI() as this would overwrite
c                    other work space.
C
C NF     INTEGER     CURRENT INVOCATION COUNT FOR DCALCA.  IF PHI CANNOT
C                    BE EVALUATED AT ALF (E.G. BECAUSE AN ARGUMENT TO
C                    AN INTRINSIC FUNCTION IS OUT OF RANGE), THEN DCALCA
C                    SHOULD SIMPLY SET NF TO 0 AND RETURN.  THIS
C                    TELLS THE ALGORITHM TO TRY A SMALLER STEP.
C
C N.B. THE DEPENDENT VARIABLE T IS NOT EXPLICITLY PASSED.  IF REQUIRED,
C IT MAY BE PASSED IN NAMED COMMON.
C
C
C PARAMETERS FOR DCALCB(N,P,L,ALF,NF,DER)
C
C N,P,L,ALF,NF ARE AS FOR DCALCA
C
C DER   D.P. ARRAY   N*M ARRAY, WHERE M IS THE NUMBER OF ONES IN INC.
C                    DCALCB MUST SET DER TO THE DERIVATIVES OF THE MODEL
C                    WITH RESPECT TO ALF.  IF THE MODEL HAS K TERMS THAT
C                    DEPEND ON ALF(I), THEN DER WILL HAVE K CONSECUTIVE
C                    COLUMNS OF DERIVATIVES WITH RESPECT TO ALF(I).  THE
C                    COLUMNS OF DER CORRESPOND TO THE ONES IN INC WHEN
C                    ONE TRAVELS THROUGH INC BY COLUMNS.  FOR EXAMPLE,
C                    IF INC HAS THE FORM...
C                      1  1  0
C                      0  1  0
C                      1  0  0
C                      0  0  1
C                    THEN THE FIRST TWO COLUMNS OF DER ARE FOR THE
C                    DERIVATIVES OF COLUMNS 1 AND 3 OF PHI WITH RESPECT
C                    TO ALF(1), COLUMNS 3 AND 4 OF DER ARE FOR THE
C                    DERIVATIVES OF COLUMNS 1 AND 2 OF PHI WITH RESPECT
C                    TO ALF(2), AND COLUMN 5 OF DER IS FOR THE DERIVA-
C                    TIVE OF COLUMN 4 OF PHI WITH RESPECT TO ALF(3).
C                    MORE SPECIFICALLY, DER(I,2) IS FOR THE DERIVATIVE
C                    OF PHI(I,3) WITH RESPECT TO ALF(1) AND DER(I,5) IS
C                    FOR THE DERIVATIVE OF PHI(I,4) WITH RESPECT TO
C                    ALF(3) (FOR I = 1,2,...,N).
C                       THE VALUE OF ALF PASSED TO DCALCB IS THE SAME AS
C                    THAT PASSED TO DCALCA THE LAST TIME IT WAS CALLED.
C                    (IF DER CANNOT BE EVALUATED, THEN DCALCB SHOULD SET
C                    NF TO 0.  THIS WILL CAUSE AN ERROR RETURN.)
C
C N.B. DER IS FOR DERIVATIVES WITH RESPECT TO ALF, NOT T.
C
C------------------------------  NOTES  -------------------------------
C
C      THIS PROGRAM WAS WRITTEN BY LINDA KAUFMAN AT BELL LABS, MURRAY
C HILL, N.J. IN 1977 AND EXTENSIVELY REVISED BY HER AND DAVID GAY IN
C 1980, 1981, 1983, 1984.  THE WORK OF DAVID GAY WAS SUPPORTED IN PART
C BY NATIONAL SCIENCE FOUNDATION GRANT MCS-7906671.
C
C--------------------------  DECLARATIONS  ----------------------------
C
C
C  ***  EXTERNAL SUBROUTINES  ***
C
      EXTERNAL DIVSET, DRNSGB
C
C DIVSET.... PROVIDES DEFAULT IV AND V VALUES.
C DRNSGB... CARRIES OUT NL2SOL ALGORITHM.
C
C  ***  LOCAL VARIABLES  ***
C
      INTEGER A1, DA1, I, IN1, IV1, K, L1, LP1, M, M0, NF
C
C  ***  SUBSCRIPTS FOR IV AND V  ***
C
      INTEGER AMAT, D, DAMAT, IN, IVNEED, J, L1SAV, MSAVE, NEXTIV,
     1        NEXTV, NFCALL, NFGCAL, PERM, R, TOOBIG, VNEED
C
C  ***  IV SUBSCRIPT VALUES  ***
C
      PARAMETER (AMAT=113, D=27, DAMAT=114, IN=112, IVNEED=3, J=70,
     1           L1SAV=111, MSAVE=115, NEXTIV=46, NEXTV=47, NFCALL=6,
     2           NFGCAL=7, PERM=58, R=61, TOOBIG=2, VNEED=4)
C
C++++++++++++++++++++++++++++++++  BODY  ++++++++++++++++++++++++++++++
C
      IF (IV(1) .EQ. 0) CALL DIVSET(1, IV, LIV, LV, V)
      IF (P .LE. 0 .OR. L .LT. 0 .OR. IINC .LE. L) GO TO 50
      IV1 = IV(1)
      IF (IV1 .EQ. 14) GO TO 90
      IF (IV1 .GT. 2 .AND. IV1 .LT. 12) GO TO 90
      IF (IV1 .EQ. 12) IV(1) = 13
      IF (IV(1) .NE. 13) GO TO 60
      IF (IV(PERM) .LE. MSAVE) IV(PERM) = MSAVE + 1
      LP1 = L + 1
      L1 = 0
      M = 0
      DO 40 I = 1, P
         M0 = M
         IF (L .EQ. 0) GO TO 20
         DO 10 K = 1, L
            IF (INC(K,I) .LT. 0 .OR. INC(K,I) .GT. 1) GO TO 50
            IF (INC(K,I) .EQ. 1) M = M + 1
 10         CONTINUE
 20      IF (INC(LP1,I) .NE. 1) GO TO 30
            M = M + 1
            L1 = 1
 30      IF (M .EQ. M0 .OR. INC(LP1,I) .LT. 0
     1                 .OR. INC(LP1,I) .GT. 1) GO TO 50
 40      CONTINUE
C
      IV(IVNEED) = IV(IVNEED) + 2*M
      L1 = L + L1
      IV(VNEED) = IV(VNEED) + N*(L1+M)
      GO TO 60
C
 50   IV(1) = 66
C
 60   CALL DRNSGB(V, ALF, B, C, V, IV, IV, L, 1, N, LIV, LV, N, M, P, V,
     1            Y)
      IF (IV(1) .NE. 14) GO TO 999
C
C  ***  STORAGE ALLOCATION  ***
C
      IV(IN) = IV(NEXTIV)
      IV(NEXTIV) = IV(IN) + 2*M
      IV(AMAT) = IV(NEXTV)
      IV(DAMAT) = IV(AMAT) + N*L1
      IV(NEXTV) = IV(DAMAT) + N*M
      IV(L1SAV) = L1
      IV(MSAVE) = M
C
C  ***  SET UP IN ARRAY  ***
C
      IN1 = IV(IN)
      DO 80 I = 1, P
         DO 70 K = 1, LP1
            IF (INC(K,I) .EQ. 0) GO TO 70
               IV(IN1) = I
               IV(IN1+1) = K
               IN1 = IN1 + 2
 70         CONTINUE
 80      CONTINUE
      IF (IV1 .EQ. 13) GO TO 999
C
 90   A1 = IV(AMAT)
      DA1 = IV(DAMAT)
      IN1 = IV(IN)
      L1 = IV(L1SAV)
      M = IV(MSAVE)
C
 100  CALL DRNSGB(V(A1), ALF, B, C, V(DA1), IV(IN1), IV, L, L1, N, LIV,
     1            LV, N, M, P, V, Y)
      IF (IV(1)-2) 110, 120, 999
C
C  ***  NEW FUNCTION VALUE (R VALUE) NEEDED  ***
C
 110  NF = IV(NFCALL)
      CALL DCALCA(N, P, L, ALF, NF, V(A1))
      IF (NF .LE. 0) IV(TOOBIG) = 1
      GO TO 100
C
C  ***  COMPUTE DR = GRADIENT OF R COMPONENTS  ***
C
 120  CALL DCALCB(N, P, L, ALF, IV(NFGCAL), V(DA1))
      IF (IV(NFGCAL) .EQ. 0) IV(TOOBIG) = 1
      GO TO 100
C
 999  RETURN
C
C  ***  LAST CARD OF  DNLSGB FOLLOWS  ***
      END
