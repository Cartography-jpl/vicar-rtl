      subroutine CGESLT(A,LDA,N,IPVT,B)
C>> 1992-05-18 CLL
C>> 1987-08-14 CGESLT Lawson  Initial code.
C
C     This subroutine solves the system of equations  (A**h) * X = B
C     using the LU factorization of A given in the array A().
c     Here A**h denotes the conjugate transpose of A.
c     ------------------------------------------------------------------
c                        Subroutine arguments
C
C     A(,)  [in]  An array of size at least N x N.  On entry must
c            contain the LU factors of an N x N  matrix, A.  It is
c            expected that this factorization will have been computed by
c            use of _GEFA, either directly or indirectly via use of
c            _GECO, _GEFS or _GEFSC.  This subr will not alter the
c            contents of A(,).
C
C     LDA  [in]  Leading dimensioning parameter for the array A(,).
C
C     N  [in]  The order of the original matrix, A.
C
C     IPVT()  [in]  An integer array of length at least N, containg a
c           record of the row interchanges made during factorization of
c           A.
C
c     B()  [inout]  On entry contains the right-side N-vector for the
c           problem, (A**h) * X = B.  On return contains the solution
c           N-vector, X.
c     ------------------------------------------------------------------
C     ERROR CONDITION
C
C        A DIVISION BY ZERO WILL OCCUR IF THE INPUT FACTOR CONTAINS A
C        ZERO ON THE DIAGONAL.  TECHNICALLY THIS INDICATES SINGULARITY
C        BUT IT IS OFTEN CAUSED BY IMPROPER ARGUMENTS OR IMPROPER
C        setting of LDA.  The user can avoid sending a singular matrix
c        to this subr by testing INFO (set by _GEFS or _GEFA) or
c        RCOND (set by _GEFSC or _GECO before calling this subr.
c        Nonsingularity is indicated by INFO .eq. 0 or RCOND .ne. ZERO.
c     ------------------------------------------------------------------
C     LINPACK. THIS VERSION DATED 08/14/78 .
C     CLEVE MOLER, UNIVERSITY OF NEW MEXICO, ARGONNE NATIONAL LAB.
c     Ref: LINPACK Users' Guide, by J. J. Dongarra, C. B. Moler,
c     J. R. Bunch, and G. W. Stewart, publ by Soc. for Indust. and Appl.
c     Math, Philadelphia, 1979.
c     Adapted from LINPACK for the JPL Math77 library by
c     C. L. Lawson, JPL, Aug 1987.
c     ------------------------------------------------------------------
c     Subprograms referenced: CDOTC
C     ------------------------------------------------------------------
      INTEGER LDA, N, IPVT(N), K, KB, L, NM1
      COMPLEX A(LDA,N), B(N), CDOTC, T, ZERO
      parameter (ZERO = (0.E0,0.E0))
C     ------------------------------------------------------------------
      NM1 = N - 1
C
C        SOLVE  CTRANS(A) * X = B
C        FIRST SOLVE  CTRANS(U) * Y = B
C
         DO 60 K = 1, N
            T = CDOTC(K-1,A(1,K),1,B(1),1)
c           5/18/92 CLL The following stmt rewritten to avoid a bug in
c           the current SUN Fortran compiler.
c           IF (A(K,K) .NE. ZERO) THEN
            if(real(A(K,K)) .ne. 0.0e0 .or.
     *          aimag(A(K,K)) .ne. 0.0e0) then
              B(K) = (B(K) - T)/CONJG(A(K,K))
            ELSE
              CALL ERMSG('CGESLT',1,0,'A diagonal element is zero','.')
              RETURN
            END IF
   60    CONTINUE
C
C        NOW SOLVE CTRANS(L)*X = Y
C
         DO 80 KB = 1, NM1
            K = N - KB
            B(K) = B(K) + CDOTC(N-K,A(K+1,K),1,B(K+1),1)
            L = IPVT(K)
            IF (L .EQ. K) GO TO 70
               T = B(L)
               B(L) = B(K)
               B(K) = T
   70       CONTINUE
   80    CONTINUE
      RETURN
      END
