      SUBROUTINE SRANGV(A, NDIM, N, U, X, HAVEC, IERR)
C     .  Copyright (C) 1989, California Institute of Technology.
C     .  U. S. Government sponsorship under
C     .  NASA contract NAS7-918 is acknowledged.
C>> 1994-11-11 SRANGV Krogh   Declared all vars.
C>> 1994-10-20 SRANGV Krogh  Changes to use M77CON
C>> 1987-04-22 SRANGV Lawson  Initial code.
c--S replaces "?": ?RANGV, ?RANG
c
c     SRANGV generates an N-component random vector, X, from a multi-
c     variate normal distribution having a specified mean vector, U,
c     and covariance matrix, A.
c     ------------------------------------------------------------------
c                        Subroutine arguments
c
c     A(,)  [inout]  Array with first dimension NDIM, and second
c           dimension at least N.  When HAVEC is false, A(,) contains
c           the given N x N symmetric covariance matrix.  This
c           subroutine will replace A by its lower-triangular Cholesky
c           factor, C, and set HAVEC = .true.
c           When HAVEC is true A(,) is assumed to contain the Cholesky
c           factor, C.
c           Note:  Only the lower triangle of the array A(,) is used by
c           this subroutine.
c
c     NDIM  [in]  First dimension of the array A(,).
c           Require NDIM .ge. N.
c
c     N  [in]  Order of the covariance matrix A and dimension of the
c           vectors U and X.
c
c     U()  [in]   Contains the N-dimensional mean vector.
c
c     X()  [out]  On return will contain the N-dimensional generated
c                 random vector.
c
c     HAVEC    [inout]  See description above for A(,).
c
c     IERR  [out]    IERR is only referenced when this subr is entered
c           with HAVEC = false.
c           In that case IERR will be set to zero if the
c           Cholesky factorization is successful.  If the factorization
c           is unsuccessful IERR will be set to the index of the row at
c           which failure is detected.  In this latter case the results
c           returned in A(,) and X() will not be valid.
c     -- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
c     Subprogram referenced: SRANG
c     -- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
c     Programmed for JPL by Carl Pitts, Heliodyne Corp., April, 1969.
c     Adapted to Fortran 77 for the JPL MATH 77 library by C. L. Lawson
c     and S. Y. Chiu, JPL, April 1987.
c     ------------------------------------------------------------------
      integer IERR, NDIM, N, J, K, L
      real             A(NDIM,N), U(N), X(N), SUM, ZERO
      real             SRANG
      logical HAVEC
      parameter(ZERO = 0.0E0)
c     ------------------------------------------------------------------
      if(.not. HAVEC) then
c     -- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
c     BEGIN PROCEDURE TO CALCULATE C-MATRIX, NOTE THE C-MATRIX
c     overwrites the lower triangle of the A matrix.
c
      DO 40 J=1,N
         DO 30 K=1,J
            SUM = ZERO
            DO 20 L=1,K-1
               SUM=SUM+A(J,L)*A(K,L)
   20       CONTINUE
            if(J .ne. K) then
               A(J,K)=(A(J,K)-SUM)/A(K,K)
            else
               SUM=A(K,K)-SUM
               if(SUM .le. ZERO) then
                  IERR = K
c                                                Error return.
                  RETURN
               endif
               A(K,K)=SQRT(SUM)
            endif
   30    continue
   40 continue
      HAVEC=.TRUE.
      IERR = 0
      endif
c                            END PROCEDURE TO CALCULATE C-MATRIX
c     ------------------------------------------------------------------
c                            BEGIN PROCEDURE TO CALCULATE RANDOM VECTOR
c
c     LOOP TO FILL Y-VECTOR WITH NORMALLY DISTRIBUTED RANDOM NUMBERS
c     WITH ZERO MEAN AND UNIT VARIANCE  NOTE THE Y-VECTOR IS STORED
c     TEMPORARILY IN THE X-VECTOR
c
      DO 70 J=1,N
         X(J)= SRANG()
   70 CONTINUE
c
c     LOOP TO CALCULATE X-VECTOR
c
      DO 90 J=N,1,-1
         SUM=U(J)
         DO 80 K=1,J
            SUM=SUM+A(J,K)*X(K)
   80    CONTINUE
         X(J)=SUM
   90 CONTINUE
      return
      end
