      subroutine DCOV3( V, MDIM, N, SING, VAR, WORK, IERR)
C>> 1994-10-20 DCOV3  Krogh  Changes to use M77CON
C>> 1987-11-24 DCOV3  Lawson  Initial code.
c--D replaces "?": ?COV3, ?COPY, ?DOT, ?SCAL
C     Computes the covariance matrix for the solution vector of a
c     least-squares problem, Ax ~=~ b.  Assumes quantities are
c     available that have been computed by the singular value
c     decomposition subroutine, _SVDRS.
c     The covariance matrix is given by
c           VAR * (V*Pseudoinverse(S)) * Transp(V*Pseudoinverse(S))
c     ------------------------------------------------------------------
c                 Subroutine Arguments
c
c     V(,) [inout] Array containing the NxN orthogonal V matrix of the
c                  singular value decomposition of the matrix, A.
c                  On return contains the NxN symmetric
c                  covariance matrix.
c     MDIM   [in]  First dimension of the array, V(,).
c                  Require MDIM .ge. N.
c     N      [in]  Order of the matrix V contained in the array V(,).
c     SING() [in]  Singular values of the matrix, A.
c     VAR    [in]  Estimate of variance of error in the right-side
c                  vector, b, of the least-squares problem, Ax ~=~ b.
c     WORK() [scratch]  Work space of size N.
c     IERR   [out] Set to 0 if ok.  Set to J > 0 if the Jth singular
c                  value is zero.  In this latter case
c                  the covariance matrix cannot be computed and the
c                  contents of V(,) on return will be meaningless.
C     ------------------------------------------------------------------
c
c     May, 1987, C. L. Lawson & S. Y. Chiu, JPL.
c     Programmed in Fortran 77 for use in the JPL MATH77 library.
c     Prefixing subprogram names with S or D for s.p. or d.p. versions.
c     Using BLAS subprograms DCOPY, DDOT, & DSCAL, and
c     MATH77 error processing subr., IERM1, which uses IERMV1, ERMSG,
C     ERFIN, and I1MACH.
C     ------------------------------------------------------------------
      integer I, IERR, J, N, MDIM
      double precision SING(N), V(MDIM,N), WORK(N), ZERO, DDOT
      double precision STDDEV, VAR
C
      parameter(ZERO = 0.0D0)
C     ------------------------------------------------------------------
      STDDEV = sqrt(VAR)
c
C              For J = 1, ...,N,  multiply col J of V by STDDEV/SING(J)
c
      do 40 J = 1,N
         if(SING(J) .eq. ZERO) then
            call IERM1('DCOV3',1,0,'Jth singular value is zero',
     *      'J',J,'.')
            IERR = J
            return
         end if
         call DSCAL(N, STDDEV/SING(J), V(1,J), 1)
   40 continue

      do 80 I = 1,N
         call DCOPY(N, V(I,1), MDIM, WORK, 1)
         do 60 J = I,N
            V(I,J) = DDOT(N, WORK, 1, V(J,1), MDIM)
   60    continue
         do 70 J = 1,I-1
            V(I,J) = V(J,I)
   70    continue
   80 continue
      IERR = 0
      end
