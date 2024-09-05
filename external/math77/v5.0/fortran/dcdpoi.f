      subroutine DCDPOI (N, LAMDA, P, Q, IERR)
c>> 1994-10-20 DCDPOI Krogh  Changes to use M77CON
c>> 1994-08-05 DCDPOI WV Snyder
c
c     Compute Q = exp(-lamda) sum (lamda**j/j!,j=0..n-1) and
c             P = exp(-lamda) sum (lamda**j/j!,j=n..infinity).
c
c     These functions are given by the incomplete gamma ratios
c     P(n,lamda) and Q(n,lamda).
c
      double precision LAMDA, P, Q
      integer N, IERR
c--D replaces "?": ?CDPOI, ?gami
c
      call dgami (DBLE(n),lamda,p,q,ierr)
      return
      end
