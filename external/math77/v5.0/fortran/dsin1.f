      double precision function DSIN1 (X)
c>>   1994-10-20 DSIN1 Krogh  Changes to use M77CON
c>>   1993-07-21 DSIN1 WV Snyder JPL Original code
c
c     Compute (X - SIN(X)) / X**2.
c     Use Taylor's series when |X| < CUT (an internal parameter).
c     Use the intrinsic function otherwise.
c
c--D replaces "?": ?SIN1
      double precision X
      double precision CUT, D1MACH, ROUND, S, X2
      parameter (CUT=0.125d0)
      external D1MACH
      integer M, N
      save N
      data N /-1/
c
      if (n .lt. 0) then
         round = d1mach(4)
         n = 1
         s = 1.0d0
10       if (s .gt. round) then
            s = s * cut * cut / ((2*n)*(2*n+1))
            n = n + 1
            go to 10
         end if
      end if
      if (abs(x) .lt. cut) then
         x2 = x*x
         m = 2*n
         s = 1.0d0 - x2/(m*(m+1))
20       if (m .gt. 4) then
            m = m - 2
            s = 1.0d0 - s*x2/(m*(m+1))
            go to 20
         end if
         dsin1 = s/6.0d0
      else
         dsin1 = (x - sin(x)) / x**3
      end if
      return
      end
