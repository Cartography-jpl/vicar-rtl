      real             function SSINHM (X)
c>>   1994-10-20 SSINHM Krogh  Changes to use M77CON
c>>   1994-05-22 SSINHM WV Snyder JPL Make SP and DP alike using CHGTYP
c>>   1993-07-21 SSINHM WV Snyder JPL Original code
c
c     Compute SINH(X) - X.
c
c--S replaces "?": ?SINHM
      real             X
      real             CUT, R1MACH, E, ROUND, X2
      parameter (CUT = 0.25e0)
      real             SP5, SP4, SP3, SP2, SP1, SQ1
      parameter ( SP5 = .255251817302048E-09)
      parameter ( SP4 = .723809046696880E-07)
      parameter ( SP3 = .109233297700241E-04)
      parameter ( SP2 = .954811583154274E-03)
      parameter ( SP1 = .452867078563929E-01)
      parameter ( SQ1 =-.471329214363072E-02*6.0E0)
      external R1MACH
      integer M, N
      save M, ROUND
      data M /-1/
c
      if (m .lt. 0) then
         round = r1mach(4)
         if (round .lt. 5.0e-14) then
c           Compute appropriate value of M depending on round-off.
            m = 3
            e = cut/6.0e0
10          if (e .gt. round) then
               m = m + 2
               e = e*cut*cut/(m*(m-1))
               go to 10
            end if
         end if
      end if
c
      if (round .lt. 5.0e-14) then
         n = m
         x2 = x*x
c        We assume m > 1
         ssinhm = 1.0e0 + x2/(n*(n-1))
20       if (n .gt. 5) then
            n = n - 2
            ssinhm = 1.0e0 + ssinhm*x2/(n*(n-1))
            go to 20
         end if
         ssinhm = x * x2 * ssinhm / 6.0e0
         return
      end if
c
c     Use a rational approximation when ABS(X) is less than 1.65,
c     else use the Fortran intrinsic function.
c
      if (x .lt. 1.65e0) then
         x2 = x*x
         ssinhm = ((((((sp5*x2+sp4)*x2+sp3)*x2+sp2)*x2+sp1)*x2+1.0e0)
     *            *x2*x)/(sq1*x2+6.0e0)
      else
         ssinhm = sinh(x) - x
      end if
      return
      end
