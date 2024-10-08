      real             function SCSHMM (X)
c>> 1994-10-20 SCSHMM Krogh  Changes to use M77CON
c>> 1993-05-07 SCSHMM WVSnyder JPL Initial code
c
c     Compute cosh(x) - 1 - x**2 using a rational approximation when
c     abs(x) is less than 2.7, else use the Fortran intrinsic function.
c
c--S replaces "?": ?CSHMM
      real             ZP3, ZP2, ZP1, ZQ4, ZQ3, ZQ2, ZQ1, X, XS
      parameter ( ZP3 = 5.59297116264720E-07 )
      parameter ( ZP2 = 1.77943488030894E-04 )
      parameter ( ZP1 = 1.69800461894792E-02 )
      parameter ( ZQ4 = 1.33412535492375E-09 * 24.0e0 )
      parameter ( ZQ3 = -5.80858944138663E-07 * 24.0e0 )
      parameter ( ZQ2 = 1.27814964403863E-04 * 24.0e0 )
      parameter ( ZQ1 = -1.63532871439181E-02 * 24.0e0 )
c     DATA ZP3/5.59297116264720E-07/,
c    *     ZP2/1.77943488030894E-04/,
c    *     ZP1/1.69800461894792E-02/,
c    *     ZQ4/1.33412535492375E-09/,
c    *     ZQ3/-5.80858944138663E-07/,
c    *     ZQ2/1.27814964403863E-04/,
c    *     ZQ1/-1.63532871439181E-02/
      xs = x * x
      if (xs .lt. 7.29e0) then
         scshmm = ((((zp3*xs+zp2)*xs+zp1)*xs+1.0e0)*xs*xs)/
     *            ((((zq4*xs+zq3)*xs+zq2)*xs+zq1)*xs+24.0e0)
      else
         scshmm = cosh(x) - 1.0e0 - 0.5e0*xs
      end if
      return
      end
