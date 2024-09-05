      real             function SPPNML (U, MU, SIGMA)
c>> 1994-10-20 SPPNML Krogh  Changes to use M77CON
c>> 1994-08-05 SPPNML WV Snyder
c
c     Compute the Percentage Point of the Normal (Gaussian) Probability
c     Distribution, g**(-1) (u;mu,sigma).  This is given in terms of the
c     inverse of the co-error function, erfci, by x = mu -
c     sigma * sqrt(2) * erfci(2*u).
c
      real             U, MU, SIGMA
c--S replaces "?": ?PPNML, ?ERFCI
      real             SQRT2, SERFCI
      external SERFCI
      parameter (SQRT2 = 1.414213562373095048801688724209698078569672e0)
c
      sppnml = mu - sigma * sqrt2 * serfci(u+u)
      return
      end
