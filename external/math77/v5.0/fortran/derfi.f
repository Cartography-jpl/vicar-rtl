      double precision function DERFI (X)
C     .  Copyright (C) 1989, California Institute of Technology.
C     .  U. S. Government sponsorship under
C     .  NASA contract NAS7-918 is acknowledged.
C--D replaces "?": ?ERFI, ?ERFCI, ?ERM1
C>> 1995-11-03 DERFI Krogh  Removed blanks in numbers for C conversion.
C>> 1994-10-20 DERFI Krogh  Changes to use M77CON
C>> 1994-04-20 DERFI CLL Edited type stmts to make DP & SP files similar
C>> 1987-10-29 DERFI Snyder  Initial code.
c
c     For -1.0 .LT. X .LT. 1.0 calculate the inverse of the error
c     function.  That is, X = ERF(ERFI).
c
c     For 0.0 .LT. X .LT. 2.0 calculate the inverse of the
c     complementary error function.  that is, X = ERFC(ERFCI).  This
c     calculation is carried out by invoking the alternate entry *ERFCI.
c
c     If X is out of range, program execution is terminated by calling
c     the error message processor.
c
c     This subprogram uses approximations due to A. Strecok from
c     Mathematics of Computation 22, (1968) pp 144-158.
c
      double precision DERFCI
      double precision X
c
c     *****     Parameters     *****************************************
c
c ERRLEV  is the error level used in case DERM1 is called.
c MAX...  is the position in C of the last coefficient of a Chebyshev
c         polynomial expansion.
c MIN...  is the position in C of the first coefficient of a Chebyshev
c         polynomial expansion.
c NC      is the upper dimension of the array of coefficients.
c NDELTA  is the number of coefficients of the Chebyshev polynomial
c         expansion used to approximate R(X) in the range
c         0.9975 .LT. X .LE. 1-5.0D-16
c NLAMDA  is the number of coefficients of the Chebyshev polynomial
c         expansion used to approximate R(X) in the range
c         0.8 .LT. X .LE. 0.9975.
c NMU     is the number of coefficients of the Chebyshev polynomial
c         expansion used to approximate R(X) in the range
c         5.0D-16 .GT. 1-X .GE. 1.D-300.
c NXI     is the number of coefficients of the Chebyshev polynomial
c         expansion used to approximate DERFCI(X)/X in the
c         range 0.0 .LE. X .LE. 0.8.
c
      integer ERRLEV, MAXDEL, MAXLAM, MAXMU, MAXXI, MINDEL, MINLAM
      integer MINMU, MINXI, NC, NDELTA, NLAMDA, NMU, NXI
      parameter (ERRLEV=0)
      parameter (MINDEL = 0)
      parameter (NDELTA = 37)
      parameter (MAXDEL = MINDEL + NDELTA)
      parameter (MINLAM = MAXDEL + 1)
      parameter (NLAMDA = 26)
      parameter (MAXLAM = MINLAM + NLAMDA)
      parameter (MINMU = MAXLAM + 1)
      parameter (NMU = 25)
      parameter (MAXMU = MINMU + NMU)
      parameter (MINXI = MAXMU + 1)
      parameter (NXI = 38)
      parameter (MAXXI = MINXI + NXI)
      parameter (NC = MAXXI)
c
c     *****     External References     ********************************
c
c D1MACH   Provides the round-off level.  Used to calculate the number
c          of coefficients to retain in each Chebyshev expansion.
c DERM1    Prints an error message and stops if X .LE. -1.0 or
c          X .GE. 1.0 (ERFI) or X .LE. 0.0 or X .GE. 2.0 (ERFCI).
c LOG      Calculates the natural logarithm.
c SQRT     Calculates the square root.
c
      double precision D1MACH
c
c     *****     Local Variables      ***********************************
c
c ARG     If ERFI or ERFCI is being approximated by a Chebyshev
c         expansion then ARG is the argument of ERFI or the argument
c         that would be used if ERFCI(X) were computed as ERFC(1-X),
c         that is, ARG = X if ERFI is being computed, or ARG = 1-X if
c         ERFCI is being computed.  If ERFI or ERFCI is being computed
c         using the initial approximation ERFI=SQRT(-LOG((1-X)*(1+X))),
c         then ARG is that initial approximation.
c C       contains the coefficients of polynomial expansions.  They are
c         stored in C in the order DELTA(0..37), LAMDA(0..26),
c         MU(0..25), XI(0..38).
c D       are used to scale the argument of the Chebyshev polynomial
c         expansion in the range 1.D-300 .LT. 1-X .LT. 0.2.
c DELTA   are coefficients of the Chebyshev polynomial expansion of R(X)
c         for 0.9975 .LT. X .LE. 1-5.0D-16.
c FIRST   is a logical SAVE variable indicating whether it is necessary
c         to calculate the number of coefficients to use for each
c         Chebyshev expansion.
c FSIGN   is X or 1.0 - X.  It is used to remember the sign to be
c         assigned to the function value.
c I, J    are used as indices.
c JIX     is an array containing MINXI, MAXXI, MINLAM, MAXLAM, MINDEL,
c         MAXDEL, MINMU, MAXMU in locations -1..6
c JMIN    is the minimum index of a coefficient in the Chebyshev
c         polynomial expansion to be used.
c LAMDA   are coefficients of the Chebyshev polynomial expansion of R(X)
c         for 0.8 .LT. X .LE. 0.9975.
c MU      are coefficients of the Chebyshev polynomial expansion of R(X)
c         for 5.0D-16 .GT. 1-X .GE. 1.D-300.
c S2      is 2.0 * S.
c S       is the argument of the Chebyshev polynomial expansion.
c SUBNAM  is the entry name.  This is used only if X is out of range.
c W1..W3  are adjacent elements of the recurrence used to evaluate the
c         Chebyshev polynomial expansion.
c XI      are coefficients of the Chebyshev polynomial expansion of
c         ERFC(X)/X for 0.0 .LE. X .LE. 0.8.
c
      double precision ARG, C(0:NC), D(6), DELTA(0:NDELTA)
      logical FIRST
      save FIRST
      double precision FSIGN
      integer I, J, JIX(-1:6)
      save JIX
      integer JMIN
      double precision LAMDA(0:NLAMDA), MU(0:NMU), S, S2
      character*6 SUBNAM
      double precision W1, W2, W3
      double precision XI(0:NXI)
c
c     *****     Equivalence Statements     *****************************
c
      equivalence (C(MINDEL),DELTA(0))
      equivalence (C(MINLAM),LAMDA(0))
      equivalence (C(MINMU),MU(0))
      equivalence (C(MINXI),XI(0))
c
c     *****     Data Statements     ************************************
c
      data D /-1.548813042373261659512742D0
     2,        2.565490123147816151928163D0
     3,       -.5594576313298323225436913D0
     4,        2.287915716263357638965891D0
     5,       -9.199992358830151031278420D0
     6,        2.794990820124599493768426D0/
c
c     DELTA(J), J = 0..NDELTA
c
      data DELTA(0) /  .9566797090204925274526373D0/
      data DELTA(1) / -.0231070043090649036999908D0/
      data DELTA(2) / -.0043742360975084077333218D0/
      data DELTA(3) / -.0005765034226511854809364D0/
      data DELTA(4) / -.0000109610223070923931242D0/
      data DELTA(5) /  .0000251085470246442787982D0/
      data DELTA(6) /  .0000105623360679477511955D0/
      data DELTA(7) /  .0000027544123300306391503D0/
      data DELTA(8) /  .0000004324844983283380689D0/
      data DELTA(9) / -.0000000205303366552086916D0/
      data DELTA(10) / -.0000000438915366654316784D0/
      data DELTA(11) / -.0000000176840095080881795D0/
      data DELTA(12) / -.0000000039912890280463420D0/
      data DELTA(13) / -.0000000001869324124559212D0/
      data DELTA(14) /  .0000000002729227396746077D0/
      data DELTA(15) /  .0000000001328172131565497D0/
      data DELTA(16) /  .0000000000318342484482286D0/
      data DELTA(17) /  .0000000000016700607751926D0/
      data DELTA(18) / -.0000000000020364649611537D0/
      data DELTA(19) / -.0000000000009648468127965D0/
      data DELTA(20) / -.0000000000002195672778128D0/
      data DELTA(21) / -.0000000000000095689813014D0/
      data DELTA(22) /  .0000000000000137032572230D0/
      data DELTA(23) /  .0000000000000062538505417D0/
      data DELTA(24) /  .0000000000000014584615266D0/
      data DELTA(25) /  .0000000000000001078123993D0/
      data DELTA(26) / -.0000000000000000709229988D0/
      data DELTA(27) / -.0000000000000000391411775D0/
      data DELTA(28) / -.0000000000000000111659209D0/
      data DELTA(29) / -.0000000000000000015770366D0/
      data DELTA(30) /  .0000000000000000002853149D0/
      data DELTA(31) /  .0000000000000000002716662D0/
      data DELTA(32) /  .0000000000000000000957770D0/
      data DELTA(33) /  .0000000000000000000176835D0/
      data DELTA(34) / -.0000000000000000000009828D0/
      data DELTA(35) / -.0000000000000000000020464D0/
      data DELTA(36) / -.0000000000000000000008020D0/
      data DELTA(37) / -.0000000000000000000001650D0/
c
      data FIRST /.TRUE./
c
      data JIX /MINXI, MAXXI, MINLAM, MAXLAM, MINDEL, MAXDEL
     1,         MINMU, MAXMU/
c
c     LAMDA(J), J = 0..NLAMDA
c
      data LAMDA(0) /  .9121588034175537733059200D0/
      data LAMDA(1) / -.0162662818676636958546661D0/
      data LAMDA(2) /  .0004335564729494453650589D0/
      data LAMDA(3) /  .0002144385700744592065205D0/
      data LAMDA(4) /  .0000026257510757648130176D0/
      data LAMDA(5) / -.0000030210910501037969912D0/
      data LAMDA(6) / -.0000000124060618367572157D0/
      data LAMDA(7) /  .0000000624066092999917380D0/
      data LAMDA(8) / -.0000000005401247900957858D0/
      data LAMDA(9) / -.0000000014232078975315910D0/
      data LAMDA(10) /  .0000000000343840281955305D0/
      data LAMDA(11) /  .0000000000335848703900138D0/
      data LAMDA(12) / -.0000000000014584288516512D0/
      data LAMDA(13) / -.0000000000008102174258833D0/
      data LAMDA(14) /  .0000000000000525324085874D0/
      data LAMDA(15) /  .0000000000000197115408612D0/
      data LAMDA(16) / -.0000000000000017494333828D0/
      data LAMDA(17) / -.0000000000000004800596619D0/
      data LAMDA(18) /  .0000000000000000557302987D0/
      data LAMDA(19) /  .0000000000000000116326054D0/
      data LAMDA(20) / -.0000000000000000017262489D0/
      data LAMDA(21) / -.0000000000000000002784973D0/
      data LAMDA(22) /  .0000000000000000000524481D0/
      data LAMDA(23) /  .0000000000000000000065270D0/
      data LAMDA(24) / -.0000000000000000000015707D0/
      data LAMDA(25) / -.0000000000000000000001475D0/
      data LAMDA(26) /  .0000000000000000000000450D0/
c
c     MU(J), J = 0..NMU
c
      data MU(0) /  .9885750640661893136460358D0/
      data MU(1) /  .0108577051845994776160281D0/
      data MU(2) / -.0017511651027627952494825D0/
      data MU(3) /  .0000211969932065633437984D0/
      data MU(4) /  .0000156648714042435087911D0/
      data MU(5) / -.0000005190416869103124261D0/
      data MU(6) / -.0000000371357897426717780D0/
      data MU(7) /  .0000000012174308662357429D0/
      data MU(8) / -.0000000001768115526613442D0/
      data MU(9) / -.0000000000119372182556161D0/
      data MU(10) /  .0000000000003802505358299D0/
      data MU(11) / -.0000000000000660188322362D0/
      data MU(12) / -.0000000000000087917055170D0/
      data MU(13) / -.0000000000000003506869329D0/
      data MU(14) / -.0000000000000000697221497D0/
      data MU(15) / -.0000000000000000109567941D0/
      data MU(16) / -.0000000000000000011536390D0/
      data MU(17) / -.0000000000000000001326235D0/
      data MU(18) / -.0000000000000000000263938D0/
      data MU(19) /  .0000000000000000000005341D0/
      data MU(20) / -.0000000000000000000022610D0/
      data MU(21) /  .0000000000000000000009552D0/
      data MU(22) / -.0000000000000000000005250D0/
      data MU(23) /  .0000000000000000000002487D0/
      data MU(24) / -.0000000000000000000001134D0/
      data MU(25) /  .0000000000000000000000420D0/
c
c     XI(J), J = 0..NXI
c
      data XI(0) /  .9928853766189408231495800D0/
      data XI(1) /  .1204675161431044864647846D0/
      data XI(2) /  .0160781993420999447257039D0/
      data XI(3) /  .0026867044371623158279591D0/
      data XI(4) /  .0004996347302357262947170D0/
      data XI(5) /  .0000988982185991204409911D0/
      data XI(6) /  .0000203918127639944337340D0/
      data XI(7) /  .0000043272716177354218758D0/
      data XI(8) /  .0000009380814128593406758D0/
      data XI(9) /  .0000002067347208683427411D0/
      data XI(10) /  .0000000461596991054300078D0/
      data XI(11) /  .0000000104166797027146217D0/
      data XI(12) /  .0000000023715009995921222D0/
      data XI(13) /  .0000000005439284068471390D0/
      data XI(14) /  .0000000001255489864097987D0/
      data XI(15) /  .0000000000291381803663201D0/
      data XI(16) /  .0000000000067949421808797D0/
      data XI(17) /  .0000000000015912343331469D0/
      data XI(18) /  .0000000000003740250585245D0/
      data XI(19) /  .0000000000000882087762421D0/
      data XI(20) /  .0000000000000208650897725D0/
      data XI(21) /  .0000000000000049488041039D0/
      data XI(22) /  .0000000000000011766394740D0/
      data XI(23) /  .0000000000000002803855725D0/
      data XI(24) /  .0000000000000000669506638D0/
      data XI(25) /  .0000000000000000160165495D0/
      data XI(26) /  .0000000000000000038382583D0/
      data XI(27) /  .0000000000000000009212851D0/
      data XI(28) /  .0000000000000000002214615D0/
      data XI(29) /  .0000000000000000000533091D0/
      data XI(30) /  .0000000000000000000128488D0/
      data XI(31) /  .0000000000000000000031006D0/
      data XI(32) /  .0000000000000000000007491D0/
      data XI(33) /  .0000000000000000000001812D0/
      data XI(34) /  .0000000000000000000000439D0/
      data XI(35) /  .0000000000000000000000106D0/
      data XI(36) /  .0000000000000000000000026D0/
      data XI(37) /  .0000000000000000000000006D0/
      data XI(38) /  .0000000000000000000000002D0/
c
c     *****     Procedures     *****************************************
c
c     Decide which approximation to use, and calculate the argument of
c     the Chebyshev polynomial expansion.
c
      fsign = x
      arg = abs(x)
      if (arg.lt.0.0d0 .or. arg.ge.1.0d0) go to 998
      if (arg.eq.0.0d0) then
         derfi = 0.0d0
         return
      end if
      if (arg.le.0.8d0) then
         s = 3.125d0*arg*arg - 1.0d0
         j = -1
      else
         if (arg.le.0.9975d0) then
            j = 1
         else
            j = 3
         end if
         arg = sqrt(-log((1.0d0-arg)*(1.0d0+arg)))
         s = d(j)*arg + d(j+1)
      end if
      go to 100
c
c     *****     entry ERFCI     ****************************************
c
      entry DERFCI (X)
c
c     Calculate the inverse of the complementary error function.
c
c     Decide which approximation to use, and calculate the argument of
c     the Chebyshev polynomial expansion.
c
      if (x.le.0.0d0 .or. x.ge.2.0d0) go to 999
      if (x.eq.1.0d0) then
         derfi = 0.0d0
         return
      end if
      fsign = 1.0d0 - x
      arg = abs(fsign)
      if (arg.le.0.8d0) then
         s = 3.125d0*arg*arg - 1.0d0
         j = -1
      else
         arg = 2.0d0 - x
         if (x.lt.1.0d0) then
            s = x
         else
            s = arg
         end if
         arg = sqrt(-log(x*arg))
         if (s.lt.5.0d-16) then
            j = 5
            s = d(5)/sqrt(arg) + d(6)
         else
            if (s.ge.0.0025d0) then
               j = 1
            else if (s.ge.5.0d-16) then
               j = 3
            end if
            s = d(j)*arg + d(j+1)
         end if
      end if
c
c     If this is the first call, calculate the degree of each expansion.
c
100   if (first) then
         first = .false.
         s2 = 0.5d0*d1mach(3)
         do 120 jmin = -1, 5, 2
            do 110 i = jix(jmin), jix(jmin+1)
               if (abs(c(i)).lt.s2) then
                  jix(jmin+1) = i
                  go to 120
               end if
110         continue
120      continue
      end if
c
c     Evaluate the Chebyshev polynomial expansion.
c
      s2 = s + s
      w1 = 0.0d0
      w2 = 0.0d0
      jmin = jix(j)
      j = jix(j+1)
200      w3 = w2
         w2 = w1
         w1 = (s2*w2 - w3) + c(j)
         j = j - 1
         if (j.gt.jmin) go to 200
      derfi = sign(arg*((s*w1 - w2) + c(jmin)), fsign)
      return
c
998   subnam = 'DERFI'
      go to 1000
999   subnam = 'DERFCI'
1000  call derm1 (subnam,1,errlev,'Argument out of range','X',x,'.')
c     In case the error level is zero, or shifted to zero by the caller:
      derfi = 0.0d0
      return
c
      end
