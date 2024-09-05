c  File SPRPL3.for contains subroutines SPRPL3, SPRPL4, SPRPL5.
      SUBROUTINE SPRPL3(X1, X2, Y1, Y2, XMIN, XMAX, YMIN, YMAX,
     *           LEFT, RIGHT, BOTTOM, TOP,
     *           TITLE, XNAME, YNAME, NLINES, NCHARS, IMAGE, IERR)
C     .  Copyright (C) 1989, California Institute of Technology.
C     .  U. S. Government sponsorship under
C     .  NASA contract NAS7-918 is acknowledged.
c>> 1994-11-02 SPRPL3 Krogh  Changes to use M77CON
c>> 1992-09-29 SPRPL3 WVS Changed tabs to spaces
c>> 1992-04-29 SPRPL3 CAO Changed ' 's (from 0-length string correction)
c>> 1992-04-07 CAO Changed 0-length strings to spaces(error from VAX com
c>> 1992-03-17 SPRPL3 CLL Removed F90 syntax.
c>> 1992-02-24 SPRPL3 CLL Changed IMAGE*(NIMAGE) to IMAGE*(*) in -PRPL5
c>> 1992-02-21 SPRPL3 CLL Changed a > to .gt.
c>> 1992-02-14 SPRPL3 CLL
C>> 1992-01-29 SPRPL1 CLL Added choice of no. of rows & cols in output.
C>> 1990-10-29 PRPL1 CLL More changes to formatting of x-grid labels.
C>> 1990-10-22 PRPL1 CLL Added FAC, XSMALL, YSMALL.
C>> 1988-05-24 PRPL1  Lawson  Initial code.
C>> 1983-04-04 C.L.Lawson & Stella Chan,JPL, Coded for Fortran 77.
C     Subr SPRPL3 will build a grid and place numeric grid labels and
c     titles in IMAGE()().  Also returns values in
c     XMIN, XMAX, YMIN, YMAX,   LEFT, RIGHT, BOTTOM, TOP.
c     ------------------------------------------------------------------
c                             SUBROUTINE ARGUMENTS
c
c  X1, X2, Y1, Y2 [in, floating point]  Min and max values of x and y
c             values to be plotted.
c  XMIN, XMAX, YMIN, YMAX [out, floating point]  Values to be assigned
c     to the edges of the plot grid.
c  LEFT, RIGHT, BOTTOM, TOP [out, integer]  Indices for use in IMAGE()()
c     for locating the edges of the plot grid.
c  TITLE [in,character]  Character string to be printed above the plot
c        grid as a title for the graph.
c  XNAME [in,character]  Character string to be  printed below the plot
c        grid to identify the abscissa variable.
c  YNAME [in,character]  Character string to be printed in a vertical
c        column at the left of the plot grid to identify the ordinate
c        variable.
c  NLINES [in] Number of lines available in IMAGE()() for the output
c            image.
c  NCHARS [in]  Number of characters per line available in IMAGE()() for
c            the output image.
c  IMAGE() [out,array of chars]  Array of at least NLINES elements,
c      each being a character variable of length at least NCHARS.
c      This subr will place grid and labeling characters in this array.
c  IERR [out,integer]  Termination status indicator.  0 means ok.
c     1 means need larger NCHARS.  2 means need larger NLINES.
c     ------------------------------------------------------------------
c                  Descriptions of some of the internal variables.
c
c  NEEDX, NEEDY [integer parameters]  These establish the minimum size
c     of the plotting region.  We require at least NEEDX columns from
c     the leftmost to the rightmost grid line, including these grid
c     lines.  We require at least NEEDY lines from the top to the bottom
c     grid line, including these grid lines.  The nominal setting of
c     NEEDX and NEEDY is 11 each.  The code should function with any
c     setting greater than 1, but for any hope for a reasonably
c     useful plot these should should not be too small.
c     ------------------------------------------------------------------
c--S replaces "?": ?PRPL3, ?PRPL4, ?PRPL5
c     Also uses IERM1, IERV1
C     ------------------------------------------------------------------
      integer BOTTOM, DELX1, DELX2
      integer I, IERR, ILINE, INDEX
      integer KMAJX, KMAJY, KMINX, KMINY, KSIZEX, KSIZEY, KTESTX
      integer L0, L1, LCOUNT, LEFT, NCHARS, NEEDX, NEEDY, NLINES
      integer RIGHT, TLEN, TOP, TOTX, TOTY, TOTYM1
      integer XLEN, YLEN, YLAB1, YLAB2, YNSPCE
      real             FKMAJX, FKMAJY, FYIND
      real             X1, X2, XIFAC, XMAX, XMIN
      real             Y1, YIFAC, Y2, YFAC, YMAX, YMIN, YSMALL, YVAL
      character*(*) TITLE, XNAME, YNAME
      character FMTY2*16, FMTX*14, FMTY*14, IMAGE(NLINES)*(*)
      parameter(NEEDX = 11, NEEDY = 11)
C     ------------------------------------------------------------------
      do 10 I = 1,NLINES
         IMAGE(I) = ' '
   10 continue
C
C       Determine values for the first and last grid lines:
c                    XMIN, XMAX,    YMIN, YMAX,
c       the number of major grid intervals: KMAJX, KMAJY,
c       the space needed for grid line labels: KSIZEX, KSIZEY,
c       and the formats for grid line labels: FMTX, FMTY.
C
      call SPRPL4(X1, X2, XMIN, XMAX, KMAJX, KMINX, FMTX, KSIZEX)
      call SPRPL4(Y1, Y2, YMIN, YMAX, KMAJY, KMINY, FMTY, KSIZEY)

      YSMALL=max(abs(YMIN),abs(YMAX))*0.0001e0
      FKMAJX=real(KMAJX)
      FMTY2 = '(' // FMTY // ')'
c
c     YNSPCE is the no. of horizontal char positions we allocate for
c     the (vertical) YNAME.
c     The leftmost and rightmost grid lines are at char positions
c     LEFT and RIGHT.  We set RIGHT = NCHARS-1, but setting LEFT depends
c     on KSIZEX and KSIZEY to allow room for the y-grid labels and for
c     the leftmost x-grid label.
c     If an x-grid label is centered at char position IC it will begin
c     in position IC-DELX1 and end in position IC+DELX2.  The leftmost
c     x-grid label will be centered at LEFT.  The rightmost x-grid label
c     will end at position NCHARS.
c     The y-grid lables all start at char position YLAB1 and end at
c     YLAB2.
c
      if(YNAME .eq. ' ') then
         YNSPCE = 0
      else
         YNSPCE = 2
      endif
      DELX1 = KSIZEX / 2
      DELX2 = KSIZEX -DELX1 - 1
      LEFT = YNSPCE + 1 +  max(KSIZEY + 1, DELX1)
      YLAB2 = LEFT - 2
      YLAB1 = YLAB2 - KSIZEY + 1
      RIGHT = NCHARS - 1
      TOTX = RIGHT - LEFT + 1
      KTESTX = max(NEEDX, DELX2 + 1)
      if(TOTX .lt. KTESTX) then
         IERR = 1
         call IERM1('SPRPL3',IERR,0,
     *      'Need larger NCHARS to plot and label the given data.',
     *      'Have NCHARS',NCHARS,',')
         call IERV1('Need at least NCHARS',NCHARS + KTESTX - TOTX,'.')
         return
      endif
c
c                                    Process TITLE if any, and set TOP.
c
      if(TITLE .eq. ' ') then
         TOP = 1
      else
         TOP = 2
         TLEN = len(TITLE)
         if(TLEN .le. TOTX) then
            L1 = NCHARS - TLEN + 1 - (TOTX-TLEN)/2
            IMAGE(1)(L1:L1+TLEN-1) = TITLE
         elseif(TLEN .le. NCHARS) then
            L1 = NCHARS - TLEN + 1
            IMAGE(1)(L1:L1+TLEN-1) = TITLE
         else
            IMAGE(1) = TITLE(1:NCHARS)
         endif
      endif
c
c                                 Process XNAME if any, and set BOTTOM.
c
      if(XNAME .eq. ' ') then
         BOTTOM = NLINES - 1
      else
         BOTTOM = NLINES - 2
         XLEN = len(XNAME)
         if(XLEN .le. TOTX) then
            L1 = NCHARS - XLEN + 1 - (TOTX-XLEN)/2
            IMAGE(NLINES)(L1:L1+XLEN-1) = XNAME
         elseif(XLEN .le. NCHARS) then
            L1 = NCHARS - XLEN + 1
            IMAGE(NLINES)(L1:L1+XLEN-1) = XNAME
         else
            IMAGE(NLINES) = TITLE(1:NCHARS)
         endif
      endif
      TOTY = BOTTOM - TOP + 1
      TOTYM1 = TOTY - 1
      if(TOTY .lt. NEEDY) then
         IERR = 2
         call IERM1('SPRPL3',IERR,0,
     *      'Need larger NLINES.',
     *      'Have NLINES',NLINES,',')
         call IERV1('Need at least NLINES',NLINES + NEEDY - TOTY,'.')
         return
      endif
c
c        Now have LEFT, RIGHT, TOP, and BOTTOM set.  Recall that
c        TOP is a smaller number than BOTTOM.
c        These are the indices in IMAGE()() of the edges of the plot
c        grid.
c
c                                          Process YNAME, if any.
      if(YNAME .ne. ' ') then
         YLEN = len(YNAME)
         if(YLEN .le. NLINES) then
            L0 =  (NLINES-YLEN)/2
            LCOUNT = YLEN
         else
            L0 = 0
            LCOUNT = NLINES
         endif
         do 25 I = 1,LCOUNT
            IMAGE(L0+I) = YNAME(I:I)
   25    continue
      endif
c
c                                          Draw grid box.
c
      do 26 I = TOP, BOTTOM
         IMAGE(I)(LEFT:LEFT) = '|'
         IMAGE(I)(RIGHT:RIGHT) = '|'
   26 continue
      do 27 I = LEFT+1, RIGHT-1
         IMAGE(TOP)(I:I) = '-'
         IMAGE(BOTTOM)(I:I) = '-'
   27 continue
c
c                       Place y grid line labels and "<" marks at
c                       right end of y grid lines.
c
      FKMAJY=real(KMAJY)
      YFAC = (YMAX-YMIN)/FKMAJY
      YIFAC = real(TOTYM1) / FKMAJY
      do 30 INDEX = 0,KMAJY
         FYIND=real(INDEX)
         ILINE = BOTTOM - int( FYIND * YIFAC + 0.5e0 )
         YVAL = YMIN + FYIND * YFAC
         if( abs(YVAL) .lt. YSMALL) then
            IMAGE(ILINE)(YLAB2-1:YLAB2) = '0.'
         else
            write(IMAGE(ILINE)(YLAB1:YLAB2), FMTY2) YVAL
         endif
         IMAGE(ILINE)(RIGHT+1:RIGHT+1) = '<'
   30 continue
c
c                       Place x grid line labels.
c
      call SPRPL5(XMIN, XMAX, KMAJX, FMTX, KSIZEX, LEFT,
     *            NCHARS, IMAGE(BOTTOM+1))
c
c            Place "|" marks at top and bottom of interior x grid lines.
c
      XIFAC = real(TOTX-1)/FKMAJX
      do 35 INDEX = 1,KMAJX-1
        L1 = LEFT + int( real(INDEX) * XIFAC + 0.5e0 )
        IMAGE(TOP)(L1:L1) = '|'
        IMAGE(BOTTOM)(L1:L1) = '|'
   35 continue
      return
      end
c     ==================================================================
      SUBROUTINE SPRPL4(A, B, C, D, KMAJOR, KMINOR, FMT, KSIZE)
C     .  Copyright (C) 1992, California Institute of Technology.
C     .  U. S. Government sponsorship under
C     .  NASA contract NAS7-918 is acknowledged.
C>> 1992-02-06 SPRPL4 CLL  Determine info for grid labeling.
C>> 1989-10-31 SCALK8 CLL  Force rounding of MKAJOR for Cray.
C>> 1985-08-02 SCALK8 Lawson  Initial code.
c  SPRPL4..   Select pleasant grid boundaries and build a format string.
c  C.L.LAWSON,JPL,1965 JUL  7
c  C.L.L.,JPL,1967 FEB 20 CHANGED TO MAKE C AND D ALWAYS
c  BE MULTIPLES OF UNIT.
c  MODIFIED BY CLL 7/14/72 FOR A,B CLOSE TO UNDER/OVER FLOW
c     ------------------------------------------------------------------
c                    Subroutine arguments
c
c  Input:   A, B          Output:  C, D, KMAJOR, KMINOR, FMT, KSIZE
C
c  A and B are min and max (or max and min) values
c     of a variable (either abcissa or ordinate) to be graphed.
c  KMAJOR is the recommended no. of major grid subdivisions.
c  KMINOR is the recommended no. of subdivisions within a major
c     subdivision.
c  C and D are pleasant values to be assigned to the leftmost and
c     rightmost grid boundaries.  Will satisfy C < D.
c     The closed interval [C,D] will generally contain the values A and
c     B, except that A and/or B may be outside [C,D] by a distance of
c     up to 0.0001 * (D-C).
c  FMT [out, char*14]  Recommended format for a single grid label value.
c     This will not contain parentheses.
c     Examples: 'ss,f05.01     ' or 'ss,1pe15.07e02'
c  KSIZE [out, integer]  No. of char positions that will be used to
c     display a number when it is output using the format, FMT.
c     ------------------------------------------------------------------
c             Description of some of the internal variables.
c
c  ESIZE, EXSIZE, FSIZE [integers]  ESIZE and FSIZE give the total no.
c     of char positions needed for an E or F format, respectively.
c     EXSIZE is the no. of digit positions needed in the exponent part
c     of an E format.
c
c  HI, LO, COUNT [integers]  COUNT = HI - LO + 1.
c     HI and LO indicate the position of the most and
c     least significant digit that must be repesented in the printed
c     output.  Number template:    x   x   x   x . x   x   x
c               Digit position:    3   2   1   0  -1  -2  -3
c     COUNT is the number of digits needed in the output.
c     For example for the number 593.62 we would have
c     HI = 2, LO = -2, and COUNT = 5.
c
c  SPAN(), KMAJ(), EKMN() [integers]  These prestored tables are
c     related by SPAN(i) = KMAJ(i) * EKMN(i).
c     This subr chooses a pair of values
c     KMAJ(i) and EKMN(i) to return as KMAJOR and KMINOR.  The KMAJ()
c     table may be commented out, in which case its values will be
c     computed as needed by KMAJ(i) = SPAN(i) / EKMIN(i).
c     The prestored values in these tables are chosen so the values of
c     SPAN() are > 10, and .le. 100, and in increasing order, and give
c     somewhat uniform logorathmic coverage of the range from 10 to 100.
c     The values of EKMN() are limited to be 2, 5, or 10.
c     The values of KMAJ() are limited to be .ge. 3 and .le. 10.
c     ------------------------------------------------------------------
      integer COUNT, ESIZE, EXSIZE, FSIZE, HI
      integer I, IG, IMAX, K, KMAJOR, KMINOR, KSIZE, LO
      parameter(IMAX = 18)
      real             A, A1, A2, B, B1, B2, BMA, C, D
      real             EKMN(IMAX), F,  FRAC, P, SMALL, SPAN(IMAX), TEMP
      real             UNIT, V, X
      character FMT*14
      data(SPAN(I),I=1,6 )/12.0e0,14.0e0,15.0e0,16.0e0,18.0e0,20.0e0/
C     data(KMAJ(I),I=1,6 )/ 6.0e0, 7.0e0, 3.0e0, 8.0e0, 9.0e0,10.0e0/
      data(EKMN(I),I=1,6 )/ 2.0e0, 2.0e0, 5.0e0, 2.0e0, 2.0e0, 2.0e0/

      data(SPAN(I),I=7,10)/ 20.0e0,25.0e0, 30.0e0,35.0e0/
c     data(KMAJ(I),I=7,10)/  4.0e0, 5.0e0,  3.0e0, 7.0e0/
      data(EKMN(I),I=7,10)/  5.0e0, 5.0e0, 10.0e0, 5.0e0/

      data(SPAN(I),I=11,16)/40.0e0,45.0e0, 50.0e0,60.0e0,70.0e0,80.0e0/
C     data(KMAJ(I),I=11,16)/ 4.0e0, 9.0e0,  5.0e0, 6.0e0, 7.0e0, 8.0e0/
      data(EKMN(I),I=11,16)/10.0e0, 5.0e0, 10.0e0,10.0e0,10.0e0,10.0e0/

      data(SPAN(I),I=17,18)/ 90.0e0,100.0e0/
C     data(KMAJ(I),I=17,18)/  9.0e0, 10.0e0/
      data(EKMN(I),I=17,18)/ 10.0e0, 10.0e0/

c     ------------------------------------------------------------------
C                          CHANGE A,B TO A1,B1, with A1 < B1
      if(A .lt. B) then
         A1 = A
         B1 = B
      elseif(A .gt. B) then
         A1= B
         B1= A
      elseif(A .eq. 0.0e0) then
         A1=-1.0e0
         B1=+1.0e0
      else
         SMALL= 0.01e0 * abs(A)
         A1=A - SMALL
         B1=B + SMALL
      endif
C                 Now we have A1 < B1
C
C                 PERTURB A1 and B1 TO AVOID BAD
C                 DECISIONS DUE TO ROUND-OFF.
      A2=A1
      B2=B1
      SMALL=(B1-A1)* 0.0001e0
      if(A1.ne.0.0e0) A1=A1+SMALL
      if(B1.ne.0.0e0) B1=B1-SMALL
      BMA=B1-A1
      if(BMA .le. 0.0e0) then
         A1=A2
         B1=B2
         BMA=B1-A1
      endif
C                Convert BMA to X*10**G = X*P with 10. < X .le. 100.
      V=log10(BMA)
      IG = int(V)
      F = V - real(IG)
      if(F .le. 0.0e0) F=F+1.0e0
      X=10.0e0 ** (F+1.0e0)
      P=BMA/X
      if(X .le. 10.0e0) then
         X=X*10.0e0
         P=P/10.0e0
         IG = IG - 1
      endif
C                         ENTER SPAN( ) TABLE USING X
      do 90 I=1,IMAX
         if(X .le. SPAN(I))go to 95
   90 continue
      I=IMAX
   95 continue
C                     DETERMINE WHETHER SPAN(I) CAN BE USED
      do 105 K=1,3
  100    UNIT=EKMN(I)*P
         C=UNIT * aint(A1/UNIT)
         if(C .gt. A1) C=C-UNIT
         D=C+SPAN(I)*P
         if(B1 .le. D) go to 110
         I=I+1
         if(I .le. IMAX) go to 100
         I=1
         P=P*10.0e0
  105 continue
C                           TROUBLE: A or B close to UNDER/OVER FLOW
c
      write(6,'(a,4e20.8)') ' SPRPL4 ERROR.. A,B,C,D,=',A,B,C,D
  110 continue
      if( abs(C) .lt. 0.0001e0 * BMA) then
         C = 0.0e0
      elseif( abs(D) .lt. 0.0001e0 * BMA) then
         D = 0.0e0
      endif
c
c     The ratio SPAN(I)/EKMN(I) is an exact integer value, however
c     the Cray X/MP sometimes returns a value less than the exact value,
c     so we added the addition of 0.5 to work around this defect.
c
      KMAJOR=int(0.5e0 + SPAN(I)/EKMN(I))
      KMINOR=EKMN(I)
c
      TEMP = log10(max(abs(C), abs(D)))
      HI = int(TEMP)
      FRAC = TEMP - real(HI)
      if(FRAC .lt. 0.0e0) HI = HI - 1
c                                     nint() rounds to nearest integer.
      LO = nint(log10(P))
      if(KMINOR .eq. 10.0e0) LO = LO + 1
*     print*,'SPRPL4..'
*     print'(/a,a/)','       A             B    KMAJOR KMINOR ',
*    *      ' C             D           HI    LO'
*     print'(/1x,2g14.6,2i3,2g14.6,2i6)',
*    *    A, B, KMAJOR, KMINOR, C, D, HI, LO
c
c        Set FSIZE to No. of char positions needed if F format is used.
c        First we assume C and D are each nonnegative.
c
      COUNT = HI - LO + 1
      if(HI .lt. 0) then
c                                       0.00xxx
         FSIZE = COUNT - HI + 1
      elseif(LO .gt. -1) then
c                                       xxx00.
         FSIZE = COUNT + LO + 1
      else
c                                       x.xx
         FSIZE = COUNT + 1
      endif
c
c     Set ESIZE to No. of char positions needed if E format is used.
c     First we assume C and D are each nonnegative.
c     EXSIZE is the No. of digit positions needed in the exponent part.
c
      if(HI .eq. 0) then
         EXSIZE = 1
      else
         EXSIZE = 1 + int(log10(real(abs(HI))))
      endif
c                                          x.xxE+yy
      ESIZE = COUNT + 3 + EXSIZE
c
c                     Adjust FSIZE and ESIZE if C or D is negative.
c
      if(C .lt. 0.0e0 .or. D .lt. 0.0E0) then
         FSIZE = FSIZE + 1
         ESIZE = ESIZE + 1
      endif
*     print'(a,i4,a,i4,a,i4)',
*    *   ' FSIZE=',FSIZE,',  ESIZE=',ESIZE,',  EXSIZE=',EXSIZE
c
c                            Build the format string.
c
      if(FSIZE .le. ESIZE) then
         KSIZE = FSIZE
         FMT(1:14) = 'ss,f  .       '
         write(FMT(5:6),'(i2.2)') FSIZE
         write(FMT(8:9),'(i2.2)') max(0,-LO)
      else
         KSIZE = ESIZE
         FMT(1:14) = 'ss,1pe  .  e  '
         write(FMT(7:8),'(i2.2)') ESIZE
         write(FMT(10:11),'(i2.2)') COUNT-1
         write(FMT(13:14),'(i2.2)') EXSIZE
      endif
      return
      end
c     ==================================================================
      SUBROUTINE SPRPL5(C, D, KMAJX, FMT, KSIZE, LEFT, NIMAGE, IMAGE)
C     .  Copyright (C) 1992, California Institute of Technology.
C     .  U. S. Government sponsorship under
C     .  NASA contract NAS7-918 is acknowledged.
C>> 1992-02-06 SPRPL5 CLL
c     SPRPL5 builds a print line of numeric grid labels for the x axis.
c     ------------------------------------------------------------------
c                    Subroutine arguments
c
c  Input:   C, D, KMAJX, FMT, KSIZE, LEFT, NIMAGE   Output:  IMAGE
C
c  C and D are pleasant values that have been assigned to the leftmost
c     and rightmost grid boundaries.  Will satisfy C < D.
c  KMAJX is the recommended no. of major grid subdivisions.
c  FMT [in, char*14]  Recommended format for a single grid label value.
c     This will not contain parentheses.
c     Examples: 'ss,f05.01     ' or 'ss,1pe15.07e02'
c  KSIZE [in, integer]  No. of char positions that will be used to
c     display a number when it is output using the format, FMT.
c  LEFT [in, integer]  Index of position in IMAGE aligned with the
c     leftmost grid line.  Indexing is 1-based.
c  NIMAGE [in, integer]  Index of last useable position in IMAGE.
c     The rightmost grid line aligns with index IMAGE-1.
c  IMAGE [out, char*NIMAGE]  Character string in which this subr will
c     build a print line of numeric grid labels.
c     ------------------------------------------------------------------
c  We assume tests have been made in [D/S]PRPL3 to assure there is
c  enough space in IMAGE() to at least place the label for the leftmost
c  x grid line.  It extends from LEFT-DELX1 to LEFT+DELX2.  DELX1 and
c  DELX2 are computed in [D/S]PRPL3 for use in tests, and are computed
c  again in this subroutine for use in placing grid labels.
c  If there is not enough space for other grid labels we just omit them.
c     ------------------------------------------------------------------
      integer AVAIL1, AVAIL2, DELX1, DELX2
      integer I1, I2, IC, INDEX, KMAJX, KSIZE, LEFT, NIMAGE
      real             C, D, FKMAJX, FXIND, IXFAC, XFAC, XSMALL, XVAL
      character FMT*14, FMT2*16, IMAGE*(*)
c     ------------------------------------------------------------------
      IMAGE(1:NIMAGE) = ' '
      AVAIL1 = 1
      AVAIL2 = NIMAGE
      FMT2 = '(' // FMT // ')'
      XSMALL = 0.0001e0 * (D-C)
      DELX1 = KSIZE / 2
      DELX2 = KSIZE -DELX1 - 1
c
c     When centering a label on position IC, the label will occupy
c     positions from IC - DELX1 through IC + DELX2.
c
c                       Try to place leftmost grid label.
c
      IC = LEFT
      if( abs(C) .lt. XSMALL) then
         IMAGE(IC:IC) = '0'
         AVAIL1 = IC+2
      else
         I1 = IC - DELX1
         I2 = IC + DELX2
         if(I1 .ge. AVAIL1 .and. I2 .le. AVAIL2) then
            write(IMAGE(I1:I2), FMT2) C
            AVAIL1 = I2+2
         endif
      endif
c
c                       Try to place rightmost grid label.
c
      IC = NIMAGE - 1
      if( abs(D) .lt. XSMALL) then
         if(IC .ge. AVAIL1 ) then
            IMAGE(IC:IC) = '0'
            AVAIL2 = IC-2
         endif
      else
         I1 = NIMAGE - KSIZE + 1
         I2 = NIMAGE
         if(I1 .ge. AVAIL1 ) then
            write(IMAGE(I1:I2), FMT2) D
            AVAIL2 = I1-2
         endif
      endif
c
c                       Try to place interior grid labels.
c
      FKMAJX=real(KMAJX)
      XFAC = (D-C)/FKMAJX
      IXFAC = real(NIMAGE - LEFT - 1) / FKMAJX
      do 30 INDEX = 1,KMAJX-1
         FXIND=real(INDEX)
         IC = LEFT + int( FXIND * IXFAC + 0.5e0 )
         XVAL = C + FXIND * XFAC
         if( abs(XVAL) .lt. XSMALL) then
            if(IC .ge. AVAIL1 .and. IC .le. AVAIL2) then
               IMAGE(IC:IC) = '0'
               AVAIL1 = IC+2
            endif
         else
            I1 = IC - DELX1
            I2 = IC + DELX2
            if(I1 .ge. AVAIL1 .and. I2 .le. AVAIL2) then
               write(IMAGE(I1:I2), FMT2) XVAL
               AVAIL1 = I2+2
            endif
         endif
   30 continue
      return
      end
