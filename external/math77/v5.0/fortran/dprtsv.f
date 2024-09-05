      subroutine DPRTSV (A, MDA, M, N, NAMES, MODE, UNIT, WIDTH)
c>> 1994-10-20 DPRTSV Krogh  Changes to use M77CON
c>> 1992-03-22 DPRTSV CAO Deleted 4 debug statements
C>> 1992-03-18 CLL Allow user to choose size of names in NAMES().
C>> 1989-03-07 DPRTSV CLL Added arguments UNIT and WIDTH
C>> 1987-11-24 DPRTSV Lawson  Initial code.
c     Prints matrix with labeling, to be called by the Singular Value
c     Analysis subroutine, [D/S]SVA.
c     ------------------------------------------------------------------
c                           Subroutine Arguments
C     All are inout arguments.  None are modified by this subroutine.
c
C     A(,)     Array containing matrix to be output.
C     MDA      First dimension of the array, A(,).
C     M, N     No. of rows and columns, respectively in the matrix
c              contained in A(,).
C     NAMES()  [character array]  Array of names.
c              If NAMES(1) contains only blanks, the rest of the NAMES()
c              array will be ignored.
C     MODE     =1  Write header for V matrix and use an F format.
C              =2  Write header for  for candidate solutions and use
c                  P format.
c     UNIT  [integer]   Selects output unit.  If UNIT .ge. 0 then UNIT
c              is the output unit number.  If UNIT = -1, output to
c              the '*' unit.
c     WIDTH [integer]   Selects width of output lines.
c              Each output line from this subroutine will have at most
c              max(26,min(124,WIDTH)) characters plus one additional
c              leading character for Fortran "carriage control".  The
c              carriage control character will always be a blank.
c     ------------------------------------------------------------------
c          This code was originally developed by Charles L. Lawson and
c     Richard J. Hanson at Jet Propulsion Laboratory in 1973.  The
c     original code was described and listed in the book,
c
c                  Solving Least Squares Problems
c                  C. L. Lawson and R. J. Hanson
c                  Prentice-Hall, 1974
c
c     Feb 1985, Mar 1987, C. L. Lawson & S. Y. Chan, JPL.
c     Adapted code from the Lawson & Hanson book to Fortran 77 for use
c     in the JPL MATH77 library.
c     Prefixing subprogram names with S or D for s.p. or d.p. versions.
c     Using generic names for intrinsic functions.
c     Adding calls to BLAS and MATH77 error processing subrs in some
c     program units.
c     1989-03-06 CLL added arguments UNIT and WIDTH.
c     ------------------------------------------------------------------
c--D replaces "?": ?PRTSV
c     ------------------------------------------------------------------
      integer I, J, J1, J2, KBLOCK, L, LENNAM
      integer M, MAXCOL, MDA, MODE, N, NAMSIZ, NBLOCK, UNIT, WIDTH
      double precision    A(MDA,N)
      character  NAMES(M)*(*)
      character*4  HEAD (2)
      character*26 FMT1(2)
      character*26 FMT2(2)
      logical      BLKNAM, STAR
C
      data HEAD(1)/' COL'/
      data HEAD(2)/'SOLN'/
      data FMT1 / '(/7x,00x,8(5x,a4,i4,1x)/)',
     *            '(/7x,00x,8(2x,a4,i4,4x)/)'/
      data FMT2 / '(1x,i4,1x,a00,1x,4p8f14.0)',
     *            '(1x,i4,1x,a00,1x,8g14.6  )'/
c     ------------------------------------------------------------------
      if (M .le. 0 .or. N .le. 0) return
C
c     The LEN function returns the char length of a single element of
c     the NAMES() array.
c
      LENNAM = len(NAMES(1))
      BLKNAM = NAMES(1) .eq. ' '
      NAMSIZ = 1
      if(.not. BLKNAM) then
         do 30 I = 1,M
            do 10 L = LENNAM, NAMSIZ+1, -1
               if(NAMES(I)(L:L) .ne. ' ') then
                  NAMSIZ = L
                  go to 20
               endif
   10       continue
   20       continue
   30    continue
      endif
c
      write(FMT1(MODE)(6:7),'(i2.2)') NAMSIZ
      write(FMT2(MODE)(12:13),'(i2.2)') NAMSIZ
      STAR = UNIT .lt. 0
      if(STAR) then
         if (MODE .eq. 1) then
            write (*,70)
         else
            write (*,80)
         endif
      else
         if (MODE .eq. 1) then
            write (UNIT,70)
         else
            write (UNIT,80)
         endif
      endif
c
c     With NAMSIZ characters allowed for the "name" and MAXCOL
c     columns of numbers, the total line width, exclusive of a
c     carriage control character, will be 6 + LENNAM + 14 * MAXCOL.
c
      MAXCOL = max(1,min(8,(WIDTH - 6 - NAMSIZ)/14))
C
      NBLOCK = (N + MAXCOL -1) / MAXCOL
      J2 = 0
C
      do 50 KBLOCK = 1, NBLOCK
         J1 = J2 + 1
         J2 = min(N, J2 + MAXCOL)
         if(STAR) then
            write (*,FMT1(MODE)) (HEAD(MODE),J,J=J1,J2)
         else
            write (UNIT,FMT1(MODE)) (HEAD(MODE),J,J=J1,J2)
         endif
C
         do 40 I=1,M
            if(STAR) then
               if(BLKNAM) then
                  write (*,FMT2(MODE)) I,' ',(A(I,J),J=J1,J2)
               else
                  write (*,FMT2(MODE)) I,NAMES(I),(A(I,J),J=J1,J2)
               endif
            else
               if(BLKNAM) then
                  write (UNIT,FMT2(MODE)) I,' ',(A(I,J),J=J1,J2)
               else
                  write (UNIT,FMT2(MODE)) I,NAMES(I),(A(I,J),J=J1,J2)
               endif
            endif
   40    continue
   50 continue
C
   70 format(/' V-Matrix of the Singular Value Decomposition of A*D.'/
     *    ' (Elements of V scaled up by a factor of 10**4)')
   80 format(/' Sequence of candidate solutions, X')
      end
