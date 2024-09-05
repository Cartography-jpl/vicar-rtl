      subroutine SSORTP (A, M, N, P)
C     .  Copyright (C) 1989, California Institute of Technology.
C     .  All rights reserved.  U. S. Government sponsorship under
C     .  NASA contract NAS7-918 is acknowledged.
c>> 1994-10-19 SSORTP  Krogh  Changes to use M77CON
c>> 1992-11-23  SSORTP  Snyder  Add entry SSORTQ.
C>> 1991-04-02  SSORTP  Snyder  Repair no permutation vector if m-n < 10
C>> 1988-11-22  SSORTP  Snyder  Initial code.
c--S replaces "?": ?SORTP, ?SORTQ
c                                                                       00001100
c     Sort the M:N-vector A.
c     A is not disturbed.  P is set so that A(P(J)) is the J'th element
c     of the sorted sequence.
c     Enter at SSORTQ to use pre-specified permutation vector.
c
c     To sort an array A' into descending order, let A = -A'
c     To sort an array A' into ascending order according to the
c     absolute value of the elements let A = ABS(A').
c     To sort an array A' into decending order according to the
c     absolute value of the elements let A = -ABS(A').                  00002100
c
      integer M, N
      real             A(*)
      integer P(*)
c
c     *****     Local Variables     ************************************
c
c BL      is the left bound of the sub-array to be sorted at the next
c         step.
c BR      is the right bound of the sub-array to be sorted at the next  00003100
c         step.
c CL      is the current left bound of the unsorted sub-array.
c CR      is the current right bound of the unsorted sub-array.
c PARTN   is the partition element.
c PTEMP   holds elements of P during exchanges.
c STACKL  keeps track of the left bounds of sub-arrays waiting to be
c         sorted.
c STACKR  keeps track of the right bounds of sub-arrays waiting to be
c         sorted.
c STKTOP  keeps track of the top of the stacks.                         00004100
c
      integer BL,BR,CL,CR
      real             PARTN
      integer PTEMP,STACKL(32),STACKR(32),STKTOP
c
c     *****     Executable Statements     ******************************
c
      DO 20002 cl = m, n
         p(cl)=cl
20002 CONTINUE                                                          00005200
      entry SSORTQ (A, M, N, P)
      IF(n-m.ge.10)THEN
         stktop=1
         stackl(1)=m
         stackr(1)=n
      GO TO 20009
20007 IF (stktop.eq.0) GO TO 20008
20009       bl=stackl(stktop)
            br=stackr(stktop)
            stktop=stktop-1
c           Choose a partitioning element.  Use the median of the first,00006200
c           middle and last elements.  Sort them so the extreme elements
c           can serve as sentinels during partitioning.
            cl=(bl+br)/2
            ptemp=p(cl)
      IF(a(p(bl)).gt.a(ptemp))THEN
               p(cl)=p(bl)
               p(bl)=ptemp
               ptemp=p(cl)
      END IF
      IF(a(p(bl)).gt.a(p(br)))THEN                                      00007200
               cr=p(bl)
               p(bl)=p(br)
               p(br)=cr
      END IF
      IF(a(ptemp).gt.a(p(br)))THEN
               p(cl)=p(br)
               p(br)=ptemp
               ptemp=p(cl)
      END IF
            p(cl)=p(br-1)                                               00008200
            p(br-1)=ptemp
            partn=a(ptemp)
c           Partition the sub-array around PARTN.  Exclude the above
c           considered elements from partitioning because they're al-
c           ready in the correct subfiles.  Stop scanning on equality to
c           prevent files containing equal values from causing a loop.
            cl=bl
            cr=br-1
20016 CONTINUE
      GO TO 20020                                                       00009200
20018 IF (a(p(cl)).ge.partn) GO TO 20019
20020             cl=cl+1
      GO TO 20018
20019 GO TO 20023
20021 IF (a(p(cr)).le.partn) GO TO 20022
20023             cr=cr-1
      GO TO 20021
20022 IF (cl.gt.cr) GO TO 20017
               ptemp=p(cl)
               p(cl)=p(cr)
               p(cr)=ptemp
      GO TO 20016                                                       00010200
20017 CONTINUE
c           Put sub-arrays on the stack if they're big enough.  Put the
c           larger under the smaller, so the smaller will be done next.
c           This makes the upper bound of the stack depth log2 (n-m+1).
c           (The "Hibbard" modification of quicksort).
      IF(cl-bl .gt. br-cr)THEN
      IF(cl-bl.gt.10)THEN
                  stktop=stktop+1
                  stackl(stktop)=bl
                  stackr(stktop)=cr                                     00011100
      END IF
      IF(br-cr.gt.10)THEN
                  stktop=stktop+1
                  stackl(stktop)=cl
                  stackr(stktop)=br
      END IF
      ELSE
      IF(br-cr.gt.10)THEN
                  stktop=stktop+1
                  stackl(stktop)=cl                                     00012100
                  stackr(stktop)=br
      END IF
      IF(cl-bl.gt.10)THEN
                  stktop=stktop+1
                  stackl(stktop)=bl
                  stackr(stktop)=cr
      END IF
      END IF
      GO TO 20007
20008 CONTINUE                                                          00013100
      END IF
c     Clean up small subfiles using insertion sort on everything.
      DO 20034 cr = m+1, n
         ptemp=p(cr)
         partn=a(ptemp)
         cl=cr
20037 IF(a(p(cl-1)).gt.partn)THEN
            p(cl)=p(cl-1)
            cl=cl-1
      IF (cl.le.m) GO TO 20038                                          00014000
      GO TO 20037
      END IF
20038 CONTINUE
         p(cl)=ptemp
20034 CONTINUE
c
      return
c
      end        
