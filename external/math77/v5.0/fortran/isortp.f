      subroutine ISORTP (I, M, N, P)
C     .  Copyright (C) 1989, California Institute of Technology.
C     .  All rights reserved.  U. S. Government sponsorship under
C     .  NASA contract NAS7-918 is acknowledged.
c>> 1992-11-23  ISORTP  Snyder  Add entry ISORTQ.
C>> 1991-04-02  ISORTP  Snyder  Repair no permutation vector if m-n < 10
C>> 1988-11-22  ISORTP  Snyder  Initial code.
c
c     Sort the M:N-vector of integers I.
c     I is not disturbed.  P is set so that I(P(J)) is the J'th element 00001100
c     of the sorted sequence.
c     Enter at ISORTQ to use pre-specified permutation vector.
c
c     To sort an array I' into descending order, let I = -I'
c     To sort an array I' into ascending order according to the
c     absolute value of the elements let I = ABS(I').
c     To sort an array I' into decending order according to the
c     absolute value of the elements let I = -ABS(I').
c
      integer M, N                                                      00002100
      integer I(*)
      integer P(*)
c
c     *****     Local Variables     ************************************
c
c BL      is the left bound of the sub-array to be sorted at the next
c         step.
c BR      is the right bound of the sub-array to be sorted at the next
c         step.
c CL      is the current left bound of the unsorted sub-array.          00003100
c CR      is the current right bound of the unsorted sub-array.
c PARTN   is the partition element.
c PTEMP   holds elements of P during exchanges.
c STACKL  keeps track of the left bounds of sub-arrays waiting to be
c         sorted.
c STACKR  keeps track of the right bounds of sub-arrays waiting to be
c         sorted.
c STKTOP  keeps track of the top of the stacks.
c
      integer BL,BR,CL,CR                                               00004100
      integer PARTN
      integer PTEMP,STACKL(32),STACKR(32),STKTOP
c
c     *****     Executable Statements     ******************************
c
      DO 20002 cl = m, n
         p(cl)=cl
20002 CONTINUE
      entry ISORTQ (I, M, N, P)
      IF(n-m.ge.10)THEN                                                 00005200
         stktop=1
         stackl(1)=m
         stackr(1)=n
      GO TO 20009
20007 IF (stktop.eq.0) GO TO 20008
20009       bl=stackl(stktop)
            br=stackr(stktop)
            stktop=stktop-1
c           Choose a partitioning element.  Use the median of the first,
c           middle and last elements.  Sort them so the extreme elements00006100
c           can serve as sentinels during partitioning.
            cl=(bl+br)/2
            ptemp=p(cl)
      IF(i(p(bl)).gt.i(ptemp))THEN
               p(cl)=p(bl)
               p(bl)=ptemp
               ptemp=p(cl)
      END IF
      IF(i(p(bl)).gt.i(p(br)))THEN
               cr=p(bl)                                                 00007100
               p(bl)=p(br)
               p(br)=cr
      END IF
      IF(i(ptemp).gt.i(p(br)))THEN
               p(cl)=p(br)
               p(br)=ptemp
               ptemp=p(cl)
      END IF
            p(cl)=p(br-1)
            p(br-1)=ptemp                                               00008100
            partn=i(ptemp)
c           Partition the sub-array around PARTN.  Exclude the above
c           considered elements from partitioning because they're al-
c           ready in the correct subfiles.  Stop scanning on equality to
c           prevent files containing equal values from causing a loop.
            cl=bl
            cr=br-1
20016 GO TO 20020
20018 IF (i(p(cl)).ge.partn) GO TO 20019
20020             cl=cl+1                                               00009100
      GO TO 20018
20019 GO TO 20023
20021 IF (i(p(cr)).le.partn) GO TO 20022
20023             cr=cr-1
      GO TO 20021
20022 IF (cl.gt.cr) GO TO 20017
               ptemp=p(cl)
               p(cl)=p(cr)
               p(cr)=ptemp
      GO TO 20016                                                       00010000
c           Put sub-arrays on the stack if they're big enough.  Put the
c           larger under the smaller, so the smaller will be done next.
c           This makes the upper bound of the stack depth log2 (n-m+1).
c           (The "Hibbard" modification of quicksort).
20017 IF(cl-bl .gt. br-cr)THEN
      IF(cl-bl.gt.10)THEN
                  stktop=stktop+1
                  stackl(stktop)=bl
                  stackr(stktop)=cr
      END IF                                                            00011000
      IF(br-cr.gt.10)THEN
                  stktop=stktop+1
                  stackl(stktop)=cl
                  stackr(stktop)=br
      END IF
      ELSE
      IF(br-cr.gt.10)THEN
                  stktop=stktop+1
                  stackl(stktop)=cl
                  stackr(stktop)=br                                     00012000
      END IF
      IF(cl-bl.gt.10)THEN
                  stktop=stktop+1
                  stackl(stktop)=bl
                  stackr(stktop)=cr
      END IF
      END IF
      GO TO 20007
20008 CONTINUE
      END IF                                                            00012900
c     Clean up small subfiles using insertion sort on everything.
      DO 20034 cr = m+1, n
         ptemp=p(cr)
         partn=i(ptemp)
         cl=cr
20037 IF(i(p(cl-1)).gt.partn)THEN
            p(cl)=p(cl-1)
            cl=cl-1
      IF (cl.le.m) GO TO 20038
      GO TO 20037                                                       00013900
      END IF
20038    p(cl)=ptemp
20034 CONTINUE
c
      return
c
      end        
