      subroutine SSORT (A, M, N)
C     .  Copyright (C) 1989, California Institute of Technology.
C     .  All rights reserved.  U. S. Government sponsorship under
C     .  NASA contract NAS7-918 is acknowledged.
C>> 1994-10-19 SSORT  Krogh  Changes to use M77CON
C>> 1988-11-22  SSORT  Snyder  Initial code.
c--S replaces "?": ?SORT, ?SORTP
c
c     Sort the M:N-vector A into ascending order.
c                                                                       00001100
c     To sort an array A' into descending order, let A = -A'
c     To sort an array A' into ascending order according to the
c     absolute value of the elements let A = ABS(A').
c     To sort an array A' into decending order according to the
c     absolute value of the elements let A = -ABS(A').
c
c     To keep track of the original elements, use SSORTP.
c
      integer M, N
c--S Next line special: I                                               00002100
      real             A(*)
c
c     *****     Local Variables     ************************************
c
c BL      is the left bound of the sub-array to be sorted at the next
c         step.
c BR      is the right bound of the sub array to be sorted at the next
c         step.
c CL      is the current left bound of the unsorted sub-array.
c CR      is the current right bound of the unsorted sub-array.         00003100
c PARTN   is the partition element.
c STACKL  keeps track of the left bounds of sub-arrays waiting to be
c         sorted.
c STACKR  keeps track of the right bounds of sub-arrays waiting to be
c         sorted.
c STKTOP  keeps track of the top of the stacks.
c TEMP    holds elements of A during exchanges.
c
      integer BL,BR,CL,CR
c--S Next line special: I                                               00004100
      real             PARTN,TEMP
      integer STACKL(32),STACKR(32),STKTOP
c
c     *****     Executable Statements     ******************************
c
      IF(n-m.ge.10)THEN
         stktop=1
         stackl(1)=m
         stackr(1)=n
      GO TO 20006                                                       00005200
20004 IF (stktop.eq.0) GO TO 20005
20006       bl=stackl(stktop)
            br=stackr(stktop)
            stktop=stktop-1
c           Choose a partitioning element.  Use the median of the first,
c           middle and last elements.  Sort them so the extreme elements
c           serve as sentinels during partitioning.
            cl=(bl+br)/2
            partn=a(cl)
      IF(a(bl).gt.partn)THEN
               a(cl)=a(bl)                                              00006200
               a(bl)=partn
               partn=a(cl)
      END IF
      IF(a(bl).gt.a(br))THEN
               temp=a(br)
               a(br)=a(bl)
               a(bl)=temp
      END IF
      IF(partn.gt.a(br))THEN
               a(cl)=a(br)                                              00007200
               a(br)=partn
               partn=a(cl)
      END IF
            a(cl)=a(br-1)
            a(br-1)=partn
c           Partition the sub-array around PARTN.  Exclude the above
c           considered elements from partitioning because they're al-
c           ready in the correct subfiles.  Stop scanning on equality to
c           prevent files containing equal values from causing a loop.
            cl=bl                                                       00008200
            cr=br-1
20013 CONTINUE
      GO TO 20017
20015 IF (a(cl).ge.partn) GO TO 20016
20017             cl=cl+1
      GO TO 20015
20016 GO TO 20020
20018 IF (a(cr).le.partn) GO TO 20019
20020             cr=cr-1
      GO TO 20018
20019 IF (cl.gt.cr) GO TO 20014
               temp=a(cl)                                               00009200
               a(cl)=a(cr)
               a(cr)=temp
      GO TO 20013
20014 CONTINUE
c           Put sub-arrays on the stack if they're big enough.  Put the
c           larger under the smaller, so the smaller will be done next.
c           This makes the upper bound of the stack depth log2 (n-m+1).
c           (The "Hibbard" modification of quicksort).
      IF(cl-bl .gt. br-cr)THEN
      IF(cl-bl.gt.10)THEN                                               00010100
                  stktop=stktop+1
                  stackl(stktop)=bl
                  stackr(stktop)=cr
      END IF
      IF(br-cr.gt.10)THEN
                  stktop=stktop+1
                  stackl(stktop)=cl
                  stackr(stktop)=br
      END IF
      ELSE                                                              00011100
      IF(br-cr.gt.10)THEN
                  stktop=stktop+1
                  stackl(stktop)=cl
                  stackr(stktop)=br
      END IF
      IF(cl-bl.gt.10)THEN
                  stktop=stktop+1
                  stackl(stktop)=bl
                  stackr(stktop)=cr
      END IF                                                            00012100
      END IF
      GO TO 20004
20005 CONTINUE
      END IF
c     Clean up small subfiles using insertion sort on everything.
      DO 20031 cr = m+1, n
         partn=a(cr)
         cl=cr
20034 IF(a(cl-1).gt.partn)THEN
            a(cl)=a(cl-1)                                               00013000
            cl=cl-1
      IF (cl.le.m) GO TO 20035
      GO TO 20034
      END IF
20035 CONTINUE
         a(cl)=partn
20031 CONTINUE
c
      return
c                                                                       00013800
      end        
