      subroutine CSORT (C, M, N, K, L, CTEMP)
C     .  Copyright (C) 1989, California Institute of Technology.
C     .  All rights reserved.  U. S. Government sponsorship under
C     .  NASA contract NAS7-918 is acknowledged.
C>> 1994-11-14  CSORT  Krogh   Declared all vars.
c>> 1988-11-22  CSORT  Snyder  Initial code.
c
c     Sort the M:N-vector of character strings C according to the (K:L)
c     substring of each element.  CTEMP is a temporary scalar character
c     string at least as long as an element of C.                       00001100
c
      integer M, N
      character*(*) C(*), CTEMP
c
c     *****     Local Variables     ************************************
c
c BL      is the left bound of the sub-array to be sorted at the next
c         step.
c BR      is the right bound of the sub array to be sorted at the next
c         step.                                                         00002100
c CL      is the current left bound of the unsorted sub-array.
c CR      is the current right bound of the unsorted sub-array.
c CTEMP   holds elements of C during exchanges.
c PARTN   is the subscript of the partition element.
c STACKL  keeps track of the left bounds of sub-arrays waiting to be
c         sorted.
c STACKR  keeps track of the right bounds of sub-arrays waiting to be
c         sorted.
c STKTOP  keeps track of the top of the stacks.
c                                                                       00003100
      integer BL,BR,CL,CR,PARTN, K, L
      integer STACKL(32),STACKR(32),STKTOP
c
c     *****     Executable Statements     ******************************
c
      IF(n-m.ge.10)THEN
         stktop=1
         stackl(1)=m
         stackr(1)=n
      GO TO 20006                                                       00004200
20004 IF (stktop.eq.0) GO TO 20005
20006       bl=stackl(stktop)
            br=stackr(stktop)
            stktop=stktop-1
c           Choose a partitioning element.  Use the median of the first,
c           middle and last elements.  Sort them so the extreme elements
c           serve as sentinels during partitioning.
            cl=(bl+br)/2
      IF(c(bl)(k:l).gt.c(cl)(k:l))THEN
               ctemp=c(cl)
               c(cl)=c(bl)                                              00005200
               c(bl)=ctemp
      END IF
      IF(c(bl)(k:l).gt.c(br)(k:l))THEN
               ctemp=c(bl)
               c(bl)=c(cl)
               c(cl)=ctemp
      END IF
      IF(c(cl)(k:l).gt.c(br)(k:l))THEN
               ctemp=c(cl)
               c(cl)=c(bl)                                              00006200
               c(bl)=ctemp
      END IF
            partn=cl
            ctemp=c(br-1)
            c(br-1)=c(cl)
            c(cl)=ctemp
c           Partition the sub-array around PARTN.  Exclude the above
c           considered elements from partitioning because they're al-
c           ready in the correct subfiles.  Stop scanning on equality to
c           prevent files containing equal values from causing a loop.  00007200
            cl=bl
            cr=br-1
20013 CONTINUE
      GO TO 20017
20015 IF (c(cl)(k:l).ge.c(partn)(k:l)) GO TO 20016
20017             cl=cl+1
      GO TO 20015
20016 GO TO 20020
20018 IF (c(cr)(k:l).le.c(partn)(k:l)) GO TO 20019
20020             cr=cr-1
      GO TO 20018
20019 IF (cl.gt.cr) GO TO 20014                                         00008200
               ctemp=c(cl)
               c(cl)=c(cr)
               c(cr)=ctemp
               if (partn.eq.cl) partn=cr
      GO TO 20013
20014 CONTINUE
c           Put sub-arrays on the stack if they're big enough.  Put the
c           larger under the smaller, so the smaller will be done next.
c           This makes the upper bound of the stack depth log2 (n-m+1).
c           (The "Hibbard" modification of quicksort).                  00009100
      IF(cl-bl .gt. br-cr)THEN
      IF(cl-bl.gt.10)THEN
                  stktop=stktop+1
                  stackl(stktop)=bl
                  stackr(stktop)=cr
      END IF
      IF(br-cr.gt.10)THEN
                  stktop=stktop+1
                  stackl(stktop)=cl
                  stackr(stktop)=br                                     00010100
      END IF
      ELSE
      IF(br-cr.gt.10)THEN
                  stktop=stktop+1
                  stackl(stktop)=cl
                  stackr(stktop)=br
      END IF
      IF(cl-bl.gt.10)THEN
                  stktop=stktop+1
                  stackl(stktop)=bl                                     00011100
                  stackr(stktop)=cr
      END IF
      END IF
      GO TO 20004
20005 CONTINUE
      END IF
c     Clean up small subfiles using insertion sort on everything.
      DO 20031 cr = m+1, n
         ctemp=c(cr)
         cl=cr                                                          00012000
20034 IF(c(cl-1)(k:l).gt.ctemp(k:l))THEN
            c(cl)=c(cl-1)
            cl=cl-1
      IF (cl.le.m) GO TO 20035
      GO TO 20034
      END IF
20035 CONTINUE
         c(cl)=ctemp
20031 CONTINUE
c                                                                       00012800
      return
c
      end        
