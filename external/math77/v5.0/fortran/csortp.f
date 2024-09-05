      subroutine CSORTP (C, M, N, K, L, P)
C     .  Copyright (C) 1989, California Institute of Technology.
C     .  All rights reserved.  U. S. Government sponsorship under
C     .  NASA contract NAS7-918 is acknowledged.
C>> 1994-11-14  CSORTP  Krogh   Declared all vars.
c>> 1992-11-23  CSORTP  Snyder  Add entry CSORTQ.
C>> 1991-04-02  CSORTP  Snyder  Repair no permutation vector if m-n < 10
c>> 1988-11-22  CSORTP  Snyder  Initial code.
c
c     Sort the M:N-vector of character strings C according to the (K:L) 00001100
c     substring of each element.  C is not disturbed.  P is set
c     so that C(P(J)) is the J'th element of the sorted sequence.
c     Enter at CSORTQ to use pre-specified permutation vector.
c
      integer M, N, K, L
      character*(*) C(*)
      integer P(*)
c
c     *****     Local Variables     ************************************
c                                                                       00002100
c BL      is the left bound of the sub-array to be sorted at the next
c         step.
c BR      is the right bound of the sub array to be sorted at the next
c         step.
c CL      is the current left bound of the unsorted sub-array.
c CR      is the current right bound of the unsorted sub-array.
c PARTN   is the partition element.
c PTEMP   holds elements of P during exchanges.
c STACKL  keeps track of the left bounds of sub-arrays waiting to be
c         sorted.                                                       00003100
c STACKR  keeps track of the right bounds of sub-arrays waiting to be
c         sorted.
c STKTOP  keeps track of the top of the stacks.
c
      integer BL,BR,CL,CR,PARTN,PTEMP
      integer STACKL(32),STACKR(32),STKTOP
c
c     *****     Executable Statements     ******************************
c
      DO 20002 cl = m, n                                                00004200
         p(cl)=cl
20002 CONTINUE
      entry CSORTQ (C, M, N, K, L, P)
      IF(n-m.ge.10)THEN
         stktop=1
         stackl(1)=m
         stackr(1)=n
      GO TO 20009
20007 IF (stktop.eq.0) GO TO 20008
20009       bl=stackl(stktop)
            br=stackr(stktop)                                           00005200
            stktop=stktop-1
            partn=p(bl)
c           Choose a partitioning element.  Use the median of the first,
c           middle and last elements.  Sort them so the extreme elements
c           can serve as sentinels during partitioning.
            cl=(bl+br)/2
            ptemp=p(cl)
      IF(c(p(bl))(k:l).gt.c(ptemp)(k:l))THEN
               p(cl)=p(bl)
               p(bl)=ptemp                                              00006200
               ptemp=p(cl)
      END IF
      IF(c(p(bl))(k:l).gt.c(p(br))(k:l))THEN
               cr=p(bl)
               p(bl)=p(br)
               p(br)=cr
      END IF
      IF(c(ptemp)(k:l).gt.c(p(br))(k:l))THEN
               p(cl)=p(br)
               p(br)=ptemp                                              00007200
               ptemp=p(cl)
      END IF
            p(cl)=p(br-1)
            p(br-1)=ptemp
            partn=p(cl)
c           Partition the sub-array around PARTN.  Exclude the above
c           considered elements from partitioning because they're al-
c           ready in the correct subfiles.  Stop scanning on equality to
c           prevent files containing equal values from causing a loop.
            cl=bl                                                       00008200
            cr=br-1
20016 CONTINUE
      GO TO 20020
20018 IF (c(p(cl))(k:l).ge.c(partn)(k:l)) GO TO 20019
20020             cl=cl+1
      GO TO 20018
20019 GO TO 20023
20021 IF (c(p(cr))(k:l).le.c(partn)(k:l)) GO TO 20022
20023             cr=cr-1
      GO TO 20021
20022 IF (cl.gt.cr) GO TO 20017
               ptemp=p(cl)                                              00009200
               p(cl)=p(cr)
               p(cr)=ptemp
      GO TO 20016
20017 CONTINUE
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
      GO TO 20007
20008 CONTINUE
      END IF
c     Clean up small subfiles using insertion sort on everything.
      DO 20034 cr = m+1, n
         ptemp=p(cr)
         cl=cr
20037 IF(c(p(cl-1))(k:l).gt.c(ptemp)(k:l))THEN
            p(cl)=p(cl-1)                                               00013000
            cl=cl-1
      IF (cl.le.m) GO TO 20038
      GO TO 20037
      END IF
20038 CONTINUE
         p(cl)=ptemp
20034 CONTINUE
c
      return
c                                                                       00013800
      end        
