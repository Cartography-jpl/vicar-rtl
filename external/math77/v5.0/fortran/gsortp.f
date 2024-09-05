      subroutine GSORTP (COMPAR, N, P)
C     .  Copyright (C) 1989, California Institute of Technology.
C     .  All rights reserved.  U. S. Government sponsorship under
C     .  NASA contract NAS7-918 is acknowledged.
C>> 1991-04-02  GSORTP  Snyder  Repair no permutation vector if m-n < 10
C>> 1988-11-22  GSORTP  Snyder  Initial code.
c
c     Sort an N-vector of objects of unknown type and organization.
c     P is set so that the P(J)'th element of the original sequence is
c     the J'th element of the sorted sequence.  The order is defined by 00001100
c     the integer function COMPAR.  An invocation COMPAR(I,J) should
c     return -1 if the I'th element of the data is to preceed the J'th
c     element in the sorted sequence, +1 if the J'th element is to
c     preceed the I'th element in the sorted sequence, and 0 if the I'th
c     and J'th elements are the same.
c
c     This subprogram is unaware of the data, and cannot manipulate it.
c     It is the caller's responsibility to make the data known to the
c     COMPAR function.
c                                                                       00002100
      integer COMPAR, N, P(*)
      external COMPAR
c
c     *****     Local Variables     ************************************
c
c BL      is the left bound of the sub-array to be sorted at the next
c         step.
c BR      is the right bound of the sub array to be sorted at the next
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
      integer BL,BR,CL,CR,PARTN,PTEMP                                   00004100
      integer STACKL(32),STACKR(32),STKTOP
c
c     *****     Executable Statements     ******************************
c
      DO 20002 cl = 1, n
         p(cl)=cl
20002 CONTINUE
      IF(n.ge.10)THEN
         stktop=1
         stackl(1)=1                                                    00005200
         stackr(1)=n
      GO TO 20009
20007 IF (stktop.eq.0) GO TO 20008
20009       bl=stackl(stktop)
            br=stackr(stktop)
            stktop=stktop-1
c           Choose a partitioning element.  Use the median of the first,
c           middle and last elements.  Sort them so the extreme elements
c           can serve as sentinels during partitioning.
            cl=(bl+br)/2                                                00006100
            partn=p(cl)
      IF(compar(p(bl),partn).gt.0)THEN
               p(cl)=p(bl)
               p(bl)=partn
               partn=p(cl)
      END IF
      IF(compar(p(bl),p(br)).gt.0)THEN
               ptemp=p(bl)
               p(bl)=p(br)
               p(br)=ptemp                                              00007100
      END IF
      IF(compar(partn,p(br)).gt.0)THEN
               p(cl)=p(br)
               p(br)=partn
               partn=p(cl)
      END IF
            p(cl)=p(br-1)
            p(br-1)=partn
c           Partition the sub-array around PARTN.  Exclude the above
c           considered elements from partitioning because they're al-   00008100
c           ready in the correct subfiles.  Stop scanning on equality to
c           prevent files containing equal values from causing a loop.
            cl=bl
            cr=br-1
20016 GO TO 20020
20018 IF (compar(p(cl),partn).ge.0) GO TO 20019
20020             cl=cl+1
      GO TO 20018
20019 GO TO 20023
20021 IF (compar(p(cr),partn).le.0) GO TO 20022                         00009000
20023             cr=cr-1
      GO TO 20021
20022 IF (cl.gt.cr) GO TO 20017
               ptemp=p(cl)
               p(cl)=p(cr)
               p(cr)=ptemp
      GO TO 20016
c           Put sub-arrays on the stack if they're big enough.  Put the
c           larger under the smaller, so the smaller will be done next.
c           This makes the upper bound of the stack depth log2 (n).     00010000
c           (The "Hibbard" modification of quicksort).
20017 IF(cl-bl .gt. br-cr)THEN
      IF(cl-bl.gt.10)THEN
                  stktop=stktop+1
                  stackl(stktop)=bl
                  stackr(stktop)=cr
      END IF
      IF(br-cr.gt.10)THEN
                  stktop=stktop+1
                  stackl(stktop)=cl                                     00011000
                  stackr(stktop)=br
      END IF
      ELSE
      IF(br-cr.gt.10)THEN
                  stktop=stktop+1
                  stackl(stktop)=cl
                  stackr(stktop)=br
      END IF
      IF(cl-bl.gt.10)THEN
                  stktop=stktop+1                                       00012000
                  stackl(stktop)=bl
                  stackr(stktop)=cr
      END IF
      END IF
      GO TO 20007
20008 CONTINUE
      END IF
c     Clean up small subfiles by using insertion sort on everything.
      DO 20034 cr = 2, n
         ptemp=p(cr)                                                    00012900
         cl=cr
20037 IF(compar(p(cl-1),ptemp).gt.0)THEN
            p(cl)=p(cl-1)
            cl=cl-1
      IF (cl.le.1) GO TO 20038
      GO TO 20037
      END IF
20038    p(cl)=ptemp
20034 CONTINUE
c                                                                       00013800
      return
c
      end        
