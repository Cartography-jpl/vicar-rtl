      subroutine EXSORT (DATAOP,MAXX,LIST,OPTION,OUTFIL)
c>>   1990-02-07 EXSORT  W. V. Snyder at JPL, 91109, Convert to SFtran
      external DATAOP
      integer MAXX, LIST(*), OPTION, OUTFIL
c
c DATAOP is a user-coded subroutine used to perform all operations on
c     the data.  The operations include acquiring data from outside of
c     the EXSORT interface, manipulating scratch files and performing
c     input and output on scratch files, moving data from one memory
c     area to another, returning sorted data from the EXSORT interface
c     to the user program, and comparing one datum with another.        00001100
c
c     calling sequence for DATAOP:
c     call DATAOP (IOP,I1,I2,IFLAG)
c     where all arguments are integers,
c     IOP defines the operation to be performed,
c     I1 is usually an index (1-4) of a file upon which to operate,
c     I2 is usually an index (1-maxx) of the data area to use.
c     IFLAG is a flag to be set by DATAOP.
c     the values of IOP and the corresponding required actions are
c     detailed below.                                                   00002100
c
c IOP  ACTION
c
c 1   Place a datum from the set to be sorted in the record area indexed
c     by I2.  I1 is irrelevant.  Set the value of IFLAG to zero if a
c     datum is available.  Set the value of IFLAG to any non-zero value
c     if the entire data set has been provided by this avenue.
c
c 2   Write the datum in the record area indexed by I2 on the intermedi-
c     ate scratch file indexed by I1.  The value of IFLAG is irrelevant.00003100
c
c 3   Place an end-of-string mark (eof, unique datum, etc), to be
c     recognized during performance of operation 4 (below), on the
c     intermediate scratch file indexed by I1.  The values of I2 and
c     IFLAG are irrelevant.
c
c 4   Read a datum from the intermediate scratch file indexed by I1 into
c     the record area indexed by I2.  Set the value of IFLAG to zero if
c     a datum is available.  Set the value of IFLAG to any non-zero
c     value if an end-of-string mark created by operation 3 is detected.00004100
c
c 5   Rewind the intermediate scratch file indexed by I1.  The values of
c     I2 and IFLAG are irrelevant.
c
c 6   If I1 is zero, a datum from the sorted data set is in the record
c     area indexed by I2.  If I1 is non-zero, the entire sorted data set
c     has been provided by this avenue.  The value of IFLAG is irrele-
c     vant.
c
c 7   Move the datum in the record area indexed by I1 to the record area00005100
c     indexed by I2.  The value of IFLAG is irrelevant.
c
c 8   Compare the datum in the record area indexed by I1 to the datum in
c     the record area indexed by I2.  Set IFLAG to some negative value
c     if the datum in the record area indexed by I1 is to be sorted
c     before the datum in the record area indexed by I2;  set the value
c     of IFLAG to zero if the order of the records is immaterial; set
c     IFLAG to some positive number if the datum in the record area
c     indexed by I1 is to be sorted after the datum in the record area
c     indexed by I2.                                                    00006100
c
c MAXX is the number of record areas available.  The in-core sort will
c     use MAXX, MAXX-1 or MAXX-2 record areas, so MAXX must be at least
c     4.
c
c LIST is the array used by INSORT for pointers.  LIST must be at least
c     MAXX words long.
c
c OPTION specifies the action to take if the data are initially ordered
c     or at worst disordered in blocks of less than MAXX, but cannot be 00007100
c     entirely sorted in core.  If OPTION is zero and the data are init
c     ially ordered, the value of OUTFIL will be the index of the file
c     containing the ordered data.  If OPTION is zero but the data are
c     not initially ordered, the value of OUTFIL will be zero, and the
c     data will have been passed via DATAOP (6,I1,I2,IFLAG).  If OPTION
c     is non-zero, the data will always be passed via DATAOP (6,...),
c     and the value of OUTFIL will be irrelevant.
c
c     *****     External References     ********************************
c                                                                       00008100
c INSRTX  sorts a block of data in memory.  INSRTX is a special entry
c         in INSORT that allows using DATAOP instead of INSORT's usual
c         2-argument compare routine.
c PVEC    converts the list produced by INSRTX to a permutation vector.
c         This is done to allow a binary search in the sorted data set.
c
c
c     *****     Local Variables     ************************************
c
      integer DUM, END, HEAD, I, IFLAG, IN(4), IN1, IN2, IN3, IN4, J    00009100
      integer KODE, L, M, MAXDAT, MAXIND, MAXSTR, MININD, MINSTR, MX1
      integer N, NBS, NSTRNG(4), OUT(2), OUTAPE, OUT1, OUT2, SPLIT, TOP
      equivalence (IN(1),IN1), (IN(2),IN2), (IN(3),IN3), (IN(4),IN4)
      equivalence (OUT(1),OUT1), (OUT(2),OUT2)
c
c     *****     Executable Statements     ******************************
c
c     Initialize.
c
      outfil=0                                                          00010200
      n=0
      kode=2
      maxdat=maxx-1
      mx1=maxx+1
      nbs=0
      minstr=1
      maxstr=2
      minind=maxx
      maxind=maxx-1
c                                                                       00011200
c     Fill the user's data area and sort it.  If end-of-input does not
c     occur, write the data on a scratch file and do a merge later,
c     if necessary.
c
20002 IF (.NOT.(n.lt.maxdat)) GO TO 20005
            n=n+1
            call dataop (1,0,n,end)
      IF (end .eq. 0) GO TO 20002
            n=n-1
      IF (.NOT.(n.eq.0)) GO TO 20007                                    00012300
      IF (.NOT.(nbs.ne.0)) GO TO 20009
      IF (nbs.ne.1) GO TO 20003
c                 One block contained all the data.  Emit the data from
c                 memory, instead of reading it from scratch.
                  call dataop (5,1,0,dum)
      ASSIGN 20010 TO NPR001
      GO TO 30001
20010 CONTINUE
20009          call dataop (6,1,0,dum)
               return
20007 CONTINUE
20005    call insrtx (dataop,n,list,head)                               00013500
         nbs=nbs+1
         outape=minstr
      IF (.NOT.(nbs.eq.1)) GO TO 20012
      IF (.NOT.(end.ne.0)) GO TO 20014
      ASSIGN 20015 TO NPR001
      GO TO 30001
20015          call dataop (6,1,0,dum)
               return
20014 DO 20016 i=1,4
               call dataop (5,i,0,dum)
               nstrng(i)=0                                              00014600
20016 CONTINUE
            nstrng(1)=1
      ASSIGN 20019 TO NPR002
      GO TO 30002
20019 GO TO 20011
c
c           Another block has been sorted.  See if it will fit on an
c           existing string.
c
20012       iflag=-1
            if (nstrng(2).ne.0) call dataop (8,head,maxind,iflag)       00015600
      IF(iflag.lt.0)THEN
               call dataop (8,head,minind,iflag)
      ELSE
               outape=maxstr
      END IF
      IF (.NOT.(iflag .ge. 0)) GO TO 20023
c              The whole block fits after the last thing written.
      ASSIGN 20024 TO NPR002
      GO TO 30002
20024 GO TO 20022
c                                                                       00016600
c              The sorted string won't fit on an existing string.  Will
c              part of it fit?
c
20023          call pvec (list,head)
               call dataop (8,list(n),minind,iflag)
      IF(iflag .lt. 0)THEN
c
c                 None of the list will fit.  Handle the list similarly
c                 to the part that won't fit.
c                                                                       00017600
                  top=n
      ELSE
c
c                 Some of it will fit.  Find out how much.
c
                  i=1
                  j=n
20027 IF(j-i.gt.1)THEN
                     split=(j+i)/2
                     call dataop (8,list(split),minind,iflag)           00018600
      IF(iflag .ge. 0)THEN
                        j=split
      ELSE
                        i=split
      END IF
      GO TO 20027
      END IF
                  split=j
c
c                 Write the part that will fit on intermediate scratch. 00019500
c
      DO 20031 j=split,n
                     call dataop (2,minstr,list(j),dum)
20031 CONTINUE
                  call dataop (7,list(n),minind,dum)
                  top=split-1
      END IF
      IF(nstrng(2).ne.0)THEN
c
c                 Determine which intermediate scratch file to use for  00020500
c                 the part that won't fit.  The rule is to use the file
c                 with the least strings.  If the number of strings is
c                 the same, use the file with the maximum final datum.
c
      IF(nstrng(1).ne.nstrng(2))THEN
                     if (nstrng(outape).ge.nstrng(3-outape))            
     *                  outape=3-outape
      ELSE
                     outape=maxstr
      END IF
                  call dataop (3,outape,0,dum)                          00021600
      ELSE
c
c                 If we are writing the first string on file 2, we must
c                 decrease the available space for sorting.
c
                  outape=2
                  maxdat=maxdat-1
      END IF
               nstrng(outape)=nstrng(outape)+1
c                                                                       00022600
c              Write the part that won't fit on intermediate scratch.
c
      DO 20038 j=1,top
                  call dataop (2,outape,list(j),dum)
20038 CONTINUE
               top=list(top)
c
c              The sorted block has been written on intermediate
c              scratch.c
20022 CONTINUE                                                          00023700
c        Test END to see if we need to sort more.
20011 IF (end .ne. 0) GO TO 20003
         call dataop (7,top,mx1-outape,dum)
         n=0
      IF (.NOT.(nstrng(2).ne.0)) GO TO 20041
      GO TO 30003
20041 GO TO 20002
c
c     All of the data have been block-sorted.  Determine whether we
c     need to do a merge.                                               00024600
c
20003 call dataop (3,1,0,dum)
      call dataop (5,1,0,dum)
      IF(nstrng(2).eq.0)THEN
c
c        All of the data are on scratch 1.
c        See what the user wants to do.
c
      IF(option.eq.0)THEN
            outfil=1                                                    00025600
            return
      END IF
20046       call dataop (4,1,1,iflag)
      IF (iflag .ne. 0) GO TO 20047
            call dataop (6,0,1,dum)
      GO TO 20046
20047    call dataop (5,1,0,dum)
      ELSE
c
c        We must do a merge.  Set some values, and then check what      00026700
c        kind of output we do for this pass.
c
         call dataop (3,2,0,dum)
         call dataop (5,2,0,dum)
         in1=1
c        IN1 is to be the file with the most strings.
         if (nstrng(1).lt.nstrng(2)) in1=2
         in2=3-in1
         out1=3
         out2=4                                                         00027700
         m=2
20048 IF(nstrng(in1).ne.1)THEN
               i=in1
               in1=in2
               in2=i
      ELSE
               kode=6
               out1=0
      END IF
            outape=1                                                    00028800
c
c           Read one record from each file to start the merge.  Sort
c           these records.  Then do the merge by writing the lowest
c           record, reading a new record from the lowest file and
c           re-ordering the records with a partial in-core merge.
c
20052          call dataop (4,in1,1,iflag)
               call dataop (4,in2,2,iflag)
      IF(m .ne. 2)THEN
                  call dataop (4,in3,3,iflag)                           00029900
                  if (m .eq. 4) call dataop (4,in4,4,iflag)
      END IF
c              Sort set of first records from each file
               call insrtx (dataop,m,list,head)
c
c              Write current lowest record,and then read a new record
c              from the same file.
c
20056             call dataop (kode,out(outape),head,dum)
                  call dataop (4,in(head),head,end)                     00031000
                  i=list(head)
      IF(end .eq. 0)THEN
c                    if i=0, head is only remaining file
      IF(i.ne.0)THEN
                        call dataop (8,head,i,iflag)
      IF(iflag .gt. 0)THEN
c
c                          Head is no longer lowest.  Merge it with
c                          chain.
c                                                                       00032000
                           l=head
                           head=i
20064                         j=list(i)
      IF (j.eq.0) GO TO 20065
                              call dataop (8,l,j,iflag)
      IF (iflag .le. 0) GO TO 20065
                              i=j
      GO TO 20064
20065                      list(i)=l
                           list(l)=j                                    00033100
      END IF
      END IF
      ELSE
c
c                    A string has terminated.
c
                     l=in(head)
                     nstrng(l)=nstrng(l)-1
                     if (nstrng(l).eq.0) call dataop (5,l,0,dum)
      IF (i.eq.0) GO TO 20057                                           00034100
                     head=i
      END IF
      GO TO 20056
c
c              All strings have terminated.  If we are doing final
c              output we are done.
c
20057 IF (kode.eq.6) GO TO 20049
c
c              Determine whether to continue the current merge pass or  00035100
c              start a new one.
c
               l=out(outape)
               nstrng(l)=nstrng(l)+1
               call dataop (3,l,0,dum)
               j=nstrng(in1)+nstrng(in2)
      IF(j.eq.2)THEN
      IF(nstrng(out1).eq.1)THEN
c
c                    The total remaining input string count is 2.  The  00036100
c                    total output string count is 1 or 2.  We will do
c                    final output with a merge order of 3 or 4
c                    depending on whether the total output string count
c                    is 1 or 2.
c
                     in3=out1
                     m=4
                     call dataop (5,out1,0,dum)
      IF(nstrng(out2).ne.0)THEN
                        m=5                                             00037100
                        in4=out2
                        call dataop (5,out2,0,dum)
      END IF
                     kode=6
                     out1=0
                     outape=2
      END IF
      ELSEIF(nstrng(in1).eq.0)THEN
      GO TO 20053
      END IF                                                            00038100
               outape=3-outape
               m=max(m-1,2)
      GO TO 20052
c
c           We must start a new merge pass.  Swap input and output
c           files.  If the total remaining input string count is 1,
c           the merge order can be temporarily raised to 3.
c
20053       m=2
c           NSTRNG(IN2) is always .ge. NSTRNG(IN1).                     00039100
            call dataop (5,out1,0,dum)
      IF(nstrng(out2).ne.0)THEN
      IF(nstrng(in2).ne.0)THEN
                  m=3
                  in3=in2
      END IF
               call dataop (5,out2,0,dum)
               i=in2
               in2=out2
               out2=i                                                   00040100
      END IF
            i=in1
            in1=out1
            out1=i
      GO TO 20048
20049 CONTINUE
      END IF
c
      call dataop (6,1,0,dum)
      return                                                            00041000
c
c
C     procedure (determine MINSTR etc.)
30003    call dataop (8,minind,maxind,iflag)
      IF(iflag .ge. 0)THEN
            i=maxstr
            maxstr=minstr
            minstr=i
            i=maxind
            maxind=minind                                               00042000
            minind=i
      END IF
      GO TO 20041
c
c
C     procedure (output the block)
30002    m=head
20079 IF(m.ne.0)THEN
            call dataop (kode,outape,m,iflag)
            top=m                                                       00043000
            m=list(top)
      GO TO 20079
      END IF
      GO TO NPR002,(20019,20024,20081)
c
c
C     procedure (perform final output from memory)
30001    kode=6
         outape=0
      ASSIGN 20081 TO NPR002                                            00043900
      GO TO 30002
20081 GO TO NPR001,(20010,20015)
c
      end
