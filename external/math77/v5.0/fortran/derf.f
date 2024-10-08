      DOUBLE PRECISION FUNCTION DERF (X)
C>> 1995-10-24 DERF Krogh  Removed blanks in numbers for C conversion.
C>> 1994-10-19 DERF Krogh  Changes to use M77CON
C>> 1991-01-14 DERF CLL Changed to use generic names: abs and sign.
C>> 1990-11-29 CLL
c>> 1989-08-16 DERF   CLL  Changes for Cray
C>> 1989-06-30 DERF CLL More digits in sqrt(PI), S.P. & D.P. more alike.
C>> 1986-03-18 DERF   Lawson  Initial code.
c--D replaces "?": ?ERF, ?ERFC, ?ERFCE, ?INITS, ?CSEVL, ?ERM1
C                                                                       00001000
C     JULY 1977 EDITION. W.FULLERTON, C3,
C     LOS ALAMOS SCIENTIFIC LAB.
C
C     REORGANIZATION OF FULLERTON'S DERF & DERFC
C     C.L.LAWSON & S.CHAN, JUNE 2 ,1983, JPL.
c     When DERFC is called with 0.5 .lt. X .le. 1.0 the original code
c     computed erfc from erfce, computing erfce from Cody's rational
c     formula whose coeffs (PS() and QS()) are only given to 18 decimal
c     digits.  For Cray D.P. of 29D precision we can do better by
c     computing DERFC as 1 - erf in this interval, since the coeffs for 00002000
c     DERF are good up to 30D.
c     With this change DERFC will also be good up to 30 or 29D for all
c     X up to XMAX, where it would underflow.
c     We compute DERFCE only for X .ge. 0.  DERFCE is computed from
c     DERF in [0, 1] and from DERFC in [1, XMAX] and so is good to
c     about 30D there.  For X > XMAX we use the Cody rational
c     approximations for DERFCE which have coeffs good only to 18D.
c     Added variable EPS3.
C     ------------------------------------------------------------------
C-- Begin mask code changes                                             00003000
C     MACHINE DEPENDENT VALUES ARE SET ON THE FIRST ENTRY
C     TO THIS CODE. EXAMPLES OF THESE VALUES FOLLOW:
C
C     SYSTEM         NTERF   NTERFC   NTERC2   SQEPS   XBIG   XMAX
C     ------         -----   ------   ------   -----   ----   ----
c     IEEE   S.P.      7        9       10    .34E-3   4.01    9.18
c     IEEE   D.P.     12       25       24    .15E-7   6.01   26.53
C     UNIVAC S.P.      8       11       11    .12E-3   4.26   9.29
C     UNIVAC D.P.     14       30       27    .13E-8   6.40   26.58
c     Cray   S.P.     11       21       21    .12E-6   5.66   75.30     00004000
c     Cray   D.P.     19       55       45    .10E-13  8.04   74.89
C
c     The above values are derived from the following values:
C
C     SYSTEM         R1MACH(1)   R1MACH(3)   D1MACH(1)   D1MACH(3)
C     ------         ---------   ---------   ---------   ---------
c     IEEE           2**(-126)   2**(-24)    2**(-1022)  2**(-53)
c     VAX            2**(-128)   2**(-27)    2**(-128)   2**(-56)
c     UNIVAC         2**(-129)   2**(-27)    2**(-1025)  2**(-60)
C     Cray           2**(-8190)  2**(-47)    2**(-8100)  2**(-94)       00005000
C
C-- End mask code changes
C     ------------------------------------------------------------------
      integer NTERC2, NTERF, NTERFC
 
      double precision D1MACH, DCSEVL, DERFC, DERFCE
      double precision CUT2, CUT3, EIGHT
      double precision EPS3, ERFCS(21), ERFCCS(59), ERC2CS(49), ETA
      double precision FAC, FIVE, FOUR
      double precision HALF, ONE, PII, PL(6), PS(9), PSUM               00006000
      double precision QL(6), QS(10), QSUM
      double precision SQEPS, SQRTPI, THREE, TWO, U
      double precision X, XBIG, XMAX, Y, YSQ, ZERO
C
      SAVE EPS3,NTERF,NTERFC,NTERC2,SQEPS,XBIG,XMAX
C
      parameter(ZERO = 0.0D0, HALF = 0.5D0, ONE = 1.0D0)
      parameter(TWO = 2.0D0, THREE = 3.0D0)
      parameter(FOUR = 4.0D0, FIVE = 5.0D0, EIGHT = 8.0D0)
      parameter(SQRTPI = 1.77245385090551602729816748334114518D0)       00007000
C
C          PII = 1/sqrt(PI)
      parameter(PII = 0.56418958354775628694807945156077258D0)
C
C          FAC = 2/sqrt(PI)
      parameter(FAC = 1.12837916709551257389615890312154516D0)
C
C          CUT1 = 15 / 32 = 0.46875D0 is not used in the DP version.
c          CUT2 = 4.0
C          CUT3 = 10.**16 WILL BE SATISFACTORY FOR ALL                  00008000
C          MACHINES HAVING RELATIVE PRECISION LESS
C          THAN 32 DECIMAL PLACES AND OVERFLOW LIMIT
C          GREATER THAN 10.**32
      parameter(CUT2 = 4.0D0, CUT3 = 1.0D16)
c
      DATA NTERF / 0 /
C
C SERIES FOR ERF        ON THE INTERVAL  0.          TO  1.00000D+00
C                                        WITH WEIGHTED ERROR   1.28D-32
C                                         LOG WEIGHTED ERROR  31.89     00009000
C                               SIGNIFICANT FIGURES REQUIRED  31.05
C                                    DECIMAL PLACES REQUIRED  32.55
C
      DATA ERFCS(  1) / -.49046121234691808039984544033376D-1     /
      DATA ERFCS(  2) / -.14226120510371364237824741899631D+0     /
      DATA ERFCS(  3) / +.10035582187599795575754676712933D-1     /
      DATA ERFCS(  4) / -.57687646997674847650827025509167D-3     /
      DATA ERFCS(  5) / +.27419931252196061034422160791471D-4     /
      DATA ERFCS(  6) / -.11043175507344507604135381295905D-5     /
      DATA ERFCS(  7) / +.38488755420345036949961311498174D-7     /     00010000
      DATA ERFCS(  8) / -.11808582533875466969631751801581D-8     /
      DATA ERFCS(  9) / +.32334215826050909646402930953354D-10    /
      DATA ERFCS( 10) / -.79910159470045487581607374708595D-12    /
      DATA ERFCS( 11) / +.17990725113961455611967245486634D-13    /
      DATA ERFCS( 12) / -.37186354878186926382316828209493D-15    /
      DATA ERFCS( 13) / +.71035990037142529711689908394666D-17    /
      DATA ERFCS( 14) / -.12612455119155225832495424853333D-18    /
      DATA ERFCS( 15) / +.20916406941769294369170500266666D-20    /
      DATA ERFCS( 16) / -.32539731029314072982364160000000D-22    /
      DATA ERFCS( 17) / +.47668672097976748332373333333333D-24    /     00011000
      DATA ERFCS( 18) / -.65980120782851343155199999999999D-26    /
      DATA ERFCS( 19) / +.86550114699637626197333333333333D-28    /
      DATA ERFCS( 20) / -.10788925177498064213333333333333D-29    /
      DATA ERFCS( 21) / +.12811883993017002666666666666666D-31    /
C
C SERIES FOR ERC2       ON THE INTERVAL  2.50000D-01 TO  1.00000D+00
C                                        WITH WEIGHTED ERROR   2.67D-32
C                                         LOG WEIGHTED ERROR  31.57
C                               SIGNIFICANT FIGURES REQUIRED  30.31
C                                    DECIMAL PLACES REQUIRED  32.42     00012000
C
      DATA ERC2CS(  1) / -.6960134660230950112739150826197D-1      /
      DATA ERC2CS(  2) / -.4110133936262089348982212084666D-1      /
      DATA ERC2CS(  3) / +.3914495866689626881561143705244D-2      /
      DATA ERC2CS(  4) / -.4906395650548979161280935450774D-3      /
      DATA ERC2CS(  5) / +.7157479001377036380760894141825D-4      /
      DATA ERC2CS(  6) / -.1153071634131232833808232847912D-4      /
      DATA ERC2CS(  7) / +.1994670590201997635052314867709D-5      /
      DATA ERC2CS(  8) / -.3642666471599222873936118430711D-6      /
      DATA ERC2CS(  9) / +.6944372610005012589931277214633D-7      /    00013000
      DATA ERC2CS( 10) / -.1371220902104366019534605141210D-7      /
      DATA ERC2CS( 11) / +.2788389661007137131963860348087D-8      /
      DATA ERC2CS( 12) / -.5814164724331161551864791050316D-9      /
      DATA ERC2CS( 13) / +.1238920491752753181180168817950D-9      /
      DATA ERC2CS( 14) / -.2690639145306743432390424937889D-10     /
      DATA ERC2CS( 15) / +.5942614350847910982444709683840D-11     /
      DATA ERC2CS( 16) / -.1332386735758119579287754420570D-11     /
      DATA ERC2CS( 17) / +.3028046806177132017173697243304D-12     /
      DATA ERC2CS( 18) / -.6966648814941032588795867588954D-13     /
      DATA ERC2CS( 19) / +.1620854541053922969812893227628D-13     /    00014000
      DATA ERC2CS( 20) / -.3809934465250491999876913057729D-14     /
      DATA ERC2CS( 21) / +.9040487815978831149368971012975D-15     /
      DATA ERC2CS( 22) / -.2164006195089607347809812047003D-15     /
      DATA ERC2CS( 23) / +.5222102233995854984607980244172D-16     /
      DATA ERC2CS( 24) / -.1269729602364555336372415527780D-16     /
      DATA ERC2CS( 25) / +.3109145504276197583836227412951D-17     /
      DATA ERC2CS( 26) / -.7663762920320385524009566714811D-18     /
      DATA ERC2CS( 27) / +.1900819251362745202536929733290D-18     /
      DATA ERC2CS( 28) / -.4742207279069039545225655999965D-19     /
      DATA ERC2CS( 29) / +.1189649200076528382880683078451D-19     /    00015000
      DATA ERC2CS( 30) / -.3000035590325780256845271313066D-20     /
      DATA ERC2CS( 31) / +.7602993453043246173019385277098D-21     /
      DATA ERC2CS( 32) / -.1935909447606872881569811049130D-21     /
      DATA ERC2CS( 33) / +.4951399124773337881000042386773D-22     /
      DATA ERC2CS( 34) / -.1271807481336371879608621989888D-22     /
      DATA ERC2CS( 35) / +.3280049600469513043315841652053D-23     /
      DATA ERC2CS( 36) / -.8492320176822896568924792422399D-24     /
      DATA ERC2CS( 37) / +.2206917892807560223519879987199D-24     /
      DATA ERC2CS( 38) / -.5755617245696528498312819507199D-25     /
      DATA ERC2CS( 39) / +.1506191533639234250354144051199D-25     /    00016000
      DATA ERC2CS( 40) / -.3954502959018796953104285695999D-26     /
      DATA ERC2CS( 41) / +.1041529704151500979984645051733D-26     /
      DATA ERC2CS( 42) / -.2751487795278765079450178901333D-27     /
      DATA ERC2CS( 43) / +.7290058205497557408997703680000D-28     /
      DATA ERC2CS( 44) / -.1936939645915947804077501098666D-28     /
      DATA ERC2CS( 45) / +.5160357112051487298370054826666D-29     /
      DATA ERC2CS( 46) / -.1378419322193094099389644800000D-29     /
      DATA ERC2CS( 47) / +.3691326793107069042251093333333D-30     /
      DATA ERC2CS( 48) / -.9909389590624365420653226666666D-31     /
      DATA ERC2CS( 49) / +.2666491705195388413323946666666D-31     /    00017000
C
C SERIES FOR ERFC       ON THE INTERVAL  0.          TO  2.50000D-01
C                                        WITH WEIGHTED ERROR   1.53D-31
C                                         LOG WEIGHTED ERROR  30.82
C                               SIGNIFICANT FIGURES REQUIRED  29.47
C                                    DECIMAL PLACES REQUIRED  31.70
C
      DATA ERFCCS(  1) / +.715179310202924774503697709496D-1        /
      DATA ERFCCS(  2) / -.265324343376067157558893386681D-1        /
      DATA ERFCCS(  3) / +.171115397792085588332699194606D-2        /   00018000
      DATA ERFCCS(  4) / -.163751663458517884163746404749D-3        /
      DATA ERFCCS(  5) / +.198712935005520364995974806758D-4        /
      DATA ERFCCS(  6) / -.284371241276655508750175183152D-5        /
      DATA ERFCCS(  7) / +.460616130896313036969379968464D-6        /
      DATA ERFCCS(  8) / -.822775302587920842057766536366D-7        /
      DATA ERFCCS(  9) / +.159214187277090112989358340826D-7        /
      DATA ERFCCS( 10) / -.329507136225284321486631665072D-8        /
      DATA ERFCCS( 11) / +.722343976040055546581261153890D-9        /
      DATA ERFCCS( 12) / -.166485581339872959344695966886D-9        /
      DATA ERFCCS( 13) / +.401039258823766482077671768814D-10       /   00019000
      DATA ERFCCS( 14) / -.100481621442573113272170176283D-10       /
      DATA ERFCCS( 15) / +.260827591330033380859341009439D-11       /
      DATA ERFCCS( 16) / -.699111056040402486557697812476D-12       /
      DATA ERFCCS( 17) / +.192949233326170708624205749803D-12       /
      DATA ERFCCS( 18) / -.547013118875433106490125085271D-13       /
      DATA ERFCCS( 19) / +.158966330976269744839084032762D-13       /
      DATA ERFCCS( 20) / -.472689398019755483920369584290D-14       /
      DATA ERFCCS( 21) / +.143587337678498478672873997840D-14       /
      DATA ERFCCS( 22) / -.444951056181735839417250062829D-15       /
      DATA ERFCCS( 23) / +.140481088476823343737305537466D-15       /   00020000
      DATA ERFCCS( 24) / -.451381838776421089625963281623D-16       /
      DATA ERFCCS( 25) / +.147452154104513307787018713262D-16       /
      DATA ERFCCS( 26) / -.489262140694577615436841552532D-17       /
      DATA ERFCCS( 27) / +.164761214141064673895301522827D-17       /
      DATA ERFCCS( 28) / -.562681717632940809299928521323D-18       /
      DATA ERFCCS( 29) / +.194744338223207851429197867821D-18       /
      DATA ERFCCS( 30) / -.682630564294842072956664144723D-19       /
      DATA ERFCCS( 31) / +.242198888729864924018301125438D-19       /
      DATA ERFCCS( 32) / -.869341413350307042563800861857D-20       /
      DATA ERFCCS( 33) / +.315518034622808557122363401262D-20       /   00021000
      DATA ERFCCS( 34) / -.115737232404960874261239486742D-20       /
      DATA ERFCCS( 35) / +.428894716160565394623737097442D-21       /
      DATA ERFCCS( 36) / -.160503074205761685005737770964D-21       /
      DATA ERFCCS( 37) / +.606329875745380264495069923027D-22       /
      DATA ERFCCS( 38) / -.231140425169795849098840801367D-22       /
      DATA ERFCCS( 39) / +.888877854066188552554702955697D-23       /
      DATA ERFCCS( 40) / -.344726057665137652230718495566D-23       /
      DATA ERFCCS( 41) / +.134786546020696506827582774181D-23       /
      DATA ERFCCS( 42) / -.531179407112502173645873201807D-24       /
      DATA ERFCCS( 43) / +.210934105861978316828954734537D-24       /   00022000
      DATA ERFCCS( 44) / -.843836558792378911598133256738D-25       /
      DATA ERFCCS( 45) / +.339998252494520890627359576337D-25       /
      DATA ERFCCS( 46) / -.137945238807324209002238377110D-25       /
      DATA ERFCCS( 47) / +.563449031183325261513392634811D-26       /
      DATA ERFCCS( 48) / -.231649043447706544823427752700D-26       /
      DATA ERFCCS( 49) / +.958446284460181015263158381226D-27       /
      DATA ERFCCS( 50) / -.399072288033010972624224850193D-27       /
      DATA ERFCCS( 51) / +.167212922594447736017228709669D-27       /
      DATA ERFCCS( 52) / -.704599152276601385638803782587D-28       /
      DATA ERFCCS( 53) / +.297976840286420635412357989444D-28       /   00023000
      DATA ERFCCS( 54) / -.126252246646061929722422632994D-28       /
      DATA ERFCCS( 55) / +.539543870454248793985299653154D-29       /
      DATA ERFCCS( 56) / -.238099288253145918675346190062D-29       /
      DATA ERFCCS( 57) / +.109905283010276157359726683750D-29       /
      DATA ERFCCS( 58) / -.486771374164496572732518677435D-30       /
      DATA ERFCCS( 59) / +.152587726411035756763200828211D-30       /
C
      DATA PS/                                                          
     *  1.00000000000036828D+0,                                         
     *  1.87051017604560834D+0,                                         
     *  1.74642369370058320D+0,                                         
     *  1.02438464807598001D+0,                                         
     *  4.07413180167223764D-1,                                         
     *  1.11870870991098165D-1,                                         
     *  2.07045775788719818D-2,                                         
     *  2.37133372752999036D-3,                                         
     *  1.29992515945788642D-4/
C
      DATA QS/                                                          00024900
     *  1.00000000000000000D+0,                                         
     *  2.99888934314798253D+0,                                         
     *  4.13030795287321183D+0,                                         
     *  3.43830153103630866D+0,                                         
     *  1.91273588328781533D+0,                                         
     *  7.40352738163508723D-1,                                         
     *  2.00387662412610424D-1,                                         
     *  3.68131014202168126D-2,                                         
     *  4.20307996290648223D-3,                                         
     *  2.30405728794132537D-4/
C
      DATA PL/                                                          
     * -2.82094791773876911D-1,                                         
     * -6.88752606824337911D+0,                                         
     * -5.38632485753818598D+1,                                         
     * -1.54309751654255924D+2,                                         
     * -1.30749393763753716D+2,                                         
     * -6.98670450907125305D+0/
C
      DATA QL/                                                          
     *  1.00000000000000000D+0,                                         
     *  2.59156442057350244D+1,                                         
     *  2.26063711030173368D+2,                                         
     *  8.02050727433854173D+2,                                         
     *  1.09991209268230208D+3,                                         
     *  4.28227932949102556D+2/
C     ------------------------------------------------------------------
C
      IF (.NOT.(NTERF .EQ. 0)) GO TO 20002
      ASSIGN 20002 TO NPR001
      GO TO 30001
20002 Y = abs(X)
      IF (.NOT.(Y .LE. SQEPS)) GO TO 20004                              00028000
        U = FAC * X
      GO TO 20003
20004 IF (.NOT.(Y .LE. ONE)) GO TO 20005
      ASSIGN 20006 TO NPR002
      GO TO 30002
20006 GO TO 20003
20005 IF (.NOT.(Y .LT. XBIG)) GO TO 20007
      ASSIGN 20008 TO NPR003
      GO TO 30003
20008   U = sign( HALF+(HALF-U), X )
      GO TO 20003
20007   U = sign ( ONE,X )
20003 DERF = U
      RETURN                                                            00029100
C
C
c     ------------------------------------------------------------------
      ENTRY DERFC(X)
C
      IF (.NOT.(NTERF .EQ. 0)) GO TO 20009
      ASSIGN 20009 TO NPR001
      GO TO 30001
20009 IF (.NOT.(X .LE. -XBIG)) GO TO 20011
        U = TWO
      GO TO 20010                                                       00030000
20011   Y = abs(X)
      IF (.NOT.(Y .LE. SQEPS)) GO TO 20013
          U = HALF + (HALF - FAC*X)
      GO TO 20012
20013 IF (.NOT.(Y .LE. ONE)) GO TO 20014
      IF (.NOT.(X .LT. HALF)) GO TO 20016
C
C                       *** Here X is in [-1,-SQEPS] or [SQEPS,0.5]
C
      ASSIGN 20017 TO NPR002
      GO TO 30002
20017       U = HALF + (HALF-U)                                         00031000
      GO TO 20015
C
C            .           *** Here X is in [0.5,1.0]
c            .  Following test on 1.0D-19 added 8/89 to make better use
c            .  of 28 D precision available on a Cray.  The stored
c            .  approximation for erfce is only good to about 18 D.
c            .  On machines having precision greater than 18 D we get
c            .  a more accurate value for erfc by computing U = erf
c            .  and then HALF - (HALF - U).
c                                                                       00032000
20016 IF (.NOT.(EPS3 .gt. 1.0D-19)) GO TO 20019
      ASSIGN 20020 TO NPR004
      GO TO 30004
20020          U = EXP(-X**2) * U
      GO TO 20018
20019 ASSIGN 20021 TO NPR002
      GO TO 30002
20021          U = HALF + (HALF-U)
20018 CONTINUE
20015 GO TO 20012
20014 IF (.NOT.(Y .LE. XMAX)) GO TO 20022
      ASSIGN 20023 TO NPR003
      GO TO 30003
20023     IF (X .LT. ZERO) U = TWO - U                                  00033100
      GO TO 20012
20022     U = ZERO
          CALL DERM1('DERFC',1,0,'X SO BIG DERFC UNDERFLOWS',           
     *    'X',X,'.')
20012 CONTINUE
20010 DERFC = U
      RETURN
C
c     ------------------------------------------------------------------
      ENTRY DERFCE(X)
C                                                                       00034300
      IF (.NOT.(NTERF .EQ. 0)) GO TO 20024
      ASSIGN 20024 TO NPR001
      GO TO 30001
20024 Y = X
      IF (.NOT.(X .LT.ZERO)) GO TO 20026
        CALL DERM1('DERFCE',1,0,'X MUST BE .GE. ZERO',                  
     *             'X',X,'.')
        U = ZERO
      GO TO 20025
20026 IF (.NOT.(X .LT. ONE)) GO TO 20027
      ASSIGN 20028 TO NPR002
      GO TO 30002
20028   U = EXP(Y*Y)*(HALF + (HALF-U))
      GO TO 20025                                                       00035300
20027 IF (.NOT.(X .LT. XMAX)) GO TO 20029
      GO TO 30005
20030 GO TO 20025
20029 IF (.NOT.( X .lt. CUT2)) GO TO 20031
      ASSIGN 20032 TO NPR004
      GO TO 30004
20032 GO TO 20025
20031 IF (.NOT.(X .LT. CUT3)) GO TO 20033
      GO TO 30006
20034 GO TO 20025
20033   U = PII / X
20025 DERFCE = U
      RETURN
C                                                                       00036400
C
C     PROCEDURE (SET PARAMETERS)
C
30001 EPS3 = D1MACH(3)
      ETA = 0.1D0 * EPS3
      call DINITS (ERFCS,  21, ETA, NTERF )
      call DINITS (ERFCCS, 59, ETA, NTERFC)
      call DINITS (ERC2CS, 49, ETA, NTERC2)
      SQEPS = sqrt (TWO*EPS3)
      XBIG = sqrt (-log(SQRTPI*EPS3))                                   00037400
      XMAX = sqrt (-log(SQRTPI*D1MACH(1)) )
      XMAX = XMAX - HALF*log(XMAX)/XMAX - 0.01D0
C
c     PRINT*,'ETA = ',ETA
c     PRINT*,'NTERF = ',NTERF
c     PRINT*,'NTERFC = ',NTERFC
c     PRINT*,'NTERC2 = ',NTERC2
c     PRINT*,'SQEPS = ',SQEPS
c     PRINT*,'XBIG = ',XBIG
c     PRINT*,'XMAX = ',XMAX                                             00038400
C
      GO TO NPR001,(20002,20009,20024)
C
C
C     PROCEDURE (DERF(X) FOR ABS(X) .LE. 1)
30002   U = X + X * DCSEVL ( TWO * X * X - ONE, ERFCS, NTERF )
      GO TO NPR002,(20006,20017,20021,20028)
C
C
C     PROCEDURE (DERFC(X) FOR 1 .LT. X .LT. XMAX)                       00039400
C
C                       *** Here, Y = abs(X)
30003   YSQ = Y * Y
      IF(YSQ .LE. FOUR)THEN
        U = EXP(-YSQ) / Y *                                             
     *     (HALF+DCSEVL((EIGHT/YSQ-FIVE)/THREE,ERC2CS,NTERC2))
      ELSE
          U = EXP(-YSQ)/Y*(HALF + DCSEVL(EIGHT/YSQ-ONE, ERFCCS, NTERFC))
      END IF
      GO TO NPR003,(20008,20023)
C                                                                       00040500
C
C     PROCEDURE (DERFCE(X) FOR 1 .LT. X .LT. XMAX)
C
C                       *** Here, Y = abs(X)
30005   YSQ = Y * Y
      IF(YSQ .LE. FOUR)THEN
        U =  (ONE/X) *                                                  
     *     (HALF+DCSEVL((EIGHT/YSQ-FIVE)/THREE,ERC2CS,NTERC2))
      ELSE
          U = (ONE/X) * (HALF + DCSEVL(EIGHT/YSQ-1., ERFCCS, NTERFC))
      END IF                                                            00041600
      GO TO 20030
C
C
C     PROCEDURE (DERFCE(X) FOR 15/32 .LE. X .LE. 4)
30004   PSUM=PS(1)+Y*(PS(2)+Y*(PS(3)+Y*(PS(4)+Y*(PS(5)+Y*(PS(6)         
     *    +Y*(PS(7)+Y*(PS(8)+Y*PS(9))))))))
        QSUM=QS(1)+Y*(QS(2)+Y*(QS(3)+Y*(QS(4)+Y*(QS(5)+Y*(QS(6)         
     *    +Y*(QS(7)+Y*(QS(8)+Y*(QS(9)+Y*QS(10)))))))))
        U=PSUM/QSUM
      GO TO NPR004,(20020,20032)
C
C                                                                       00042800
C     PROCEDURE (DERFCE(X) FOR 4 .LE. X .LE. 10**16)
30006   YSQ=1.D0/Y/Y
        PSUM=PL(1)+YSQ*(PL(2)+YSQ*(PL(3)+YSQ*(PL(4)+YSQ*(PL(5)          
     *    +YSQ*PL(6)))))
        QSUM=QL(1)+YSQ*(QL(2)+YSQ*(QL(3)+YSQ*(QL(4)+YSQ*(QL(5)          
     *    +YSQ*QL(6)))))
        U=(PII+YSQ*PSUM/QSUM)/Y
      GO TO 20034
C
C
      END        
