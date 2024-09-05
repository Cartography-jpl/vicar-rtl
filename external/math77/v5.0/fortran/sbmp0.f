      subroutine SBMP0 (X, AMPL, THETA)
C>> 1995-11-03 SBMP0 Krogh  Removed blanks in numbers for C conversion.
C>> 1994-11-11 SBMP0 Krogh   Declared all vars.
C>> 1994-10-20 SBMP0 Krogh  Changes to use M77CON
C>> 1991-01-14 SBMP0 CLL Changed to generic name SQRT
C>> 1990-11-29 SBMP0 CLL Changed subroutine name to SBMP0
C>> 1985-12-03 D9B0MP Lawson  Initial code.
C JULY 1977 EDITION.  W. FULLERTON, C3, LOS ALAMOS SCIENTIFIC LAB.
C C.L.LAWSON & S.CHAN, JPL, 1984 FEB ADAPTED TO JPL MATH77 LIBRARY.
C
C EVALUATE THE MODULUS AND PHASE FOR THE BESSEL J0 AND Y0 FUNCTIONS.
C     (13-JAN-83) : UTAH: MISSING ARGUMENT 37 INSERTED FOLLOWING
C                   37H.. IN LAST CALL TO XERROR
C     ------------------------------------------------------------------
c--S replaces "?": ?BMP0, ?INITS, ?CSEVL, ?ERM1, ?ERV1
C     ------------------------------------------------------------------
      integer NBM0, NBT02, NBM02, NBTH0
      real             X, AMPL, THETA, BM0CS(37), BT02CS(39), ETA
      real             BM02CS(40), BTH0CS(44), XMAX, PI4
      real             Z, R1MACH, SCSEVL
C
      SAVE ETA, NBM0, NBT02, NBM02, NBTH0, XMAX
C
C SERIES FOR BM0        ON THE INTERVAL  1.56250E-02 TO  6.25000E-02
C                                        WITH WEIGHTED ERROR   4.40E-32
C                                         LOG WEIGHTED ERROR  31.36
C                               SIGNIFICANT FIGURES REQUIRED  30.02
C                                    DECIMAL PLACES REQUIRED  32.14
C
      DATA BM0CS(  1) / +.9211656246827742712573767730182E-1      /
      DATA BM0CS(  2) / -.1050590997271905102480716371755E-2      /
      DATA BM0CS(  3) / +.1470159840768759754056392850952E-4      /
      DATA BM0CS(  4) / -.5058557606038554223347929327702E-6      /
      DATA BM0CS(  5) / +.2787254538632444176630356137881E-7      /
      DATA BM0CS(  6) / -.2062363611780914802618841018973E-8      /
      DATA BM0CS(  7) / +.1870214313138879675138172596261E-9      /
      DATA BM0CS(  8) / -.1969330971135636200241730777825E-10     /
      DATA BM0CS(  9) / +.2325973793999275444012508818052E-11     /
      DATA BM0CS( 10) / -.3009520344938250272851224734482E-12     /
      DATA BM0CS( 11) / +.4194521333850669181471206768646E-13     /
      DATA BM0CS( 12) / -.6219449312188445825973267429564E-14     /
      DATA BM0CS( 13) / +.9718260411336068469601765885269E-15     /
      DATA BM0CS( 14) / -.1588478585701075207366635966937E-15     /
      DATA BM0CS( 15) / +.2700072193671308890086217324458E-16     /
      DATA BM0CS( 16) / -.4750092365234008992477504786773E-17     /
      DATA BM0CS( 17) / +.8615128162604370873191703746560E-18     /
      DATA BM0CS( 18) / -.1605608686956144815745602703359E-18     /
      DATA BM0CS( 19) / +.3066513987314482975188539801599E-19     /
      DATA BM0CS( 20) / -.5987764223193956430696505617066E-20     /
      DATA BM0CS( 21) / +.1192971253748248306489069841066E-20     /
      DATA BM0CS( 22) / -.2420969142044805489484682581333E-21     /
      DATA BM0CS( 23) / +.4996751760510616453371002879999E-22     /
      DATA BM0CS( 24) / -.1047493639351158510095040511999E-22     /
      DATA BM0CS( 25) / +.2227786843797468101048183466666E-23     /
      DATA BM0CS( 26) / -.4801813239398162862370542933333E-24     /
      DATA BM0CS( 27) / +.1047962723470959956476996266666E-24     /
      DATA BM0CS( 28) / -.2313858165678615325101260800000E-25     /
      DATA BM0CS( 29) / +.5164823088462674211635199999999E-26     /
      DATA BM0CS( 30) / -.1164691191850065389525401599999E-26     /
      DATA BM0CS( 31) / +.2651788486043319282958336000000E-27     /
      DATA BM0CS( 32) / -.6092559503825728497691306666666E-28     /
      DATA BM0CS( 33) / +.1411804686144259308038826666666E-28     /
      DATA BM0CS( 34) / -.3298094961231737245750613333333E-29     /
      DATA BM0CS( 35) / +.7763931143074065031714133333333E-30     /
      DATA BM0CS( 36) / -.1841031343661458478421333333333E-30     /
      DATA BM0CS( 37) / +.4395880138594310737100799999999E-31     /
C
C SERIES FOR BTH0       ON THE INTERVAL  0.          TO  1.56250E-02
C                                        WITH WEIGHTED ERROR   2.66E-32
C                                         LOG WEIGHTED ERROR  31.57
C                               SIGNIFICANT FIGURES REQUIRED  30.67
C                                    DECIMAL PLACES REQUIRED  32.40
C
      DATA BTH0CS(  1) / -.24901780862128936717709793789967E+0     /
      DATA BTH0CS(  2) / +.48550299609623749241048615535485E-3     /
      DATA BTH0CS(  3) / -.54511837345017204950656273563505E-5     /
      DATA BTH0CS(  4) / +.13558673059405964054377445929903E-6     /
      DATA BTH0CS(  5) / -.55691398902227626227583218414920E-8     /
      DATA BTH0CS(  6) / +.32609031824994335304004205719468E-9     /
      DATA BTH0CS(  7) / -.24918807862461341125237903877993E-10    /
      DATA BTH0CS(  8) / +.23449377420882520554352413564891E-11    /
      DATA BTH0CS(  9) / -.26096534444310387762177574766136E-12    /
      DATA BTH0CS( 10) / +.33353140420097395105869955014923E-13    /
      DATA BTH0CS( 11) / -.47890000440572684646750770557409E-14    /
      DATA BTH0CS( 12) / +.75956178436192215972642568545248E-15    /
      DATA BTH0CS( 13) / -.13131556016891440382773397487633E-15    /
      DATA BTH0CS( 14) / +.24483618345240857495426820738355E-16    /
      DATA BTH0CS( 15) / -.48805729810618777683256761918331E-17    /
      DATA BTH0CS( 16) / +.10327285029786316149223756361204E-17    /
      DATA BTH0CS( 17) / -.23057633815057217157004744527025E-18    /
      DATA BTH0CS( 18) / +.54044443001892693993017108483765E-19    /
      DATA BTH0CS( 19) / -.13240695194366572724155032882385E-19    /
      DATA BTH0CS( 20) / +.33780795621371970203424792124722E-20    /
      DATA BTH0CS( 21) / -.89457629157111779003026926292299E-21    /
      DATA BTH0CS( 22) / +.24519906889219317090899908651405E-21    /
      DATA BTH0CS( 23) / -.69388422876866318680139933157657E-22    /
      DATA BTH0CS( 24) / +.20228278714890138392946303337791E-22    /
      DATA BTH0CS( 25) / -.60628500002335483105794195371764E-23    /
      DATA BTH0CS( 26) / +.18649748964037635381823788396270E-23    /
      DATA BTH0CS( 27) / -.58783732384849894560245036530867E-24    /
      DATA BTH0CS( 28) / +.18958591447999563485531179503513E-24    /
      DATA BTH0CS( 29) / -.62481979372258858959291620728565E-25    /
      DATA BTH0CS( 30) / +.21017901684551024686638633529074E-25    /
      DATA BTH0CS( 31) / -.72084300935209253690813933992446E-26    /
      DATA BTH0CS( 32) / +.25181363892474240867156405976746E-26    /
      DATA BTH0CS( 33) / -.89518042258785778806143945953643E-27    /
      DATA BTH0CS( 34) / +.32357237479762298533256235868587E-27    /
      DATA BTH0CS( 35) / -.11883010519855353657047144113796E-27    /
      DATA BTH0CS( 36) / +.44306286907358104820579231941731E-28    /
      DATA BTH0CS( 37) / -.16761009648834829495792010135681E-28    /
      DATA BTH0CS( 38) / +.64292946921207466972532393966088E-29    /
      DATA BTH0CS( 39) / -.24992261166978652421207213682763E-29    /
      DATA BTH0CS( 40) / +.98399794299521955672828260355318E-30    /
      DATA BTH0CS( 41) / -.39220375242408016397989131626158E-30    /
      DATA BTH0CS( 42) / +.15818107030056522138590618845692E-30    /
      DATA BTH0CS( 43) / -.64525506144890715944344098365426E-31    /
      DATA BTH0CS( 44) / +.26611111369199356137177018346367E-31    /
C
C SERIES FOR BM02       ON THE INTERVAL  0.          TO  1.56250E-02
C                                        WITH WEIGHTED ERROR   4.72E-32
C                                         LOG WEIGHTED ERROR  31.33
C                               SIGNIFICANT FIGURES REQUIRED  30.00
C                                    DECIMAL PLACES REQUIRED  32.13
C
      DATA BM02CS(  1) / +.9500415145228381369330861335560E-1      /
      DATA BM02CS(  2) / -.3801864682365670991748081566851E-3      /
      DATA BM02CS(  3) / +.2258339301031481192951829927224E-5      /
      DATA BM02CS(  4) / -.3895725802372228764730621412605E-7      /
      DATA BM02CS(  5) / +.1246886416512081697930990529725E-8      /
      DATA BM02CS(  6) / -.6065949022102503779803835058387E-10     /
      DATA BM02CS(  7) / +.4008461651421746991015275971045E-11     /
      DATA BM02CS(  8) / -.3350998183398094218467298794574E-12     /
      DATA BM02CS(  9) / +.3377119716517417367063264341996E-13     /
      DATA BM02CS( 10) / -.3964585901635012700569356295823E-14     /
      DATA BM02CS( 11) / +.5286111503883857217387939744735E-15     /
      DATA BM02CS( 12) / -.7852519083450852313654640243493E-16     /
      DATA BM02CS( 13) / +.1280300573386682201011634073449E-16     /
      DATA BM02CS( 14) / -.2263996296391429776287099244884E-17     /
      DATA BM02CS( 15) / +.4300496929656790388646410290477E-18     /
      DATA BM02CS( 16) / -.8705749805132587079747535451455E-19     /
      DATA BM02CS( 17) / +.1865862713962095141181442772050E-19     /
      DATA BM02CS( 18) / -.4210482486093065457345086972301E-20     /
      DATA BM02CS( 19) / +.9956676964228400991581627417842E-21     /
      DATA BM02CS( 20) / -.2457357442805313359605921478547E-21     /
      DATA BM02CS( 21) / +.6307692160762031568087353707059E-22     /
      DATA BM02CS( 22) / -.1678773691440740142693331172388E-22     /
      DATA BM02CS( 23) / +.4620259064673904433770878136087E-23     /
      DATA BM02CS( 24) / -.1311782266860308732237693402496E-23     /
      DATA BM02CS( 25) / +.3834087564116302827747922440276E-24     /
      DATA BM02CS( 26) / -.1151459324077741271072613293576E-24     /
      DATA BM02CS( 27) / +.3547210007523338523076971345213E-25     /
      DATA BM02CS( 28) / -.1119218385815004646264355942176E-25     /
      DATA BM02CS( 29) / +.3611879427629837831698404994257E-26     /
      DATA BM02CS( 30) / -.1190687765913333150092641762463E-26     /
      DATA BM02CS( 31) / +.4005094059403968131802476449536E-27     /
      DATA BM02CS( 32) / -.1373169422452212390595193916017E-27     /
      DATA BM02CS( 33) / +.4794199088742531585996491526437E-28     /
      DATA BM02CS( 34) / -.1702965627624109584006994476452E-28     /
      DATA BM02CS( 35) / +.6149512428936330071503575161324E-29     /
      DATA BM02CS( 36) / -.2255766896581828349944300237242E-29     /
      DATA BM02CS( 37) / +.8399707509294299486061658353200E-30     /
      DATA BM02CS( 38) / -.3172997595562602355567423936152E-30     /
      DATA BM02CS( 39) / +.1215205298881298554583333026514E-30     /
      DATA BM02CS( 40) / -.4715852749754438693013210568045E-31     /
C
C SERIES FOR BT02       ON THE INTERVAL  1.56250E-02 TO  6.25000E-02
C                                        WITH WEIGHTED ERROR   2.99E-32
C                                         LOG WEIGHTED ERROR  31.52
C                               SIGNIFICANT FIGURES REQUIRED  30.61
C                                    DECIMAL PLACES REQUIRED  32.32
C
      DATA BT02CS(  1) / -.24548295213424597462050467249324E+0     /
      DATA BT02CS(  2) / +.12544121039084615780785331778299E-2     /
      DATA BT02CS(  3) / -.31253950414871522854973446709571E-4     /
      DATA BT02CS(  4) / +.14709778249940831164453426969314E-5     /
      DATA BT02CS(  5) / -.99543488937950033643468850351158E-7     /
      DATA BT02CS(  6) / +.85493166733203041247578711397751E-8     /
      DATA BT02CS(  7) / -.86989759526554334557985512179192E-9     /
      DATA BT02CS(  8) / +.10052099533559791084540101082153E-9     /
      DATA BT02CS(  9) / -.12828230601708892903483623685544E-10    /
      DATA BT02CS( 10) / +.17731700781805131705655750451023E-11    /
      DATA BT02CS( 11) / -.26174574569485577488636284180925E-12    /
      DATA BT02CS( 12) / +.40828351389972059621966481221103E-13    /
      DATA BT02CS( 13) / -.66751668239742720054606749554261E-14    /
      DATA BT02CS( 14) / +.11365761393071629448392469549951E-14    /
      DATA BT02CS( 15) / -.20051189620647160250559266412117E-15    /
      DATA BT02CS( 16) / +.36497978794766269635720591464106E-16    /
      DATA BT02CS( 17) / -.68309637564582303169355843788800E-17    /
      DATA BT02CS( 18) / +.13107583145670756620057104267946E-17    /
      DATA BT02CS( 19) / -.25723363101850607778757130649599E-18    /
      DATA BT02CS( 20) / +.51521657441863959925267780949333E-19    /
      DATA BT02CS( 21) / -.10513017563758802637940741461333E-19    /
      DATA BT02CS( 22) / +.21820381991194813847301084501333E-20    /
      DATA BT02CS( 23) / -.46004701210362160577225905493333E-21    /
      DATA BT02CS( 24) / +.98407006925466818520953651199999E-22    /
      DATA BT02CS( 25) / -.21334038035728375844735986346666E-22    /
      DATA BT02CS( 26) / +.46831036423973365296066286933333E-23    /
      DATA BT02CS( 27) / -.10400213691985747236513382399999E-23    /
      DATA BT02CS( 28) / +.23349105677301510051777740800000E-24    /
      DATA BT02CS( 29) / -.52956825323318615788049749333333E-25    /
      DATA BT02CS( 30) / +.12126341952959756829196287999999E-25    /
      DATA BT02CS( 31) / -.28018897082289428760275626666666E-26    /
      DATA BT02CS( 32) / +.65292678987012873342593706666666E-27    /
      DATA BT02CS( 33) / -.15337980061873346427835733333333E-27    /
      DATA BT02CS( 34) / +.36305884306364536682359466666666E-28    /
      DATA BT02CS( 35) / -.86560755713629122479172266666666E-29    /
      DATA BT02CS( 36) / +.20779909972536284571238399999999E-29    /
      DATA BT02CS( 37) / -.50211170221417221674325333333333E-30    /
      DATA BT02CS( 38) / +.12208360279441714184191999999999E-30    /
      DATA BT02CS( 39) / -.29860056267039913454250666666666E-31    /
C
      DATA PI4 / 0.785398163397448309615660845819876E0 /
      DATA NBM0, NBT02, NBM02, NBTH0, XMAX / 4*0, 0.E0 /
C     ------------------------------------------------------------------
      IF (NBM0 .eq. 0) then
      ETA = 0.1E0 * R1MACH(3)
      call SINITS (BM0CS, 37, ETA,  NBM0 )
      call SINITS (BT02CS, 39, ETA, NBT02)
      call SINITS (BM02CS, 40, ETA, NBM02)
      call SINITS (BTH0CS, 44, ETA, NBTH0)
C
      XMAX = 0.04E0/R1MACH(4)
      endif
C
      IF (X.LT.4.E0) THEN
        AMPL = 0.E0
        THETA = 0.E0
        CALL SERM1 ('SBMP0',1,0,'X MUST BE .GE. 4','X',X,'.')
      END IF
C
      IF (X .le. 8.E0) then
      Z = (128.E0/(X*X) - 5.E0)/3.E0
      AMPL = (.75E0 + SCSEVL (Z, BM0CS, NBM0))/SQRT(X)
      THETA = X - PI4 + SCSEVL (Z, BT02CS, NBT02)/X
      RETURN
      endif
C
      IF (X.GT.XMAX) THEN
        AMPL = 0.E0
        THETA = 0.E0
        CALL SERM1 ('SBMP0',2,0,'NO PRECISION BECAUSE X .GT. XMAX',
     *              'X',X,',')
        CALL SERV1 ('XMAX',XMAX,'.')
      END IF
C
      Z = 128.E0/(X*X) - 1.E0
      AMPL = (.75E0 + SCSEVL (Z, BM02CS, NBM02))/SQRT(X)
      THETA = X - PI4 + SCSEVL (Z, BTH0CS, NBTH0)/X
      RETURN
C
      END
