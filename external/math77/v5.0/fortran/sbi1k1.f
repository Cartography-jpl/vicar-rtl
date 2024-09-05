      subroutine SBI1K1 (x, BI1, BK1, want, status)
C>> 1995-11-03 SBI1K1 Krogh  Removed blanks in numbers for C conversion.
C>> 1994-10-20 SBI1K1 Krogh  Changes to use M77CON
C>> 1994-04-19 SBI1K1 CLL Edited to make DP & SP files similar.
C>> 1990-09-25 SBI1K1 WV Snyder JPL Use Fullerton codes from CMLIB
C--S replaces "?": ?BI1K1, ?CSEVL, ?ERM1, ?INITS
c
c     Compute hyperbolic Bessel functions I1 and K1.
c     Approximations originally produced by Wayne Fullerton, LASL.
c
c     *****     Formal Arguments     ***********************************
c
c X [in] is the argument at which the functions are to be evaluated.
c BI1, BK1 [out] are the function values.
c WANT [integer,in] indicates the functions to be computed, and their
c                   scaling:
c     ABS(WANT) =
c        1 means compute I1(X)
c        2 means compute K1(X)
c        3 means compute both of I1(X) and K1(X)
c     WANT < 0 means compute EXP(-X)*I1(X) and/or EXP(X)*K1(X).
c     WANT=0 or ABS(WANT) > 3 causes an error message to be produced.
c STATUS [integer,out] indicates the outcome:
c        0 means normal computation,
c        1 means K1(X) is zero due to underflow,
c        < 0 means an error occurred:
c           -1 means WANT=0 or ABS(WANT)>3,
c           -2 means X was so big that I1(X) overflowed,
c           -3 means X was zero or negative and K1(X) is to be computed,
c           -4 means X was so small K1(X) overflows.
c     Negative values of STATUS are produced when the error message
c     processor is called with LEVEL=0; positive values of STATUS are
c     accompanied by LEVEL=-2.  See the description of the error message
c     handler for a description of the error level effects.
c     If status = -2 then BI1 = the largest representable number;
c     if status = -3 or -4 then BK1 = the largest representable number.
c     ------------------------------------------------------------------
      real             X, BI1, BK1
      integer WANT, STATUS
c
c     *****     External References     ********************************
c
      real             R1MACH, SCSEVL
      external SCSEVL, IERM1, SINITS, R1MACH, SERM1
c
c     *****     Local Variables     ************************************
c
      real             BI1CS(17), AI1CS(46), AI12CS(69)
      real              BK1CS(16), AK1CS(38), AK12CS(33)
      real             EXP10, EXPM10, LSQ2PI, LSQPI2
      parameter (EXP10 = (+.2202646579480671651695790E+5))
C     EXP10 = EXP(10)
      parameter (EXPM10 = (+.4539992976248485153559152E-4))
C     EXPM10 = EXP(-10)
      parameter (LSQ2PI = (+.9189385332046727417803296E+0))
C     LSQ2PI = LOG(SQRT (2 PI))
      parameter (LSQPI2 = (+.2257913526447274323630975E+0))
C     LSQPI2 = LOG(SQRT(PI / 2))
      real             I1NS, XIN, XKN, Y, Z
      real             BOUNDK, XIMAX, XIMIN, XISML, XKMAX, XKMIN, XKSML
      integer NTI1, NTAI1, NTAI12, NTK1, NTAK1, NTAK12
      save NTI1, NTAI1, NTAI12, XIMAX, XIMIN, XISML
      save BOUNDK, NTK1, NTAK1, NTAK12, XKMAX, XKMIN, XKSML
c
c     *****     Data     ***********************************************
c
C SERIES FOR BI1        ON THE INTERVAL  0.          TO  9.00000E+00
C                                        WITH WEIGHTED ERROR   1.44E-32
C                                         LOG WEIGHTED ERROR  31.84
C                               SIGNIFICANT FIGURES REQUIRED  31.45
C                                    DECIMAL PLACES REQUIRED  32.46
C
      DATA BI1CS(  1) / -.19717132610998597316138503218149E-2     /
      DATA BI1CS(  2) / +.40734887667546480608155393652014E+0     /
      DATA BI1CS(  3) / +.34838994299959455866245037783787E-1     /
      DATA BI1CS(  4) / +.15453945563001236038598401058489E-2     /
      DATA BI1CS(  5) / +.41888521098377784129458832004120E-4     /
      DATA BI1CS(  6) / +.76490267648362114741959703966069E-6     /
      DATA BI1CS(  7) / +.10042493924741178689179808037238E-7     /
      DATA BI1CS(  8) / +.99322077919238106481371298054863E-10    /
      DATA BI1CS(  9) / +.76638017918447637275200171681349E-12    /
      DATA BI1CS( 10) / +.47414189238167394980388091948160E-14    /
      DATA BI1CS( 11) / +.24041144040745181799863172032000E-16    /
      DATA BI1CS( 12) / +.10171505007093713649121100799999E-18    /
      DATA BI1CS( 13) / +.36450935657866949458491733333333E-21    /
      DATA BI1CS( 14) / +.11205749502562039344810666666666E-23    /
      DATA BI1CS( 15) / +.29875441934468088832000000000000E-26    /
      DATA BI1CS( 16) / +.69732310939194709333333333333333E-29    /
      DATA BI1CS( 17) / +.14367948220620800000000000000000E-31    /
C
C SERIES FOR AI1        ON THE INTERVAL  1.25000E-01 TO  3.33333E-01
C                                        WITH WEIGHTED ERROR   2.81E-32
C                                         LOG WEIGHTED ERROR  31.55
C                               SIGNIFICANT FIGURES REQUIRED  29.93
C                                    DECIMAL PLACES REQUIRED  32.38
C
      DATA AI1CS(  1) / -.2846744181881478674100372468307E-1      /
      DATA AI1CS(  2) / -.1922953231443220651044448774979E-1      /
      DATA AI1CS(  3) / -.6115185857943788982256249917785E-3      /
      DATA AI1CS(  4) / -.2069971253350227708882823777979E-4      /
      DATA AI1CS(  5) / +.8585619145810725565536944673138E-5      /
      DATA AI1CS(  6) / +.1049498246711590862517453997860E-5      /
      DATA AI1CS(  7) / -.2918338918447902202093432326697E-6      /
      DATA AI1CS(  8) / -.1559378146631739000160680969077E-7      /
      DATA AI1CS(  9) / +.1318012367144944705525302873909E-7      /
      DATA AI1CS( 10) / -.1448423418183078317639134467815E-8      /
      DATA AI1CS( 11) / -.2908512243993142094825040993010E-9      /
      DATA AI1CS( 12) / +.1266388917875382387311159690403E-9      /
      DATA AI1CS( 13) / -.1664947772919220670624178398580E-10     /
      DATA AI1CS( 14) / -.1666653644609432976095937154999E-11     /
      DATA AI1CS( 15) / +.1242602414290768265232168472017E-11     /
      DATA AI1CS( 16) / -.2731549379672432397251461428633E-12     /
      DATA AI1CS( 17) / +.2023947881645803780700262688981E-13     /
      DATA AI1CS( 18) / +.7307950018116883636198698126123E-14     /
      DATA AI1CS( 19) / -.3332905634404674943813778617133E-14     /
      DATA AI1CS( 20) / +.7175346558512953743542254665670E-15     /
      DATA AI1CS( 21) / -.6982530324796256355850629223656E-16     /
      DATA AI1CS( 22) / -.1299944201562760760060446080587E-16     /
      DATA AI1CS( 23) / +.8120942864242798892054678342860E-17     /
      DATA AI1CS( 24) / -.2194016207410736898156266643783E-17     /
      DATA AI1CS( 25) / +.3630516170029654848279860932334E-18     /
      DATA AI1CS( 26) / -.1695139772439104166306866790399E-19     /
      DATA AI1CS( 27) / -.1288184829897907807116882538222E-19     /
      DATA AI1CS( 28) / +.5694428604967052780109991073109E-20     /
      DATA AI1CS( 29) / -.1459597009090480056545509900287E-20     /
      DATA AI1CS( 30) / +.2514546010675717314084691334485E-21     /
      DATA AI1CS( 31) / -.1844758883139124818160400029013E-22     /
      DATA AI1CS( 32) / -.6339760596227948641928609791999E-23     /
      DATA AI1CS( 33) / +.3461441102031011111108146626560E-23     /
      DATA AI1CS( 34) / -.1017062335371393547596541023573E-23     /
      DATA AI1CS( 35) / +.2149877147090431445962500778666E-24     /
      DATA AI1CS( 36) / -.3045252425238676401746206173866E-25     /
      DATA AI1CS( 37) / +.5238082144721285982177634986666E-27     /
      DATA AI1CS( 38) / +.1443583107089382446416789503999E-26     /
      DATA AI1CS( 39) / -.6121302074890042733200670719999E-27     /
      DATA AI1CS( 40) / +.1700011117467818418349189802666E-27     /
      DATA AI1CS( 41) / -.3596589107984244158535215786666E-28     /
      DATA AI1CS( 42) / +.5448178578948418576650513066666E-29     /
      DATA AI1CS( 43) / -.2731831789689084989162564266666E-30     /
      DATA AI1CS( 44) / -.1858905021708600715771903999999E-30     /
      DATA AI1CS( 45) / +.9212682974513933441127765333333E-31     /
      DATA AI1CS( 46) / -.2813835155653561106370833066666E-31     /
C
C SERIES FOR AI12       ON THE INTERVAL  0.          TO  1.25000E-01
C                                        WITH WEIGHTED ERROR   1.83E-32
C                                         LOG WEIGHTED ERROR  31.74
C                               SIGNIFICANT FIGURES REQUIRED  29.97
C                                    DECIMAL PLACES REQUIRED  32.66
C
      DATA AI12CS(  1) / +.2857623501828012047449845948469E-1      /
      DATA AI12CS(  2) / -.9761097491361468407765164457302E-2      /
      DATA AI12CS(  3) / -.1105889387626237162912569212775E-3      /
      DATA AI12CS(  4) / -.3882564808877690393456544776274E-5      /
      DATA AI12CS(  5) / -.2512236237870208925294520022121E-6      /
      DATA AI12CS(  6) / -.2631468846889519506837052365232E-7      /
      DATA AI12CS(  7) / -.3835380385964237022045006787968E-8      /
      DATA AI12CS(  8) / -.5589743462196583806868112522229E-9      /
      DATA AI12CS(  9) / -.1897495812350541234498925033238E-10     /
      DATA AI12CS( 10) / +.3252603583015488238555080679949E-10     /
      DATA AI12CS( 11) / +.1412580743661378133163366332846E-10     /
      DATA AI12CS( 12) / +.2035628544147089507224526136840E-11     /
      DATA AI12CS( 13) / -.7198551776245908512092589890446E-12     /
      DATA AI12CS( 14) / -.4083551111092197318228499639691E-12     /
      DATA AI12CS( 15) / -.2101541842772664313019845727462E-13     /
      DATA AI12CS( 16) / +.4272440016711951354297788336997E-13     /
      DATA AI12CS( 17) / +.1042027698412880276417414499948E-13     /
      DATA AI12CS( 18) / -.3814403072437007804767072535396E-14     /
      DATA AI12CS( 19) / -.1880354775510782448512734533963E-14     /
      DATA AI12CS( 20) / +.3308202310920928282731903352405E-15     /
      DATA AI12CS( 21) / +.2962628997645950139068546542052E-15     /
      DATA AI12CS( 22) / -.3209525921993423958778373532887E-16     /
      DATA AI12CS( 23) / -.4650305368489358325571282818979E-16     /
      DATA AI12CS( 24) / +.4414348323071707949946113759641E-17     /
      DATA AI12CS( 25) / +.7517296310842104805425458080295E-17     /
      DATA AI12CS( 26) / -.9314178867326883375684847845157E-18     /
      DATA AI12CS( 27) / -.1242193275194890956116784488697E-17     /
      DATA AI12CS( 28) / +.2414276719454848469005153902176E-18     /
      DATA AI12CS( 29) / +.2026944384053285178971922860692E-18     /
      DATA AI12CS( 30) / -.6394267188269097787043919886811E-19     /
      DATA AI12CS( 31) / -.3049812452373095896084884503571E-19     /
      DATA AI12CS( 32) / +.1612841851651480225134622307691E-19     /
      DATA AI12CS( 33) / +.3560913964309925054510270904620E-20     /
      DATA AI12CS( 34) / -.3752017947936439079666828003246E-20     /
      DATA AI12CS( 35) / -.5787037427074799345951982310741E-22     /
      DATA AI12CS( 36) / +.7759997511648161961982369632092E-21     /
      DATA AI12CS( 37) / -.1452790897202233394064459874085E-21     /
      DATA AI12CS( 38) / -.1318225286739036702121922753374E-21     /
      DATA AI12CS( 39) / +.6116654862903070701879991331717E-22     /
      DATA AI12CS( 40) / +.1376279762427126427730243383634E-22     /
      DATA AI12CS( 41) / -.1690837689959347884919839382306E-22     /
      DATA AI12CS( 42) / +.1430596088595433153987201085385E-23     /
      DATA AI12CS( 43) / +.3409557828090594020405367729902E-23     /
      DATA AI12CS( 44) / -.1309457666270760227845738726E-23     /
      DATA AI12CS( 45) / -.3940706411240257436093521417557E-24     /
      DATA AI12CS( 46) / +.4277137426980876580806166797352E-24     /
      DATA AI12CS( 47) / -.4424634830982606881900283123029E-25     /
      DATA AI12CS( 48) / -.8734113196230714972115309788747E-25     /
      DATA AI12CS( 49) / +.4045401335683533392143404142428E-25     /
      DATA AI12CS( 50) / +.7067100658094689465651607717806E-26     /
      DATA AI12CS( 51) / -.1249463344565105223002864518605E-25     /
      DATA AI12CS( 52) / +.2867392244403437032979483391426E-26     /
      DATA AI12CS( 53) / +.2044292892504292670281779574210E-26     /
      DATA AI12CS( 54) / -.1518636633820462568371346802911E-26     /
      DATA AI12CS( 55) / +.8110181098187575886132279107037E-28     /
      DATA AI12CS( 56) / +.3580379354773586091127173703270E-27     /
      DATA AI12CS( 57) / -.1692929018927902509593057175448E-27     /
      DATA AI12CS( 58) / -.2222902499702427639067758527774E-28     /
      DATA AI12CS( 59) / +.5424535127145969655048600401128E-28     /
      DATA AI12CS( 60) / -.1787068401578018688764912993304E-28     /
      DATA AI12CS( 61) / -.6565479068722814938823929437880E-29     /
      DATA AI12CS( 62) / +.7807013165061145280922067706839E-29     /
      DATA AI12CS( 63) / -.1816595260668979717379333152221E-29     /
      DATA AI12CS( 64) / -.1287704952660084820376875598959E-29     /
      DATA AI12CS( 65) / +.1114548172988164547413709273694E-29     /
      DATA AI12CS( 66) / -.1808343145039336939159368876687E-30     /
      DATA AI12CS( 67) / -.2231677718203771952232448228939E-30     /
      DATA AI12CS( 68) / +.1619029596080341510617909803614E-30     /
      DATA AI12CS( 69) / -.1834079908804941413901308439210E-31     /
c
C
C SERIES FOR  BK1         ON THE INTERVAL  0.          TO  4.00000E+00
C                                        WITH WEIGHTED ERROR   9.16E-32
C                                         LOG WEIGHTED ERROR  31.04
C                               SIGNIFICANT FIGURES REQUIRED  30.61
C                                    DECIMAL PLACES REQUIRED  31.64
C
      DATA  BK1CS(  1) / +.25300227338947770532531120868533E-1     /
      DATA  BK1CS(  2) / -.35315596077654487566723831691801E+0     /
      DATA  BK1CS(  3) / -.12261118082265714823479067930042E+0     /
      DATA  BK1CS(  4) / -.69757238596398643501812920296083E-2     /
      DATA  BK1CS(  5) / -.17302889575130520630176507368979E-3     /
      DATA  BK1CS(  6) / -.24334061415659682349600735030164E-5     /
      DATA  BK1CS(  7) / -.22133876307347258558315252545126E-7     /
      DATA  BK1CS(  8) / -.14114883926335277610958330212608E-9     /
      DATA  BK1CS(  9) / -.66669016941993290060853751264373E-12    /
      DATA  BK1CS( 10) / -.24274498505193659339263196864853E-14    /
      DATA  BK1CS( 11) / -.70238634793862875971783797120000E-17    /
      DATA  BK1CS( 12) / -.16543275155100994675491029333333E-19    /
      DATA  BK1CS( 13) / -.32338347459944491991893333333333E-22    /
      DATA  BK1CS( 14) / -.53312750529265274999466666666666E-25    /
      DATA  BK1CS( 15) / -.75130407162157226666666666666666E-28    /
      DATA  BK1CS( 16) / -.91550857176541866666666666666666E-31    /
C
C SERIES FOR AK1        ON THE INTERVAL  1.25000E-01 TO  5.00000E-01
C                                        WITH WEIGHTED ERROR   3.07E-32
C                                         LOG WEIGHTED ERROR  31.51
C                               SIGNIFICANT FIGURES REQUIRED  30.71
C                                    DECIMAL PLACES REQUIRED  32.30
C
      DATA AK1CS(  1) / +.27443134069738829695257666227266E+0     /
      DATA AK1CS(  2) / +.75719899531993678170892378149290E-1     /
      DATA AK1CS(  3) / -.14410515564754061229853116175625E-2     /
      DATA AK1CS(  4) / +.66501169551257479394251385477036E-4     /
      DATA AK1CS(  5) / -.43699847095201407660580845089167E-5     /
      DATA AK1CS(  6) / +.35402774997630526799417139008534E-6     /
      DATA AK1CS(  7) / -.33111637792932920208982688245704E-7     /
      DATA AK1CS(  8) / +.34459775819010534532311499770992E-8     /
      DATA AK1CS(  9) / -.38989323474754271048981937492758E-9     /
      DATA AK1CS( 10) / +.47208197504658356400947449339005E-10    /
      DATA AK1CS( 11) / -.60478356628753562345373591562890E-11    /
      DATA AK1CS( 12) / +.81284948748658747888193837985663E-12    /
      DATA AK1CS( 13) / -.11386945747147891428923915951042E-12    /
      DATA AK1CS( 14) / +.16540358408462282325972948205090E-13    /
      DATA AK1CS( 15) / -.24809025677068848221516010440533E-14    /
      DATA AK1CS( 16) / +.38292378907024096948429227299157E-15    /
      DATA AK1CS( 17) / -.60647341040012418187768210377386E-16    /
      DATA AK1CS( 18) / +.98324256232648616038194004650666E-17    /
      DATA AK1CS( 19) / -.16284168738284380035666620115626E-17    /
      DATA AK1CS( 20) / +.27501536496752623718284120337066E-18    /
      DATA AK1CS( 21) / -.47289666463953250924281069568000E-19    /
      DATA AK1CS( 22) / +.82681500028109932722392050346666E-20    /
      DATA AK1CS( 23) / -.14681405136624956337193964885333E-20    /
      DATA AK1CS( 24) / +.26447639269208245978085894826666E-21    /
      DATA AK1CS( 25) / -.48290157564856387897969868800000E-22    /
      DATA AK1CS( 26) / +.89293020743610130180656332799999E-23    /
      DATA AK1CS( 27) / -.16708397168972517176997751466666E-23    /
      DATA AK1CS( 28) / +.31616456034040694931368618666666E-24    /
      DATA AK1CS( 29) / -.60462055312274989106506410666666E-25    /
      DATA AK1CS( 30) / +.11678798942042732700718421333333E-25    /
      DATA AK1CS( 31) / -.22773741582653996232867840000000E-26    /
      DATA AK1CS( 32) / +.44811097300773675795305813333333E-27    /
      DATA AK1CS( 33) / -.88932884769020194062336000000000E-28    /
      DATA AK1CS( 34) / +.17794680018850275131392000000000E-28    /
      DATA AK1CS( 35) / -.35884555967329095821994666666666E-29    /
      DATA AK1CS( 36) / +.72906290492694257991679999999999E-30    /
      DATA AK1CS( 37) / -.14918449845546227073024000000000E-30    /
      DATA AK1CS( 38) / +.30736573872934276300799999999999E-31    /
C
C SERIES FOR AK12       ON THE INTERVAL  0.          TO  1.25000E-01
C                                        WITH WEIGHTED ERROR   2.41E-32
C                                         LOG WEIGHTED ERROR  31.62
C                               SIGNIFICANT FIGURES REQUIRED  30.25
C                                    DECIMAL PLACES REQUIRED  32.38
C
      DATA AK12CS(  1) / +.6379308343739001036600488534102E-1      /
      DATA AK12CS(  2) / +.2832887813049720935835030284708E-1      /
      DATA AK12CS(  3) / -.2475370673905250345414545566732E-3      /
      DATA AK12CS(  4) / +.5771972451607248820470976625763E-5      /
      DATA AK12CS(  5) / -.2068939219536548302745533196552E-6      /
      DATA AK12CS(  6) / +.9739983441381804180309213097887E-8      /
      DATA AK12CS(  7) / -.5585336140380624984688895511129E-9      /
      DATA AK12CS(  8) / +.3732996634046185240221212854731E-10     /
      DATA AK12CS(  9) / -.2825051961023225445135065754928E-11     /
      DATA AK12CS( 10) / +.2372019002484144173643496955486E-12     /
      DATA AK12CS( 11) / -.2176677387991753979268301667938E-13     /
      DATA AK12CS( 12) / +.2157914161616032453939562689706E-14     /
      DATA AK12CS( 13) / -.2290196930718269275991551338154E-15     /
      DATA AK12CS( 14) / +.2582885729823274961919939565226E-16     /
      DATA AK12CS( 15) / -.3076752641268463187621098173440E-17     /
      DATA AK12CS( 16) / +.3851487721280491597094896844799E-18     /
      DATA AK12CS( 17) / -.5044794897641528977117282508800E-19     /
      DATA AK12CS( 18) / +.6888673850418544237018292223999E-20     /
      DATA AK12CS( 19) / -.9775041541950118303002132480000E-21     /
      DATA AK12CS( 20) / +.1437416218523836461001659733333E-21     /
      DATA AK12CS( 21) / -.2185059497344347373499733333333E-22     /
      DATA AK12CS( 22) / +.3426245621809220631645388800000E-23     /
      DATA AK12CS( 23) / -.5531064394246408232501248000000E-24     /
      DATA AK12CS( 24) / +.9176601505685995403782826666666E-25     /
      DATA AK12CS( 25) / -.1562287203618024911448746666666E-25     /
      DATA AK12CS( 26) / +.2725419375484333132349439999999E-26     /
      DATA AK12CS( 27) / -.4865674910074827992378026666666E-27     /
      DATA AK12CS( 28) / +.8879388552723502587357866666666E-28     /
      DATA AK12CS( 29) / -.1654585918039257548936533333333E-28     /
      DATA AK12CS( 30) / +.3145111321357848674303999999999E-29     /
      DATA AK12CS( 31) / -.6092998312193127612416000000000E-30     /
      DATA AK12CS( 32) / +.1202021939369815834623999999999E-30     /
      DATA AK12CS( 33) / -.2412930801459408841386666666666E-31     /
c
      data NTI1 /0/
c
c     *****     Statement Functions     ********************************
c
      xin(z) = ximax + 0.5*log(z/(1.0-3.0/(8.0*z))**2)
      xkn(z) = xkmax - 0.5*log(z/(1.0+3.0/(8.0*z))**2)
c
c     *****     Executable Statements     ******************************
c
      if (nti1 .eq. 0) then
         z = 0.1*R1MACH(3)
         boundk = 1.693 - 6.965E-3 * log(z)
         call SINITS (bi1cs, 17, z,   nti1  )
         call SINITS (ai1cs, 46, z, ntai1 )
         call SINITS (ai12cs, 69, z, ntai12)
         call SINITS (bk1cs, 16, z, ntk1  )
         call SINITS (ak1cs, 38, z, ntak1 )
         call SINITS (ak12cs, 33, z, ntak12)
         ximin  = 2.0E0*R1MACH(1)
         xisml  = sqrt (80.0E0*z)
         ximax = log(R1MACH(2)) + lsq2pi
         ximax = xin(xin(ximax))
         xksml = sqrt (40.0E0*z)
         xkmax = -log(R1MACH(1))
         xkmin = exp (max(-xkmax, -log(R1MACH(2))) + 0.01E0)
         xkmax = lsqpi2 + xkmax
         xkmax = xkn(xkn(xkn(xkmax)))
      end if
c
      if (want.eq.0 .or. abs(want).gt.3) then
         call ierm1('SBI1K1',-1,0,'WANT = 0 OR ABS(WANT) > 3','WANT',
     1      want,'.')
         status=-1
         return
      end if
      status = 0
c
c     Compute I1 if requested, or if needed for K1 computation.
c
      y = abs(x)
      if (y .le. 3.0E0) then
         if (abs(want).ne.2 .or. y.le.boundk) then
            if (y .lt. ximin) then
c              if (y .ne. 0.0E0)
c    *           call SERM1('SBI1K1',2,-1,
c    *                'ABS(X) SO SMALL I1 UNDERFLOWS', 'X',x,'.')
               BI1 = 0.0
c              status = 2
            else
               if (y.le.xisml) then
                  i1ns = 0.5E0*x
               else
                  i1ns = x*(0.875E0+SCSEVL(y*y/4.5E0-1.0E0,bi1cs,nti1))
               end if
               if (want.lt.0) then
                  BI1 = exp(-y) * i1ns
               else
                  BI1 = i1ns
               end if
            end if
         end if
      else if (abs(want) .ne. 2) then
         if (y .le. 8.0E0) then
            BI1 = SCSEVL ((48.0E0/y-11.0E0)/5.0E0, ai1cs, ntai1)
         else
            BI1 = SCSEVL (16.0E0/y-1.0E0, ai12cs, ntai12)
         end if
         BI1 = sign((0.375E0 + BI1)/sqrt(y), x)
         if (want .gt. 0) then
            if (y .gt. ximax) then
               call SERM1('SBI1K1',-2,0,'ABS(X) SO BIG I1 OVERFLOWS',
     *              'X', x, '.')
               status = -2
               BI1 = R1MACH(2)
c              y > ximax => y > xkmax
               BK1 = 0.0
               if (x .gt. 0.0) return
            else
               BI1 = (exp(y-10.0e0) * BI1) * exp10
            end if
         end if
      end if
c
c     Compute K1 if requested.
c
      if (abs(want) .gt. 1) then
         if (x .le. 0.0E0) then
            call SERM1('SBI1K1',-3,0,'X IS ZERO OR NEGATIVE','X',x,'.')
            status = -3
            BK1 = R1MACH(2)
         else if (x .le. boundk) then
            if (x .lt. xkmin) then
               call SERM1('SBI1K1',-4,0,
     *              'X IS SO SMALL K1 OVERFLOWS','X', x,'.')
               status = -4
               BK1 = R1MACH(2)
            else
               if (x .le. xksml) then
                  y = 0.E0
               else
                  y = x*x
               end if
               BK1 = log(0.5E0*x)*i1ns + (0.75E0 +
     1            SCSEVL (0.5E0*y-1.0E0,  bk1cs, ntk1))/x
               if (want .lt. 0) BK1 = exp(x) * BK1
            end if
         else
            y = 16.0E0 / x
            if (x .le. 8.0E0) then
               BK1 = SCSEVL ((y-5.0E0)/3.0E0, ak1cs, ntak1)
            else
               BK1 = SCSEVL (y-1.0E0, ak12cs, ntak12)
            end if
            BK1 = (1.25E0 + BK1) / sqrt(x)
            if (want .gt. 0) then
               if (x .gt. xkmax) then
                  call SERM1('SBI1K1',1,-1,'X SO BIG K1 UNDERFLOWS','X',
     1               x,'.')
                  BK1 = 0.0e0
                  status = 1
               else
                  BK1 = (exp(10.0e0-x) * BK1) * expm10
               end if
            end if
         end if
      end if
C
      return
c
      end
