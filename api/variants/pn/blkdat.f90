module blkdat_mod
    contains
    SUBROUTINE BLKDAT()
        use plot_mod
        use prgprm_mod
        use coeffs_mod
        use esparm_mod
        use escomn_mod
        use pden_mod
        use econ_mod
        use htcal_mod
        use contrl_mod
        use rancom_mod
        use screen_mod
        use varcom_mod
        use fvsstdcm_mod
        implicit none
        !----------
        !  **BLKDAT--PN   DATE OF LAST REVISION:  05/09/12
        !----------
        !
        !     SEE **MAIN** FOR DICTIONARY OF VARIABLE NAMES.
        !
        !----------
        INTEGER I,J,k
        !----------
        ! TYPE DECLARATIONS AND COMMON STATEMENT FOR CONTROL VARIABLES.
        !----------
        COR2(:) = 1.0
        HCOR2(:) = 1.0
        RCOR2(:) = 1.0
        BKRAT(:) = 0.0
        TREFMT = '(I4,T1,I7,F6.0,I1,A3,F4.1,F3.1,2F3.0,F4.1,I1,3(I2,I2),2I1,I2,2I3,2I1,F3.0)'

        YR = 10.0
        IRECNT = 0
        ICCODE = 0

        IREAD = 15
        ISTDAT = 2
        JOLIST = 3
        JOSTND = 16
        JOSUM = 4
        JOTREE = 8
        !----------
        ! COMMON STATEMENT FOR ESCOMN VARIABLE
        !----------
        XMIN = (/ 1.0, 1.5, 1.5, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.4, &
              1.0,1.0,1.0, 1.3, 1.5, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, &
              1.0, 1.0, 1.0, 1.0, 1.0, 1.5, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, &
              1.0, 1.0/)
        ISPSPE = (/ 17,21,22,23,24,25,26,27,28,33,34,35,36,37/)
        HHTMAX = (/(20.0,k=1,21),50.0,(20.0,k=1,17)/)
        DBHMID = (/1.0,3.0,5.0,7.0,9.0,12.0,16.0,20.0,24.0,28.0/)
        BNORML = (/1.0,1.0,1.0,1.046,1.093,1.139,1.186,1.232,1.278,1.325,1.371, &
              1.418,1.464,1.510,1.557,1.603,1.649,1.696,1.742,1.789/)
        IFORCD = (/  0,  0,  0,  0,  0,  0,  0,  0,  0,  0, &
                     0,  0,  0,  0,  0,  0,  0,  0,  0,  0/)
        IFORST = (/  0,  0,  0,  0,  0,  0,  0,  0,  0,  0, &
                     0,  0,  0,  0,  0,  0,  0,  0,  0,  0/)

        !   OCURHT ZEROES OUT PROBABILITIES WHICH CANNOT OCCUR BY DEFINITION.
        OCURHT(:,:) = 0.0

        !   OCURNF ZEROES OUT PROBABILITIES ON NATIONAL FORESTS BY SPECIES.
        OCURNF(:,:) = 0.0

        !----------
        ! COMMON STATEMENT FOR PLOT VARIABLES.
        !----------
        !     SPECIES LIST FOR PACIFIC COAST VARIANT
        !
        !     1 = PACIFIC SILVER FIR (SF)      ABIES AMABILIS
        !     2 = WHITE FIR (WF)               ABIES CONCOLOR
        !     3 = GRAND FIR (GF)               ABIES GRANDIS
        !     4 = SUBALPINE FIR (AF)           ABIES LASIOCARPA
        !     5 = CALIFORNIA RED FIR (RF)/     ABIES MAGNIFICA
        !         SHASTA RED FIR
        !     6 = SITKA SPRUCE (SS)            PICEA SITCHENSIS
        !     7 = NOBLE FIR (NF)               ABIES PROCERA
        !     8 = ALASKA CEDAR (YC)/           CALLITROPSIS NOOTKATENSIS
        !         WESTERN LARCH                LARIX OCCIDENTALIS
        !     9 = INCENSE CEDAR (IC)           LIBOCEDRUS DECURRENS
        !    10 = ENGELMANN SPRUCE (ES)        PICEA ENGELMANNII
        !    11 = LODGEPOLE PINE (LP)          PINUS CONTORTA
        !    12 = JEFFREY PINE (JP)            PINUS JEFFREYI
        !    13 = SUGAR PINE (SP)              PINUS LAMBERTIANA
        !    14 = WESTERN WHITE PINE (WP)      PINUS MONTICOLA
        !    15 = PONDEROSA PINE (PP)          PINUS PONDEROSA
        !    16 = DOUGLAS-FIR (DF)             PSEUDOTSUGA MENZIESII
        !    17 = COAST REDWOOD (RW)           SEQUOIA SEMPERVIRENS
        !    18 = WESTERN REDCEDAR (RC)        THUJA PLICATA
        !    19 = WESTERN HEMLOCK (WH)         TSUGA HETEROPHYLLA
        !    20 = MOUNTAIN HEMLOCK (MH)        TSUGA MERTENSIANA
        !    21 = BIGLEAF MAPLE (BM)           ACER MACROPHYLLUM
        !    22 = RED ALDER (RA)               ALNUS RUBRA
        !    23 = WHITE ALDER (WA) /           ALNUS RHOMBIFOLIA
        !         PACIFIC MADRONE              ARBUTUS MENZIESII
        !    24 = PAPER BIRCH (PB)             BETULA PAPYRIFERA
        !    25 = GIANT CHINQUAPIN (GC) /      CHRYSOLEPIS CHRYSOPHYLLA
        !         TANOAK                       LITHOCARPUS DENSIFLORUS
        !    26 = QUAKING ASPEN (AS)           POPULUS TREMULOIDES
        !    27 = BLACK COTTONWOOD (CW)        POPULUS TRICHOCARPA
        !    28 = OREGON WHITE OAK (WO) /      QUERCUS GARRYANA
        !         CALIFORNIA BLACK OAK         QUERCUS KELLOGGII
        !    29 = WESTERN JUNIPER (WJ)         JUNIPERUS OCCIDENTALIS
        !    30 = SUBALPINE LARCH (LL)         LARIX LYALLII
        !    31 = WHITEBARK PINE (WB)          PINUS ALBICAULIS
        !    32 = KNOBCONE PINE (KP)           PINUS ATTENUATA
        !    33 = PACIFIC YEW (PY)             TAXUS BREVIFOLIA
        !    34 = PACIFIC DOGWOOD (DG)         CORNUS NUTTALLII
        !    35 = HAWTHORN (HT)                CRATAEGUS sp.
        !    36 = BITTER CHERRY (CH)           PRUNUS EMARGINATA
        !    37 = WILLOW (WI)                  SALIX sp.
        !    38 = ---
        !    39 = OTHER (OT)
        !----------

        JSP = (/ &
          'SF ',   'WF ',   'GF ',   'AF ',   'RF ',   'SS ',   'NF ', &
          'YC ',   'IC ',   'ES ',   'LP ',   'JP ',   'SP ',   'WP ', &
          'PP ',   'DF ',   'RW ',   'RC ',   'WH ',   'MH ',   'BM ', &
          'RA ',   'WA ',   'PB ',   'GC ',   'AS ',   'CW ',   'WO ', &
          'WJ ',   'LL ',   'WB ',   'KP ',   'PY ',   'DG ',   'HT ', &
          'CH ',   'WI ',   '   ',   'OT '/)

        FIAJSP = (/ &
            '011',   '015',   '017',   '019',   '020',   '098',   '022', &
            '042',   '081',   '093',   '108',   '116',   '117',   '119', &
            '122',   '202',   '211',   '242',   '263',   '264',   '312', &
            '351',   '352',   '375',   '431',   '746',   '747',   '815', &
            '064',   '072',   '101',   '103',   '231',   '492',   '500', &
            '768',   '920',   '   ',   '999'/)

        PLNJSP = (/ &
            'ABAM  ','ABCO  ','ABGR  ','ABLA  ','ABMA  ','PISI  ','ABPR  ', &
            'CANO9 ','CADE27','PIEN  ','PICO  ','PIJE  ','PILA  ','PIMO3 ', &
            'PIPO  ','PSME  ','SESE3 ','THPL  ','TSHE  ','TSME  ','ACMA3 ', &
            'ALRU2 ','ALRH2 ','BEPA  ','CHCHC4','POTR5 ','POBAT ','QUGA4 ', &
            'JUOC  ','LALY  ','PIAL  ','PIAT  ','TABR2 ','CONU4 ','CRATA ', &
            'PREM  ','SALIX ','      ','2TREE '/)

        JTYPE = (/ &
            130,170,250,260,280,290,310,320,330,420, &
            470,510,520,530,540,550,570,610,620,640, &
            660,670,680,690,710,720,730,830,850,999,(0,k=1,92) /)

        NSP(:,1) = (/ &
            'SF1','WF1','GF1','AF1','RF1','SS1','NF1','YC1', &
            'IC1','ES1','LP1','JP1','SP1','WP1','PP1','DF1','RW1','RC1', &
            'WH1','MH1','BM1','RA1','WA1','PB1','GC1','AS1','CW1','WO1','WJ1', &
            'LL1','WB1','KP1','PY1','DG1','HT1','CH1','WI1','__1','OT1' /)

        NSP(:,2) = (/ &
            'SF2','WF2','GF2','AF2','RF2','SS2','NF2','YC2', &
            'IC2','ES2','LP2','JP2','SP2','WP2','PP2','DF2','RW2','RC2', &
            'WH2','MH2','BM2','RA2','WA2','PB2','GC2','AS2','CW2','WO2','WJ2', &
            'LL2','WB2','KP2','PY2','DG2','HT2','CH2','WI2','__2','OT2' /)

        NSP(:,3) = (/ &
            'SF3','WF3','GF3','AF3','RF3','SS3','NF3','YC3', &
            'IC3','ES3','LP3','JP3','SP3','WP3','PP3','DF3','RW3','RC3', &
            'WH3','MH3','BM3','RA3','WA3','PB3','GC3','AS3','CW3','WO3','WJ3', &
            'LL3','WB3','KP3','PY3','DG3','HT3','CH3','WI3','__3','OT3' /)
        !----------
        ! COMMON STATEMENT FOR COEFFS VARIABLES
        !----------
        !   HT1 AND HT2 ARE HEIGHT DUBBING COEFFICIENTS FOR TREES 5.0" DBH
        !   AND LARGER.
        !----------
        HT1 = (/ &
            5.487, 5.308, 5.308, 5.313, 5.313, 5.517, 5.327, 5.143, 5.188, 5.188, 4.865, &
            5.333, 5.382, 5.382, 5.333, 5.563, 5.188, 5.233, 5.355, 5.081, &
            4.700, 4.875, 5.152, 5.152, 5.152, 5.152, 5.152, 5.152, 5.152, &
            5.188, 5.188, 5.188, 5.188, 5.152, 5.152, 5.152, 5.152, 5.152, 5.152/)

        HT2 = (/ &
            -16.701, -13.624, -13.624, -15.321, -15.321, -17.944, -15.450, -13.497, &
            -13.801, -13.801, -9.305, -17.762, -15.866, -15.866, -17.762, -16.475, &
            -13.801, -14.737, -13.878, -13.430, -6.326, -8.639, &
            -13.576, -13.576, -13.576, -13.576, -13.576, -13.576, -13.576, &
            -13.801, -13.801, -13.801, -13.801, -13.576, -13.576, -13.576, -13.576, &
            -13.576, -13.576/)

        !  SIGMAR VALUES FOR SF,SS,DF,RC,WH,RA MULTIPLIED BY .75
        !  TO CORRECT FOR BIAS IN THE FITTING PROCEDURE.
        !  **REFIT CHANGES SPCS SS,DF,RC,WH (1,6,16,18,19) DMD 060396
        !  **REFIT OF WO(28) by GOULD&HARRINGTON ESM 041910

        SIGMAR = (/ &
            0.3428, 0.4390, 0.4390, 0.3960, 0.3102, 0.3769, 0.4275, &
            0.3931, 0.4842, 0.4842, 0.3690, 0.3222, 0.5494, 0.5494, 0.3222, &
            0.2679, 0.4842, 0.3625, 0.3402, 0.3751, 0.5107, &
            0.3328, (0.5357,k=1,5), 0.236, 0.5357, (0.4842,k=1,4), (0.5357,k=1,6)/)
        !----------
        ! DATA STATEMENTS FOR VARIABLES IN VARCOM COMMON BLOCK.
        !----------
        !   HTT1 IS USED TO STORE THE HEIGHT DUBBING COEFFICIENTS FOR TREES
        !   LESS THAN 5.0" DBH.

        !   HTT1(ISPC,1) IS USED TO STORE THE CONSTANT COEFFICIENT.
        HTT1(:,1) = (/ &
            1.3134, 1.4769, 1.4769 , 1.4261, 1.3526, 1.3526, 1.7100, &
            1.5907, 1.5907, 1.5907, 0.9717, 1.0756, 0.9717, 0.9717, 1.0756, &
            7.1391, 1.5907, 2.3115, 1.3608, 1.2278, &
            (0.0994,k=1,9), (1.5907,k=1,4), (0.0994,k=1,6) /)

        !   HTT1(ISPC,2) IS USED TO STORE THE DBH COEFFICIENT.
        HTT1(:,2) = (/ &
            0.3432, 0.3579, 0.3579, 0.3334, 0.3335, 0.3335, 0.2943, (0.3040,k=1,3), &
            0.3934, 0.4369, 0.3934, 0.3934, 0.4369, 4.2891, 0.3040, 0.2370, &
            0.6151, 0.4000, (4.9767,k=1,9), (0.3040,k=1,4), (4.9767,k=1,6) /)

        !   HTT1(ISPC,3) IS USED TO STORE THE CR COEFFICIENT.
        HTT1(:,3) = (/ &
            0.0366, 0.0, 0.0, 0.0, 0.0367, 0.0367, 0.0, 0.0, 0.0, 0.0, 0.0339, 0.0, &
            0.0339, 0.0339, 0.0, -0.7150, 0.0, -0.0556, (0.0,k=1,21) /)

        !   HTT1(ISPC,4) IS USED TO STORE THE DBH SQUARED COEFFICIENT.
        HTT1(:,4) = (/ &
            (0.0,k=1,15), 0.2750, 0.0, 0.0, -0.0442, (0.0,k=1,20) /)

        !   HTT1(ISPC,5) IS USED TO STORE THE DUMMY VARIABLE FOR MANAGED/UNMANAGED STANDS.
        HTT1(:,5) = (/ &
            (0.0,k=1,6), 0.1054, (0.0,k=1,3), 0.3044, 0.0, 0.3044, 0.3044, 0.0, &
            2.0393, 0.0, 0.3218, 0.0829, (0.0,k=1,20) /)

        !   HTT1(ISPC,6) THRU HTT1(ISPC,9) ARE NOT USED. SET TO 0.0
        HTT1(:,6:9) = 0.0

        BB0 = (/ &
            0.0071839, -0.30935  , -0.30935  ,  2.75780 ,  0.0      , &
            -0.2050542, -564.38   ,  0.6192   ,128.8952205,  2.75780 , &
            -0.0968   ,128.8952205, -4.62536  , -4.62536  ,128.8952205, &
            -0.954038 ,  0.6192   , -0.2050542, -1.7307   , 22.8741   , &
            0.6192   , 59.5864   ,  0.6192   ,  0.6192   ,  0.6192   , &
            0.6192   ,  0.6192   , -0.954038  ,  0.6192   ,  0.0      , &
            0.6192   ,  0.6192   ,  0.6192   ,  0.6192   ,  0.6192   , &
            0.6192   ,  0.6192   ,  0.6192   ,  0.6192    /)

        BB1 = (/ &
            0.0000571,  1.2383   ,  1.2383   ,  0.83312 ,  1.51744  , &
            1.449615 , 22.25     , -5.3394   , -0.016959 ,  0.83312 , &
            0.02679  , -0.016959 ,  1.346399 ,  1.346399 , -0.016959 , &
            0.109757 , -5.3394   ,  1.449615 ,  0.1394   ,  0.950234 , &
            -5.3394   ,  0.7953   , -5.3394   , -5.3394   , -5.3394   , &
            -5.3394   , -5.3394   , 0.109757      , -5.3394   ,  1.46897  , &
            -5.3394   , -5.3394   , -5.3394   , -5.3394   , -5.3394   , &
            -5.3394   , -5.3394   , -5.3394   , -5.3394    /)

        BB2 = (/ &
            1.39005  ,  0.001762 ,  0.001762 ,  0.015701 ,  1.4151E-6, &
            -0.01780992,  0.04995  , 240.29    ,  1.23114  ,  0.015701 , &
            -9.309E-5 ,  1.23114  ,-135.354483,-135.354483,  1.23114  , &
            5.58178E-2, 240.29    ,-0.01780992, -0.0616   , -2.06465E-3, &
            240.29    ,  0.00194  , 240.29    , 240.29    , 240.29    , &
            240.29    , 240.29    ,5.58178E-2 , 240.29    ,  0.0092466, &
            240.29    , 240.29    , 240.29    , 240.29    , 240.29    , &
            240.29    , 240.29    , 240.29    , 240.29     /)

         BB3 = (/ &
            0.0      , -5.4E-6   , -5.4E-6   , 22.71944  , -0.0440853, &
            6.519748E-5,  6.80     , 3368.9    , -0.7864   , 22.71944  , &
            0.0      , -0.7864   ,  0.0      ,  0.0      , -0.7864   , &
            7.92236E-3, 3368.9    ,6.519748E-5,  0.0137   ,  0.5      , &
            3368.9    , -0.0007403, 3368.9    , 3368.9    , 3368.9    , &
            3368.9    , 3368.9    ,7.92236E-3 , 3368.9    , -2.3957E-4, &
            3368.9    , 3368.9    , 3368.9    , 3368.9    , 3368.9    , &
            3368.9    , 3368.9    , 3368.9    , 3368.9     /)

         BB4 = (/ &
            0.0        ,  2.046E-7 ,  2.046E-7   , -0.63557  , -3.0495E6 , &
            -1.095593E-23, 2843.21   ,  0.0        ,  2.49717  , -0.63557  , &
            0.0        ,  2.49717  ,  0.0        ,  0.0      ,  2.49717  , &
            -7.33819E-4  ,  0.0      ,-1.095593E-23,  0.00192  ,  1.365566 , &
            0.0        ,  0.9198   ,  0.0        ,  0.0      ,  0.0      , &
            0.0        ,  0.0      ,-7.33819E-4  ,  0.0      ,  1.1122E-6, &
            0.0        ,  0.0      ,  0.0        ,  0.0      ,  0.0      , &
            0.0        ,  0.0      ,  0.0        ,  0.0       /)

        BB5 = (/ &
            0.0      , -4.04E-13 , -4.04E-13 ,  0.0      , 5.72474E-4, &
            -5.611879 , 34735.54  ,  0.0      , -0.0045042,  0.0      , &
            0.0      , -0.0045042,  0.0      ,  0.0      , -0.0045042, &
            1.97693E-4,  0.0      , -5.611879 ,  0.00007  ,  2.045963 , &
            0.0      ,  0.0      ,  0.0      ,  0.0      ,  0.0      , &
            0.0      ,  0.0      ,1.97693E-4 ,  0.0      , -0.12528, &
            0.0      ,  0.0      ,  0.0      ,  0.0      ,  0.0      , &
            0.0      ,  0.0      ,  0.0      ,  0.0       /)

        BB6 = (/ &
            0.0      , -6.2056   , -6.2056   ,  0.0      ,  0.0      , &
            2.418604 ,  0.0      ,  0.0      ,  0.33022  ,  0.0      , &
            0.0      ,  0.33022  ,  0.0      ,  0.0      ,  0.33022  , &
            0.0      ,  0.0      ,  2.418604 ,  0.0      ,  0.0      , &
            0.0      ,  0.0      ,  0.0      ,  0.0      ,  0.0      , &
            0.0      ,  0.0      ,  0.0      ,  0.0      ,  0.039636 , &
            0.0      ,  0.0      ,  0.0      ,  0.0      ,  0.0      , &
            0.0      ,  0.0      ,  0.0      ,  0.0       /)

        BB7 = (/ &
            0.0      ,  2.097    ,  2.097    ,  0.0      ,  0.0      , &
            -0.2593110,  0.0      ,  0.0      ,100.43     ,  0.0      , &
            0.0      ,100.43     ,  0.0      ,  0.0      ,100.43     , &
            0.0      ,  0.0      , -0.2593110,  0.0      ,  0.0      , &
            0.0      ,  0.0      ,  0.0      ,  0.0      ,  0.0      , &
            0.0      ,  0.0      ,  0.0      ,  0.0      , -4.278E-4 , &
            0.0      ,  0.0      ,  0.0      ,  0.0      ,  0.0      , &
            0.0      ,  0.0      ,  0.0      ,  0.0       /)

        BB8 = (/ &
            0.0      , -0.09411  , -0.09411  ,  0.0      ,  0.0      , &
            1.351445E-4,  0.0      ,  0.0      ,  0.0      ,  0.0      , &
            0.0      ,  0.0      ,  0.0      ,  0.0      ,  0.0      , &
            0.0      ,  0.0      ,1.351445E-4,  0.0      ,  0.0      , &
            0.0      ,  0.0      ,  0.0      ,  0.0      ,  0.0      , &
            0.0      ,  0.0      ,  0.0      ,  0.0      ,  1.7039E-6, &
            0.0      ,  0.0      ,  0.0      ,  0.0      ,  0.0      , &
            0.0      ,  0.0      ,  0.0      ,  0.0       /)

        BB9 = (/ &
            0.0        , -4.382E-5 , -4.382E-5   ,  0.0      ,  0.0      , &
            -1.701139E-12,  0.0      ,  0.0        ,  0.0      ,  0.0      , &
            0.0        ,  0.0      ,  0.0        ,  0.0      ,  0.0      , &
            0.0        ,  0.0      ,-1.701139E-12,  0.0      ,  0.0      , &
            0.0        ,  0.0      ,  0.0        ,  0.0      ,  0.0      , &
            0.0        ,  0.0      ,  0.0        ,  0.0      , 73.57     , &
            0.0        ,  0.0      ,  0.0        ,  0.0      ,  0.0      , &
            0.0        ,  0.0      ,  0.0        ,  0.0       /)

        BB10 = (/ &
            0.0       ,  2.007E-11,  2.007E-11 ,  0.0      ,  0.0      , &
            7.964197E-27,  0.0      ,  0.0       ,  0.0      ,  0.0      , &
            0.0       ,  0.0      ,  0.0       ,  0.0      ,  0.0      , &
            0.0       ,  0.0      ,7.964197E-27,  0.0      ,  0.0      , &
            0.0       ,  0.0      ,  0.0       ,  0.0      ,  0.0      , &
            0.0       ,  0.0      ,  0.0       ,  0.0      , -0.12528, &
            0.0       ,  0.0      ,  0.0       ,  0.0      ,  0.0      , &
            0.0       ,  0.0      ,  0.0       ,  0.0       /)

        BB11 = (/ &
            0.0      ,  2.054E-17,  2.054E-17,  0.0      ,  0.0      , &
            -86.43     ,  0.0      ,  0.0      ,  0.0      ,  0.0      , &
            0.0      ,  0.0      ,  0.0      ,  0.0      ,  0.0      , &
            0.0      ,  0.0      ,-86.43     ,  0.0      ,  0.0      , &
            0.0      ,  0.0      ,  0.0      ,  0.0      ,  0.0      , &
            0.0      ,  0.0      ,  0.0      ,  0.0      ,  0.039636, &
            0.0      ,  0.0      ,  0.0      ,  0.0      ,  0.0      , &
            0.0      ,  0.0      ,  0.0      ,  0.0       /)

        BB12 = (/ &
            0.0      ,-84.93     ,-84.93     ,  0.0      ,  0.0      , &
            0.0      ,  0.0      ,  0.0      ,  0.0      ,  0.0      , &
            0.0      ,  0.0      ,  0.0      ,  0.0      ,  0.0      , &
            0.0      ,  0.0      ,  0.0      ,  0.0      ,  0.0      , &
            0.0      ,  0.0      ,  0.0      ,  0.0      ,  0.0      , &
            0.0      ,  0.0      ,  0.0      ,  0.0      , -4.278E-4 , &
            0.0      ,  0.0      ,  0.0      ,  0.0      ,  0.0      , &
            0.0      ,  0.0      ,  0.0      ,  0.0       /)

        BB13 = (/(0.0,k=1,29), 1.7039E-6,  (0.0,k=1,9)/)

        REGNBK = 2.999

        S0 = 55329D0
        SS = 55329.0

        LSCRN = .FALSE.
        JOSCRN = 6

        JOSUME = 13

        KOLIST = 27
        FSTOPEN = .FALSE.

    END SUBROUTINE BLKDAT
end module blkdat_mod
