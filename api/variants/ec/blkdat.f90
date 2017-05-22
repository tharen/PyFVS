module blkdat_mod
        contains
        subroutine blkdat()
        use prgprm_mod
        use coeffs_mod
        use esparm_mod
        use escomn_mod
        use pden_mod
        use econ_mod
        use htcal_mod
        use contrl_mod
        use plot_mod
        use rancom_mod
        use screen_mod
        use varcom_mod
        use fvsstdcm_mod
        implicit none
        !----------
        !  **BLKDAT--EC   DATE OF LAST REVISION:  05/09/12
        !----------
        !
        !     SEE **MAIN** FOR DICTIONARY OF VARIABLE NAMES.
        !
        !----------
        INTEGER I,J,k
        !----------
        !  SPECIES LIST FOR EAST CASCADES VARIANT.
        !
        !   1 = WESTERN WHITE PINE      (WP)    PINUS MONTICOLA
        !   2 = WESTERN LARCH           (WL)    LARIX OCCIDENTALIS
        !   3 = DOUGLAS-FIR             (DF)    PSEUDOTSUGA MENZIESII
        !   4 = PACIFIC SILVER FIR      (SF)    ABIES AMABILIS
        !   5 = WESTERN REDCEDAR        (RC)    THUJA PLICATA
        !   6 = GRAND FIR               (GF)    ABIES GRANDIS
        !   7 = LODGEPOLE PINE          (LP)    PINUS CONTORTA
        !   8 = ENGELMANN SPRUCE        (ES)    PICEA ENGELMANNII
        !   9 = SUBALPINE FIR           (AF)    ABIES LASIOCARPA
        !  10 = PONDEROSA PINE          (PP)    PINUS PONDEROSA
        !  11 = WESTERN HEMLOCK         (WH)    TSUGA HETEROPHYLLA
        !  12 = MOUNTAIN HEMLOCK        (MH)    TSUGA MERTENSIANA
        !  13 = PACIFIC YEW             (PY)    TAXUS BREVIFOLIA
        !  14 = WHITEBARK PINE          (WB)    PINUS ALBICAULIS
        !  15 = NOBLE FIR               (NF)    ABIES PROCERA
        !  16 = WHITE FIR               (WF)    ABIES CONCOLOR
        !  17 = SUBALPINE LARCH         (LL)    LARIX LYALLII
        !  18 = ALASKA CEDAR            (YC)    CALLITROPSIS NOOTKATENSIS
        !  19 = WESTERN JUNIPER         (WJ)    JUNIPERUS OCCIDENTALIS
        !  20 = BIGLEAF MAPLE           (BM)    ACER MACROPHYLLUM
        !  21 = VINE MAPLE              (VN)    ACER CIRCINATUM
        !  22 = RED ALDER               (RA)    ALNUS RUBRA
        !  23 = PAPER BIRCH             (PB)    BETULA PAPYRIFERA
        !  24 = GIANT CHINQUAPIN        (GC)    CHRYSOLEPIS CHRYSOPHYLLA
        !  25 = PACIFIC DOGWOOD         (DG)    CORNUS NUTTALLII
        !  26 = QUAKING ASPEN           (AS)    POPULUS TREMULOIDES
        !  27 = BLACK COTTONWOOD        (CW)    POPULUS BALSAMIFERA var. TRICHOCARPA
        !  28 = OREGON WHITE OAK        (WO)    QUERCUS GARRYANA
        !  29 = CHERRY AND PLUM SPECIES (PL)    PRUNUS sp.
        !  30 = WILLOW SPECIES          (WI)    SALIX sp.
        !  31 = OTHER SOFTWOODS         (OS)
        !  32 = OTHER HARDWOODS         (OH)
        !
        !  SURROGATE EQUATION ASSIGNMENT:
        !
        !  FROM THE EC VARIANT:
        !      USE 6(GF) FOR 16(WF)
        !      USE OLD 11(OT) FOR NEW 12(MH) AND 31(OS)
        !
        !  FROM THE WC VARIANT:
        !      USE 19(WH) FOR 11(WH)
        !      USE 33(PY) FOR 13(PY)
        !      USE 31(WB) FOR 14(WB)
        !      USE  7(NF) FOR 15(NF)
        !      USE 30(LL) FOR 17(LL)
        !      USE  8(YC) FOR 18(YC)
        !      USE 29(WJ) FOR 19(WJ)
        !      USE 21(BM) FOR 20(BM) AND 21(VN)
        !      USE 22(RA) FOR 22(RA)
        !      USE 24(PB) FOR 23(PB)
        !      USE 25(GC) FOR 24(GC)
        !      USE 34(DG) FOR 25(DG)
        !      USE 26(AS) FOR 26(AS) AND 32(OH)
        !      USE 27(CW) FOR 27(CW)
        !      USE 28(WO) FOR 28(WO)
        !      USE 36(CH) FOR 29(PL)
        !      USE 37(WI) FOR 30(WI)
        !----------
        !     TYPE DECLARATIONS AND COMMON STATEMENT FOR CONTROL VARIABLES.
        !----------
        BKRAT(:) = 0.0
        
        COR2(:) = 1.0
        HCOR2(:) = 1.0
        RCOR2(:) = 1.0
        
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
        !   COMMON STATEMENT FOR ESCOMN VARIABLE
        !----------
        XMIN = (/ &
                1.0, 1.0, 1.0, 0.5, 0.5, 0.5, 1.0, 0.5, 0.5, 1.0, &
                1.0, 0.5, 1.0, 1.0, 1.0, 0.5, 1.5, 1.0, 1.0, 1.0, &
                1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, &
                0.5, 1.0/)
            
        DBHMID = (/1.0,3.0,5.0,7.0,9.0,12.0,16.0,20.0,24.0,28.0/)
        
        ISPSPE = (/20,21,22,23,24,25,26,27,28,29,30/)
        
        BNORML = (/1.0,1.0,1.0,1.046,1.093,1.139,1.186,1.232,1.278,1.325, &
                1.371,1.418,1.464,1.510,1.557,1.603,1.649,1.696,1.742,1.789/)
        
        HHTMAX = (/ &
                23.0, 27.0, 21.0, 21.0, 22.0, 20.0, 24.0, 18.0, 18.0, 17.0, &
                20.0, 22.0, 20.0, 20.0, 20.0, 20.0, 20.0, 20.0, 20.0, 20.0, &
                20.0, 50.0, 20.0, 20.0, 20.0, 20.0, 20.0, 20.0, 20.0, 20.0, &
                22.0, 20.0/)
            
        IFORCD = (/103,104,105,106,621,110,113,114,116,117, &
                118,109,111,112,412,402,108,102,115,  0/)
        
        IFORST = (/  3,  4,  5,  4,  7, 10,  4, 14, 16, 17, &
                4,  9, 11, 12, 19, 20, 11,  9, 12,  4/)
        
        !----------
        !     OCURHT ZEROES OUT PROBABILITIES WHICH CANNOT OCCUR BY DEFINITION.
        !     DIMENSIONED AT (16,MAXSP) WHERE THE FIRST INDEX IS A HABITAT TYPE.
        !     NONE OF THE SPECIES IN THIS VARIANT HAVE NATURAL REGEN.
        !----------
        OCURHT(:,:) = 0.0
        
        !----------
        !     OCURNF ZEROES OUT PROBABILITIES ON NATIONAL FORESTS BY SPECIES.
        !     DIMENSIONED AT (20,MAXSP) WHERE THE FIRST INDEX IS A NATIONAL FOREST.
        !     NONE OF THE SPECIES IN THIS VARIANT HAVE NATURAL REGEN.
        !----------
        OCURNF(:,:) = 0.0
        
        !----------
        !     COMMON STATEMENT FOR PLOT VARIABLES.
        !----------
        JSP = (/ &
            'WP ',   'WL ',   'DF ',   'SF ',   'RC ', &
            'GF ',   'LP ',   'ES ',   'AF ',   'PP ', &
            'WH ',   'MH ',   'PY ',   'WB ',   'NF ', &
            'WF ',   'LL ',   'YC ',   'WJ ',   'BM ', &
            'VN ',   'RA ',   'PB ',   'GC ',   'DG ', &
            'AS ',   'CW ',   'WO ',   'PL ',   'WI ', &
            'OS ',   'OH '/)
        !
        !  NOTE: VINE MAPLE IS CONSIDERED A SHRUB BY FIA (CODE 324); THE FIA
        !  CODE FOR ROCKY MTN MAPLE (321) WAS APPARENTLY USED IN THE INVENTORY
        !  FOR VINE MAPLE. HOWEVER, IN THIS VARIANT, FIA CODE 321 IS BEING
        !  ASSOCIATED WITH VINE MAPLE IN THE SPECIES TRANSLATOR.
        !
        FIAJSP = (/ &
            '119',   '073',   '202',   '011',   '242', &
            '017',   '108',   '093',   '019',   '122', &
            '263',   '264',   '231',   '101',   '022', &
            '015',   '072',   '042',   '064',   '312', &
            '324',   '351',   '375',   '431',   '492', &
            '746',   '747',   '815',   '760',   '920', &
            '298',   '998'/)
        
        PLNJSP = (/ &
            'PIMO3 ','LAOC  ','PSME  ','ABAM  ','THPL  ', &
            'ABGR  ','PICO  ','PIEN  ','ABLA  ','PIPO  ', &
            'TSHE  ','TSME  ','TABR2 ','PIAL  ','ABPR  ', &
            'ABCO  ','LALY  ','CANO9 ','JUOC  ','ACMA3 ', &
            'ACCI  ','ALRU2 ','BEPA  ','CHCHC4','CONU4 ', &
            'POTR5 ','POBAT ','QUGA4 ','PRUNU ','SALIX ', &
            '2TE   ','2TD   '/)
        
        JTYPE = (/130,170,250,260,280,290,310,320,330,420, &
                470,510,520,530,540,550,570,610,620,640, &
                660,670,680,690,710,720,730,830,850,999,(0,k=1,92)/)
        !
        NSP(:,1) = (/ &
            'WP1','WL1','DF1','SF1','RC1','GF1','LP1','ES1','AF1','PP1', &
            'WH1','MH1','PY1','WB1','NF1','WF1','LL1','YC1','WJ1','BM1', &
            'VN1','RA1','PB1','GC1','DG1','AS1','CW1','WO1','PL1','WI1', &
            'OS1','OH1'/)
            
        NSP(:,2) = (/ &
            'WP2','WL2','DF2','SF2','RC2','GF2','LP2','ES2','AF2','PP2', &
            'WH2','MH2','PY2','WB2','NF2','WF2','LL2','YC2','WJ2','BM2', &
            'VN2','RA2','PB2','GC2','DG2','AS2','CW2','WO2','PL2','WI2', &
            'OS2','OH2'/)
            
        NSP(:,3) = (/ &
            'WP3','WL3','DF3','SF3','RC3','GF3','LP3','ES3','AF3','PP3', &
            'WH3','MH3','PY3','WB3','NF3','WF3','LL3','YC3','WJ3','BM3', &
            'VN3','RA3','PB3','GC3','DG3','AS3','CW3','WO3','PL3','WI3', &
            'OS3','OH3'/)
            
        !----------
        !   COMMON STATEMENT FOR COEFFS VARIABLES
        !----------
        !   HT1 AND HT2 ARE HEIGHT DUBBING COEFFICIENTS FOR TREES 5.0" DBH
        !   AND LARGER FOR SPECIES USING EQUATIONS FROM WC.
        !----------
        HT1 = (/ &
                5.035,    4.961,   4.920,   5.032,   4.896, &
                5.032,    4.854,   4.948,   4.834,   4.884, &
                5.298,    3.9715,  5.188,   5.188,   5.327, &
                5.032,    5.188,   5.143,   5.152,   4.700, &
                4.700,    4.886,   5.152,   5.152,   5.152, &
                5.152,    5.152,   5.152,   5.152,   5.152, &
                3.9715,   5.152/)
        
        HT2 = (/ &
            -10.674,  -8.247,  -9.003,  -10.482,  -8.391, &
            -10.482,  -8.296,  -9.041,  -9.042,   -9.741, &
            -13.240,  -6.7145, -13.801, -13.801,  -15.450, &
            -10.482,  -13.801, -13.497, -13.576,  -6.326, &
            -6.326,   -8.792,  -13.576, -13.576,  -13.576, &
            -13.576,  -13.576, -13.576, -13.576,  -13.576, &
            -6.7145,  -13.576/)
        
        SIGMAR = (/ &
             0.5086,   0.3340,  0.3420,  0.4320,  0.5500, &
             0.3890,   0.3060,  0.3970,  0.4690,  0.3660, &
             0.4104,   0.3220,  0.4842,  0.4842,  0.4275, &
             0.3890,   0.4842,  0.3931,  0.5357,  0.5107, &
             0.5107,   0.7487,  0.5357,  0.5357,  0.5357, &
             0.5357,   0.5357,  0.236,   0.5357,  0.5357, &
             0.3220,   0.5357/)
        !----------
        !   STATEMENTS FOR VARIABLES IN VARCOM COMMON BLOCK
        !----------
        !   HTT1 IS USED TO STORE THE HEIGHT DUBBING COEFFICIENTS FOR TREES
        !   LESS THAN 5.0" DBH FOR SPECIES USING EQUATIONS FROM WC.
        !   DIMENSIONED AT (MAXSP,9)
        !
        HTT1(:,1) = (/ &
        
        !   HTT1(ISPC,1) IS USED TO STORE THE CONSTANT COEFFICIENT.
            (0.0,k=1,10), &
            1.3608,    0.0, 1.5907, 1.5907, 1.7100, &
               0.0, 1.5907, 1.5907, 0.0994, 0.0994, &
            0.0994, 0.0994, 0.0994, 0.0994, 0.0994, &
            0.0994, 0.0994, 0.0994, 0.0994, 0.0994, &
               0.0, 0.0994/)
        
        !   HTT1(ISPC,2) IS USED TO STORE THE DBH COEFFICIENT.
        HTT1(:,2) = (/ &
            (0.0,k=1,10), &
            0.6151,    0.0, 0.3040, 0.3040, 0.2943, &
               0.0, 0.3040, 0.3040, 4.9767, 4.9767, &
            4.9767, 4.9767, 4.9767, 4.9767, 4.9767, &
            4.9767, 4.9767, 4.9767, 4.9767, 4.9767, &
               0.0, 4.9767/)
        
        !   HTT1(ISPC,3) IS USED TO STORE THE CR COEFFICIENT.
        HTT1(:,3) = (/(0.0,k=1,32)/)
        
        !   HTT1(ISPC,4) IS USED TO STORE THE DBH SQUARED COEFFICIENT.
        HTT1(:,4) = (/(0.0,k=1,10), &
            -0.0442,     0.,     0.,     0.,     0., &
            (0.0,k=1,17)/)
        
        !   HTT1(ISPC,5) IS USED TO STORE THE DUMMY VARIABLE FOR
        !   MANAGED/UNMANAGED STANDS.
        HTT1(:,5) = (/ &
            (0.0,k=1,10), &
            0.0829,    0.0,    0.0,    0.0, 0.1054, &
            (0.0,k=1,17)/)
        
        !   HTT1(ISPC,6) THRU HTT1(ISPC,9) ARE NOT USED. SET TO 0.0
        HTT1(:,6:9) = 0.0
        
        BB0 = (/ &
            0.37504453,         0.0,         0.0,   -0.30935,         0.0, &
              -0.30935,     9.89331,     2.75780,   -0.07831, 128.8952205, &
               -1.7307,     22.8741,      0.6192,     0.6192,     -564.38, &
              -0.30935,         0.0,      0.6192,     0.6192,      0.6192, &
                0.6192,     59.5864,      0.6192,     0.6192,      0.6192, &
                0.6192,      0.6192,   -0.954038,     0.6192,      0.6192, &
               22.8741,      0.6192/)
        
        BB1 = (/ &
               0.92503,     1.46897,    -0.37496,     1.2383,      1.3283, &
                1.2383,    -0.19177,     0.83312,     0.0149,   -0.016959, &
                0.1394,    0.950234,     -5.3394,    -5.3394,       22.25, &
                1.2383,     1.46897,     -5.3394,    -5.3394,     -5.3394, &
               -5.3394,      0.7953,     -5.3394,    -5.3394,     -5.3394, &
               -5.3394,     -5.3394,    0.109757,    -5.3394,     -5.3394, &
              0.950234,     -5.3394/)
        
        BB2 = (/ &
            -0.0207959,   0.0092466,     1.36164,   0.001762,     -0.0174, &
              0.001762,     0.00124,    0.015701, -4.0818E-5,     1.23114, &
               -0.0616, -0.00206465,      240.29,     240.29,     0.04995, &
              0.001762,   0.0092466,      240.29,     240.29,      240.29, &
                240.29,     0.00194,      240.29,     240.29,      240.29, &
                240.29,      240.29,  5.58178E-2,     240.29,      240.29, &
           -0.00206465,      240.29/)
        
        BB3 = (/ &
            -2.4881068, -0.00023957, -0.00243434,    -5.4E-6,      1.4711, &
               -5.4E-6,    -0.00082,    22.71944,        0.0,     -0.7864, &
                0.0137,         0.5,      3368.9,     3368.9,        6.80, &
               -5.4E-6,  -2.3957E-4,      3368.9,     3368.9,      3368.9, &
                3368.9,    -0.00074,      3368.9,     3368.9,      3368.9, &
                3368.9,      3368.9,  7.92236E-3,     3368.9,      3368.9, &
                   0.5,      3368.9/)
        
        BB4 = (/ &
                   0.0,   1.1122E-6,      -79.97,   2.046E-7,         0.0, &
              2.046E-7,     0.01387,    -0.63557,        0.0,     2.49717, &
               0.00192,    1.365566,         0.0,        0.0,     2843.21, &
              2.046E-7,   1.1122E-6,         0.0,        0.0,         0.0, &
                   0.0,      0.9198,         0.0,        0.0,         0.0, &
                   0.0,         0.0, -7.33819E-4,        0.0,         0.0, &
              1.365566,         0.0/)
        
        BB5 = (/ &
                   0.0,    -0.12528,     -0.2828,  -4.04E-13,         0.0, &
             -4.04E-13,  -0.0000455,         0.0,        0.0,   -0.004504, &
               0.00007,    2.045963,         0.0,        0.0,    34735.54, &
             -4.04E-13,    -0.12528,         0.0,        0.0,         0.0, &
                   0.0,         0.0,         0.0,        0.0,         0.0, &
                   0.0,         0.0,  1.97693E-4,        0.0,         0.0, &
              2.045963,         0.0/)
        
        BB6 = (/ &
                   0.0,    0.039636,     1.87947,    -6.2056,         0.0, &
               -6.2056,         0.0,         0.0,        0.0,     0.33022, &
                1.8219,         0.0,         0.0,        0.0,         0.0, &
               -6.2056,    0.039636,         0.0,        0.0,         0.0, &
                   0.0,         0.0,         0.0,        0.0,         0.0, &
                   0.0,         0.0,         0.0,        0.0,         0.0, &
                   0.0,         0.0/)
        
        BB7 = (/ &
                   0.0,  -0.0004278,   -0.022399,      2.097,         0.0, &
                 2.097,         0.0,         0.0,        0.0,      100.43, &
              0.199298,         0.0,         0.0,        0.0,         0.0, &
                 2.097,   -4.278E-4,         0.0,        0.0,         0.0, &
                   0.0,         0.0,         0.0,        0.0,         0.0, &
                   0.0,         0.0,         0.0,        0.0,         0.0, &
                   0.0,         0.0/)
        
        BB8 = (/ &
                   0.0,   1.7039E-6,    0.966998,   -0.09411,         0.0, &
              -0.09411,         0.0,         0.0,        0.0,         0.0, &
               0.00438,         0.0,         0.0,        0.0,         0.0, &
              -0.09411,   1.7039E-6,         0.0,        0.0,         0.0, &
                   0.0,         0.0,         0.0,        0.0,         0.0, &
                   0.0,         0.0,         0.0,        0.0,         0.0, &
                   0.0,         0.0/)
        
        BB9 = (/ &
                   0.0,       73.57,         0.0,-0.00004382,         0.0, &
           -0.00004382,         0.0,         0.0,        0.0,         0.0, &
                   0.0,         0.0,         0.0,        0.0,         0.0, &
           -0.00004382,       73.57,         0.0,        0.0,         0.0, &
                   0.0,         0.0,         0.0,        0.0,         0.0, &
                   0.0,         0.0,         0.0,        0.0,         0.0, &
                   0.0,         0.0/)
        
        BB10 = (/ &
                   0.0,    -0.12528,         0.0,  2.007E-11,         0.0, &
             2.007E-11,         0.0,         0.0,        0.0,         0.0, &
                   0.0,         0.0,         0.0,        0.0,         0.0, &
             2.007E-11,    -0.12528,         0.0,        0.0,         0.0, &
                   0.0,         0.0,         0.0,        0.0,         0.0, &
                   0.0,         0.0,         0.0,        0.0,         0.0, &
                   0.0,         0.0/)
        
        BB11 = (/ &
                   0.0,    0.039636,         0.0, -2.054E-17,         0.0, &
            -2.054E-17,         0.0,         0.0,        0.0,         0.0, &
                   0.0,         0.0,         0.0,        0.0,         0.0, &
            -2.054E-17,    0.039636,         0.0,        0.0,         0.0, &
                   0.0,         0.0,         0.0,        0.0,         0.0, &
                   0.0,         0.0,         0.0,        0.0,         0.0, &
                   0.0,         0.0/)
        
        BB12 = (/ &
                   0.0,  -0.0004278,         0.0,     -84.73,         0.0, &
                -84.73,         0.0,         0.0,        0.0,         0.0, &
                   0.0,         0.0,         0.0,        0.0,         0.0, &
                -84.73,   -4.278E-4,         0.0,        0.0,         0.0, &
                   0.0,         0.0,         0.0,        0.0,         0.0, &
                   0.0,         0.0,         0.0,        0.0,         0.0, &
                   0.0,         0.0/)
        
        BB13 = (/ &
                   0.0,   1.7039E-6,         0.0,        0.0,         0.0, &
                   0.0,         0.0,         0.0,        0.0,         0.0, &
                   0.0,         0.0,         0.0,        0.0,         0.0, &
                   0.0,   1.7039E-6,         0.0,        0.0,         0.0, &
                   0.0,         0.0,         0.0,        0.0,         0.0, &
                   0.0,         0.0,         0.0,        0.0,         0.0, &
                   0.0,         0.0/)
        
        REGNBK = 2.999
        
        S0 = 55329D0
        SS = 55329.0
        
        LSCRN = .FALSE.
        JOSCRN = 6
        
        JOSUME = 13
        
        KOLIST = 27
        FSTOPEN = .FALSE.
        
    end subroutine blkdat
end module blkdat_mod
