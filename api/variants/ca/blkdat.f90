module blkdat_mod
    contains
    SUBROUTINE BLKDAT()
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
        !  **BLKDAT--CA   DATE OF LAST REVISION:  05/09/12
        !----------
        !
        !     SEE **MAIN** FOR DICTIONARY OF VARIABLE NAMES.
        !
        !----------
        INTEGER I,J
        integer :: k
        !----------
        !     TYPE DECLARATIONS AND COMMON STATEMENT FOR CONTROL VARIABLES.
        !----------
        COR2(:) = 1.0
        HCOR2(:) = 1.0
        RCOR2(:) = 1.0
        BKRAT(:) = 1.0
        
        TREFMT = '(I4,T1,I7,F6.0,I1,A3,F4.1,F3.1,2F3.0,F4.1,I1,3(I2,I2),2I1,I2,2I3,2I1,F3.0)'
        
        YR  = 10.0
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
        XMIN = (/ 0.5, 0.5, 0.3, 0.8, 0.8, 0.8, 0.8, 0.3, 0.5, 1.2, &
                1.0, 1.0, 1.0, 1.0, 1.0, 0.8, 0.8, 1.0, 0.8, 1.2, &
                1.0, 0.5, 1.0, 0.3, 0.8, 1.0, 0.5, 1.0, 1.0, 0.8, &
                1.0, 0.8, 1.0, 0.5, 0.8, 0.8, 0.5, 0.8, 0.5, 0.8, &
                1.0, 0.5, 1.0, 1.2, 1.2, 1.0, 0.3, 0.5, 0.75 /)
        ISPSPE = (/24,26,27,28,29,30,31,32,33,34,35,36,37,38,39,40, &
                41,42,43,44,45,46,47,48/)
        HHTMAX = (/ (20.0,k=1,49) /)
        DBHMID = (/1.0,3.0,5.0,7.0,9.0,12.0,16.0,20.0,24.0,28.0/)
        BNORML = (/ &
                1.0,1.0,1.0,1.046,1.093,1.139,1.186,1.232,1.278,1.325,1.371, &
                1.418,1.464,1.510,1.557,1.603,1.649,1.696,1.742,1.789/)
        IFORCD = (/  0,  0,  0,  0,  0,  0,  0,  0,  0,  0, &
                0,  0,  0,  0,  0,  0,  0,  0,  0,  0/)
        IFORST = (/  0,  0,  0,  0,  0,  0,  0,  0,  0,  0, &
                0,  0,  0,  0,  0,  0,  0,  0,  0,  0/)
        !
        !     OCURHT ZEROES OUT PROBABILITIES WHICH CANNOT OCCUR BY DEFINITION.
        !
        OCURHT(:,:) = 0.0
        !
        !     OCURNF ZEROES OUT PROBABILITIES ON NATIONAL FORESTS BY SPECIES.
        !
        OCURNF(:,:) = 0.0
        !----------
        !     COMMON STATEMENT FOR PLOT VARIABLES.
        !
        !     SPECIES LIST FOR ICASCA VARIANT
        !
        !     1 = PORT ORFORD CEDAR (PC)          CHAMAECYPARIS LAWSONIANA
        !     2 = INCENSE CEDAR (IC)              LIBOCEDRUS DECURRENS
        !     3 = WESTERN REDCEDAR (RC)           THUJA PLICATA
        !     4 = WHITE FIR (WF)                  ABIES CONCOLOR
        !     5 = CALIFORNIA RED FIR (RF)         ABIES MAGNIFICA (MAGNIFICA)
        !     6 = SHASTA RED FIR (SH)             ABIES MAGNIFICA (SHASTENSIS)
        !     7 = DOUGLAS-FIR (DF)                PSEUDOTSUGA MENZIESII
        !     8 = WESTERN HEMLOCK (WH)            TSUGA HETEROPHYLLA
        !     9 = MOUNTAIN HEMLOCK (MH)           TSUGA MERTENSIANA
        !    10 = WHITEBARK PINE (WB)             PINUS ALBICAULIS
        !    11 = KNOBCONE PINE (KP)              PINUS ATTENUATA
        !    12 = LODGEPOLE PINE (LP)             PINUS CONTORTA
        !    13 = COULTER PINE (CP)               PINUS COULTERI
        !    14 = LIMBER PINE (LM)                PINUS FLEXILIS (FLEXILIS)
        !    15 = JEFFREY PINE (JP)               PINUS JEFFREYI
        !    16 = SUGAR PINE (SP)                 PINUS LAMBERTIANA
        !    17 = WESTERN WHITE PINE (WP)         PINUS MONTICOLA
        !    18 = PONDEROSA PINE (PP)             PINUS PONDEROSA
        !    19 = MONTEREY PINE (MP)              PINUS RADIATA
        !    20 = GRAY PINE (GP)                  PINUS SABINIANA
        !    21 = WESTERN JUNIPER (WJ)            JUNIPERUS OCCIDENTALIS
        !    22 = BREWER SPRUCE (BR)              PICEA BREWERIANA
        !    23 = GIANT SEQUOIA (GS)              SEQUOIADENDRON GIGANTEUM
        !    24 = PACIFIC YEW (PY)                TAXUS BREVIFOLIA
        !    25 = OTHER SOFTWOODS (OS)
        !    26 = COAST LIVE OAK (LO)             QUERCUS AGRIFOLIA
        !    27 = CANYON LIVE OAK (CY)            QUERCUS CHRYSOLEPSIS
        !    28 = BLUE OAK (BL)                   QUERCUS DOUGLASII
        !    29 = ENGELMANN OAK (EO)              QUERCUS ENGELMANNI
        !    30 = OREGON WHITE OAK (WO)           QUERCUS GARRYANA
        !    31 = CALIFORNIA BLACK OAK (BO)       QUERCUS KELLOGGII
        !    32 = VALLEY WHITE OAK (VO)           QUERCUS LOBATA
        !    33 = INTERIOR LIVE OAK (IO)          QUERCUS WISLIZENII
        !    34 = BIGLEAF MAPLE (BM)              ACER MACROPHYLLUM
        !    35 = CALIFORNIA BUCKEYE (BU)         AESCULUS CALIFORNICA
        !    36 = RED ALDER (RA)                  ALNUS RUBRA
        !    37 = PACIFIC MADRONE (MA)            ARBUTUS MENZIESII
        !    38 = GIANT CHINQUAPIN (GC)           CHRYSOLEPIS CHRYSOPHYLLA
        !    39 = PACIFIC DOGWOOD (DG)            CORNUS NUTTALLII
        !    40 = OREGON ASH (FL)                 FRAXINUS LATIFOLIA
        !    41 = WALNUT (WN)                     JUGLANS sp.
        !    42 = TANOAK (TO)                     LITHOCARPUS DENSIFLORUS
        !    43 = CALIFORNIA SYCAMORE (SY)        PLATANUS RACEMOSA
        !    44 = QUAKING ASPEN (AS)              POPULUS TREMULOIDES
        !    45 = BLACK COTTONWOOD (CW)           POPULUS TRICHOCARPA
        !    46 = WILLOW (WI)                     SALIX sp.
        !    47 = CALIFORNIA NUTMEG (CN)          TORREYA CALIFORNICA
        !    48 = CALIFORNIA LAUREL (CL)          UMBELLULARIA CALIFORNICA
        !    49 = OTHER HARDWOODS (OH)
        !----------
        JSP = (/ &
                'PC ',   'IC ',   'RC ',   'WF ',   'RF ',   'SH ',   'DF ', &
                'WH ',   'MH ',   'WB ',   'KP ',   'LP ',   'CP ',   'LM ', &
                'JP ',   'SP ',   'WP ',   'PP ',   'MP ',   'GP ',   'WJ ', &
                'BR ',   'GS ',   'PY ',   'OS ',   'LO ',   'CY ',   'BL ', &
                'EO ',   'WO ',   'BO ',   'VO ',   'IO ',   'BM ',   'BU ', &
                'RA ',   'MA ',   'GC ',   'DG ',   'FL ',   'WN ',   'TO ', &
                'SY ',   'AS ',   'CW ',   'WI ',   'CN ',   'CL ',   'OH '/)
        !
        FIAJSP = (/ &
                '041',   '081',   '242',   '015',   '020',   '021',   '202', &
                '263',   '264',   '101',   '103',   '108',   '109',   '113', &
                '116',   '117',   '119',   '122',   '124',   '127',   '064', &
                '092',   '212',   '231',   '298',   '801',   '805',   '807', &
                '811',   '815',   '818',   '821',   '839',   '312',   '333', &
                '351',   '361',   '431',   '492',   '542',   '600',   '631', &
                '730',   '746',   '747',   '920',   '251',   '981',   '998'/)
        !
        PLNJSP = (/ &
                'CHLA  ','CADE27','THPL  ','ABCO  ','ABMA  ','ABSH  ','PSME  ', &
                'TSHE  ','TSME  ','PIAL  ','PIAT  ','PICO  ','PICO3 ','PIFL2 ', &
                'PIJE  ','PILA  ','PIMO3 ','PIPO  ','PIRA2 ','PISA2 ','JUOC  ', &
                'PIBR  ','SEGI2 ','TABR2 ','2TE   ','QUAG  ','QUCH2 ','QUDO  ', &
                'QUEN  ','QUGA4 ','QUKE  ','QULO  ','QUWI2 ','ACMA3 ','AECA  ', &
                'ALRU2 ','ARME  ','CHCHC4','CONU4 ','FRLA  ','JUGLA ','LIDE3 ', &
                'PLRA  ','POTR5 ','POBAT ','SALIX ','TOCA  ','UMCA  ','2TD   '/)
        !
        JTYPE = (/130,170,250,260,280,290,310,320,330,420, &
                470,510,520,530,540,550,570,610,620,640, &
                660,670,680,690,710,720,730,830,850,999,(0,k=1,92)/)
        !
        NSP(:,1) = (/ &
                'PC1','IC1','RC1','WF1','RF1','SH1','DF1','WH1','MH1','WB1', &
                'KP1','LP1','CP1','LM1','JP1','SP1','WP1','PP1','MP1','GP1', &
                'WJ1','BR1','GS1','PY1','OS1','LO1','CY1','BL1','EO1','WO1', &
                'BO1','VO1','IO1','BM1','BU1','RA1','MA1','GC1','DG1','FL1', &
                'WN1','TO1','SY1','AS1','CW1','WI1','CN1','CL1','OH1'/)
        NSP(:,2) = (/ &
                'PC2','IC2','RC2','WF2','RF2','SH2','DF2','WH2','MH2','WB2', &
                'KP2','LP2','CP2','LM2','JP2','SP2','WP2','PP2','MP2','GP2', &
                'WJ2','BR2','GS2','PY2','OS2','LO2','CY2','BL2','EO2','WO2', &
                'BO2','VO2','IO2','BM2','BU2','RA2','MA2','GC2','DG2','FL2', &
                'WN2','TO2','SY2','AS2','CW2','WI2','CN2','CL2','OH2'/)
        NSP(:,3) = (/ &
                'PC3','IC3','RC3','WF3','RF3','SH3','DF3','WH3','MH3','WB3', &
                'KP3','LP3','CP3','LM3','JP3','SP3','WP3','PP3','MP3','GP3', &
                'WJ3','BR3','GS3','PY3','OS3','LO3','CY3','BL3','EO3','WO3', &
                'BO3','VO3','IO3','BM3','BU3','RA3','MA3','GC3','DG3','FL3', &
                'WN3','TO3','SY3','AS3','CW3','WI3','CN3','CL3','OH3'/)
        !----------
        !   COMMON STATEMENT FOR COEFFS VARIABLES
        !----------
        !   HT1 AND HT2 ARE HEIGHT DUBBING COEFFICIENTS FOR TREES 5.0" DBH
        !   AND LARGER.
        !
        HT1 = (/ &
                4.7874,   5.2052,   4.7874,   5.2180,   5.2973,   5.2973, &
                5.3076,   4.7874,   4.7874,   4.7874,   4.6843,   4.8358, &
                4.7874,   4.7874,   5.1419,   5.3371,   5.2649,   5.3820, &
                4.7874,   4.6236,   4.7874,   4.7874,   4.7874,   4.7874, &
                4.7874,   4.6618,   4.6618,   4.6618,   4.6618,   3.8314, &
                4.4907,   4.6618,   4.6618,   4.6618,   4.6618,   4.6618, &
                4.4809,   4.6618,   4.6618,   4.6618,   4.6618,   4.6618, &
                4.6618,   4.6618,   4.6618,   4.6618,   4.6618,   4.6618, &
                4.6618 /)
        !
        HT2 = (/ &
                -7.3170, -20.1443,  -7.3170, -14.8682, -17.2042, -17.2042, &
                -14.4740,  -7.3170,  -7.3170,  -7.3170,  -6.5516,  -9.2077, &
                -7.3170,  -7.3170, -19.8143, -19.3151, -15.5907, -20.4097, &
                -7.3170, -13.0049,  -7.3170,  -7.3170,  -7.3170,  -7.3170, &
                -7.3170,  -8.3312,  -8.3312,  -8.3312,  -8.3312,  -4.8221, &
                -7.7030,  -8.3312,  -8.3312,  -8.3312,  -8.3312,  -8.3312, &
                -7.5989,  -8.3312,  -8.3312,  -8.3312,  -8.3312,  -8.3312, &
                -8.3312,  -8.3312,  -8.3312,  -8.3312,  -8.3312,  -8.3312, &
                -8.3312 /)
        !
        !  RESIDUAL ERROR ESTIMATES MULTIPLIED BY 0.75 TO APPROXIMATE
        !  CORRECTION FOR MEASUREMENT ERROR
        !  GIANT SEQUOIA VALUE (#23) MAY BE OFF, NEED TO FIND REG RUN TO SEE
        !
        SIGMAR = (/ &
                0.4936, 0.4936, 0.4936, 0.4391, 0.4112, 0.4112, 0.4791, &
                0.4687, 0.4687, 0.4169, 0.4392, 0.4169, 0.4392, 0.4392, &
                0.4458, 0.4687, 0.4745, 0.4458, 0.4458, 0.4392, 0.4392, &
                0.4391, 0.4408, 0.4392, 0.4458, 0.5998, 0.5998, 0.5998, &
                0.5998, 0.5998, 0.5998, 0.5998, 0.5998, 0.5998, 0.5998, &
                0.5998, 0.6608, 0.6608, 0.6608, 0.5998, 0.5998, 0.5166, &
                0.5998, 0.5998, 0.5998, 0.5998, 0.4392, 0.5998, 0.5998/)
        !----------
        !   DATA STATEMENTS FOR VARIABLES IN VARCOM COMMON BLOCK.
        !----------
        HTT1(:,:) = 0.0

        !----------
        !   BB_ ARRAYS HOLD SITE CURVE COEFFICIENTS BASED ON SITE SPECIES
        !   THOSE IN:
        !       FIRST   POSITION ARE KING DOUGLAS-FIR; WEYERH FOR PAP 8
        !       SECOND  POSITION ARE DOLPH WHITE FIR PSW 185;
        !       THIRD   POSITION ARE HANN-SCRIVANI DOUGLAS-FIR
        !       FOURTH  POSITION ARE HANN-SCRIVANI PONDEROSA PINE
        !       FIFTH   POSITION ARE DOLPH RED FIR PSW 206
        !       SIXTH   POSITION ARE DAHMS LODGEPOLE RP-PNW-8
        !       SEVENTH POSITION ARE POWERS BLACK OAK RES NOTE PSW-262
        !       EIGHTH  POSITION ARE PORTER & WIANT TANOAK JOF 4/95 286-287
        !       NINTH   POSITION ARE PORTER & WIANT MADRONE  (DITTO)
        !       TENTH   POSITION ARE PORTER & WIANT RED ALDER  (DITTO)
        !----------
        BB0(:) = (/      2500.,      69.91, -6.21693, -6.54707,        0.0, &
                -0.0968,      6.413,    0.204,    0.375,      0.649, &
                (0.0,k=1,39) /)
        !
        BB1(:) = (/  -0.954038,    38.0202, 0.281176, 0.288169,    1.51744, &
                0.02679,      0.322,   39.787,   31.233,     17.556, &
                (0.0,k=1,39) /)
        !
        BB2(:) = (/   0.109757,   -1.05213,  1.14354,  1.21297, 1.41512E-6, &
                -0.00009309,         0.,       0.,       0.,         0., &
                (0.0,k=1,39) /)
        !
        BB3(:) = (/   0.055818,   0.009557,       0.,       0., -0.0440853, &
                (0.0,k=1,44) /)
        !
        BB4(:) = (/  0.0079224, 101.842894,       0.,       0., -3.04951E6, &
                (0.0,k=1,44) /)
        !
        BB5(:) = (/ -0.0007338,  -0.001442,       0.,       0., 5.72474E-4, &
                (0.0,k=1,44) /)
        !
        BB6(:) = (/  0.0001977,   1.679259,   (0.0,k=1,47) /)
        !
        BB7(:) = 0.0
        !
        BB8(:) = 0.0
        !
        BB9(:) = 0.0
        !
        BB10(:) = 0.0
        !
        BB11(:) = 0.0
        !
        BB12(:) = 0.0
        !
        BB13(:) = 0.0
        !
        REGNBK = 2.999
        !
        S0 = 55329D0
        SS = 55329.
        !
        LSCRN = .FALSE.
        JOSCRN = 6
        !
        JOSUME = 13
        !
        KOLIST = 27
        FSTOPEN = .FALSE.
        !
    END SUBROUTINE BLKDAT
    
end module blkdat_mod