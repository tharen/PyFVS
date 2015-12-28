      SUBROUTINE CCFCAL(ISPC,D,H,JCR,P,LTHIN,CCFT,CRWDTH,MODE)
      use contrl_mod
      use plot_mod
      use prgprm_mod
      implicit none
C----------
C  **CCFCAL--CI   DATE OF LAST REVISION:   08/14/12
C----------
C  THIS ROUTINE COMPUTES CROWN WIDTH AND CCF FOR INDIVIDUAL TREES.
C  CALLED FROM DENSE, PRTRLS, SSTAGE, AND CVCW.
C
C  ARGUMENT DEFINITIONS:
C    ISPC = NUMERIC SPECIES CODE
C       D = DIAMETER AT BREAST HEIGHT
C       H = TOTAL TREE HEIGHT
C     JCR = CROWN RATIO IN PERCENT (0-100)
C       P = TREES PER ACRE
C   LTHIN = .TRUE. IF THINNING HAS JUST OCCURRED
C         = .FALSE. OTHERWISE
C    CCFT = CCF REPRESENTED BY THIS TREE
C  CRWDTH = CROWN WIDTH OF THIS TREE
C    MODE = 1 IF ONLY NEED CCF RETURNED
C           2 IF ONLY NEED CRWDTH RETURNED
C----------
COMMONS
C----------
C  DIMENSION AND DATA STATEMENTS FOR INTERNAL VARIABLES.
C
C     CCF COEFFICIENTS FOR TREES THAT ARE GREATER THAN 10.0 IN. DBH:
C      RD1 -- CONSTANT TERM IN CROWN COMPETITION FACTOR EQUATION,
C             SUBSCRIPTED BY SPECIES
C      RD2 -- COEFFICIENT FOR SUM OF DIAMETERS TERM IN CROWN
C             COMPETITION FACTOR EQUATION,SUBSCRIPTED BY SPECIES
C      RD3 -- COEFFICIENT FOR SUM OF DIAMETER SQUARED TERM IN
C             CROWN COMPETITION EQUATION, SUBSCRIPTED BY SPECIES
C
C     CCF COEFFICIENTS FOR TREES THAT ARE LESS THAN 10.0 IN. DBH:
C      RDA -- MULTIPLIER.
C      RDB -- EXPONENT.  CCF(I) = RDA*DBH**RDB
C----------
C     SPECIES LIST FOR CENTRAL IDAHO VARIANT.
C
C     1 = WESTERN WHITE PINE (WP)          PINUS MONTICOLA
C     2 = WESTERN LARCH (WL)               LARIX OCCIDENTALIS
C     3 = DOUGLAS-FIR (DF)                 PSEUDOTSUGA MENZIESII
C     4 = GRAND FIR (GF)                   ABIES GRANDIS
C     5 = WESTERN HEMLOCK (WH)             TSUGA HETEROPHYLLA
C     6 = WESTERN REDCEDAR (RC)            THUJA PLICATA
C     7 = LODGEPOLE PINE (LP)              PINUS CONTORTA
C     8 = ENGLEMANN SPRUCE (ES)            PICEA ENGELMANNII
C     9 = SUBALPINE FIR (AF)               ABIES LASIOCARPA
C    10 = PONDEROSA PINE (PP)              PINUS PONDEROSA
C    11 = WHITEBARK PINE (WB)              PINUS ALBICAULIS
C    12 = PACIFIC YEW (PY)                 TAXUS BREVIFOLIA
C    13 = QUAKING ASPEN (AS)               POPULUS TREMULOIDES
C    14 = WESTERN JUNIPER (WJ)             JUNIPERUS OCCIDENTALIS
C    15 = CURLLEAF MOUNTAIN-MAHOGANY (MC)  CERCOCARPUS LEDIFOLIUS
C    16 = LIMBER PINE (LM)                 PINUS FLEXILIS
C    17 = BLACK COTTONWOOD (CW)            POPULUS BALSAMIFERA VAR. TRICHOCARPA
C    18 = OTHER SOFTWOODS (OS)
C    19 = OTHER HARDWOODS (OH)
C
C  SURROGATE EQUATION ASSIGNMENT:
C
C  FROM THE IE VARIANT:
C      USE 17(PY) FOR 12(PY)             (IE17 IS REALLY TT2=LM)
C      USE 18(AS) FOR 13(AS)             (IE18 IS REALLY UT6=AS)
C      USE 13(LM) FOR 11(WB) AND 16(LM)  (IE13 IS REALLY TT2=LM)
C      USE 19(CO) FOR 17(CW) AND 19(OH)  (IE19 IS REALLY CR38=OH)
C
C  FROM THE UT VARIANT:
C      USE 12(WJ) FOR 14(WJ)
C      USE 20(MC) FOR 15(MC)             (UT20 = SO30=MC, WHICH IS
C                                                  REALLY WC39=OT)
C----------
C  SOURCES OF CCF COEFFICIENTS:
C     1 = NORTHERN IDAHO VARIANT INT-133 TABLE 8: WESTERN WHITE PINE
C     2 = NORTHERN IDAHO VARIANT INT-133 TABLE 8: WESTERN LARCH
C     3 = NORTHERN IDAHO VARIANT INT-133 TABLE 8: DOUGLAS-FIR
C     4 = NORTHERN IDAHO VARIANT INT-133 TABLE 8: GRAND FIR
C     5 = NORTHERN IDAHO VARIANT INT-133 TABLE 8: WESTERN HEMLOCK
C     6 = NORTHERN IDAHO VARIANT INT-133 TABLE 8: WESTERN RED CEDAR
C     7 = NORTHERN IDAHO VARIANT INT-133 TABLE 8: LODGEPOLE PINE
C     8 = NORTHERN IDAHO VARIANT INT-133 TABLE 8: ENGELMANN SPRUCE
C     9 = NORTHERN IDAHO VARIANT INT-133 TABLE 8: SUBALPINE FIR
C    10 = NORTHERN IDAHO VARIANT INT-133 TABLE 8: PONDEROSA PINE
C    11 = NORTHERN IDAHO VARIANT INT-133 TABLE 8: LODGEPOLE PINE
C    12 = NORTHERN IDAHO VARIANT INT-133 TABLE 8: LODGEPOLE PINE
C    13 = NORTHERN IDAHO VARIANT INT-133 TABLE 8: WESTERN RED CEDAR
C    14 = NORTHERN IDAHO VARIANT INT-133 TABLE 8: LODGEPOLE PINE
C    15 = PAINE AND HANN TABLE 2: CALIFORNIA BLACK OAK
C    16 = NORTHERN IDAHO VARIANT INT-133 TABLE 8: LODGEPOLE PINE
C    17 = NORTHERN IDAHO VARIANT INT-133 TABLE 8: WESTERN HEMLOCK
C    18 = NORTHERN IDAHO VARIANT INT-133 TABLE 8: MOUNTAIN HEMLOCK
C    19 = NORTHERN IDAHO VARIANT INT-133 TABLE 8: WESTERN HEMLOCK
C
C      WYKOFF, CROOKSTON, STAGE, 1982. USER'S GUIDE TO THE STAND
C        PROGNOSIS MODEL. GEN TECH REP INT-133. OGDEN, UT:
C        INTERMOUNTAIN FOREST AND RANGE EXP STN. 112P.
C----------
      LOGICAL LTHIN
      INTEGER MODE,JCR,ISPC
      REAL CRWDTH,CCFT,P,H,D
      REAL RD1(MAXSP),RD2(MAXSP),RD3(MAXSP),RDA(MAXSP),RDB(MAXSP)
      REAL  B1(MAXSP),B2(MAXSP),B3(MAXSP),B4(MAXSP),B5(MAXSP),B6(MAXSP)
      REAL CL,BAREA
C----------
C  DATA STATEMENTS
C----------
      DATA RD1/
     &     0.03,     0.02,     0.11,     0.04,     0.03,
     &     0.03,  0.01925,     0.03,     0.03,     0.03,
     &  0.01925,  0.01925,     0.03,   .01925,    .0204,
     &  0.01925,     0.03,     0.03,     0.03/
C
      DATA RD2/
     &   0.0167,   0.0148,   0.0333,   0.0270,   0.0215,
     &   0.0238,  0.01676,   0.0173,   0.0216,   0.0180,
     &  0.01676,  0.01676,   0.0238,  0.01676,   0.0246,
     &  0.01676,   0.0215,   0.0215,   0.0215/
C
      DATA RD3/
     &  0.00230,  0.00338,  0.00259,  0.00405,  0.00363,
     &  0.00490,  0.00365,  0.00259,  0.00405,  0.00281,
     &  0.00365,  0.00365,  0.00490,  0.00365,   0.0074,
     &  0.00365,  0.00363,  0.00363,  0.00363/
C
      DATA RDA/
     & 0.009884, 0.007244, 0.017299, 0.015248, 0.011109,
     & 0.008915, 0.009187, 0.007875, 0.011402, 0.007813,
     & 0.009187, 0.009187, 0.008915, 0.009187,      0.0,
     & 0.009187, 0.011109, 0.011109, 0.011109/
C
      DATA RDB/
     &   1.6667,   1.8182,   1.5571,   1.7333,   1.7250,
     &   1.7800,   1.7600,   1.7360,   1.7560,   1.7680,
     &   1.7600,   1.7600,   1.7800,   1.7600,      0.0,
     &   1.7600,   1.7250,   1.7250,   1.7250/
C
C  BIAS CORRECT COEFFICIENTS FOR SPECIES FROM IE VARIANT
C     B1  = BIAS CORRECTION COEFFICIENT
C     B2  = CONSTANT TERM
C     B3  = LOG-NATURAL OF CROWN LENGTH COEFFICIENT
C     B4  = LOG-NATURAL OF DBH COEFFICIENT
C     B5  = LOG-NATURAL OF TOTAL TREE HEIGHT COEFFICIENT
C     B6  = LOG-NATURAL OF BASAL AREA COEFFICIENT
C
      DATA B1/
     &       0.,       0.,       0.,       0.,       0.,
     &       0.,       0.,       0.,       0.,       0.,
     &  1.03992,  1.03992,  1.03597,       0.,       0.,
     &  1.03992,  1.02460,       0.,  1.02460/
C
      DATA B2/
     &       0.,       0.,       0.,       0.,       0.,
     &       0.,       0.,       0.,       0.,       0.,
     &  1.58777,  1.58777,  1.46111,       0.,       0.,
     &  1.58777,  1.35223,       0.,  1.35223/
C
      DATA B3/
     &       0.,       0.,       0.,       0.,       0.,
     &       0.,       0.,       0.,       0.,       0.,
     &  0.30812,  0.30812,  0.26289,       0.,       0.,
     &  0.30812,  0.24844,       0.,  0.24844/
C
      DATA B4/
     &       0.,       0.,       0.,       0.,       0.,
     &       0.,       0.,       0.,       0.,       0.,
     &  0.64934,  0.64934,  0.18779,       0.,       0.,
     &  0.64934,  0.41212,       0.,  0.41212/
C
      DATA B5/
     &       0.,       0.,       0.,       0.,       0.,
     &       0.,       0.,       0.,       0.,       0.,
     & -0.38964, -0.38964,      0.0,       0.,       0.,
     & -0.38964, -0.10436,       0., -0.10436/
C
      DATA B6/
     &       0.,       0.,       0.,       0.,       0.,
     &       0.,       0.,       0.,       0.,       0.,
     &      0.0,      0.0,      0.0,       0.,       0.,
     &      0.0,  0.03539,       0.,  0.03539/
C----------
C  INITIALIZE RETURN VARIABLES.
C----------
      CCFT = 0.
      CRWDTH = 0.
C----------
C  COMPUTE CCF.
C----------
      SELECT CASE (ISPC)
C
      CASE (11,12,13,14,15,16)
      IF (D .GE. 1.0) THEN
        CCFT = RD1(ISPC) + D*RD2(ISPC) + D*D*RD3(ISPC)
      ELSEIF (D .GT. 0.1) THEN
        IF(ISPC .EQ. 15)THEN
          CCFT = D * (RD1(ISPC)+RD2(ISPC)+RD3(ISPC))
        ELSE
          CCFT = RDA(ISPC) * (D**RDB(ISPC))
        ENDIF
      ELSE
        IF(ISPC .EQ. 15)THEN
          CCFT = D * (RD1(ISPC)+RD2(ISPC)+RD3(ISPC))
        ELSE
          CCFT = 0.001
        ENDIF
      ENDIF
C
      CASE(17,19)
        IF (D .GE. 10.0) THEN
          CCFT = RD1(ISPC) + D*RD2(ISPC) + D*D*RD3(ISPC)
        ELSEIF (D .GT. 0.1) THEN
          CCFT = RDA(ISPC) * (D**RDB(ISPC))
        ELSE
          CCFT = 0.001
        ENDIF
C
      CASE DEFAULT
        IF (D.GE.10.0) THEN
          CCFT = RD1(ISPC) + D*RD2(ISPC) + D*D*RD3(ISPC)
        ELSE
          CCFT = RDA(ISPC) * (D**RDB(ISPC))
        ENDIF
C
      END SELECT
C
      CCFT=CCFT*P
C----------
C  COMPUTE CROWN WIDTH
C----------
      SELECT CASE (ISPC)
C
      CASE(11,12,13,16,17,19)
        IF(JCR .LE. 0) GO TO 100
        CL = FLOAT(JCR)*H*.01
        BAREA = BA
        IF(LTHIN.OR.LFIRE) BAREA=OLDBA
        IF(BAREA.LE.0. .OR. CL.LE.0. .OR. H.LE.0. .OR.D.LE.0.)GO TO 100
        IF(BAREA.LE.1.)BAREA=1.
        CRWDTH=B1(ISPC)*EXP(B2(ISPC)+B3(ISPC)*ALOG(CL)+B4(ISPC)*
     &           ALOG(D)+B5(ISPC)*ALOG(H)+B6(ISPC)*ALOG(BAREA))
  100   CONTINUE
        IF(CRWDTH .LT. 0.1)CRWDTH=0.1
C
      CASE DEFAULT
        CRWDTH = SQRT(CCFT/0.001803)
      END SELECT
C
        IF(CRWDTH .GT. 99.9) CRWDTH=99.9
C
      RETURN
      END
