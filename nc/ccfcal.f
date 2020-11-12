      SUBROUTINE CCFCAL(ISPC,D,H,JCR,P,LTHIN,CCFT,CRWDTH,MODE)
      IMPLICIT NONE
C----------
C NC $Id$
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
C
C
      INCLUDE 'PRGPRM.F77'
C
C
      INCLUDE 'PLOT.F77'
C
C
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
C      RDB -- EXPONENT.  CCF(I) = RDA * (DBH**RDB)
C
C
C  CROWN WIDTH EQUATIONS FOR REGION 5:
C  FROM WARBINGTON/LEVITAN & DIXON --- SEE DOCUMENTATION IN
C                                      SUBROUTINE **R5CRWD**
C
C  CROWN WIDTH EQUATIONS FOR REGION 6:
C  FROM DONNELLY --- SEE DOCUMENTATION IN SUBROUTINE **R6CRWD**
C
C  SPECIES ORDER:
C  1=OC  2=SP  3=DF  4=WF  5=M   6=IC  7=BO  8=TO  9=RF 10=PP 11=OH
C
C  SOURCES OF CCF COEFFICIENTS:
C     1 = PAINE AND HANN TABLE 2: DOUGLAS-FIR
C     2 = PAINE AND HANN TABLE 2: SUGAR PINE
C     3 = PAINE AND HANN TABLE 2: DOUGLAS-FIR
C     4 = PAINE AND HANN TABLE 2: WHITE/GRAND FIR
C     5 = PAINE AND HANN TABLE 2: MADRONE
C     6 = PAINE AND HANN TABLE 2: INCENSE CEDAR
C     7 = PAINE AND HANN TABLE 2: CALIFORNIA BLACK OAK
C     8 = PAINE AND HANN TABLE 2: TANOAK
C     9 = PAINE AND HANN TABLE 2: RED FIR
C    10 = PAINE AND HANN TABLE 2: PONDEROSA PINE
C    11 = PAINE AND HANN TABLE 2: TANOAK
C
C      PAINE AND HANN, 1982. MAXIMUM CROWN WIDTH EQUATIONS FOR
C        SOUTHWESTERN OREGON TREE SPECIES. RES PAP 46, FOR RES LAB
C        SCH FOR, OSU, CORVALLIS. 20PP.
C----------
      LOGICAL LTHIN
      REAL RD1(MAXSP),RD2(MAXSP),RD3(MAXSP),RDA(MAXSP),RDB(MAXSP)
      REAL CRWDTH,CCFT,P,H,D
      INTEGER MODE,JCR,ISPC
      INTEGER IDANUW
      LOGICAL LDANUW
C
      DATA RD1/
     & .0388, .0392, .0388, .0690,.0212,.0194,.0204  ,.0356,.0172,
     & .0219, .0356/
      DATA RD2/
     & .0269, .0180, .0269, .0225,.0167, .0142, .0246,  .0273,
     & .00877, .0169, .0273/
      DATA RD3/
     & .00466, .00207, .00466, .00183, .00330, .00261, .0074 ,
     & .00524, .00112, .00325, .00524/
      DATA RDA/
     & 0.009884,0.007244,0.017299,0.015248,0.011109
     &, 0.008915,0.009187,0.007875,0.011402,0.007813,0.011109/
      DATA RDB/
     &   1.6667,  1.8182,  1.5571,  1.7333,  1.7250,
     &   1.7800,  1.7600,  1.7360,  1.7560,  1.7780,  1.7250/
C----------
C  DUMMY ARGUMENT NOT USED WARNING SUPPRESSION SECTION
C----------
      IDANUW = JCR
      LDANUW = LTHIN
C
C----------
C  INITIALIZE RETURN VARIABLES.
C----------
      CCFT = 0.
      CRWDTH = 0.
C----------
C  COMPUTE CCF
C----------
      IF(MODE.EQ.1) THEN
        IF (D .GE. 1.0) THEN
          CCFT= RD1(ISPC)+D*RD2(ISPC)+D*D*RD3(ISPC)
        ELSEIF (D .GT. 0.1) THEN
          CCFT=RDA(ISPC)*D**RDB(ISPC)
        ELSE
          CCFT=.001
        ENDIF
        CCFT = CCFT * P
      ENDIF
C----------
C  COMPUTE CROWN WIDTH
C----------
      IF(MODE.EQ.2) THEN
        IF(IFOR.EQ.4 .OR. IFOR.EQ.7) GO TO 100
        CALL R5CRWD (ISPC,D,H,CRWDTH)
        GO TO 200
C----------
C  REGION 6 CROWN WIDTH
C----------
  100   CONTINUE
        CALL R6CRWD (ISPC,D,H,CRWDTH)
C----------
C  LIMIT CROWN WIDTH FOR PRINTING ON TREELIST.
C----------
  200   CONTINUE
        IF(CRWDTH .GT. 99.9) CRWDTH=99.9
      ENDIF
C
      RETURN
      END
