      SUBROUTINE SMHGDG (IT,ISPC,H,D,HG5,DG5,ICYC,JOSTND,DEBUG,MODE)
      IMPLICIT NONE
C----------
C  **SMHGDG--WC  DATE OF LAST REVISION:  08/07/13
C----------
C  THIS ROUTINE CALCULATES THE HEIGHT GROWTH AND DIAMETER GROWTH
C  OF SMALL TREES (D<3.0 IN). BASED ON WORK BY PETER GOULD AND
C  CONNIE HARRINGTON, INITIAL MODELS FOR SMALL-TREE DIAMETER
C  GROWTH (OCTOBER 31, 2011)
C  CALLED FROM **ESSUBH **REGENT
C
C  INPUT VARIABLES
C  IT     - TREE NUMBER
C  ISPC   - SPECIES 
C  H      - BEGINING HEIGHT
C  D      - BEGINING DIAMETER
C  MODE   - = 0 IF CALL IS FROM ESSUBH
C           = 1 IF CALL IS FROM REGENT 
C
C  RETURN VARIABLES
C  HG5    - 5 YEAR HEIGHT INCREMANT
C  DG5    - 5 YEAR DIAMETER INCREMENT
COMMONS
C
      INCLUDE 'PRGPRM.F77'
C
      INCLUDE 'PLOT.F77'
C
      INCLUDE 'ARRAYS.F77'
C
      INCLUDE 'VARCOM.F77'
C
C
COMMONS
C
      LOGICAL DEBUG
      INTEGER I,IT,J,ICYC,JOSTND,MODE,ISPC
      REAL    H,HG,DGS,DG1,HBH,DBHA,D,BAS,AVHT
      REAL    DMAX(MAXSP),ALPHA(0:10,MAXSP),BETA(MAXSP),HG5,DG5
      REAL    PTBA,PTBA2,PTBAL,PTBAL2,BOOST,CR,RELHT,RELHT2,SI
      CHARACTER VVER*7
C
C  DATA STATEMENTS
C
      DATA DMAX /
     & 1.704, 1.496, 1.639, 1.196, 1.515,
     & 3.396, 2.939, 1.540, 1.683, 1.885,
     & 1.653, 1.798, 2.474, 2.474, 1.798,
     & 5.373, 2.849, 2.790, 3.419, 1.383,
     & 3.094, 3.094, 2.011, 2.166, 3.094,
     & 2.475, 3.713, 0.986, 1.219, 0.623,
     & 0.807, 0.586, 0.860, 1.003, 1.890,
     & 2.166, 2.166, 0.000, 5.373/
C
      DATA BETA /
     & 0.247400, 0.217500, 0.179700, 0.205600, 0.216800,
     & 0.216800, 0.282200, 0.216800, 0.281500, 0.170400,
     & 0.168200, 0.216800, 0.216800, 0.216800, 0.236900,
     & 0.163500, 0.172700, 0.182900, 0.172700, 0.302900,
     & 0.216800, 0.216800, 0.216800, 0.216800, 0.216800,
     & 0.216800, 0.216800, 0.216800, 0.216800, 0.216800,
     & 0.216800, 0.168200, 0.216800, 0.216800, 0.216800,
     & 0.216800, 0.216800, 0.000000, 0.163500/
C
      DATA ((ALPHA(I,J), I=0,9),J=1,1) /
     & 2.94450, 0.00000, 0.00000, 0.00680, 0.00000, 0.00000,-0.18950,
     & 0.00000,-1.40490,-0.01680/
      DATA ((ALPHA(I,J), I=0,9),J=2,2) /
     & 1.75360, 0.00000, 0.29280, 0.00090, 0.00000,-0.04460,-2.03490,
     & 0.00000,-1.38390,-0.00330/
      DATA ((ALPHA(I,J), I=0,9),J=3,3) /
     & 2.35710, 0.00520, 0.00000, 0.00060, 0.00000,-0.42690,-1.22190,
     & 0.00000, 0.00000,-0.01700/
      DATA ((ALPHA(I,J), I=0,9),J=4,4) /
     & 2.58390, 0.00000, 0.04100, 0.00200, 0.00000,-0.01520,-2.20600,
     & 0.00000,-0.59150,-0.00090/
      DATA ((ALPHA(I,J), I=0,9),J=5,5) /
     & 2.47430, 0.00000, 0.00000, 0.00320, 0.00000,-0.89340,-2.27090,
     & 0.00000,-1.06900, 0.00000/
      DATA ((ALPHA(I,J), I=0,9),J=6,6) /
     & 3.82050, 0.00000, 0.05230, 0.00510, 0.00000,-0.41020,-1.69680,
     & 0.00000,-1.40010,-0.01090/
      DATA ((ALPHA(I,J), I=0,9),J=7,7) /
     & 0.33760, 0.00000, 0.00000, 0.01010, 0.00000, 0.00000, 0.00000,
     & 0.00000, 0.00000,-0.00430/
      DATA ((ALPHA(I,J), I=0,9),J=8,8) /
     &-2.02160, 0.00630, 0.00000, 0.00000, 0.71750, 0.00000, 0.00000,
     & 0.00000, 0.00000, 0.00000/
      DATA ((ALPHA(I,J), I=0,9),J=9,9) /
     & 0.59960, 0.00000, 0.00000, 0.00800, 0.00000, 0.00000, 0.00000,
     &-1.04790, 0.00000, 0.00000/
      DATA ((ALPHA(I,J), I=0,9),J=10,10) /
     & 0.04520, 0.00800, 0.00000, 0.00710, 0.00000, 0.00000, 0.00000,
     & 0.00000, 0.00000, 0.00000/
      DATA ((ALPHA(I,J), I=0,9),J=11,11) /
     & 1.74000, 0.00000, 0.37180, 0.00270, 0.00000,-0.17120,-2.13590,
     & 0.00000,-0.72660,-0.00740/
      DATA ((ALPHA(I,J), I=0,9),J=12,12) /
     & 1.84510, 0.00000, 0.00000, 0.01670, 0.00000,-1.47370, 0.00000,
     & 0.00000,-0.41030,-0.01120/
      DATA ((ALPHA(I,J), I=0,9),J=13,13) /
     & 3.80850, 0.00000, 0.00000, 0.00230, 0.00000,-0.42650,-2.09130,
     & 0.00000,-1.39320,-0.00930/
      DATA ((ALPHA(I,J), I=0,9),J=14,14) /
     & 3.80850, 0.00000, 0.00000, 0.00230, 0.00000,-0.42650,-2.09130,
     & 0.00000,-1.39320,-0.00930/
      DATA ((ALPHA(I,J), I=0,9),J=15,15) /
     & 1.84510, 0.00000, 0.00000, 0.01670, 0.00000,-1.47370, 0.00000,
     & 0.00000,-0.41030,-0.01120/
      DATA ((ALPHA(I,J), I=0,9),J=16,16) /
     & 2.44730, 0.00000, 0.00000, 0.00980, 0.00000,-0.42900,-0.17100,
     & 0.00000,-0.18790,-0.01100/
      DATA ((ALPHA(I,J), I=0,9),J=17,17) /
     & 2.95270, 0.00000, 0.00000, 0.00660, 0.00000, 0.00000,-0.47340,
     & 0.00000,-0.73940,-0.02070/
      DATA ((ALPHA(I,J), I=0,9),J=18,18) /
     & 1.68150, 0.00000, 0.00000, 0.00680, 0.00000, 0.00000, 0.00000,
     & 0.00000,-0.60490,-0.01210/
      DATA ((ALPHA(I,J), I=0,9),J=19,19) /
     & 2.95270, 0.00000, 0.00000, 0.00660, 0.00000, 0.00000,-0.47340,
     & 0.00000,-0.73940,-0.02070/
      DATA ((ALPHA(I,J), I=0,9),J=20,20) /
     & 2.67620, 0.00240, 0.00000, 0.00060, 0.00000,-0.43090,-1.62050,
     & 0.00000,-0.59300,-0.00510/
      DATA ((ALPHA(I,J), I=0,9),J=21,21) /
     &-1.24210, 0.01240, 0.00000, 0.00000, 0.41610, 0.00000, 0.00000,
     & 0.00000, 0.00000, 0.00000/
      DATA ((ALPHA(I,J), I=0,9),J=22,22) /
     & 1.45930, 0.00000, 0.00000, 0.00850, 0.00000,-0.60000, 0.00000,
     & 0.00000,-1.22800, 0.00000/
      DATA ((ALPHA(I,J), I=0,9),J=23,23) /
     &-1.19000, 0.01580, 0.00000, 0.00000, 0.66000, 0.00000, 0.00000,
     & 0.00000, 0.00000, 0.00000/
      DATA ((ALPHA(I,J), I=0,9),J=24,24) /
     &-1.24210, 0.01240, 0.00000, 0.00000, 0.78130, 0.00000, 0.00000,
     & 0.00000, 0.00000, 0.00000/
      DATA ((ALPHA(I,J), I=0,9),J=25,25) /
     &-1.24210, 0.01240, 0.00000, 0.00000, 0.63820, 0.00000, 0.00000,
     & 0.00000, 0.00000, 0.00000/
      DATA ((ALPHA(I,J), I=0,9),J=26,26) /
     &-1.24210, 0.01240, 0.00000, 0.00000, 0.60130, 0.00000, 0.00000,
     & 0.00000, 0.00000, 0.00000/
      DATA ((ALPHA(I,J), I=0,9),J=27,27) /
     &-1.24210, 0.01240, 0.00000, 0.00000, 0.60130, 0.00000, 0.00000,
     & 0.00000, 0.00000, 0.00000/
      DATA ((ALPHA(I,J), I=0,9),J=28,28) /
     &-2.19100, 0.00000, 0.00000, 0.00000, 0.71910,-3.13210, 0.00000,
     & 0.00000, 0.00000, 0.00000/
      DATA ((ALPHA(I,J), I=0,9),J=29,29) /
     & 0.37550, 0.01200, 0.00000, 0.00000, 0.00000, 0.00000, 0.00000,
     & 0.00000, 0.00000, 0.00000/
      DATA ((ALPHA(I,J), I=0,9),J=30,30) /
     & 1.05270, 0.00000, 0.35800, 0.00190, 0.00000, 0.00000,-0.60080,
     & 0.00000,-0.74510,-0.01010/
      DATA ((ALPHA(I,J), I=0,9),J=31,31) /
     & 2.49490, 0.00000, 0.00000, 0.00490, 0.00000,-0.20850,-1.70010,
     & 0.00000,-0.79520,-0.01770/
      DATA ((ALPHA(I,J), I=0,9),J=32,32) /
     &-0.80850, 0.00000, 0.50010, 0.00000, 0.00000, 0.00000, 0.00000,
     & 0.00000, 0.00000,-0.00810/
      DATA ((ALPHA(I,J), I=0,9),J=33,33) /
     & 1.51560, 0.00000, 0.00000, 0.00120, 0.00000, 0.00000,-0.54780,
     & 0.00000,-0.61230, 0.00000/
      DATA ((ALPHA(I,J), I=0,9),J=34,34) /
     &-3.83450, 0.00000, 0.00000, 0.00000, 1.07010, 0.00000, 0.00000,
     & 0.00000, 0.00000, 0.00000/
      DATA ((ALPHA(I,J), I=0,9),J=35,35) /
     & 3.55210, 0.00000, 0.00000, 0.00020, 0.00000, 0.00000,-0.59320,
     & 0.00000,-0.50290,-0.00380/
      DATA ((ALPHA(I,J), I=0,9),J=36,36) /
     &-1.24210, 0.01240, 0.00000, 0.00000, 0.73120, 0.00000, 0.00000,
     & 0.00000, 0.00000, 0.00000/
      DATA ((ALPHA(I,J), I=0,9),J=37,37) /
     &-1.24210, 0.01240, 0.00000, 0.00000, 0.65980, 0.00000, 0.00000,
     & 0.00000, 0.00000, 0.00000/
      DATA ((ALPHA(I,J), I=0,9),J=38,38) /
     & 0.00000, 0.00000, 0.00000, 0.00000, 0.00000, 0.00000, 0.00000,
     & 0.00000, 0.00000, 0.00000/
      DATA ((ALPHA(I,J), I=0,9),J=39,39) /
     & 2.44730, 0.00000, 0.00000, 0.00980, 0.00000,-0.35750,-0.17100,
     & 0.00000,-0.18790,-0.01100/
C----------
C  CALCULATE 5 YEAR HEIGHT AND DIAMETER GROWTH
C----------

      IF(DEBUG)THEN
        WRITE(JOSTND,*)' ENTERING SMHGDG-IT,H,D,',
     &  'AVH,FINT= ',IT,H,D,AVH,FINT
        IF(IT.GT.0)WRITE(JOSTND,*)' ICR(IT),PCT(IT),PTBAA(ITRE(IT))= ',
     &   ICR(IT),PCT(IT),PTBAA(ITRE(IT))
        WRITE(JOSTND,*)' ITRE= ',ITRE
        WRITE(JOSTND,*)' PTBAA= ',PTBAA     
      ENDIF
C
      IF((MODE.EQ.0).OR.(IT.LE.0))THEN
        CR=0.5
        BAS=0.
        PTBAL=0.
        PTBA=0.
        RELHT=0.
        AVHT=(5.0/FINT)*AVH +((FINT-5.0)/FINT)*ATAVH
        IF(AVHT .GT. 0.0) RELHT=H/AVHT
        IF(RELHT .GT. 1.5)RELHT=1.5
      ELSE
        CR=ICR(IT)*0.01
        PTBAL = PTBALT(IT) 
        PTBA = PTBAA(ITRE(IT))	
        BAS = PTBA-PTBAL
        RELHT = 0.0
        AVHT=AVH
        IF(MODE.EQ.1)AVHT=(5.0/FINT)*AVH +((FINT-5.0)/FINT)*ATAVH
        IF(AVHT.GT.0.0) RELHT=H/AVHT
        IF(RELHT.GT.1.5)RELHT=1.5
      ENDIF
C----------
C  INITIALIZE TRANSFORMED VARIABLES
C  IF DOUGLAS-FIR IN WC TRANSLATE SI
C  CURTIS TO KING FOR THIS EQN
C----------
      CALL VARVER(VVER)
      SI= SITEAR(ISPC)
      IF ((ISPC.EQ.16).AND.(VVER(:2).EQ.'WC')) THEN
        SI=5.21486+0.66486*SI
      ENDIF
      PTBAL2 = LOG(PTBAL + 2.71)
      PTBA2 = LOG(PTBA + 2.71)
      RELHT2 = SQRT(RELHT)
      BOOST = 1/(1+EXP(-3.1 + 0.18*PTBA))
C
      HBH=H
      IF(H.GE.4.5)HBH=4.5
      DBHA=D+BETA(ISPC)*HBH
      DGS=DMAX(ISPC)/(1.+EXP(ALPHA(0,ISPC)+ALPHA(1,ISPC)*PTBA
     &   +ALPHA(2,ISPC)*PTBA2+ALPHA(3,ISPC)*PTBAL+ALPHA(4,ISPC)*PTBAL2
     &   +ALPHA(5,ISPC)*BOOST+ALPHA(6,ISPC)*CR + ALPHA(7,ISPC)*RELHT
     &   +ALPHA(8,ISPC)*RELHT2 +ALPHA(9,ISPC)*SI))
      HG5=DGS/BETA(ISPC)
      IF(H.LT.4.5)THEN
        IF((H+HG5).GE.4.5)THEN
           DG5=BETA(ISPC)*(H+HG5-4.5)
        ELSE
            DG5=0.
        ENDIF
      ELSE
        DG5=DGS
      ENDIF
C
      IF (DEBUG)THEN
        WRITE(JOSTND,*)'BETA(ISPC),DMAXISPC),(ALPHA(I,ISPC),I=0,6)= ',
     &  BETA(ISPC),DMAX(ISPC),(ALPHA(I,ISPC),I=0,6)
        WRITE(JOSTND,*)' LEAVING SMHGDG, ICYC,IT,D,DGS,DG5,',
     &  'HG5,DBHA,HBH,ISPC= ',ICYC,IT,D,DGS,DG5,HG5,DBHA,HBH,ISPC
        WRITE(JOSTND,*)' H,PTBAL,CR,BAS,PTBA,RELHT,AVH,SI= ',
     &  H,PTBAL,CR,BAS,PTBA,RELHT,AVHT,SI
      ENDIF
      RETURN
      END
