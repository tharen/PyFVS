      SUBROUTINE MORTS
      IMPLICIT NONE
C----------
C  **MORTS--SN   DATE OF LAST REVISION:  09/09/13
C----------
C  THIS SUBROUTINE COMPUTES PERIODIC MORTALITY RATES FOR
C  EACH TREE RECORD AND THEN REDUCES THE NUMBER OF TREES/ACRE
C  REPRESENTED BY THE TREE RECORD.
C  THIS ROUTINE IS CALLED FROM **TREGRO** WHEN CYCLING FOR GROWTH
C  PREDICTION.  ENTRY **MORCON** IS CALLED TO LOAD SITE DEPENDENT
C  CONSTANTS.
C  SDI BASED MORTALITY IS USED AS LONG AS THE STAND DIAMETER METRIC
C  (QMD OR REINEKE'S DIAMETER)IS LESS THAN 10 INCHECS, AT WHICH TIME
C  BAMAX BASED MORTALITY TAKES OVER. IF NOT SET BY THE USER, BAMAX IS
C  DETERMINED FROM MAX SDI AT 10" DBH.
C----------
COMMONS
C
C
      INCLUDE 'PRGPRM.F77'
C
C
      INCLUDE 'ARRAYS.F77'
C
C
      INCLUDE 'CONTRL.F77'
C
C
      INCLUDE 'OUTCOM.F77'
C
C
      INCLUDE 'PLOT.F77'
C
C
      INCLUDE 'COEFFS.F77'
C
C
      INCLUDE 'ESTREE.F77'
C
C
      INCLUDE 'MULTCM.F77'
C
C
      INCLUDE 'PDEN.F77'
C
C
      INCLUDE 'VARCOM.F77'
C
C
      INCLUDE 'WORKCM.F77'
C
C
COMMONS
C----------
C  DEFINITIONS:
C
C     PMSC -- CONSTANT TERMS FOR EACH SPECIES FOR THE BACKGROUND
C             MORTALITY
C             RATE EQUATION (B0).
C      PMD -- COEFFICIENTS FOR EACH SPECIES FOR THE DIAMETER TERM
C             IN THE BACKGROUND MORTALITY RATE EQUATION (B1).
C    PMDSQ -- COEFFICIENTS FOR EACH SPECIES FOR THE DIAMETER SQUARED
C             TERM IN THE BACKGROUND MORTALITY RATE EQUATION (B2).
C       C0 -- CONSTANT TERM FOR SDI RELATIONSHIP
C             LN(T) = LN(C0)-1.605 *LN(QMD)
C        I -- TREE SUBSCRIPT.
C        D -- TREE DIAMETER.
C       RI -- ESTIMATED ANNUAL BACKGROUND MORTALITY RATE BASED ON
C             HAMILTON'S
C       RN -- MORTALITY RATE THAT WILL MATCH THE TREND IN TREES PER
C             ACRE THAT IS PREDICTED FROM THE SDI RELATIONSHIP
C      RIP -- WEIGHTED AVERAGE MORTALITY RATE BASED ON TREE DBH, RI,
C             AND RN.
C        P -- NUMBER OF TREES PER ACRE REPRESENTED BY A TREE.
C      WKI -- SCALAR USED FOR CALCULATION AND TEMPORARY STORAGE
C             OF TREE MORTALITY (TREES/ACRE).
C----------
      REAL PRM(6)
      INTEGER MYACTS(2)
      REAL PMSC(MAXSP),PMD(MAXSP),TN10TMP
      REAL CREDIT,TEMP,XMORE,ADJFAC,QMDNEW,BANEW,TNEW,DQ10N,SD2SQN
      REAL TN,SUMTRE,VLOS,PRES,X,RIP,RI,WKI,B1,B0,D1,D2,XMORT,RN,DIFF
      REAL TPRIME,CEPT,SLP,T85M,D85M,T55M,D55M,TEM,TN10,T55D10,SDIFIA
      REAL T85D10,TMD10,T55D0,T85D0,TMD0,CONST,DELTBA,DQ10
      REAL CIOBDS,BRATIO,BARK,D,P,SD2SQ,T,TREEIT,DQ0,SUMKIL,G
      INTEGER J,IX,IG,IULIM,IGRP,KBIG,KPOINT,IXF
      INTEGER IP,ITODO,KNT2,IDMFLG,KSPC,IPATH,IS,I1,I2,I3,ISPC
      INTEGER KNT,NTODO,I,NP,IACTK,IDATE,ISPCC,IPASS
      LOGICAL DEBUG,LINCL
      REAL TMMSB,T85MSB,TMORE,TEMEFF,TPACLS,DBHEND,BADEAD
      REAL DIA0,D10,DR0,DR10,SUMDR0,SUMDR10,SUMDR10N,DR10N,D10N,SDQ0
      DATA MYACTS/94,97/
C----------
C  DATA STATEMENTS.
C  BACKGROUND MORTALITY CONSTANTS
C----------
      DATA PMSC /
     1   5.1676998,   9.6942997,   5.1676998,   5.5876999,   5.5876999,
     2   5.5876999,   5.1676998,   5.5876999,   5.5876999,   5.5876999,
     3   5.5876999,   5.5876999,   5.5876999,   5.5876999,   5.5876999,
     4   5.5876999,   5.1676998,   5.1676998,   5.1676998,   5.1676998,
     5   5.1676998,   5.1676998,   5.1676998,   5.9617000,   5.1676998,
     6   5.1676998,   5.9617000,   5.9617000,   5.9617000,   5.1676998,
     7   5.1676998,   5.1676998,   5.1676998,   5.1676998,   5.9617000,
     8   5.9617000,   5.1676998,   5.9617000,   5.1676998,   5.1676998,
     9   5.1676998,   5.9617000,   5.9617000,   5.9617000,   5.9617000,
     O   5.1676998,   5.9617000,   5.1676998,   5.9617000,   5.1676998,
     A   5.9617000,   5.1676998,   5.9617000,   5.1676998,   5.9617000,
     B   5.1676998,   5.1676998,   5.1676998,   5.9617000,   5.9617000,
     C   5.9617000,   5.9617000,   5.9617000,   5.9617000,   5.9617000,
     D   5.9617000,   5.9617000,   5.1676998,   5.9617000,   5.9617000,
     E   5.9617000,   5.9617000,   5.9617000,   5.9617000,   5.9617000,
     F   5.9617000,   5.9617000,   5.9617000,   5.9617000,   5.1676998,
     G   5.1676998,   5.1676998,   5.1676998,   5.1676998,   5.1676998,
     H   5.1676998,   5.1676998,   5.5876999,   5.9617000,   5.9617000/


      DATA PMD /
     1  -0.0077681,  -0.0127328,  -0.0077681,  -0.0053480,  -0.0053480,
     2  -0.0053480,  -0.0077681,  -0.0053480,  -0.0053480,  -0.0053480,
     3  -0.0053480,  -0.0053480,  -0.0053480,  -0.0053480,  -0.0053480,
     4  -0.0053480,  -0.0077681,  -0.0077681,  -0.0077681,  -0.0077681,
     5  -0.0077681,  -0.0077681,  -0.0077681,  -0.0340128,  -0.0077681,
     6  -0.0077681,  -0.0340128,  -0.0340128,  -0.0340128,  -0.0077681,
     7  -0.0077681,  -0.0077681,  -0.0077681,  -0.0077681,  -0.0340128,
     8  -0.0340128,  -0.0077681,  -0.0340128,  -0.0077681,  -0.0077681,
     9  -0.0077681,  -0.0340128,  -0.0340128,  -0.0340128,  -0.0340128,
     O  -0.0077681,  -0.0340128,  -0.0077681,  -0.0340128,  -0.0077681,
     A  -0.0340128,  -0.0077681,  -0.0340128,  -0.0077681,  -0.0340128,
     B  -0.0077681,  -0.0077681,  -0.0077681,  -0.0340128,  -0.0340128,
     C  -0.0340128,  -0.0340128,  -0.0340128,  -0.0340128,  -0.0340128,
     D  -0.0340128,  -0.0340128,  -0.0077681,  -0.0340128,  -0.0340128,
     E  -0.0340128,  -0.0340128,  -0.0340128,  -0.0340128,  -0.0340128,
     F  -0.0340128,  -0.0340128,  -0.0340128,  -0.0340128,  -0.0077681,
     G  -0.0077681,  -0.0077681,  -0.0077681,  -0.0077681,  -0.0077681,
     H  -0.0077681,  -0.0077681,  -0.0053480,  -0.0340128,  -0.0340128/
C   SPECIES ORDER
C----------
C  INITIALIZE.
C----------
C  DQ0     = START OF CYCLE MEAN SQUARE DIAMETER
C  DR0     = START OF CYCLE REINEKE'S DIAMETER
C  T       = TREES/ACRE AT START OF CYCLE
C  DQ10    = QUADRATIC DBH AT END OF CYCLE IF TREES/ACRE IS HELD CONSTANT
C  DR10    = REINEKE'S DBH AT END OF CYCLE IF TREES/ACRE IS HELD CONSTANT
C  TN10    = PREDICTED TREES/ACRE AT THE END OF THE CYCLE
C  RN      = TREES/ACRE LOSS EXPRESSED AS PER YEAR LOSS
C-----------
C  SEE IF WE NEED TO DO SOME DEBUG.
C-----------
      CALL DBCHK (DEBUG,'MORTS',5,ICYC)
      IF(DEBUG)WRITE(JOSTND,9000)ICYC
 9000 FORMAT('ENTERING SUBROUTINE MORTS  CYCLE =',I4)
C      IF(DEBUG)WRITE(JOSTND,*)'IN MORTS AFTER 9000, T ICYC,=',T,ICYC
C---------
C  CALCULATE DYNAMIC STOCKING LEVELS AND ESTIMATE FOREST TYPE.
C  IF USER HAS SPECIFIED THAT INPUT FOREST TYPE IS TO BE USED
C  THROUGHOUT THE SIMULATION, OR INCOMMING STAND CAUSED SDIMAX
C  SHIFT THEN SKIP CALL TO FORTYP
C---------
      IF((ICYC .GE. 1) .AND. (.NOT. LFLAGV)) THEN
        IF (.NOT. LFIXSD) THEN
C----------
C  RECALCULATE FOREST TYPE AND LOAD NEW SDIMAX VALUES IF NOT FIXED
C----------
      IXF=4
      CALL FORTYP(IXF,SDIFIA)
        IF (DEBUG) WRITE(JOSTND,*)'IFORTP= ',IFORTP,' ICYC= ',ICYC,
     1' LFLAGV= ',LFLAGV,' LFIXSD= ',LFIXSD,' LBAMAX= ',LBAMAX,
     2' BAMAX= ',BAMAX
C----------
C  IF USER SPECIFIED SDIMAX OR BAMAX, THEN MODIFY SDIDEF
C----------
          DO 4 I=1,MAXSP
          IF(MAXSDI(I) .GE. 1) GO TO 4
          IF (LBAMAX)THEN
            SDIDEF(I)=BAMAX/(0.5454154*(PMSDIU/100.))
          ELSE
            SDIDEF(I)=SDIFIA
          ENDIF
    4     CONTINUE
        ENDIF
      ENDIF
C----------
C  INITIALIZATIONS
C----------
      TREEIT= 0.
      KNT=0
C-----------
C  PROCESS MORTMULT KEYWORD.
C-----------
      CALL OPFIND(1,MYACTS(1),NTODO)
      IF(NTODO .EQ. 0)GO TO 25
      DO 24 I=1,NTODO
      CALL OPGET(I,4,IDATE,IACTK,NP,PRM)
      CALL OPDONE(I,IY(ICYC))
      ISPCC=IFIX(PRM(1))
      IF(ISPCC .EQ. 0)GO TO 21
      XMMULT(ISPCC)=PRM(2)
      XMDIA1(ISPCC)=PRM(3)
      XMDIA2(ISPCC)=PRM(4)
      GO TO 24
   21 CONTINUE
      DO 22 ISPCC=1,MAXSP
      XMMULT(ISPCC)=PRM(2)
      XMDIA1(ISPCC)=PRM(3)
      XMDIA2(ISPCC)=PRM(4)
   22 CONTINUE
   24 CONTINUE
   25 CONTINUE
      IF(DEBUG)WRITE(JOSTND,9010)ICYC,RMSQD,T
 9010 FORMAT('IN MORTS 9010 ICYC,RMSQD,T= ',
     & I5,2F10.2)
      IF(DEBUG)WRITE(JOSTND,*)'IN MORTS AFTER 9010, T ICYC,=',T,ICYC
       IF(RMSQD .EQ. 0.) THEN
        CEPMRT=0.
        SLPMRT=0.
      ENDIF
C----------
C IF BARE GROUND PLANT, LIMITS WERE NOT ADJUSTED FROM A PERCENT TO A
C PROPORTION IN CRATET, ADJUST THEM HERE.
C----------
      IF(PMSDIL .GT. 1.0)PMSDIL = PMSDIL/100.
      IF(PMSDIU .GT. 1.0)PMSDIU = PMSDIU/100.
C----------
C INITIALIZE STARTING VALUES.
C DQ0, CALCULATED BELOW, IS THE SAME AS RMSQD THAT IS CALCULATED IN DENSE
C WHEN DBHSDI=0.
C----------
      IPASS=0
      SUMKIL= 0.0
C----------
C  ESTIMATE QUADRATIC MEAN DIAMETER 10 YEARS HENCE.
C----------
      T=0.0
      DQ0=0.
      SDQ0=0.
      SD2SQ=0.
      SUMDR0=0.
      SUMDR10=0.
      DR0=0.
      DR10=0.
      DO 20 ISPC=1,MAXSP
C      IF(DEBUG)WRITE(JOSTND,*)'IN MORTS AFTER DO 20, T ICYC,ISPC=',T,
C     &ICYC,ISPC
      I1=ISCT(ISPC,1)
      IF(I1 .LE. 0)GO TO 20
      I2=ISCT(ISPC,2)
      DO 12 I3=I1,I2
C      IF(DEBUG)WRITE(JOSTND,*)'IN MORTS AFTER DO 12, T ICYC',T,ICYC
      I=IND1(I3)
      P=PROB(I)
      IS=ISP(I)
      WK2(I) = 0.0
      D=DBH(I)
      IF(LZEIDE.AND.(D.LT.DBHZEIDE))GO TO 12    ! BRANCH IF D IS LT MIN DBH
      IF(.NOT.LZEIDE.AND.(D.LT.DBHSTAGE))GO TO 12
      BARK=BRATIO(IS,D,HT(I))
      G = (DG(I)/BARK) * (FINT/5.0)
      CIOBDS=(2.0*D*G+G*G)
      SD2SQ=SD2SQ+P*(D*D+CIOBDS)
      SDQ0=SDQ0+P*(D)**2.
      IF(LZEIDE)THEN
        SUMDR10=SUMDR10+P*(D+G)**1.605
        SUMDR0=SUMDR0+P*(D)**1.605
      ENDIF
      T=T+P
   12 CONTINUE
   20 CONTINUE
      IF(DEBUG)WRITE(JOSTND,*)'SDQ0,SD2SQ,SUMDR0,SUMDR10= ',
     &SDQ0,SD2SQ,SUMDR0,SUMDR10
C----------
C  SOME EVENTS CAN CHANGE THE TRAJECTORY OF A STAND. FOR EXAMPLE,
C  REGENERATION, THINNING, USER MORTALITY, FIRE, I&P EFFECTS.
C  IF THE TRAJECTORY IS CHANGED,
C  RESET SLOPE AND INTERCEPT FOR CALCULATION.
C  (THE VALUE OF 1. IS TO ALLOW FOR ROUNDING ERROR)
C----------
      IF(ICYC.GT.1 .AND. ABS(T-TPAMRT).GT.1.) THEN
        CEPMRT=0.
        SLPMRT=0.
        IF(DEBUG)WRITE(JOSTND,*)'RESETTING SLOPE,INTERCEPT T,TPAMRT= ',
     &  T,TPAMRT
      ENDIF
C
      IF(T .LT. 1.0) GO TO 45
      DQ0=SQRT(SDQ0/T)
      DQ10=SQRT(SD2SQ/T)
      IF(LZEIDE)THEN
        DR0=(SUMDR0/T)**(1./1.605)
        DR10=(SUMDR10/T)**(1./1.605)
      ENDIF
      IF(DEBUG)WRITE(JOSTND,*)'IN MORTS, T,ISISP,LZEIDE= ',
     *T,ISISP,LZEIDE
      IF(LZEIDE)THEN
        DIA0=DR0
        D10=DR10
      ELSE
        DIA0=DQ0
        D10=DQ10
      ENDIF
   10 CONTINUE
      DELTBA=.005454154*D10*D10*T-BA
C
      IF(DEBUG) WRITE(JOSTND,9020) DQ0,DQ10,T,SD2SQ,DR0,DR10
 9020 FORMAT('IN MORTS  DQ0 = ',F8.2,' DQ10 = ',F10.2,
     &' T = ',F8.2,' SD2SQ = ',F10.2,' DR0= ',F10.2,' DR10= ',F10.2)
C----------
C  IF D IS TOO LOW, NUMERICAL PROBLEMS OCCUR. RESET IF NECESSARY.
C----------
       IF(DIA0 .LT. 0.3) THEN
         D10 = 0.3 + D10 - DIA0
         DIA0  = 0.3
         IF(DEBUG) WRITE(JOSTND,*)'RESETTING DIA0,D10= ',DIA0,D10
       ENDIF
C
C***********************************************************************
C
C THIS SECTION OF CODE COMPUTES MORTALITY IN TERMS OF THE RELATIONSHIP
C T = CONSTANT*(D**-1.605)  WHERE T IS THE TOTAL NUMBER OF TREES
C AND D IS THE QUADRATIC MEAN DIAMETER OR REINEKE DIAMATER.
C
C DEFAULT LIMITS: LO = 55% MAX SDI (PMSDIL); UP = 85% MAX SDI (PMSDIU)
C
C FROM RELATIONSHIPS BETWEEN LN(TREES PER ACRE) VS
C LN(DIAMETER), THE MAXIMUM LINE, 85 PERCENT OF MAXIMUM
C LINE, AND 55 PERCENT OF MAXIMUM LINE, WERE DETERMINED.  TREES PER ACRE
C IS HELD CONSTANT AS LN(D) INCREASES UNTIL THE 55 PERCENT LINE IS
C REACHED.  THEN TREES PER ACRE STARTS DECREASING ACCORDING TO A COMPUTE
C LINEAR FN UNTIL IT REACHES THE 85 PERCENT LINE.  FROM THIS POINT ON,
C TREES PER ACRE FOLLOWS DOWN THE 85 PERCENT LINE.  FOR STANDS WHERE THE
C INITIAL NUMBER OF TREES EXCEEDS THE MAXIMUM LINE, NUMBER OF TREES IS
C DECREASED TO THE MAXIMUM LINE THE FIRST CYCLE, AND TO THE 85 PERCENT L
C THIS SECOND CYCLE.  FOR STANDS WITH AN INITIAL NUMBER OF TREES BETWEEN
C 85 PERCENT LINE AND MAXIMUM LINES, THE NUMBER OF TREES IS DECREASED
C TO THE 85 PERCENT LINE THE FIRST CYCLE.
C
C THIS SECTION OF CODE WAS DEVELOPED BY GARY DIXON FMSC FT COLLINS, CO.
C
C
C DEFINITION OF VARIABLES IMPORTANT TO THIS SECTION -
C
C     T      = TOTAL NUMBER OF TREES
C     ISISP = INDEX OF SPECIES WITH MAXIMUM BASAL AREA
C     TMD0   = MAXIMUM NUMBER OF TREES FOR A GIVEN DQ0 OR DR0
C     T85D0  = 85 PERCENT LEVEL OF TMD0
C     T55D0  = 55 PERCENT LEVEL OF TMD0
C     TMD10  = MAXIMUM NUMBER OF TREES FOR A GIVEN DQ10 OR DR10
C     T85D10 = 85 PERCENT LEVEL OF TMD10
C     T55D10 = 55 PERCENT LEVEL OF TMD10
C     D55M   = DIAMETER VALUE CORRESPONDING TO LN(T) ON THE 55 PERCENT L
C     D85M   = DIAMETER VALUE CORRESPONDING TO THE POINT WHERE THE LINEA
C              INTERSECTS THE 85 PERCENT LINE
C   SLPMRT   = SLOPE COEFFICIENT FOR THE LINEAR FN
C   CEPMRT   = INTERCEPT COEFFICIENT FOR THE LINEAR FN
C     KNT    = COUNTER FOR NUMBER OF ITERATIONS IN DETERMINING COEFFICIE
C     TREEIT = NUMBER OF TREES VALUE USED IN ITERATION PROCESS
C     TEM    = TEMPORARY STORAGE VARIABLE
C     IPATH  = PATH THROUGH LINEAR FN COEFFICIENT SECTION -
C              1 IF ITERATIVE DETERMINATION
C              2 IF STRAIGHT COMPUTATION
C      TMMSB = MAXIMUM NUMBER OF TREES FOR A GIVEN DQ10 OR DR10
C               ACCORDING TO THE MATURE STAND BOUNDARY FUNCTION
C     T85MSB = 85 PERCENT LEVEL OF TMMSB
C----------
C SDIMAX IS USED HERE TO CARRY WEIGHTED SDI MAXIMUM
C----------
      CALL SDICAL(SDIMAX)
      BAMAX = SDIMAX*PMSDIU*0.5454154
      CONST = SDIMAX / 0.02483133
      IF(DEBUG)WRITE(JOSTND,*)'SDIMAX,CONST,BAMAX= ',
     &SDIMAX,CONST,BAMAX
C----------
C IF SDIMAX IS LESS THAN 5, ASSUME CLIMATE HAS CHANGED ENOUGH THAT THE
C SITE WILL NO LONGER SUPPORT TREES, AND KILL ALL EXISTING TREES.
C----------
      IF(SDIMAX .LT. 5)THEN
        TN10=0.
        GO TO 271
      ENDIF
C
      IPATH = 0
      IF(T .GT. 35000.) T=35000.
C----------
C GIVEN DQ0, DETERMINE MAXIMUM TREES, 85 PERCENT LEVEL AND 55 PERCENT LE
C----------
  190 CONTINUE
      TMD0 = CONST * (DIA0 ** (-1.605))
      IF(TMD0 .GT. 35000.0) TMD0 = 35000.0
      T85D0 = TMD0 * PMSDIU
      T55D0 = PMSDIL * TMD0
      IF(DEBUG)WRITE(JOSTND,*)'TMD0,PMSDIU,PMSDIL,T85D0,T55D0= ',
     &TMD0,PMSDIU,PMSDIL,T85D0,T55D0
C----------
C GIVEN DQ10 OR DR10 DETERMINE MAXIMUM TREES, 85 PERCENT LEVEL,
C AND 55 PERCENT L
C----------
      TMD10 = CONST * (D10**(-1.605))
      IF(TMD10 .GT. 35000.0) TMD10 = 35000.0
      T85D10 = TMD10 * PMSDIU
      T55D10 = PMSDIL * TMD10
      IF(DEBUG)WRITE(JOSTND,*)'TMD10,PMSDIU,PMSDIL,T85D10,T55D10= ',
     &TMD10,PMSDIU,PMSDIL,T85D10,T55D10
      IF(DEBUG)WRITE(JOSTND,9040)ICYC,SDIMAX
 9040 FORMAT('IN MORTS ICYC,MAX SDI =',I5,F10.1)
C----------
C IF MSB IS IN EFFECT, THEN COMPUTE NECESSARY MSB PARAMETERS
C----------
      IF(SLPMSB .NE. 0.)THEN
        CEPMSB = ALOG(CONST*(QMDMSB**(-1.605)))-SLPMSB*ALOG(QMDMSB)
      ENDIF
      IF(DEBUG)WRITE(JOSTND,*)'MATURE STAND BOUNDARY SETTINGS, QMDMSB,'
     &,'CEPMSB,SLPMSB,D10= ',QMDMSB,CEPMSB,SLPMSB,D10 
C----------
C IF TOTAL NUMBER OF TREES IS BETWEEN 85 PERCENT AND COMPUTED MAXIMUM,
C KILL BACK TO 85 PERCENT LEVEL.
C----------
      IF(T .LE. T85D0) GO TO 210
      TN10 = T85D10
      GO TO 270
  210 CONTINUE
C----------
C IF TOTAL NUMBER OF TREES IS BETWEEN 55 PERCENT AND 85 PERCENT, FIND
C LINEAR FN COEFFICIENTS (ITERATIVE METHOD) AND KILL ACCORDING TO
C LINEAR FN.
C----------
      IF(T .LE. T55D0) GO TO 240
C----------
C SPECIAL CASE WHERE T IS CLOSE TO THE 85% LINE AT DIA0
C----------
      IF(ABS(T85D0-T).LE.5.)THEN
        TN10=T85D10
        GO TO 270
      ENDIF
      KNT = 1
      TREEIT = T + 0.1 * T
      IPATH = 1
  220 CONTINUE
      TEM = TREEIT
      IF(IPATH .EQ. 2) TEM = T
      IF(DEBUG)WRITE(JOSTND,*)'MORTS 220,TEM,CONST',TEM,CONST
      D55M = (ALOG(TEM) - ALOG(PMSDIL*CONST)) / (-1.605)
      T55M = ALOG(TEM)
      D85M = D55M * 1.25
      IF(DEBUG)WRITE(JOSTND,*)'D55M,T55M,D85M= ',D55M,T55M,D85M
  221 IF(D85M .GT. 5.0) D85M = 5.0
      IF(D85M .LT. 0.125)D85M=0.125
      T85M = ALOG(CONST * (EXP(D85M) ** (-1.605)) * PMSDIU)
      SLP = (T85M-T55M) / (D85M - D55M)
      IF(DEBUG)WRITE(JOSTND,*)'D55M,D85M,T55M,T85M,SLP= ',
     &D55M,D85M,T55M,T85M,SLP
      IF(SLP .GT. -0.5 .AND. D85M .LT. 5.0)THEN
        D85M = D85M + .1
        GO TO 221
      ENDIF
      CEPT = T55M - SLP * D55M
      IF(T .LE. T55D0) GO TO 230
      IF(DEBUG)WRITE(JOSTND,*)'MORTS,359,DIA0',DIA0
      TPRIME = CEPT + SLP * ALOG(DIA0)
      DIFF = T - EXP(TPRIME)
      IF(DEBUG)WRITE(JOSTND,9050) DIA0,D10,T,TREEIT,TEM,D55M,
     *T55M,D85M,T85M,SLP,CEPT,TPRIME,DIFF,KNT
 9050 FORMAT('MORTS 9050',13F9.3,I4)
      IF(DIFF .LE. 5.0 .AND. DIFF .GE. -5.0) GO TO 230
      TREEIT = TREEIT + 0.5 * DIFF
      KNT = KNT + 1
      IF(KNT .LE. 100) GO TO 220
  230 CONTINUE
      IF(SLPMRT .EQ. 0.) SLPMRT = SLP
      IF(CEPMRT .EQ. 0.) CEPMRT = CEPT
      IF(DEBUG)WRITE(JOSTND,*)'D10,CEPMRT,SLPMRT= ',D10,CEPMRT,SLPMRT
      TEM = ALOG(D10)
      TN10 = CEPMRT + SLPMRT*TEM
      TN10 = EXP(TN10)
      IF(TN10 .GE. T85D10)  TN10 = T85D10
      GO TO 270
  240 CONTINUE
C----------
C IF TOTAL NUMBER OF TREES IS LESS THAN 55 PERCENT AT DIA0 BUT GREATER TH
C 55 PERCENT AT D10, FIND LINEAR FN (STRAIGHT COMPUTATION) AND KILL
C ACCORDING TO LINEAR FN.
C IF TOTAL NUMBER OF TREES IS LESS THAN 55 PERCENT AT DIA0 AND D10,
C HOLD NUMBER OF TREES CONSTANT.
C----------
      IF(T .LE. T55D10) THEN
           TN10 = T
           GO TO 270
      ELSE
           IPATH = 2
           GO TO 220
      ENDIF
  270 CONTINUE
      IF(DEBUG)WRITE(JOSTND,9060)DIA0,D10,T,TN10,TREEIT,
     *CONST,KNT
 9060 FORMAT('MORTS 9060',6(F10.3,3X),I4)
C----------
C BOUND TN10 (THE NUMBER OF TREES REMAINING IN THE STAND AFTER MORTALITY)
C   IF TN10 IS GREATER THAN T, SET TN10 = T
C   IF TN10 IS SMALL, JUST KILL ALL THE TREES
C----------
  271 CONTINUE
      IF(TN10 .GT. T)TN10=T
      IF(TN10 .LT. 0.1)TN10=0.
C
C***********************************************************************
C
      RN=1.0-(1.0-((T-TN10)/T))**(1./FINT)
      IF(DEBUG) WRITE(JOSTND,*) 'TESTMORTS, RN=',RN,
     *'T=',T,'TN10=',TN10
C----------
C  START LOOP TO ESTIMATE SDI BASED MORTALITY RATE.
C  TREES ARE PROCESSED ONE AT A TIME WITHIN A SPECIES.
C----------
      DO 50 ISPC=1,MAXSP
      I1=ISCT(ISPC,1)
      IF(I1.LE.0) GO TO 50
      I2=ISCT(ISPC,2)
      XMORT = XMMULT(ISPC)
      D1=XMDIA1(ISPC)
      D2=XMDIA2(ISPC)
      B0 = PMSC(ISPC)
      B1 = PMD(ISPC)
C----------
C  START TREE LOOP WITHIN SPECIES.
C----------
      DO 40 I3=I1,I2
C----------
C  INITIALIZE FOR NEXT TREE.
C----------
      I=IND1(I3)
      P=PROB(I)
      WKI=0.0
      WK2(I)=0.0
      IF(P.LE.0.0) GO TO 40
      D=DBH(I)
C----------
C  COMPUTE BACKGROUND MORTALITY RATE RI
C----------
      RI=(1.0/(1.0+EXP(B0+B1*D)))
C----------
C TEST RUNS SHOW BACKGROUND MORTALITY RATE IS HIGH, CUT IT IN HALF.
C----------
      RI = 0.5 * RI
C----------
C  MERGE ESTIMATES OF RI AND RN.
C----------
      RIP=RI
C----------
C IF SDI NOT IN EFFECT YET SET RIP TO BACKGROUND MORTALITY RATE.
C OTHERWISE SET TO SDI MORTALITY RATE.
C----------
      RIP=RN
      TEM=CONST*(D10**(-1.605))
      IF(TEM .GT. 35000.0)TEM=35000.0
      TEM=TEM*PMSDIL
      IF(T .LE. TEM .OR. RN .LE. 0.0)RIP=RI
      IF(RIP.GT.1.0) RIP=1.0
      X=1.0
      IF(D .GE. D1 .AND. D .LT. D2)X=XMORT
C----------
C APPLY MORTALITY MULTIPLIER ONLY TO BACKGROUND RATE
C----------
      IF(RIP .EQ. RN) X=1.0
      WKI=P*(1.0-(1.0-RIP)**FINT)*X
      IF(WKI.GT.P) WKI=P
      IF(DEBUG) WRITE(JOSTND,9070) I,D,RI,RN,RIP
 9070 FORMAT('MORTALITY RATE ESTIMATES FOR TREE ',I4,', DBH = ',F6.2/
     *' RI = ',F7.5,' RN = ',F7.5,' RIP = ',F7.5)
      WK2(I)=WKI
C----------
C  END OF TREE LOOP.  PRINT DEBUG INFO IF DESIRED.
C----------
      IF(DEBUG) THEN
        PRES=P-WKI
        VLOS=WKI*CFV(I)/FINT
        WRITE(JOSTND,9080) I,ISPC,D,P,WKI,PRES,VLOS
 9080   FORMAT('IN MORTS, I=',I4,',  ISPC=',I3,',  DBH=',F7.2,
     &       ',  INIT PROB=',F9.3,
     &       ',  TREES DYING=',F9.3,'  RES PROB=',F9.3,
     &       ',  VOL LOST=',F9.3)
      ENDIF
   40 CONTINUE
C----------
C  END OF SPECIES LOOP.  PRINT DEBUG INFO IF DESIRED.
C----------
      IF(DEBUG) THEN
        WRITE(JOSTND,9090)  ISPC,B0,B1
 9090   FORMAT('IN MORTS,  ISPC=',I3,
     &       11X,'B0=',F8.6,',  B1=',F8.6)
      ENDIF
   50 CONTINUE
C----------
C DISTRIBUTE MORTALITY BY VARIANT SPECIFIC METHOD
C IF ALL TREES ARE BEING KILLED, WE DON'T NEED TO DO THIS
C----------
      SUMTRE = 0.0
      IF(RIP .EQ. RN)SUMTRE = T-TN10
      IF(SUMTRE .LT. 0.0)SUMTRE=0.0
      IF(TN10 .GE. 0.1)THEN
        CALL VARMRT(SUMTRE,DEBUG,SUMKIL)
      ENDIF
C----------
C ESTIMATE NEW QUADRATIC MEAN DIAMETER AND SEE HOW BAD OUR
C INITIAL ESTIMATE WAS.
C----------
      TN=0.0
      SD2SQN=0.0
      SUMDR10N=0.
      DR10N=0.
      IPASS = IPASS+1
      DO 30 I=1,ITRN
      P=PROB(I)-WK2(I)
      IS=ISP(I)
      D=DBH(I)
      IF(LZEIDE.AND.(D.LT.DBHZEIDE))GO TO 30    ! BRANCH IF D IS LT MIN DBH
      IF(.NOT.LZEIDE.AND.(D.LT.DBHSTAGE))GO TO 30
      BARK=BRATIO(IS,D,HT(I))
      G = (DG(I)/BARK) * (FINT/5.0)
      CIOBDS=(2.0*D*G+G*G)
      SD2SQN=SD2SQN+P*(D*D+CIOBDS)
      IF(LZEIDE)THEN
      SUMDR10N=SUMDR10N+P*(D+G)**(1.605)
      ENDIF
      TN=TN+P
   30 CONTINUE
      IF(TN .EQ. 0.0)GO TO 35
      DQ10N=SQRT(SD2SQN/TN)
      IF(LZEIDE)THEN
        DR10N=(SUMDR10N/TN)**(1./1.605)
      ENDIF
      IF(DEBUG)WRITE(JOSTND,*)'MORTS CHECK DIA. IPASS,DQ10,DR10',
     *'DQ10N,DR10N= ',IPASS,DQ10,DR10,DQ10N,DR10N
      IF(LZEIDE)THEN
        D10N=DR10N
      ELSE
        D10N=DQ10N
      ENDIF
      IF(IPASS .EQ. 10)GO TO 35
      DIFF=ABS(D10-D10N)
      IF(DIFF .GT. 0.1)THEN
C----------
C SOMETIMES SELECTIVE KILLING CAN DECREASE QMD.  IF SO, TAKE
C CALCULATED MORTALITY AND RECALIBRATE NEXT CYCLE.
C----------
        IF(D10N .LE. DIA0)THEN
          IPATH = 0
          GO TO 35
        ENDIF
        D10=D10N
        GO TO 10
      ENDIF
   35 CONTINUE
C----------
C IF ALTERNATE MORTALITY IS IN EFFECT, THEN COMPUTE NECESSARY PARAMETERS
C AND KILL ADDITIONAL TREES TO SIMULATE STAND BREAK-UP. QMD WILL CHANGE
C SO SET FLAG TO RECALIBRATE NEXT CYCLE.
C----------
      TMMSB=0.
      T85MSB=0.
      IF(D10.GT.QMDMSB .AND. TN.GT.0.)THEN
        TMMSB=EXP(CEPMSB+SLPMSB*ALOG(D10))
        T85MSB=TMMSB*PMSDIU
        TMORE=TN-T85MSB
        IF(TMORE .LT. 0.)TMORE=0.
        IF(DEBUG)WRITE(JOSTND,*)'ALTERNATE MORTALITY LOGIC, D10,TN,',
     &  'QMDMSB,CEPMSB,SLPMSB,TMMSB,T85MSB,PMSDIU,TMORE= '
        IF(DEBUG)WRITE(JOSTND,*)
     &  D10,TN,QMDMSB,CEPMSB,SLPMSB,TMMSB,T85MSB,PMSDIU,TMORE  
C----------
C MAKE SURE MSB EFFICIENCY IS SET HIGH ENOUGH; SINCE MORTALITY IS
C CONCENTRATED IN A DBH RANGE, FIRST COMPUTE THE TPA LEFT IN THE DBH
C RANGE AFTER DENSITY RELATED MORTALITY HAS BEEN ACCOUNTED FOR.
C CALL SUBROUTINE MSBMRT TO KILL TMORE TREES AND SET RECALIBRATE FLAG.
C
C IF THERE AREN'T ENOUGH TREES LEFT IN THE CLASS TO ACHIEVE THE
C ALTERNATE MORTALITY LEVEL EVEN IF THEY ARE ALL KILLED, THEN CANCEL 
C THE ALTERNATE MORTALITY LOGIC; IF THE EFFICIENCY IS SET TO LOW TO
C ACHIEVE THE ALTERNATE MORTALITY LEVEL THEN RECOMPUTE IT AND CONTINUE
C WITH THE LOGIC.
C----------
        TPACLS=0.
        DO I=1,ITRN
        BARK=BRATIO(ISP(I),DBH(I),HT(I))
        DBHEND=DBH(I)+(DG(I)/BARK)*(FINT/5.0)
        IF(DBHEND.GE.DLOMSB .AND. DBHEND.LT.DHIMSB)THEN
          TPACLS=TPACLS+PROB(I)-WK2(I)
        ENDIF
        ENDDO
        IF(DEBUG)WRITE(JOSTND,*)'ALT MORT LOGIC DLOMSB,DHIMSB,TPACLS',
     &  ' = ',DLOMSB,DHIMSB,TPACLS
C
        IF(TMORE .GT. TPACLS)THEN
          WRITE(JOSTND,351)TPACLS,TMORE
  351     FORMAT(/,2('***************'/),'WARNING: FOR ALTERNATE ',
     &    'MORTALITY, TPA IN DBH CLASS OF ',F8.1,
     &    ' TREES/ACRE IS LESS THAN THE ADDITIONAL MORTALITY TPA',
     &    /,9X,' OF ',F8.1,' TREES/ACRE.  '
     &    'ALTERNATE MORTALITY CANCELLED.',/2('***************'/),/)
          GO TO 353
        ENDIF
C
        TEMEFF=EFFMSB
        IF(MFLMSB .EQ. 3) THEN
          TEMEFF=TMORE/TPACLS
        ELSE
          IF(TPACLS*TEMEFF .LT. TMORE)THEN
            TEMEFF=TMORE/TPACLS
            WRITE(JOSTND,352)EFFMSB,TEMEFF
  352       FORMAT(/,2('***************'/),'WARNING: FOR ALTERNATE ',
     &      'MORTALITY, MORTALITY EFFICIENCY OF ',F8.4,
     &      ' IS TOO LOW TO REACH THE ADDITIONAL MORTALITY LEVEL. ',
     &      /,9X,'MORTALITY EFFICIENCY RESET TO ',F8.4,
     &      ' FOR FURTHER PROCESSING.',
     &      /2('***************'/),/)
          ENDIF
        ENDIF
        CALL MSBMRT(TEMEFF,TMORE,DLOMSB,DHIMSB,MFLMSB,DEBUG)
        IPATH=0
      ENDIF
  353 CONTINUE
C----------
C  LOOP THROUGH TREES AND CHECK FOR SIZE (aka AGE) CAP RESTRICTIONS
C----------
      DO 354 I=1,ITRN
      IS = ISP(I)
      D = DBH(I)
      P = PROB(I)
      BARK=BRATIO(IS,D,HT(I))
      G = (DG(I)/BARK) * (FINT/5.0)
      IDMFLG=IFIX(SIZCAP(IS,3))
      IF((D+G).GE.SIZCAP(IS,1) .AND. IDMFLG.NE.1) THEN
        WK2(I) = AMAX1(WK2(I),(P*SIZCAP(IS,2)*FINT/5.0))
        IF(WK2(I) .GT. P)WK2(I)=P
        IF(DEBUG)WRITE(JOSTND,*)'SIZE CAP RESTRICTION IMPOSED, ',
     &  'I,IS,D,P,SIZCAP 1-3,WK2 = ',
     &  I,IS,D,P,SIZCAP(IS,1),SIZCAP(IS,2),SIZCAP(IS,3),WK2(I)
      ENDIF
  354 CONTINUE
C----------
C  CHECK TO SEE IF BA IS STILL WITHIN LIMITS. IF OUT OF BOUNDS,
C  ADJUST THE MORTALITY VALUES PROPORTIONATELY ACROSS ALL TREE RECORDS.
C----------
      KNT2=0
9001  CONTINUE
      TNEW=0.
      BANEW=0.
      QMDNEW=0.
      BADEAD=0.
      DO 36 I=1,ITRN
      P=PROB(I)-WK2(I)
      D=DBH(I)
      BARK=BRATIO(ISP(I),D,HT(I))
      G = (DG(I)/BARK) * (FINT/5.0)
      IF(DEBUG)WRITE(JOSTND,*)'I,DG,BARK,FINT,G= ',I,DG(I),BARK,FINT,G
      TNEW=TNEW+P
      BANEW=BANEW+(0.0054542*(D+G)**2.)*P
      BADEAD=BADEAD+(0.0054542*(D+G)**2.)*WK2(I)
      QMDNEW=QMDNEW + ((D+G)**2.)*P
      IF(DEBUG)WRITE(JOSTND,*)'I,P,D,G,TNEW,BANEW,QMDNEW,BADEAD= ',
     &I,P,D,G,TNEW,BANEW,QMDNEW,BADEAD 
   36 CONTINUE
      IF(TNEW .GT. 0.) THEN
        QMDNEW=SQRT(QMDNEW/TNEW)
      ELSE
        QMDNEW = 0.
      ENDIF
      IF(DEBUG)WRITE(JOSTND,*)'ICYC,BANEW,BAMAX,TNEW,QMDNEW,KNT2= ',
     &ICYC,BANEW,BAMAX,TNEW,QMDNEW,KNT2
      IF((BANEW-BAMAX) .GT. 1.) THEN
C----------
C       CALCULATE ADJUSTMENT FACTOR NEEDED TO GET RESIDUAL BA WITHIN
C       THE BA MAXIMUM LIMIT. INCREASE IT BY 10% AND PLACE A LOWER
C       BOUND ON THE FACTOR TO SPEED UP ITERATION.
C----------
        ADJFAC = ((BANEW-BAMAX)/BADEAD)
        IF(DEBUG)WRITE(JOSTND,*)'BANEW,BAMAX,ADJFAC= ',
     &  BANEW,BAMAX,ADJFAC
C----------
C       LOOP THROUGH THE TREE LIST AND ADJUST THE MORTALITY VALUES.
C----------
        TNEW=0.
        DO 1500 I=1,ITRN
        P=PROB(I)
        WKI=WK2(I)*(1+ADJFAC)
        IF(WKI.GT.P) WKI=P
        WK2(I)=WKI
        IF(DEBUG)WRITE(JOSTND,*)'ADJUSTING FOR BAMAX I,P,WKI= ',
     &  I,P,WKI   
C----------
C       PRINT DEBUG INFO IF DESIRED.
C----------
        TNEW=TNEW+P-WKI
        IF(.NOT.DEBUG) GO TO 1500
        PRES=P-WKI
        VLOS=WKI*CFV(I)/FINT
        WRITE(JOSTND,9080) I,ISPC,D,P,WKI,PRES,VLOS
 1500   CONTINUE
C----------
C LOOP BACK AND SEE IF THE BAMAX TARGET HAS BEEN ACHIEVED YET.
C (I.E. IF THE COMPUTED MORTALITY RATE EXCEEDED THE PROB, AND WE 
C  HAD TO LIMIT MORTALITY TO THE PROB VALUE FOR SOME TREE RECORDS,
C  THEN WE MAY NOT REACH THE BA LIMIT IN ONE PASS.) 
C----------
        KNT2=KNT2+1
        IF(KNT2 .LT. 100)GO TO 9001
C
        IPATH=0
        IF(DEBUG)WRITE(JOSTND,*)'AFTER BA ADJUSTMENT RESIDUAL TPA = ',
     &  TNEW
      ENDIF
      TPAMRT=TNEW
   45 CONTINUE
C
C
C
C----------
C  COMPUTE THE FIXMORT OPTION.  LOOP OVER ALL SCHEDULED FIXMORT'S
C  LINCL IS USED TO INDICATE WHETHER A TREE GETS AFFECTED OR NOT
C----------
      CALL OPFIND (1,MYACTS(2),NTODO)
      IF (NTODO.GT.0) THEN
        IF(DEBUG)WRITE(JOSTND,*)'FIXMORT PROCESSING, ITODO= ',ITODO
         DO 300 ITODO=1,NTODO
         CALL OPGET (ITODO,6,IDATE,IACTK,NP,PRM)
         IF (IACTK.LT.0) GOTO 300
         CALL OPDONE(ITODO,IY(ICYC))
         ISPCC=IFIX(PRM(1))
         IF(NP .LE. 4)THEN
           IF(PRM(2).GT. 1.0)PRM(2)=1.0
         ENDIF
         IF(PRM(3).LT. 0.0)PRM(3)=0.0
         IF(PRM(4).LE. 0.0)PRM(4)=999.
         IP=1
         IF (NP.GT.4) THEN
            IF(PRM(5).LT.3.)THEN
               IF(PRM(2).GT. 1.0)PRM(2)=1.0
               IF(PRM(2).LT. 0.0)PRM(2)=0.0
            ENDIF 
            IF (PRM(5).EQ.1.0) THEN
               IP=2
            ELSEIF (PRM(5).EQ.2.0) THEN
               IP=3
            ELSEIF (PRM(5).EQ.3.) THEN
               IP=4
            ENDIF
         ENDIF
C----------
C  SET FLAG FOR POINT MORTALITY, OR KILLING FROM ABOVE
C    PRM(6)    POINT      SIZE   KBIG     KILL DIRECTION
C      0         NO        NO     0                       DEFAULT CONDITION
C      1         YES       NO     0
C     10         NO        YES    1       BOTTOM UP
C     11         YES       YES    1       BOTTOM UP
C     20         NO        YES    2       TOP DOWN
C     21         YES       YES    2       TOP DOWN
C----------
         KPOINT=0
         KBIG=0
         IF(PRM(6).GT.0.)THEN
           IF(PRM(6) .EQ. 1)THEN
             KPOINT=1
           ELSEIF(PRM(6) .EQ. 10)THEN
             KBIG=1
           ELSEIF(PRM(6) .EQ. 11)THEN
             KPOINT=1
             KBIG=1
           ELSEIF(PRM(6) .EQ. 20)THEN
             KBIG=2
           ELSEIF(PRM(6) .EQ. 21)THEN
             KPOINT=1
             KBIG=2
           ENDIF
         ENDIF
         IF (ITRN.GT.0) THEN
C----------
C IF CONCENTRATING MORTALITY ON A POINT, AND/OR BY SIZE TREES IS IN
C EFFECT, DETERMINE EFFECT OF THIS FIXMORT AND REALLOCATE BY POINT:
C   REALLOCATE ALL MORTALITY IF REPLACE OPTION OR MULTIPLY OPTION
C   ARE IN EFFECT.
C   ONLY REALLOCATE ADDITIONAL MORTALITY IF "ADD" OPTION IS IN EFFECT
C   ONLY REALLOCATE ADDITIONAL MORTALITY IF "MAX" OPTION IS IN EFFECT
C   (I.E. MORTALITY OVER AND ABOVE WHAT WAS PREVIOUSLY PREDICTED.
C----------
            IF(KBIG.GE.1 .OR. (KPOINT.EQ.1 .AND. IPTINV.GT.1)) THEN
              XMORE=0.
              DO 199 I=1,ITRN
              LINCL = .FALSE.
              IF(ISPCC.EQ.0 .OR. ISPCC.EQ.ISP(I))THEN
                LINCL = .TRUE.
              ELSEIF(ISPCC.LT.0)THEN
                IGRP = -ISPCC
                IULIM = ISPGRP(IGRP,1)+1
                DO 90 IG=2,IULIM
                IF(ISP(I) .EQ. ISPGRP(IGRP,IG))THEN
                  LINCL = .TRUE.
                  GO TO 91
                ENDIF
   90           CONTINUE
              ENDIF
   91         CONTINUE
              IF (LINCL .AND.
     >          (PRM(3).LE.DBH(I) .AND. DBH(I).LT.PRM(4))) THEN
                GOTO (191,192,193,194),IP
  191           CONTINUE
                XMORE=XMORE+PROB(I)*PRM(2)
                WK2(I)=0.
                GOTO 199
  192           CONTINUE
                XMORE=XMORE+(AMAX1(0.0,PROB(I)-WK2(I))*PRM(2))
                GOTO 199
  193           CONTINUE
                TEMP=AMAX1(WK2(I),(PROB(I)*PRM(2)))
                IF(TEMP .GT. WK2(I)) THEN
                  XMORE=XMORE+TEMP-WK2(I)
                ENDIF
                GOTO 199
  194           CONTINUE
                XMORE=XMORE+WK2(I)*PRM(2)
                WK2(I)=0.
                GOTO 199
              ENDIF
  199         CONTINUE
              IF(DEBUG)WRITE(JOSTND,*)'KPOINT,KBIG,ITRN,XMORE= ',
     &                 KPOINT,KBIG,ITRN,XMORE
              CREDIT=0.
              DO 201 I=1,ITRN
              IWORK1(I)=IND1(I)
              IF(KBIG .EQ. 1)THEN
                WORK3(I)=(-1.0)*
     &                  (DBH(I)+DG(I)/BRATIO(ISP(I),DBH(I),HT(I)))
              ELSE
                WORK3(I)=DBH(I)+DG(I)/BRATIO(ISP(I),DBH(I),HT(I))
              ENDIF
  201         CONTINUE
              CALL RDPSRT(ITRN,WORK3,IWORK1,.FALSE.)
              IF(DEBUG)WRITE(JOSTND,*)'DBH= ',(DBH(IG),IG=1,ITRN)
              IF(DEBUG)WRITE(JOSTND,*)'IWORK1= ',(IWORK1(IG),IG=1,ITRN)
              IF(DEBUG)WRITE(JOSTND,*)'WK2= ',(WK2(IG),IG=1,ITRN)
C
              IF(KBIG.GE.1 .AND. KPOINT.EQ.0)THEN
C
C  CONCENTRATION BY SIZE ONLY
C
                DO 310 I=1,ITRN
                IX=IWORK1(I)
                LINCL = .FALSE.
                IF(ISPCC.EQ.0 .OR. ISPCC.EQ.ISP(IX))THEN
                  LINCL = .TRUE.
                ELSEIF(ISPCC.LT.0)THEN
                  IGRP = -ISPCC
                  IULIM = ISPGRP(IGRP,1)+1
                  DO 92 IG=2,IULIM
                  IF(ISP(IX) .EQ. ISPGRP(IGRP,IG))THEN
                    LINCL = .TRUE.
                    GO TO 93
                  ENDIF
   92             CONTINUE
                ENDIF
   93           CONTINUE
                IF (LINCL .AND.
     >          (PRM(3).LE.DBH(IX) .AND. DBH(IX).LT.PRM(4))) THEN
                  TEMP=CREDIT+PROB(IX)-WK2(IX)
                  IF((TEMP .LE. XMORE).OR.
     >               (ABS(TEMP-XMORE).LT.0.0001))THEN
                    CREDIT=CREDIT+PROB(IX)-WK2(IX)
                    WK2(IX)=PROB(IX)
                  ELSE
                    WK2(IX)=WK2(IX)+XMORE-CREDIT
                    CREDIT=XMORE
                    GO TO 295
                  ENDIF
                ENDIF
  310           CONTINUE
                GO TO 295
C
              ELSEIF(KPOINT.EQ.1 .AND. KBIG.EQ.0)THEN
C
C  CONCENTRATION ON POINTS ONLY
C
              DO 205 J=1,IPTINV
              DO 204 I=1,ITRN
              IF(ITRE(I) .NE. J)GO TO 204
              LINCL = .FALSE.
              IF(ISPCC.EQ.0 .OR. ISPCC.EQ.ISP(I))THEN
                LINCL = .TRUE.
              ELSEIF(ISPCC.LT.0)THEN
                IGRP = -ISPCC
                IULIM = ISPGRP(IGRP,1)+1
                DO 94 IG=2,IULIM
                IF(ISP(I) .EQ. ISPGRP(IGRP,IG))THEN
                  LINCL = .TRUE.
                  GO TO 95
                ENDIF
   94           CONTINUE
              ENDIF
   95         CONTINUE
              IF (LINCL .AND.
     >          (PRM(3).LE.DBH(I) .AND. DBH(I).LT.PRM(4))) THEN
                TEMP=CREDIT+PROB(I)-WK2(I)
                IF((TEMP .LE. XMORE).OR.
     >             (ABS(TEMP-XMORE).LT.0.0001))THEN
                  CREDIT=CREDIT+PROB(I)-WK2(I)
                  WK2(I)=PROB(I)
                ELSE
                  WK2(I)=WK2(I)+XMORE-CREDIT
                  CREDIT=XMORE
                  GO TO 295
                ENDIF
              ENDIF
  204         CONTINUE
  205         CONTINUE
              GO TO 295
C
C  CONCENTRATION BY SIZE ON POINTS (POINTS HAVE PRIORITY, SO TREES
C  WILL BE KILLED BY SIZE ON ONE POINT BEFORE MOVING TO THE NEXT
C  POINT TO START WITH THE BIGGEST/SMALLEST TREES ON THAT POINT.
              ELSE
              DO 312 J=1,IPTINV
              DO 311 I=1,ITRN
              IX=IWORK1(I)
              IF(ITRE(IX) .NE. J)GO TO 311
              LINCL = .FALSE.
              IF(ISPCC.EQ.0 .OR. ISPCC.EQ.ISP(IX))THEN
                LINCL = .TRUE.
              ELSEIF(ISPCC.LT.0)THEN
                IGRP = -ISPCC
                IULIM = ISPGRP(IGRP,1)+1
                DO 96 IG=2,IULIM
                IF(ISP(IX) .EQ. ISPGRP(IGRP,IG))THEN
                  LINCL = .TRUE.
                  GO TO 97
                ENDIF
   96           CONTINUE
              ENDIF
   97         CONTINUE
              IF (LINCL .AND.
     >          (PRM(3).LE.DBH(IX) .AND. DBH(IX).LT.PRM(4))) THEN
                TEMP=CREDIT+PROB(IX)-WK2(IX)
                IF((TEMP .LE. XMORE).OR.
     >             (ABS(TEMP-XMORE).LT.0.0001))THEN
                  CREDIT=CREDIT+PROB(IX)-WK2(IX)
                  WK2(IX)=PROB(IX)
                ELSE
                  WK2(IX)=WK2(IX)+XMORE-CREDIT
                  CREDIT=XMORE
                  GO TO 295
                ENDIF
              ENDIF
  311         CONTINUE
  312         CONTINUE
              GO TO 295
              ENDIF
C
            ENDIF
C----------
C  NORMAL FIXMORT PROCESSING WHEN POINT OR SIZE CONCENTRATION
C  IS NOT IN EFFECT.
C----------
            DO 290 I=1,ITRN
              LINCL = .FALSE.
              IF(ISPCC.EQ.0 .OR. ISPCC.EQ.ISP(I))THEN
                LINCL = .TRUE.
              ELSEIF(ISPCC.LT.0)THEN
                IGRP = -ISPCC
                IULIM = ISPGRP(IGRP,1)+1
                DO 98 IG=2,IULIM
                IF(ISP(I) .EQ. ISPGRP(IGRP,IG))THEN
                  LINCL = .TRUE.
                  GO TO 99
                ENDIF
   98           CONTINUE
              ENDIF
   99         CONTINUE
            IF (LINCL .AND.
     >         (PRM(3).LE.DBH(I) .AND. DBH(I).LT.PRM(4))) THEN
               GOTO (610,620,630,640),IP
  610          CONTINUE
               WK2(I)=PROB(I)*PRM(2)
               GOTO 290
  620          CONTINUE
               WK2(I)=WK2(I)+(AMAX1(0.0,PROB(I)-WK2(I))*PRM(2))
               GOTO 290
  630          CONTINUE
               WK2(I)=AMAX1(WK2(I),(PROB(I)*PRM(2)))
               GOTO 290
  640          CONTINUE
               WK2(I)=AMIN1(PROB(I),WK2(I)*PRM(2))
               GOTO 290
            ENDIF
  290       CONTINUE
  295    CONTINUE
         IF(DEBUG)WRITE(JOSTND,*)'ITODO,WK2= ',
     &    ITODO,(WK2(IG),IG=1,ITRN)
         ENDIF
  300    CONTINUE
      ENDIF
      RETURN
C
      ENTRY MORCON
C----------
C  ENTRY POINT FOR LOADING MORTALITY MODEL CONSTANTS THAT
C  REQUIRE ONE-TIME RESOLUTION.
C----------
      CEPMRT = 0.
      SLPMRT = 0.
      TPAMRT = 0.
      RETURN
      END
