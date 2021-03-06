      SUBROUTINE SDICAL (XMAX)
      use contrl_mod
      use varcom_mod
      use plot_mod
      use arrays_mod
      use prgprm_mod
      implicit none
C----------
C  $Id$
C----------
C  THIS SUBROUTINE COMPUTES THE MAXIMUM SDI IN EFFECT FOR A STAND.
C  MAXIMUM SDI IS A WEIGHTED AVERAGE OF THE SDI MAXIMUMS BY SPECIES
C  CONTAINED IN THE SDIDEF() ARRAY.  THE WEIGHT IS THE PROPORTION
C  OF TOTAL STAND BASAL AREA REPRESENTED BY A SPECIES.
C
C  IT ALSO ADJUSTS THE BAMAX VALUE TO MATCH THE MAXIMUM SDI VALUE IF
C  A USER DEFINED BA MAXIMUM IS NOT IN EFFECT.
C
C  THIS ROUTINE IS CALLED FROM **GRINCR** TO SET VALUES FOR THE EVENT
C  MONITOR VARIABLES "BSDIMAX" AND "ASDIMAX".  ITS ALSO CALLED FROM
C  **MORTS** IN VARIANTS WHICH USE SDI BASED MORTALITY ALGORITHMS.
C----------
COMMONS
C----------
C  DEFINITIONS:
C    BAXSP -- BASAL AREA BY SPECIES
C      DBH -- TREE DIAMETER.
C    LINCL -- USED TO INDICATE WHETHER A TREE GETS INCLUDED IN
C             THE CALCULATION OR NOT.
C     PROB -- NUMBER OF TREES PER ACRE REPRESENTED BY A TREE.
C   TREEBA -- BA REPRESENTED BY A TREE RECORD
C    TOTBA -- TOTAL STAND BA
C     XMAX -- MAXIMUM SDI WEIGHTED BY BA PROPORTION
C    MAPNE -- INDICATES GROUP THAT A SPECIES BELONGS TO FOR THE
C             SILVAH-TYPE DENSITY CALCULATION
C----------
      INTEGER ISPC,JSPEC,I,IWHO,IGRP,IULIM,IG,JPNUM,MAPNE(108),IEQN,IT
      INTEGER ISLFTM(108)
      REAL BAXSP(MAXSP),XMAX,TOTBA,TREEBA,DLO,DHI,SDIC,SPROB,TREERD
      REAL CCCL,CRA,CCFT,CWDI,SDSQ,A,B,TPACRE,SDIC2
      REAL CRD,CLBA,CLQMD,CLKNT,D2,SUMD2,TEM
      REAL BA1,BA2,BA3,BA4,BA5,BATOT,RATIO1,RATIO2,RATIO3,RATIO5
      LOGICAL DEBUG,LINCL
      CHARACTER VVER*7
C
      DATA MAPNE /
     &2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2,
     &2, 2, 2, 2, 2, 2, 3, 3, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 3,
     &2, 1, 2, 2, 2, 1, 2, 2, 2, 2, 2, 2, 2, 1, 3, 3, 3, 3, 3, 3,
     &3, 3, 3, 3, 3, 3, 2, 2, 3, 3, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2,
     &2, 2, 2, 2, 2, 2, 2, 2, 3, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 3,
     &2, 2, 2, 2, 2, 2, 2, 2/
C
      DATA ISLFTM /
C     1  2  3  4  5  6  7  8  9  0  1  2  3  4  5  6  7  8  9  0
C
     &0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0,
     &0, 0, 0, 0, 0, 1, 1, 0, 0, 1, 1, 0, 0, 0, 2, 2, 2, 2, 2, 1,
     &0, 1, 0, 0, 0, 1, 0, 1, 0, 0, 0, 0, 0, 1, 2, 2, 2, 2, 2, 2,
     &2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
     &0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0,
     &0, 0, 0, 0, 0, 0, 0, 0/
C-----------
C  SEE IF WE NEED TO DO SOME DEBUG.
C-----------
      CALL DBCHK (DEBUG,'SDICAL',6,ICYC)
      IF(DEBUG)WRITE(JOSTND,2)ICYC
    2 FORMAT(' ENTERING SUBROUTINE SDICAL CYCLE =',I4)
C----------
C DETERMINE BA BY SPECIES, AND TOTAL STAND BA
C----------
      XMAX = 0.0
      TOTBA = 0.0
      DO 13 I=1,MAXSP
      BAXSP(I)=0.0
   13 CONTINUE
      IF(ITRN .LE. 0) GO TO 16
      DO 14 I=1,ITRN
      ISPC = ISP(I)
      TREEBA = 0.0054542*DBH(I)*DBH(I)*PROB(I)
      BAXSP(ISPC)= BAXSP(ISPC) + TREEBA
      TOTBA = TOTBA + TREEBA
   14 CONTINUE
      IF(DEBUG)THEN
        WRITE(JOSTND,17)BAXSP
   17   FORMAT(' BAXSP(1-MAXSP) =',10(11F10.4,/' ',16X))
        WRITE(JOSTND,18)TOTBA
   18   FORMAT(' TOTAL BA = ',F10.4)
      ENDIF
C----------
C COMPUTE MAXIMUM SDI WEIGHTED BY BA PROPORTION
C----------
      IF(TOTBA .LE. 0.) GO TO 16
      DO 15 I=1,MAXSP
      XMAX = XMAX + SDIDEF(I) * BAXSP(I)
   15 CONTINUE
      XMAX = XMAX / TOTBA
C----------
C ADJUST MAXIMUM BA IF A USER-DEFINED VALUE IS NOT IN EFFECT.
C----------
      IF(.NOT.LBAMAX) THEN
        BAMAX = XMAX * 0.5454154 * PMSDIU
      ELSE
        TEM=PMSDIU
        IF(TEM .GT. 1.0) TEM=TEM/100.
        XMAX = BAMAX/(0.5454154*TEM)
      ENDIF
C
   16 CONTINUE

      IF(DEBUG)WRITE(JOSTND,*)' CALLING CLMAXDEN,',
     > ' CURRENT SDI MAXIMUM = ',XMAX
C
C     FURTHER MODIFY XMAX FOR CLIMATE-CHANGE
C
      CALL CLMAXDEN (SDIDEF,XMAX)
      IF(DEBUG)WRITE(JOSTND,*)' LEAVING SDICAL, SDI MAXIMUM = ',XMAX
C
      RETURN
C
      ENTRY SDICLS (JSPEC,DLO,DHI,IWHO,SDIC,SDIC2,A,B,JPNUM)
C
C----------
C  THIS ENTRY COMPUTES THE SDI FOR A GIVEN CLASS OF TREES
C  DEFINED BY SPEICES AND DBH RANGE, STAGE'S
C  SUMMATION METHOD (1968 RES. NOTE INT-77)
C  ZEIDE METHOD (ZEIDE, B., CAN. J. FOR. RES. VOL. 13, 1983;
C                SHAW, J.D., WAJF 15(1) 2000)
C
C  JSPEC = SPECIES INDEX (0 FOR ALL SPECIES)
C    DLO = LOWER DBH LIMIT (GREATER THAN OR EQUAL TO)
C    DHI = UPPER DBH LIMIT (LESS THAN)
C   IWHO = 1 IF PROB NEEDS TO BE USED IN THE CALCULATION
C          2 IF WK4 NEEDS TO BE USED IN THE CALCULATION (PARTS OF CUTS)
C   SDIC = STAND DENSITY INDEX FOR THE DEFINED CLASS (STAGE)
C   SDIC2 = STAND DENSITY INDEX FOR THE DEFINED CLASS (ZEIDE METHOD)
C    A,B = SDI SUMMATION PARAMETR ESTIMATES
C  JPNUM = POINT NUMBER (IN FVS SEQUENCE FORMAT), 0 = ALL POINTS
C----------
      CALL DBCHK (DEBUG,'SDICLS',6,ICYC)
      IF(DEBUG)WRITE(JOSTND,*)' ENTERING SDICLS (ENTRY PT IN SDICAL)'
C----------
C FIRST, DETERMINE STAGE'S METHOD PARAMETERS BASED ON ALL TREES IN THE
C STAND.
C----------
      SDSQ = 0.
      SPROB = 0.
      SDIC = 0.
C
      IF(ITRN .LE. 0) GO TO 100
      DO 50 I=1,ITRN
      IF(DBH(I).LT.DBHSTAGE)GO TO 50    ! BRANCH IF D IS LT MIN DBH
      IF(IWHO .EQ. 1) THEN
        TPACRE=PROB(I)
      ELSE
        TPACRE=WK4(I)
      ENDIF
      SDSQ = SDSQ + (DBH(I)**2.0)*TPACRE
      SPROB=SPROB+TPACRE
   50 CONTINUE
      IF(SPROB .EQ. 0.)THEN
        SDIC = 0.
      ELSE
        A = (10.0**(-1.605))*(1.-1.605/2.)*((SDSQ/SPROB)**(1.605/2.))
        B = (10.0**(-1.605))*(1.605/2.)*((SDSQ/SPROB)**(1.605/2. - 1.))
        SDIC = SPROB*A + B*SDSQ
      ENDIF
      IF(DEBUG)WRITE(JOSTND,*)' IN SDICLS ITRN,SDSQ,SPROB,SDIC,A,B,JPNUM
     &= ',ITRN,SDSQ,SPROB,SDIC,A,B,JPNUM
C----------
C NEXT, DETERMINE SDI BY SPECIES AND SIZE CLASS IN THE DEFINED CLASS
C----------
      SDIC = 0.
      SDIC2=0.
C
      IF(ITRN .LE. 0) GO TO 100
      DO 70 I=1,ITRN
      IF(JPNUM.GT.0 .AND. ITRE(I).NE.JPNUM)GO TO 70
      IF(IWHO .EQ. 1) THEN
        TPACRE=PROB(I)
      ELSE
        TPACRE=WK4(I)
      ENDIF
      IF(JPNUM .GT. 0)TPACRE=TPACRE*(PI-FLOAT(NONSTK))
C
      LINCL = .FALSE.
      IF(JSPEC.EQ.0)THEN
        LINCL=.TRUE.
      ELSEIF((JSPEC.EQ.ISP(I)).AND..NOT.LEAVESP(ISP(I)))THEN
        LINCL = .TRUE.
      ELSEIF(JSPEC.LT.0)THEN
        IGRP = -JSPEC
        IULIM = ISPGRP(IGRP,1)+1
        DO 60 IG=2,IULIM
        IF((ISP(I) .EQ. ISPGRP(IGRP,IG)).AND..NOT.LEAVESP(ISP(I)))THEN
          LINCL = .TRUE.
          GO TO 61
        ENDIF
   60   CONTINUE
      ENDIF
   61 CONTINUE
C
      IF(LINCL)THEN
        IF(DBH(I).GE.DLO .AND. DBH(I).LT.DHI)THEN
          IF(DBH(I).GE.DBHZEIDE)SDIC2=SDIC2+TPACRE*(DBH(I)/10.)**1.605
          IF(DBH(I).GE.DBHSTAGE)SDIC = SDIC + (A+B*(DBH(I)**2.0))*TPACRE
          IF(DEBUG)WRITE(JOSTND,*)' CALC SDIC- I,SDIC,SDIC2,JSPEC,',
     &    'ISP,DBH,TPACRE,A,B,= ',I,SDIC,SDIC2,JSPEC,ISP(I),
     &     DBH(I),TPACRE,A,B
        ENDIF
      ENDIF
   70 CONTINUE
      IF(DEBUG)WRITE(JOSTND,*)' LEAVING SDICLS SDIC,SDIC2,JPNUM= ',
     &                          SDIC,SDIC2,JPNUM
C
  100 CONTINUE
      RETURN
C
      ENTRY CCCLS (JSPEC,DLO,DHI,IWHO,CRA,JPNUM)
C
C----------
C  THIS ENTRY COMPUTES THE CANOPY COVER FOR A GIVEN CLASS OF TREES
C  DEFINED BY SPEICES AND DBH RANGE, USING THE METHOD DESCRIBED
C  BY CROOKSTON & STAGE GTR-RMRS-24, 1999.
C
C  JSPEC = SPECIES INDEX (0 FOR ALL SPECIES)
C    DLO = LOWER DBH LIMIT (GREATER THAN OR EQUAL TO)
C    DHI = UPPER DBH LIMIT (LESS THAN)
C   IWHO = 1 IF PROB NEEDS TO BE USED IN THE CALCULATION
C          2 IF WK4 NEEDS TO BE USED IN THE CALCULATION (PARTS OF CUTS)
C    CRA = SUM OF CROWN AREAS FOR THE DEFINED CLASS
C   CCCL = CANOPY COVER FOR THE DEFINED CLASS
C  JPNUM = POINT NUMBER (IN FVS SEQUENCE FORMAT), 0 = ALL POINTS
C----------
      CALL DBCHK (DEBUG,'CCCLS',5,ICYC)
      IF(DEBUG)WRITE(JOSTND,*)' ENTERING CCCLS (ENTRY PT IN SDICAL)'
C----------
C DETERMINE CROWN COVER BY SPECIES AND SIZE CLASS
C----------
      CCCL = 0.
      CRA = 0.
C
      IF(ITRN .LE. 0) GO TO 200
      DO 150 I=1,ITRN
      IF(JPNUM.GT.0 .AND. ITRE(I).NE.JPNUM)GO TO 150
C
      LINCL = .FALSE.
      IF((JSPEC.EQ.0 .OR. JSPEC.EQ.ISP(I)).AND..NOT.LEAVESP(ISP(I)))THEN
        LINCL = .TRUE.
      ELSEIF(JSPEC.LT.0)THEN
        IGRP = -JSPEC
        IULIM = ISPGRP(IGRP,1)+1
        DO 120 IG=2,IULIM
        IF((ISP(I) .EQ. ISPGRP(IGRP,IG)).AND..NOT.LEAVESP(ISP(I)))THEN
          LINCL = .TRUE.
          GO TO 121
        ENDIF
  120   CONTINUE
      ENDIF
  121 CONTINUE
C
      IF(LINCL)THEN
        IF(DBH(I).GE.DLO .AND. DBH(I).LT.DHI)THEN
          IF(IWHO .EQ. 1) THEN
            TPACRE=PROB(I)
          ELSE
            TPACRE=WK4(I)
          ENDIF
          IF(JPNUM .GT. 0)TPACRE=TPACRE*(PI-FLOAT(NONSTK))
          CWDI = CRWDTH(I)
          CRA = CRA + CWDI*CWDI*TPACRE
          IF(DEBUG)WRITE(JOSTND,*)' I,JSPEC,ISP,DBH,HT,ICR,TPACRE,',
     &    'CWDI,CRA= ',I,JSPEC,ISP(I),DBH(I),HT(I),ICR(I),TPACRE,CWDI,
     &    CRA
        ENDIF
      ENDIF
  150 CONTINUE
      CCCL = 100.0*CRA*0.785398/43560.
      CCCL = 100.0*(1.0-EXP(-CCCOEF*CCCL))
      IF(DEBUG)WRITE(JOSTND,*)' LEAVING CCCLS ITRN,CCCL,CRA,JPNUM,
     &CCCOEF= ',ITRN,CCCL,CRA,JPNUM,CCCOEF
C
  200 CONTINUE
      RETURN
C
      ENTRY RDCLS (JSPEC,DLO,DHI,IWHO,SUMD2,CLKNT,CRD,JPNUM)
C
C----------
C  THIS ENTRY COMPUTES THE RELATIVE DENSITY FOR A GIVEN CLASS OF TREES
C  DEFINED BY SPEICES AND DBH RANGE, USING THE METHOD DESCRIBED
C  BY CURTIS, ROBERT O.,1982. "A SIMPLE INDEX OF STAND DENSITY FOR
C  DOUGLAS-FIR", FOREST SCIENCE 28:92-94.
C
C  JSPEC = SPECIES INDEX (0 FOR ALL SPECIES)
C    DLO = LOWER DBH LIMIT (GREATER THAN OR EQUAL TO)
C    DHI = UPPER DBH LIMIT (LESS THAN)
C   IWHO = 1 IF PROB NEEDS TO BE USED IN THE CALCULATION
C          2 IF WK4 NEEDS TO BE USED IN THE CALCULATION (PARTS OF CUTS)
C    CRD = RELATIVE DENSITY FOR THE DEFINED CLASS
C  CLKNT = NUMBER OF TREES IN THE SPECIFIED CLASS
C  CLQMD = QMD OF THE TREES IN THE SPECIFIED CLASS
C   CLBA = BASAL AREA OF THE TREES IN THE SPECIFIED CLASS
C  JPNUM = POINT NUMBER (IN FVS SEQUENCE FORMAT), 0 = ALL POINTS
C----------
      CALL DBCHK (DEBUG,'RDCLS',5,ICYC)
      IF(DEBUG)WRITE(JOSTND,*)' ENTERING RDCLS (ENTRY PT IN SDICAL)'
C----------
C DETERMINE RELATIVE DENSITY BY SPECIES AND SIZE CLASS
C----------
      CLBA = 0.
      CLQMD = 0.
      CLKNT = 0
      CRD = 0.
      SUMD2 = 0.
C
      IF(ITRN .LE. 0) GO TO 260
      DO 250 I=1,ITRN
      IF(JPNUM.GT.0 .AND. ITRE(I).NE.JPNUM)GO TO 250
C
      LINCL = .FALSE.
      IF((JSPEC.EQ.0 .OR. JSPEC.EQ.ISP(I)).AND..NOT.LEAVESP(ISP(I)))THEN
        LINCL = .TRUE.
      ELSEIF(JSPEC.LT.0)THEN
        IGRP = -JSPEC
        IULIM = ISPGRP(IGRP,1)+1
        DO 220 IG=2,IULIM
        IF((ISP(I) .EQ. ISPGRP(IGRP,IG)).AND..NOT.LEAVESP(ISP(I)))THEN
          LINCL = .TRUE.
          GO TO 221
        ENDIF
  220   CONTINUE
      ENDIF
  221 CONTINUE
C
      IF(LINCL)THEN
        IF(DBH(I).GE.DLO .AND. DBH(I).LT.DHI)THEN
          IF(IWHO .EQ. 1) THEN
            TPACRE=PROB(I)
          ELSE
            TPACRE=WK4(I)
          ENDIF
          IF(JPNUM .GT. 0)TPACRE=TPACRE*(PI-FLOAT(NONSTK))
          CLKNT = CLKNT + TPACRE
          D2 = DBH(I)*DBH(I)*TPACRE
          SUMD2 = SUMD2 + D2
          CLBA = CLBA + D2*0.0054542
          CLQMD = CLQMD + D2
          IF(DEBUG)WRITE(JOSTND,*)' I,JSPEC,ISP,DBH,HT,TPACRE,CLKNT,',
     &    'CLBA,CLQMD,D2,SUMD2,JPNUM= ',
     &    I,JSPEC,ISP(I),DBH(I),HT(I),TPACRE,CLKNT,CLBA,CLQMD,D2,SUMD2,
     &    JPNUM
        ENDIF
      ENDIF
  250 CONTINUE
      IF(CLKNT .GT. 0.)CLQMD = SQRT(CLQMD/CLKNT)
      IF(CLQMD .GT. 0.)CRD = CLBA/SQRT(CLQMD)
      IF(DEBUG)WRITE(JOSTND,*)' LEAVING RDCLS ITRN,CLBA,CLQMD,CRD,CLKNT,
     &SUMD2,JPNUM= ',ITRN,CLBA,CLQMD,CRD,CLKNT,SUMD2,JPNUM
C
  260 CONTINUE
      RETURN
C
      ENTRY RDCLS2 (JSPEC,DLO,DHI,IWHO,CRD,JPNUM)
C
C----------
C  THIS ENTRY COMPUTES THE RELATIVE DENSITY FOR A GIVEN CLASS OF TREES
C  DEFINED BY SPEICES AND DBH RANGE, USING THE METHOD DESCRIBED
C  BY MARQUIS, D.A., AND R.L. ERNST. 1992. USER'S GUIDE TO SILVAH.
C  USDA FOREST SERVICE, NORTHEAST FOREST EXP STA, GEN TECH REP NE-96.
C
C  JSPEC = SPECIES INDEX (0 FOR ALL SPECIES)
C    DLO = LOWER DBH LIMIT (GREATER THAN OR EQUAL TO)
C    DHI = UPPER DBH LIMIT (LESS THAN)
C   IWHO = 1 IF PROB NEEDS TO BE USED IN THE CALCULATION
C          2 IF WK4 NEEDS TO BE USED IN THE CALCULATION (PARTS OF CUTS)
C    CRD = RELATIVE DENSITY FOR THE DEFINED CLASS
C  JPNUM = POINT NUMBER (IN FVS SEQUENCE FORMAT), 0 = ALL POINTS
C----------
      CALL DBCHK (DEBUG,'RDCLS2',6,ICYC)
      IF(DEBUG)WRITE(JOSTND,*)' ENTERING RDCLS2 (ENTRY PT IN SDICAL)'
C----------
C DETERMINE RELATIVE DENSITY BY SPECIES AND SIZE CLASS
C----------
      CRD = 0.
C
C  THIS IS CURRENTLY ALLOWED ONLY FOR THE NE VARIANT
C
      CALL VARVER(VVER)
      IF(VVER(:2).NE.'NE')GO TO 360
C
      IF(ITRN .LE. 0) GO TO 360
      DO 350 I=1,ITRN
      IF(JPNUM.GT.0 .AND. ITRE(I).NE.JPNUM)GO TO 350
C
      LINCL = .FALSE.
      IF((JSPEC.EQ.0 .OR. JSPEC.EQ.ISP(I)).AND..NOT.LEAVESP(ISP(I)))THEN
        LINCL = .TRUE.
      ELSEIF(JSPEC.LT.0)THEN
        IGRP = -JSPEC
        IULIM = ISPGRP(IGRP,1)+1
        DO 320 IG=2,IULIM
        IF((ISP(I) .EQ. ISPGRP(IGRP,IG)).AND..NOT.LEAVESP(ISP(I)))THEN
          LINCL = .TRUE.
          GO TO 321
        ENDIF
  320   CONTINUE
      ENDIF
  321 CONTINUE
C
      IF(LINCL)THEN
        IF((DBH(I).GE.DLO).AND.(DBH(I).LT.DHI).AND.(DBH(I).GE.1.))THEN
          IF(IWHO .EQ. 1) THEN
            TPACRE=PROB(I)
          ELSE
            TPACRE=WK4(I)
          ENDIF
          IF(JPNUM .GT. 0)TPACRE=TPACRE*(PI-FLOAT(NONSTK))
          IEQN=MAPNE(ISP(I))
          D2 = DBH(I)*DBH(I)
          
          IF(IEQN .EQ. 1)THEN
            CRD= CRD + MAX(0.,TPACRE*(0.0033033+0.020426*
     &                     DBH(I)+0.0006776*D2))
          ELSEIF(IEQN .EQ. 2)THEN
            CRD= CRD + MAX(0.,TPACRE*(-0.027142+0.024257*
     &                     DBH(I)+0.0015225*D2))
          ELSEIF(IEQN .EQ. 3)THEN
            CRD= CRD + MAX(0.,TPACRE*(-0.0027935+0.0058959*
     &                     DBH(I)+0.0047289*D2))
          ENDIF
          IF(DEBUG)WRITE(JOSTND,*)' I,FIAJSP,JSPEC,ISP,DBH,HT,TPACRE,',
     &    'D2,CRD,JPNUM,IEQN= ',
     &    I,FIAJSP(ISP(I)),JSPEC,ISP(I),DBH(I),HT(I),TPACRE,D2,
     &CRD,JPNUM,IEQN
        ENDIF
  345   CONTINUE
      ENDIF
  350 CONTINUE
      IF(DEBUG)WRITE(JOSTND,*)' LEAVING RDCLS2 ICYC,ITRN,',
     &'CRD,JPNUM,IEQN= ',ICYC,ITRN,CRD,JPNUM,IEQN
C
  360 CONTINUE
      RETURN
C
      ENTRY RDSLTR (JSPEC,IT,TREERD)
C
C----------
C  THIS ENTRY COMPUTES THE RELATIVE DENSITY CONTRIBUTION OF A GIVEN
C  TREE, USING THE METHOD DESCRIBED
C  BY MARQUIS, D.A., AND R.L. ERNST. 1992. USER'S GUIDE TO SILVAH.
C  USDA FOREST SERVICE, NORTHEAST FOREST EXP STA, GEN TECH REP NE-96.
C
C  JSPEC = TREE SPECIES
C     IT = TREE INDEX
C TREERD = RELATIVE DENSITY CONTRIBUTION OF THIS TREE
C----------
      CALL DBCHK (DEBUG,'RDSLTR',6,ICYC)
      IF(DEBUG)WRITE(JOSTND,*)' ENTERING RDSLTR (ENTRY PT IN SDICAL)'
C----------
C DETERMINE RELATIVE DENSITY BY SPECIES AND SIZE CLASS
C----------
C
C  THIS IS CURRENTLY ALLOWED ONLY FOR THE NE VARIANT
C
      CALL VARVER(VVER)
      IF(VVER(:2).NE.'NE')GO TO 365
C
      TREERD = 0.
C
      IF(DBH(IT).LT.1.)GOTO 365
      IEQN=MAPNE(JSPEC)
      D2 = DBH(IT)*DBH(IT)
      IF(IEQN .EQ. 1)THEN
        TREERD = MAX(0.,(0.0033033+0.020426*DBH(IT)+0.0006776*D2))
      ELSEIF(IEQN .EQ. 2)THEN
        TREERD = MAX(0.,(-0.027142+0.024257*DBH(IT)+0.0015225*D2))
      ELSEIF(IEQN .EQ. 3)THEN
        TREERD = MAX(0.,(-0.0027935+0.0058959*DBH(IT)+0.0047289*D2))
      ENDIF
      IF(DEBUG)WRITE(JOSTND,*)' LEAVING RDSLTR  - IEQN,JSPEC,IT,DBH,',
     &' TREERD= ',IEQN,JSPEC,IT,DBH(IT),TREERD
  365 CONTINUE
      RETURN
C
      ENTRY SILFTY
C
C----------
C  THIS ENTRY COMPUTES THE FOREST TYPE ACCORDING TO THE SILVAH
C  DEFINITIONS. THE SILVAH FOREST TYPE IS USED TO DETERMINE WHETHER
C  A THINRDSL TYPE THINNING IS VALID; A THINRDSL IS ONLY VALID IN
C  ONE OF THE DEFINED TYPES.
C
C  ISILFT = SILVAH FOREST TYPE (CARRIED IN THE VARCOM COMMON BLOCK)
C     0 = NO, THIS STAND DOES NOT MEET ONE OF THE DEFINED TYPES
C     1 = NORTHERN HARDWOOD: STANDS HAVING 65% OF THE BASAL AREA IN
C         SUGAR MAPLE, RED MAPLE, AMERICAN BEECH, YELLOW BIRCH, SWEET
C         BIRCH, EASTERN HEMLOCK, AMERICAN BASSWOOD, CUCUMBERTREE, BLACK
C         CHERRY, WHITE ASH, OR YELLOW-POPLAR
C     2 = NORTHERN HARDWOOD-HEMLOCK: STANDS MEETING THE REQUIREMENTS FOR
C         THE NORTHERN HARDWOOD TYPE THAT ALSO HAVE AT LEAST 50% OF THE
C         BASAL AREA IN EASTERN HEMLOCK
C     3 = ALLEGHENY HARDWOOD: STANDS MEETING THE REQUIREMENTS FOR THE
C         NORTHERN HARDWOOD TYPE THAT HAVE AT LEAST 25% OF THE BASAL AREA
C         IN BLACK CHERRY, WHITE ASH, AND YELLOW-POPLAR, AND LESS THAN
C         50% OF THE BASAL AREA IN EASTERN HEMLOCK
C     4 = OAK-HICKORY: STANDS THAT HAVE AT LEAST PART OF THEIR BASAL AREA
C         IN ANY OAK OR ANY HICKORY SPECIES
C     5 = TRANSITION: STANDS THAT HAVE AT LEAST 65% OF THE BASAL AREA IN
C         SPECIES OF EITHER THE NORTHERN HARDWOOD OR OAK-HICKORY TYPES,
C         BUT DO NOT QUALIFY FOR ANY OF THE OTHER TYPES ALONE
C
C----------
      CALL DBCHK (DEBUG,'SILFTY',6,ICYC)
      IF(DEBUG)WRITE(JOSTND,*)' ENTERING SILFTY (ENTRY PT IN SDICAL)'
C
      ISILFT = 0
      BA1=0.
      BA2=0.
      BA3=0.
      BA4=0.
      BA5=0.
      BATOT=0.
      RATIO1 = 0.
      RATIO2 = 0.
      RATIO3 = 0.
      RATIO5 = 0.
C
C  THIS IS CURRENTLY ALLOWED ONLY FOR THE NE VARIANT
C
      CALL VARVER(VVER)
      IF(VVER(:2).NE.'NE')GO TO 410
C
      DO 400 I=1,ITRN
      ISPC=ISP(I)
      BATOT=BATOT+DBH(I)*DBH(I)*0.0054542*PROB(I)
      IF(ISLFTM(ISPC) .EQ. 1)THEN
        BA1=BA1+DBH(I)*DBH(I)*0.0054542*PROB(I)
        IF(ISPC.EQ.16)BA2=BA2+DBH(I)*DBH(I)*0.0054542*PROB(I)
        IF(ISPC.EQ.54 .OR. ISPC.EQ.42 .OR. ISPC.EQ.46)
     &     BA3=BA3+DBH(I)*DBH(I)*0.0054542*PROB(I)
      ELSEIF(ISLFTM(ISPC) .EQ. 2)THEN
        BA4=BA4+DBH(I)*DBH(I)*0.0054542*PROB(I)
      ENDIF
  400 CONTINUE
      BA5=BA1+BA4
      IF(DEBUG)WRITE(JOSTND,*)' IN SILFTY BATOT,BA1,BA2,BA3,BA4,BA5= ',
     &  BATOT,BA1,BA2,BA3,BA4,BA5
      IF(BATOT .GT. 0.)THEN
        RATIO1 = BA1/BATOT
        RATIO2 = BA2/BATOT
        RATIO3 = BA3/BATOT
        RATIO5 = BA5/BATOT
      ENDIF
      IF(RATIO1 .GE. 0.65)ISILFT=1
      IF(RATIO1 .GE. 0.65 .AND. RATIO2.GE.0.50)ISILFT=2
      IF(RATIO1 .GE. 0.65 .AND. RATIO3 .GE. 0.25 .AND. RATIO2.LT.0.50)
     &  ISILFT=3
      IF(BA4 .GT. 0.)ISILFT=4
      IF(ISILFT.EQ.0 .AND. RATIO5.GE.0.65)ISILFT=5
  410 CONTINUE
      IF(DEBUG)WRITE(JOSTND,*)' LEAVING SILFTY ISILFT= ',ISILFT
      RETURN
C
      END
