      SUBROUTINE DGF(DIAM)
      IMPLICIT NONE
C----------
C CA $Id$
C----------
C  THIS SUBROUTINE COMPUTES THE VALUE OF DDS (CHANGE IN SQUARED
C  DIAMETER) FOR EACH TREE RECORD, AND LOADS IT INTO THE ARRAY
C  WK2.  DDS IS PREDICTED FROM SITE INDEX, LOCATION, SLOPE,
C  ASPECT, ELEVATION, DBH, CROWN RATIO, BASAL AREA IN LARGER TREES,
C  AND CCF.  THE SET OF TREE DIAMETERS TO BE USED IS PASSED AS THE
C  ARGUEMENT DIAM.  THE PROGRAM THUS HAS THE FLEXIBILITY TO
C  PROCESS DIFFERENT CALIBRATION OPTIONS.  THIS ROUTINE IS CALLED
C  BY **DGDRIV** DURING CALIBRATION AND WHILE CYCLING FOR GROWTH
C  PREDICTION.  ENTRY **DGCONS** IS CALLED BY **RCON** TO LOAD SITE
C  DEPENDENT COEFFICIENTS THAT NEED ONLY BE RESOLVED ONCE.
C----------
COMMONS
C
C
      INCLUDE 'PRGPRM.F77'
C
C
      INCLUDE 'CALCOM.F77'
C
C
      INCLUDE 'ARRAYS.F77'
C
C
      INCLUDE 'COEFFS.F77'
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
      INCLUDE 'PDEN.F77'
C
C
      INCLUDE 'VARCOM.F77'
C
C
COMMONS
C
C  DEFINITIONS OF INTERNAL VARIABLES.
C
C     DIAM -- ARRAY LOADED WITH TREE DIAMETERS (PASSED AS AN
C             ARGUEMENT).
C    DGFOR -- ARRAY LOADED WITH FOREST SPECIFIC CONSTANTS
C     DGDS -- ARRAY LOADED WITH DIAMETER SQUARED COEFFICIENTS
C     DGLD -- ARRAY CONTAINING COEFFICIENTS FOR THE LOG(DIAMETER)
C             TERM IN THE DDS MODEL.
C     DGCR -- ARRAY CONTAINING THE COEFFICIENTS FOR THE CROWN
C             RATIO TERM IN THE DDS MODEL.
C   DGCRSQ -- ARRAY CONTAINING THE COEFFICIENTS FOR THE CROWN
C             RATIO SQUARED TERM IN THE DDS MODEL.
C    DGBAL -- ARRAY CONTAINING COEFFICIENTS FOR THE BASAL AREA IN
C             LARGER TREES TERM IN THE DDS MODEL.
C   DGDBAL -- ARRAY CONTAINING COEFFICIENTS FOR THE INTERACTION
C             BETWEEN BASAL AREA IN LARGER TREES AND LN(DBH).
C    DGLBA -- ARRAY OF COEFFICIENTS FOR LOG(BASIL AREA).
C   DGSITE -- ARRAY OF COEFFICIENTS OF LOG(SITE).
C     DGEL -- ARRAY OF COEFFICIENTS OF ELEVATION.
C   DGELSQ -- ELEVATION SQUARED COEFFICIENTS.
C   DGCASP -- ARRAY OF COEFFICIENTS OF COS(ASPECT)*SLOPE.
C   DGSASP -- ARRAY OF COEFFICIENTS FOR SIN(ASPECT)*SLOPE.
C   DGSLOP -- ARRAY OF COEFFICIENTS FOR SLOPE.
C   DGSLSQ -- ARRAY OF COEFFICIENTS FOR SLOPE**2.
C   DGPCCF -- ARRAY OF COEFFICIENTS FOR POINT CROWN COMPETITION FACTOR.
C    DGHAH -- ARRAY OF COEFFICIENTS FOR THE RELATIVE HEIGHT TERM.
C   MAPSPC -- ARRAY MAPPING THE 49 SPECIES IN THIS VARIANT TO ONE
C             OF THE 13 GROWTH EQUATIONS
C   MAPLOC -- ARRAY MAPPING THE FOREST AND SPECIES TO THE
C             LOCATION CONSTANT
C   OBSERV -- NUMBER OF OBSERVATIONS IN DEVELOPMENT OF THE GROWTH
C             EQUATION
C----------
C SPECIES ORDER IN CA VARIANT:
C  1=PC  2=IC  3=RC  4=WF  5=RF  6=SH  7=DF  8=WH  9=MH 10=WB
C 11=KP 12=LP 13=CP 14=LM 15=JP 16=SP 17=WP 18=PP 19=MP 20=GP
C 21=JU 22=BR 23=GS 24=PY 25=OS 26=LO 27=CY 28=BL 29=EO 30=WO
C 31=BO 32=VO 33=IO 34=BM 35=BU 36=RA 37=MA 38=GC 39=DG 40=FL
C 41=WN 42=TO 43=SY 44=AS 45=CW 46=WI 47=CN 48=CL 49=OH
C
C ORDER OF DIAMETER GROWTH EQUATIONS:
C  1=IC    ALSO USED FOR PC & RC
C  2=WF    ALSO USED FOR BR
C  3=RF/SH
C  4=DF
C  5=KP    ALSO USED FOR CP, LM, GP, JU, PY, & CN
C  6=LP    ALSO USED FOR WB
C  7=SP    ALSO USED FOR WH & MH
C  8=WP
C  9=PP    ALSO USED FOR JP, MP, & OS
C 10=OAKS (LO/CY/BL/EO/WO/BO/VO/IO)
C          ALSO USED FOR BM, BU, RA, OA, WN, SY, AS, CW, WI, CL, OH
C 11=MA    ALSO USED FOR GC & DG
C 12=GS    BORROWED FROM WS VARIANT
C 13=TO    BORROWED FROM NC VARIANT
C
      INTEGER ISPC,I1,I2,JSPC,I3,I,IPCCF
      REAL CONSPP,D,CR,BAL,RELHT,CRID,PBAL,DDS,TDDS,XPPDDS,SASP,XSITE
      REAL DIAM(MAXTRE),DGLD(13),DGLBA(13),DGCR(13),DGCRSQ(13)
      REAL DGDBAL(13),DGBAL(13),DGFOR(5,13),DGDS(13),DGEL(13)
      REAL DGSASP(13),DGCASP(13),DGSLOP(13),DGSLSQ(13),DGELSQ(13)
      INTEGER MAPLOC(10,13),MAPSPC(MAXSP),OBSERV(13)
      REAL DGSITE(13),DGPCCF(13),DGHAH(13)
C
      DATA MAPSPC/
     &  1,  1,  1,  2,  3,  3,  4,  7,  7,  6,  5,  6,  5,  5,  9,
     &  7,  8,  9,  9,  5,  5,  2, 12,  5,  9, 10, 10, 10, 10, 10,
     & 10, 10, 10, 10, 10, 10, 11, 11, 11, 10, 10, 13, 10, 10, 10,
     & 10,  5, 10, 10/
C
      DATA DGLD/
     & 0.950418, 1.182104, 1.186676, 0.716226, 1.077154,
     & 1.218279, 0.886150, 0.825682, 0.738750, 1.310111,
     & 0.955569, 0.0     , 0.99531 /
C
      DATA DGCR/
     & 1.815305, 2.856578, 2.763519, 3.272451,-0.276387,
     & 3.167164, 1.478650, 1.675208, 3.454857, 0.271183,
     & 0.0     , 0.0     , 2.08524 /
C
      DATA DGCRSQ/
     & 0.0     ,-1.093354,-0.871061,-1.642904, 1.063732,
     &-1.568333, 0.0     , 0.0     ,-1.773805, 0.0     ,
     & 0.0     , 0.0     ,-0.98396 /
C
      DATA DGSITE/
     & 0.820451, 0.365679, 0.492695, 0.759305, 0.0     ,
     & 0.566946, 0.963375, 0.724300, 1.011504, 0.213526,
     & 1.334008, 0.0     , 0.00659 /
C
      DATA DGDBAL/
     &-0.005433,-0.005992,-0.003728,-0.008787, 0.0     ,
     & 0.0     ,-0.006263,-0.002133,-0.013091, 0.0     ,
     &-0.005893, 0.0     ,-0.00147 /
C
      DATA DGLBA/
     &-0.000016,-0.058039,-0.122905,-0.028564, 0.0     ,
     &-0.267873,-0.129146,-0.203636,-0.131185, 0.0     ,
     &-0.408462, 0.0     , 0.0     /
C
      DATA DGBAL/
     & 0.0     , 0.0     , 0.0     , 0.0     ,-0.000893,
     & 0.0     , 0.0     , 0.0     , 0.0     , 0.0     ,
     & 0.0     , 0.0     , 0.0     /
C
      DATA DGPCCF/
     &-0.000779,-0.001014, 0.0     ,-0.000224, 0.0     ,
     &-0.000338, 0.0     , 0.0     ,-0.000593,-0.000473,
     & 0.0     , 0.0     ,-0.00018 /
C
      DATA DGHAH/
     & 0.0     , 0.0     , 0.0     , 0.0     , 0.0     ,
     & 0.0     , 0.0     , 0.0     , 0.0     , 0.0     ,
     & 0.0     , 0.0     , 0.50155 /
C----------
C  IDTYPE IS A HABITAT TYPE INDEX THAT IS COMPUTED IN **RCON**.
C  ASPECT IS STAND ASPECT.  OBSERV CONTAINS THE NUMBER OF
C  OBSERVATIONS BY HABITAT CLASS BY SPECIES FOR THE UNDERLYING
C  MODEL (THIS DATA IS ACTUALLY USED BY **DGDRIV** FOR
C  CALIBRATION).
C----------
      DATA  OBSERV/
     &  613., 3759., 2062., 5400.,   84.,
     &  372.,  561.,  253., 2482.,  306.,
     &  336., 1762., 6504./
C----------
C  DGFOR CONTAINS LOCATION CLASS CONSTANTS FOR EACH SPECIES.
C  MAPLOC IS AN ARRAY WHICH MAPS FOREST ONTO A LOCATION CLASS.
C----------
      DATA MAPLOC/
     & 1,1,1,1,2,1,1,1,1,1,
     & 1,1,1,1,1,1,1,1,1,1,
     & 1,1,1,2,1,1,1,1,1,1,
     & 1,2,2,1,3,4,5,4,4,5,
     & 1,1,1,1,1,1,1,1,1,1,
     & 1,1,1,2,1,1,1,1,1,1,
     & 1,1,1,1,1,1,1,1,1,1,
     & 1,1,1,1,1,1,1,1,1,1,
     & 1,1,1,1,1,1,1,1,1,1,
     & 1,1,1,1,1,1,1,1,1,1,
     & 1,1,1,1,1,1,1,1,1,1,
     & 1,1,1,1,1,1,1,1,1,1,
     & 1,1,1,1,1,1,1,1,1,1/
C
      DATA DGFOR/
     & -3.428338, -3.966547,  0.0     ,  0.0     ,  0.0     ,
     & -2.108357,  0.0     ,  0.0     ,  0.0     ,  0.0     ,
     & -2.073942, -1.943608,  0.0     ,  0.0     ,  0.0     ,
     & -1.877695, -2.099646, -2.211587, -1.955301, -2.078432,
     &  0.564402,  0.0     ,  0.0     ,  0.0     ,  0.0     ,
     & -2.058828, -1.596998,  0.0     ,  0.0     ,  0.0     ,
     & -2.397678,  0.0     ,  0.0     ,  0.0     ,  0.0     ,
     & -1.626879,  0.0     ,  0.0     ,  0.0     ,  0.0     ,
     & -2.922255,  0.0     ,  0.0     ,  0.0     ,  0.0     ,
     & -1.958189,  0.0     ,  0.0     ,  0.0     ,  0.0     ,
     & -3.344700,  0.0     ,  0.0     ,  0.0     ,  0.0     ,
     &  0.0     ,  0.0     ,  0.0     ,  0.0     ,  0.0     ,
     & -0.94563 ,  0.0     ,  0.0     ,  0.0     ,  0.0     /
C----------
C  DGDS CONTAINS COEFFICIENTS FOR THE DIAMETER SQUARED TERMS
C  IN THE DIAMETER INCREMENT MODELS    ARRAYED BY FOREST BY
C  SPECIES.
C----------
      DATA DGDS/
     & -0.0002385,
     & -0.0006362,
     & -0.0004572,
     & -0.0002723,
     &  0.0      ,
     & -0.0014178,
     & -0.0002528,
     & -0.0000731,
     & -0.0004708,
     & -0.0003048,
     &  0.0      ,
     &  0.0      ,
     & -0.000373 /
C----------
C  DGEL CONTAINS THE COEFFICIENTS FOR THE ELEVATION TERM IN THE
C  DIAMETER GROWTH EQUATION.
C  DGSASP CONTAINS THE COEFFICIENTS FOR THE SIN(ASPECT)*SLOPE
C  TERM IN THE DIAMETER GROWTH EQUATION.  DGCASP CONTAINS THE
C  COEFFICIENTS FOR THE COS(ASPECT)*SLOPE TERM IN THE DIAMETER
C  GROWTH EQUATION.  DGSLOP CONTAINS THE COEFFICIENTS FOR THE
C  SLOPE TERM IN THE DIAMETER GROWTH EQUATION.  DGSLSQ CONTAINS
C  COEFFICIENTS FOR THE (SLOPE)**2 TERM IN THE DIAMETER GROWTH
C  MODELS.  ALL OF THESE ARRAYS ARE SUBSCRIPTED BY SPECIES.
C----------
      DATA DGCASP/
     & 0.0     ,-0.315227,-0.444594,-0.151727, 0.649870,
     & 0.0     ,-0.280294,-0.179510, 0.0     , 0.0     ,
     & 0.0     , 0.0     ,-0.19935 /
C
      DATA DGSASP/
     & 0.0     , 0.097350, 0.139180, 0.018681, 0.951834,
     & 0.0     ,-0.014463,-0.562259, 0.0     , 0.0     ,
     & 0.0     , 0.0     ,-0.03587 /
C
      DATA DGSLOP/
     & 0.0     ,-0.206267, 0.0     ,-0.339369, 0.0     ,
     & 0.0     ,-0.581722,-0.544867, 0.0     , 0.0     ,
     & 0.0     , 0.0     , 0.73530 /
C
      DATA DGSLSQ/
     & 0.0     , 0.0     , 0.0     , 0.0     , 0.0     ,
     & 0.0     , 0.0     , 0.0     , 0.0     , 0.0     ,
     & 0.0     , 0.0     ,-0.99561 /
C
      DATA DGEL/
     & 0.0     , 0.0301  , 0.0248,-0.0141  , 0.0     ,
     & 0.0     , 0.0     , 0.0   ,-0.003784, 0.0049  ,
     & 0.0     , 0.0     , 0.0     /
C
      DATA DGELSQ/
     & 0.0     ,-0.00030732,-0.00033429, 0.00024083, 0.0       ,
     & 0.0     , 0.0       , 0.0       , 0.00006660,-0.00008781,
     & 0.0     , 0.0       , 0.0     /
C
      LOGICAL DEBUG
C-----------
C  CHECK FOR DEBUG.
C-----------
      CALL DBCHK (DEBUG,'DGF',3,ICYC)
      IF(DEBUG) WRITE(JOSTND,3)ICYC
    3 FORMAT(' ENTERING SUBROUTINE DGF  CYCLE =',I5)
C----------
C  DEBUG OUTPUT: MODEL COEFFICIENTS.
C----------
      IF(DEBUG) WRITE(JOSTND,*) 'IN DGF,HTCON=',HTCON,
     *'ELEV=',ELEV,'RELDEN=',RELDEN
      IF(DEBUG)
     & WRITE(JOSTND,9000) DGCON,DGDSQ
 9000 FORMAT(/11(1X,F10.5))
C----------
C  BEGIN SPECIES LOOP.  ASSIGN VARIABLES WHICH ARE SPECIES
C  DEPENDENT
C----------
      DO 20 ISPC=1,MAXSP
      I1=ISCT(ISPC,1)
      IF(I1.EQ.0) GO TO 20
      I2=ISCT(ISPC,2)
      JSPC=MAPSPC(ISPC)
      CONSPP= DGCON(ISPC) + COR(ISPC)
C----------
C  BEGIN TREE LOOP WITHIN SPECIES ISPC.
C----------
      DO 10 I3=I1,I2
      I=IND1(I3)
      D=DIAM(I)
      IF (D.LE.0.0) GOTO 10
      CR=ICR(I)*0.01
      BAL = (1.0 - (PCT(I)/100.)) * BA
      IPCCF=ITRE(I)
      RELHT = 0.0
      IF(AVH .GT. 0.0) RELHT=HT(I)/AVH
      IF(RELHT .GT. 1.5)RELHT=1.5
C----------
C  GIANT SEQUOIA USES DIFFERENT FUNCTIONAL FORM
C----------
      IF(ISPC .EQ. 23) THEN
        CRID = ((ICR(I)*ICR(I)) / (ALOG(D+1.)))/1000.
        IF(D .LT. 2.) CRID=1.8
        PBAL=PTBAA(ITRE(I))*(1.0-(PCT(I)/100.))
        IF(PBAL .LT. 0.) PBAL=BAL
        DDS = CONSPP + 1.26883*ALOG(D) - 0.35325*D*D/1000.
     &      + 0.27986*CRID - 0.79922*PBAL/(ALOG(D+1.))/100.
      ELSE
C----------
C  THIS FUNCTION OCCASIONALLY GIVES UNDERFLOW ERROR ON PC. SPLITTING
C  IT UP INTO TWO PARTS IS A TEMPORARY SOLUTION WHICH WORKS. GD 2/20/97
C----------
         DDS = CONSPP + DGLD(JSPC)*ALOG(D)
     &       + CR*(DGCR(JSPC) + CR*DGCRSQ(JSPC))
     &       + DGDSQ(ISPC)*D*D  + DGDBAL(JSPC)*BAL/(ALOG(D+1.0))
         DDS = DDS + DGPCCF(JSPC)*PCCF(IPCCF) + DGHAH(JSPC)*RELHT
     &       + DGLBA(JSPC)*ALOG(BA) + DGBAL(JSPC)*BAL
      ENDIF
C----------
C  TANOAK EQN IS A 5-YR ESTIMATE, ADJUST TO 10-YR BASIS.
C
      IF(ISPC .EQ. 42) THEN
        TDDS=EXP(DDS)
        DDS=ALOG(TDDS*2.)
      ENDIF
C----------
    5 IF(DEBUG) WRITE(JOSTND,8000)
     &I,ISPC,CONSPP,D,BA,CR,BAL,PCCF(IPCCF),RELDEN,HT(I),AVH
 8000 FORMAT(1H0,'IN DGF 8000F',2I5,9F11.4)
C---------
C     CALL PPDGF TO GET A MODIFICATION VALUE FOR DDS THAT ACCOUNTS
C     FOR THE DENSITY OF NEIGHBORING STANDS.
C
      XPPDDS=0.
      CALL PPDGF (XPPDDS)
C
      DDS=DDS+XPPDDS
C---------
      IF(DDS.LT.-9.21) DDS=-9.21
      WK2(I)=DDS
C----------
C  END OF TREE LOOP.  PRINT DEBUG INFO IF DESIRED.
C----------
      IF(DEBUG)THEN
      WRITE(JOSTND,9001) I,ISPC,DDS
 9001 FORMAT(' IN DGF, I=',I4,',  ISPC=',I3,',  LN(DDS)=',F7.4)
      ENDIF
   10 CONTINUE
C----------
C  END OF SPECIES LOOP.
C----------
   20 CONTINUE
      IF(DEBUG) WRITE(JOSTND,100)ICYC
  100 FORMAT(' LEAVING SUBROUTINE DGF  CYCLE =',I5)
      RETURN
      ENTRY DGCONS
C----------
C  ENTRY POINT FOR LOADING COEFFICIENTS OF THE DIAMETER INCREMENT
C  MODEL THAT ARE SITE SPECIFIC AND NEED ONLY BE RESOLVED ONCE.
C----------
C  CHECK FOR DEBUG.
C----------
      CALL DBCHK (DEBUG,'DGF',3,ICYC)
C----------
C  ENTER LOOP TO LOAD SPECIES DEPENDENT VECTORS.
C----------
      DO 30 ISPC=1,MAXSP
      JSPC=MAPSPC(ISPC)
      ISPFOR=MAPLOC(IFOR,JSPC)
      SASP =
     &                 +(DGSASP(JSPC) * SIN(ASPECT)
     &                 + DGCASP(JSPC) * COS(ASPECT)
     &                 + DGSLOP(JSPC)) * SLOPE
     &                 + DGSLSQ(JSPC) * SLOPE * SLOPE
      XSITE=SITEAR(ISPC)
C     IF(ISPC.EQ. 9)XSITE=XSITE*3.281
C----------
C  GIANT SEQUOIA USES DIFFERENT FORMULATION.
C----------
      IF(ISPC .EQ. 23) THEN
        DGCON(23) = -0.4297 + 0.01401*SITEAR(23)
        DGDSQ(23) = 0.
      ELSE
        DGCON(ISPC) =
     &                   DGFOR(ISPFOR,JSPC)
     &                 + DGEL(JSPC) * ELEV
     &                 + DGELSQ(JSPC)*ELEV*ELEV
     &                 + DGSITE(JSPC)*ALOG(XSITE)
     &                 + SASP
        DGDSQ(ISPC)=DGDS(JSPC)
      ENDIF
      ATTEN(ISPC)=OBSERV(JSPC)
      SMCON(ISPC)=0.
      IF(DEBUG)WRITE(JOSTND,9030)DGFOR(ISPFOR,JSPC),
     &DGEL(JSPC),ELEV,DGSASP(JSPC),ASPECT,
     &DGCASP(JSPC),DGSLOP(JSPC),SLOPE,DGSITE(JSPC),
     &XSITE,DGCON(ISPC),SASP
 9030 FORMAT(' IN DGF 9030',13F9.5)
C----------
C  IF READCORD OR REUSCORD WAS SPECIFIED (LDCOR2 IS TRUE) ADD
C  LN(COR2) TO THE BAI MODEL CONSTANT TERM (DGCON).  COR2 IS
C  INITIALIZED TO 1.0 IN BLKDATA.
C----------
      IF (LDCOR2.AND.COR2(ISPC).GT.0.0) DGCON(ISPC)=DGCON(ISPC)
     &  + ALOG(COR2(ISPC))
   30 CONTINUE
      RETURN
      END
