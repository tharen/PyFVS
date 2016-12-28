      SUBROUTINE DGF(DIAM)
      use plot_mod
      use arrays_mod
      use contrl_mod
      use coeffs_mod
      use outcom_mod
      use pden_mod
      use prgprm_mod
      use calcom_mod
      implicit none
C----------
C  **DGF--CS    DATE OF LAST REVISION:  05/19/08
C----------
C  THIS SUBROUTINE COMPUTES THE VALUE OF DDS (CHANGE IN SQUARED
C  DIAMETER) FOR EACH TREE RECORD, AND LOADS IT INTO THE ARRAY
C  WK2.  DIAMETER GROWTH IS PREDICTED FROM DBH, SITE INDEX, AND
C  CROWN RATIO.  DIAMETER GROWTH IS THEN MULTIPLIED BY A MODIFIER
C  FUNCTION WHICH IS COMPUTED FROM DBH, STAND'S AVERAGE DBH, MAX
C  BASAL AREA EXPECTED FOR THE SPECIES, AND TREE'S BASAL AREA.
C  AN ADJUSTMENT FACTOR, COMPUTED FROM DBH, IS THEN ADDED.  THE
C  SET OF TREE DIAMETERS TO BE USED IS PASSED AS THE ARGUMENT TO
C  DIAM.  THE PROGRAM THUS HAS THE FLEXIBILITY TO PROCESS DIFFERENT
C  CALIBRATION OPTIONS.  THIS ROUTINE IS CALLED BY **DGDRIV** DURING
C  CALIBRATION AND WHILE CYCLING FOR GROWTH PREDICTION.  ENTRY
C  **DGCONS** IS CALLED BY **RCON** TO LOAD SITE DEPENDENT COEFFICIENTS
C  THAT NEED ONLY BE RESOLVED ONCE.
C
C  DIAMETER GROWTH EQUATIONS ARE FROM CS-TWIGS VERSION 3.01
C  OR FOR DIAMETER GROWTH SEE: U.S. DEPARTMENT OF AGRICULTURE, FOREST
C  SERVICE. 1983. BIOMASS ENERGY TECHNOLOGY FINAL REPORT, SILVICULTURAL
C  PRODUCTION SYSTEMS, DEPARTMENT OF ENERGY, CONTRACT DE-AI01-80CS83013,
C  PROJECTING FUTURE SUPPLIES OF BIOMASS FOR ENERGY IN THE NORTH CENTRAL
C  UNITED STATES. ST. PAUL, MN: U.S. DEPARTMENT OF AGRICULTURE, FOREST
C  SERVICE, NORTH CENTRAL FOREST EXPERIMENT STATION. 38 P.
C----------
COMMONS
      INCLUDE 'TWIGCOM.F77'
C
C----------
C  VARIABLES DEFINED:
C  PADG  -- POTENTIAL ANNUAL DIAMETER GROWTH
C  CM    -- DIAMETER GROWTH COMPETITION MODIFIER FUNCTION
C  DELD  -- TREE'S DIAMETER GROWTH FOR ONE YEAR
C  COEFFICIENTS DEFINED:
C  A1-A6 -- COEFFICIENTS FOR PADG EQUATION
C  B1-B3 -- COEFFICIENTS FOR CM EQUATION
C           THESE HAVE BEEN MOVED TO SUBROUTINE **BALMOD** SO THE
C           COMPETITION MODIFER CAN BE ALSO BE APPLIED TO HEIGHT GROWTH.
C----------
      LOGICAL DEBUG
C
      REAL DIAM(MAXTRE)
      REAL A1(MAXSP),A2(MAXSP),A3(MAXSP),A4(MAXSP),A5(MAXSP),
     &   A6(MAXSP),TEMD(MAXTRE)
C
      INTEGER I,ISPC,I1,I2,I3,ILOOP
      REAL CR,BAL,D,TRBA,PADG,CM,DELD,QTRBA,QDBH,BARK,BRATIO,DIAGR
      REAL DDS,XPPDDS,SI
C----------
C  COEFFICIENTS FOR GROWTH EQUATION.
C----------
      DATA A1/
     % 2*.039224814,.039963074,2*.039963074,2*.039963074,2*.064466063,
     % 4*.035025816,10*.052744503,.093056063,3*.065654684,.13392107,
     % 3*.16952048,.093056063,8*.093662312,.077536771,.093056063,
     % .048570057,3*.065654684,.060901323,2*.063318474,.071763840,
     % .087972567,.038250878,4*.036276511,2*.045092032,.086549455,
     % 8*.081021551,10*.093056063,7*.13392107,12*.071576415/
      DATA A2/
     % 2*.51499115,5*.14593018,2*.43471091,4*.49430180,
     % 10*.55319832,.64110355,3*.50860453,.54183673,3*.72999623,
     % .64110355,8*.62629256,.59211834,.64110355,.61582056,3*.50860453,
     % .55887542,2*.59455266,.53296542,.65940355,.54171348,4*.60464863,
     % 2*.62527056,.69260309,8*.41667582,10*.64110355,7*.54183673,
     % 12*.64291031/
      DATA A3/
     % 2*.01485853,5*.009709743,2*.010537837,4*.014747469,10*.018400205,
     % .030538159,3*.014956433,.030730406,3*.076329602,.030538159,
     % 8*.034261393,.016180938,.030538159,.014586389,3*.014956433,
     % .013610611,2*.015945405,.018148446,.041193333,.013671775,
     % 4*.010893541,2*.021865447,.03687525,8*.013232815,10*.030538159,
     % 7*.030730406,12*.029429823/
      DATA A4/
     % 2*.39741560,5*.82518771,2*.080160555,4*.49661284,
     % 10*.42131961,.62208533,3*.72314405,.75132310,3*.49370041,
     % .62208533,8*.68950711,.61431290,.62208533,.85146939,3*.72314405,
     % .73497649,2*.69043253,.79907058,.72584313,.85592983,4*.20650912,
     % 2*.67031654,.0062666089,8*.093047877,10*.62208533,7*.7513231,
     % 12*.29425402/
      DATA A5/
     % 2*.00071802997,5*.00047569978,2*.0074173819,4*.0077438829,
     % 10*.0053479814,.0,3*.00030529956,.0031738981,3*.0057751026,
     % .0,8*.0043781142,.0,.0,.0011077119,3*.00030529956,
     % .0034681864,2*.003852588,.0020395398,.0013168097,.0028003458,
     % 4*.0068989163,2*.00017213157,.012731064,8*.0032288682,10*.0,
     % 7*.0031738981,12*.0012965451/
      DATA A6/
     % 2*.074913965,5*.045452244,2*.080556935,4*.0,
     % 10*.043100701,.085091328,3*.0585274,.0,3*.0049691855,
     % .085091328,8*.0042879169,.08770846,.085091328,.011212651,
     % 3*.0585274,.0084744098,2*.011731807,.013021288,.034728069,
     % .0,4*.073312664,2*.058600031,.026572205,8*.11176141,
     % 10*.085091328,7*.0,12*.13964282/
C-----------
C  SEE IF WE NEED TO DO SOME DEBUG.
C-----------
      CALL DBCHK (DEBUG,'DGF',3,ICYC)
      IF(DEBUG) WRITE(JOSTND,3)ICYC
    3 FORMAT(' ENTERING SUBROUTINE DGF  CYCLE =',I5)
C----------
C  STORE DIAMETERS FOR ITERATIVE PROCESSING
C----------
      DO 4 I=1,MAXTRE
      TEMD(I)=DIAM(I)
    4 CONTINUE
C----------
C  BEGIN SPECIES LOOP.  ASSIGN VARIABLES WHICH ARE SPECIES DEPENDENT
C----------
      DO 20 ISPC=1,MAXSP
      I1=ISCT(ISPC,1)
      IF(I1.EQ.0) GO TO 20
      I2=ISCT(ISPC,2)
      SI=SITEAR(ISPC)
C----------
C  BEGIN TREE LOOP WITHIN SPECIES ISPC.
C----------
      DO 10 I3=I1,I2
      I=IND1(I3)
      CR=FLOAT(ICR(I))/10.
      BAL=(1.0-(PCT(I)/100.))*BA
      IF(DEBUG)WRITE(JOSTND,*)' CR=',CR,' BAL=',BAL,
     & ' PCT=',PCT(I),' BA=',BA
C----------
C  ITERATE 10 TIMES, SINCE DG EQUATION IS AN ANNUAL BASIS
C----------
      DO 1000 ILOOP=1,10
      D=TEMD(I)
      IF(DEBUG)WRITE(JOSTND,*)' I=',I,' D=',D,' SI=',SI
      TRBA=D*D*.0054542
      WK2(I)=0.0
      IF(D .LE. 0.0) GO TO 10
C----------
C  COMPUTE POTENTIAL ANNUAL DIAMETER GROWTH (PADG)
C----------
      PADG = (A1(ISPC)*TRBA**A2(ISPC)-A3(ISPC)*TRBA)
     &       *(A4(ISPC)+A5(ISPC)*SI+A6(ISPC)*CR)
      IF (PADG .LT. 0.0) PADG = 0.0
      CALL BALMOD(ISPC,D,BAL,BA,CM,DEBUG)
C----------
C  COMPUTE ACTUAL DIAMETER GROWTH FOR ONE YEAR (DELD)
C----------
      DELD = (PADG*CM)
      IF (DEBUG)WRITE(JOSTND,*)' DELD=',DELD,' PADG=',PADG,' CM=',CM
      IF (DELD .LE. 0.0) DELD = 0.000001
      QTRBA=DELD+TRBA
      QDBH=(QTRBA/.0054542)**.5
      TEMD(I)=QDBH
 1000 CONTINUE
C
      BARK=BRATIO(ISPC,TEMD(I),HT(I))
      DIAGR=(TEMD(I)-DIAM(I))*BARK
      IF(DEBUG)
     &WRITE(JOSTND,*)' I,TEMD,DIAM,BARK,DIAGR= ',
     &I,TEMD(I),DIAM(I),BARK,DIAGR
      IF (LDCOR2 .AND. COR2(ISPC) .GT. 0.0) DIAGR=DIAGR*COR2(ISPC)
      IF(DIAGR.LE. .0001) DIAGR=.0001
      IF(DEBUG)WRITE(16,*)' I,ISPC,COR2,DIAGR,LDCOR2= ',
     &I,ISPC,COR2(ISPC),DIAGR,LDCOR2
      DDS=DIAGR*(2.0*DIAM(I)*BARK+DIAGR)
      IF(DEBUG)WRITE(16,*)' I,DIAGR,DIAM(I),BARK,DDS= ',
     &I,DIAGR,DIAM(I),BARK,DDS
C---------
C     CALL PPDGF TO GET A MODIFICATION VALUE FOR DDS THAT ACCOUNTS
C     FOR THE DENSITY OF NEIGHBORING STANDS.
C
      XPPDDS=0.
      CALL PPDGF (XPPDDS)
C
C---------
      IF (DDS .GT. 0.0) WK2(I)=ALOG(DDS)+COR(ISPC)+XPPDDS
   10 CONTINUE
C----------
C  END OF SPECIES LOOP.
C----------
   20 CONTINUE
      IF(DEBUG)WRITE(JOSTND,100)ICYC
  100 FORMAT(' LEAVING SUBROUTINE DGF  CYCLE =',I5)
      RETURN
      ENTRY DGCONS
C----------
C  ENTER LOOP TO LOAD SPECIES DEPENDENT VECTORS.
C----------
      DO 30 ISPC=1,MAXSP
      ATTEN(ISPC)=1000.
      DGCON(ISPC)=0.
      SMCON(ISPC)=0.
   30 CONTINUE
      RETURN
      END
