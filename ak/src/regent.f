      SUBROUTINE REGENT(LESTB,ITRNIN)
      use htcal_mod
      use multcm_mod
      use pden_mod
      use arrays_mod
      use contrl_mod
      use coeffs_mod
      use outcom_mod
      use plot_mod
      use varcom_mod
      use prgprm_mod
      use calcom_mod
      use estcor_mod
      implicit none
C----------
C  **REGENT--AK   DATE OF LAST REVISION:  01/11/12
C----------
C  THIS SUBROUTINE COMPUTES HEIGHT AND DIAMETER INCREMENTS FOR
C  SMALL TREES.  THE HEIGHT INCREMENT MODEL IS APPLIED TO TREES
C  THAT ARE LESS THAN 5 INCHES DBH
C  AND THE DBH INCREMENT MODEL IS APPLIED TO TREES THAT ARE LESS
C  THAN 3 INCHES DBH.  FOR TREES THAT ARE GREATER THAN 2 INCHES
C  DBH, HEIGHT INCREMENT PREDICTIONS
C  ARE AVERAGED WITH THE PREDICTIONS FROM THE LARGE TREE MODEL.
C  DIAMETER IS ASSIGNED FROM A HEIGHT-DIAMETER FUNCTION
C  INCREMENT IS COMPUTED BY SUBTRACTION.
C  THIS ROUTINE IS CALLED FROM **CRATET** DURING CALIBRATION AND
C  FROM **TREGRO** DURING CYCLING.  ENTRY **REGCON** IS CALLED FROM
C  **RCON** TO LOAD MODEL PARAMETERS THAT NEED ONLY BE RESOLVED ONCE.
C----------
COMMONS
C----------
C  DIMENSIONS FOR INTERNAL VARIABLES:
C
C   CORTEM -- A TEMPORARY ARRAY FOR PRINTING CORRECTION TERMS.
C   NUMCAL -- A TEMPORARY ARRAY FOR PRINTING NUMBER OF HEIGHT
C             INCREMENT OBSERVATIONS BY SPECIES.
C    RHCON -- CONSTANT FOR THE HEIGHT INCREMENT MODEL.
C     XMAX -- UPPER END OF THE RANGE OF DIAMETERS OVER WHICH HEIGHT
C             INCREMENT PREDICTIONS FROM SMALL AND LARGE TREE MODELS
C             ARE AVERAGED.
C     XMIN -- LOWER END OF THE RANGE OF DIAMETERS OVER WHICH HEIGHT
C             INCREMENT PREDICTIONS FROM THE SMALL AND LARGE TREE
C             ARE AVERAGED.
C----------
      EXTERNAL RANN
      LOGICAL DEBUG,LESTB,LSKIPH
      INTEGER KK,ISPEC,KOUT,IPCCF,IREFI,N,KSPC,I3,I,IP
      INTEGER IICR,K,L,ITRNIN,NUMCAL(MAXSP),I1,I2,ISPC
      REAL CON,XMX,XMN,RSI,D,H,BARK,TBAL,TCR,RAN,RCR
      REAL CRCODE,PTCCF,POTHTG,SATBA,BBALIM,PNTWT,BAWT,HBA
      REAL XHMOD,HTGR,ZZRAN,XPPMLT,XWT,HK,DKK,DK,DDS,HTNEW
      REAL SCALE3,CORNEW,SNP,SNX,SNY,EDH,P,TERM,XMIN(MAXSP),XMAX(MAXSP)
      REAL CORTEM(MAXSP),DIAM(MAXSP),REGYR,FNT,SCALE,SCALE2,XRHGRO
      REAL BACHLO,HBALIM,XRDGRO,BRATIO
      CHARACTER SPEC*2
C----------
C  DATA STATEMENTS.
C----------
      DATA XMAX / 13*5.0 /, XMIN/ 9*2.0, 2*3.0, 2*2.0 /
      DATA REGYR/ 5.0 /
      DATA DIAM /0.4,5*0.3,0.4,0.4,0.3,3*0.2,0.3/
C-----------
C  CHECK FOR DEBUG.
C-----------
      LSKIPH=.FALSE.
      CALL DBCHK (DEBUG,'REGENT',6,ICYC)
      IF(DEBUG) WRITE(JOSTND,9980)ICYC
 9980 FORMAT('ENTERING SUBROUTINE REGENT  CYCLE =',I5)
C----------
C  IF THIS IS THE FIRST CALL TO REGENT, BRANCH TO STATEMENT 40 FOR
C  MODEL CALIBRATION.
C----------
      IF(LSTART) GOTO 40
      CALL MULTS (3,IY(ICYC),XRHMLT)
      CALL MULTS(6,IY(ICYC),XRDMLT)
      IF (ITRN.LE.0) GO TO 91
C----------
C  HEIGHT INCREMENT IS DERIVED FROM A HEIGHT-AGE CURVE AND IS NOMINALLY
C  BASED ON A 10-YEAR GROWTH PERIOD.  SCALE IS USED TO CONVERT
C  HEIGHT INCREMENT PREDICTIONS TO A FINT-YEAR PERIOD.  DIAMETER
C  INCREMENT IS PREDICTED FROM CHANGE IN HEIGHT, AND IS SCALED TO A 10-
C  YEAR PERIOD BY APPLICATION OF SCALE2.  DIAMETER INCREMENT
C  IS CONVERTED TO A FINT-YEAR BASIS IN **UPDATE**.
C----------
      FNT=FINT
      IF(LESTB) THEN
        IF(FINT.LE.5.0) THEN
          LSKIPH=.TRUE.
        ELSE
          FNT=FNT-5.0
        ENDIF
      ENDIF
      SCALE=FNT/REGYR
      SCALE2=YR/FNT
C----------
C  ENTER GROWTH PREDICTION LOOP.  PROCESS EACH SPECIES AS A GROUP;
C  LOAD CONSTANTS FOR NEXT SPECIES.
C----------
      DO 30 ISPC=1,MAXSP
      I1=ISCT(ISPC,1)
      IF(I1.EQ.0) GO TO 30
      I2=ISCT(ISPC,2)
      XRHGRO=XRHMLT(ISPC)
      XRDGRO=XRDMLT(ISPC)
      CON=RHCON(ISPC) * EXP(HCOR(ISPC))
      XMX=XMAX(ISPC)
      XMN=XMIN(ISPC)
      RSI=SITEAR(ISPC)
      IF(RSI.LT.40.)RSI=40.
C----------
C  PROCESS NEXT TREE RECORD.
C----------
      DO 25 I3=I1,I2
      I=IND1(I3)
      IP=ITRE(I)
      D=DBH(I)
      IF(D .GE. XMX)GO TO 25
      H=HT(I)
      BARK=BRATIO(ISPC,D,H)
      TBAL=(1.0 - (PCT(I)/100.)) * BA
C----------
C  BYPASS INCREMENT CALCULATIONS IF CALLED FROM ESTAB AND THIS IS NOT A
C  NEWLY CREATED TREE.
C----------
      IF(LESTB) THEN
        IF(I.LT.ITRNIN) GO TO 25
C----------
C  ASSIGN CROWN RATIO FOR NEWLY ESTABLISHED TREES.
C----------
        TCR = 0.89722 - 0.0000461*PCCF(IP)
    1   CONTINUE
        RAN = BACHLO(0.0,1.0,RANN)
        IF(RAN .LT. -1.0 .OR. RAN .GT. 1.0) GO TO 1
        TCR = TCR + 0.07985 * RAN
        IF(TCR .GT. .90) TCR = .90
        IF(TCR .LT. .20) TCR = .20
        ICR(I)=(TCR*100.0)+0.5
      ENDIF
      IICR=((ICR(I)-1)/10)+1
      IF(IICR.GT.9) IICR=9
      RCR=FLOAT(IICR)
      K=I
      L=0
      IF(LSKIPH) THEN
        HTG(K)=0.0
        GO TO 5
      ENDIF
C----------
C     RETURN HERE TO PROCESS NEXT TRIPLE.
C----------
    2 CONTINUE
C----------
C CALCULATE HEIGHT GROWTH --- RA AND CW FROM WC VARIANT
C----------
      IF(ISPC.EQ.10 .OR. ISPC.EQ.11) THEN
        CRCODE = FLOAT(ICR(I))/10.0
        PTCCF = PCCF(IP)
C----------
C   FOR THE INTERIM, HARDWOODS GET 1 FOOT OF HEIGHT GROWTH/5 YEARS
C----------
        POTHTG = CON * (10.2 - 0.03*PTCCF)
        IF(POTHTG .LT. 1.) POTHTG=1.
        SATBA=280.
        IF(D .LT. 2.0) SATBA=280.*(D/2.)
        IF(D .LT. 0.5) SATBA=70.
        HBALIM = SATBA-1.
C----------
C  COMPUTE A BASAL AREA TERM FOR THE HEIGHT GROWTH MODIFIER THAT IS
C  A WEIGHTED COMBINATION OF THE POINT BASAL AREA AND THE STAND
C  BASAL AREA.  DONNELLY 7/18/97.
C----------
        PNTWT = 0.9
        BAWT = PNTWT*BAAA(IP) + (1.0-PNTWT)*BA
C
        IF(BAWT .GT. HBALIM) THEN
          HBA=HBALIM
        ELSE
          HBA = BAWT
        ENDIF
        IF(HBA .LT. 1.0) HBA=1.0
        XHMOD=(1.0-EXP(-2.0*(SATBA-HBA)/HBA))*CRCODE*0.12
        IF(XHMOD .GT. 1.0) XHMOD=1.0
        HTGR=POTHTG * XHMOD
C----------
C   ADD RANDOM AFFECTS. LIMIT TO + OR -  1/10 HTGR
C----------
        RAN=0.
        IF(DGSD .GE. 1.0) THEN
    3     CONTINUE
          RAN = BACHLO(0.0,1.0,RANN)
          IF(RAN.LT.-1.0 .OR. RAN.GT.1.0) GO TO 3
        ENDIF
        HTGR = HTGR + RAN*0.1*HTGR
C
        IF(DEBUG)WRITE(JOSTND,*)'I,ISPC,PTCCF,HBA,CRCODE,RAN=',
     &  I,ISPC,PTCCF,HBA,CRCODE,RAN
        IF(DEBUG)WRITE(JOSTND,*)'I,POTHTG,XHMOD,SCALE,HTGR= ',
     &  I,POTHTG,XHMOD,SCALE,HTGR
C
        HTGR = HTGR * XRHGRO * SCALE
        IF(HTGR .LT. 0.1) HTGR = 0.1
      ELSE
        HTGR = -4.0166 + 0.451*RCR - 0.00611*TBAL + 0.09426*RSI
    4   CONTINUE
        ZZRAN = 0.0
C       IF(DGSD.GE.1.0) ZZRAN=BACHLO(0.0,1.0,RANN)
C       IF((ZZRAN .GT. 0.5) .OR. (ZZRAN .LT. -2.0)) GO TO 4
C       IF(DEBUG)WRITE(JOSTND,9984) HTGR,ZZRAN,XRHGRO,SCALE
C9984 FORMAT('IN REGENT 9984 FORMAT',4(F10.4,2X))
        HTGR = (HTGR +ZZRAN*0.1)*XRHGRO * SCALE * CON
      ENDIF
C----------
C     GET A MULTIPLIER FOR THIS TREE FROM PPREGT TO ACCOUNT FOR
C     THE DENSITY EFFECTS OF NEIGHBORING TREES.
C
      XPPMLT=0.
      CALL PPREGT (XPPMLT)
C----------
      HTGR = HTGR + XPPMLT
C-------------
C     COMPUTE WEIGHTS FOR THE LARGE AND SMALL TREE HEIGHT INCREMENT
C     ESTIMATES.  IF DBH IS LESS THAN OR EQUAL TO XMN, THE LARGE TREE
C     PREDICTION IS IGNORED (XWT=0.0).
C----------
      XWT=(D-XMN)/(XMX-XMN)
      IF(D.LE.XMN.OR.LESTB) XWT = 0.0
C----------
C     COMPUTE WEIGHTED HEIGHT INCREMENT FOR NEXT TRIPLE.
C----------
      IF(DEBUG)WRITE(JOSTND,9985)XWT,HTGR,HTG(K),I,K
 9985 FORMAT('IN REGENT 9985 FORMAT',3(F10.4,2X),2I7)
      HTG(K)=HTGR*(1.0-XWT) + XWT*HTG(K)
      IF(HTG(K) .LT. .1) HTG(K) = .1
C----------
C CHECK FOR SIZE CAP COMPLIANCE.
C----------
      IF((H+HTG(K)).GT.SIZCAP(ISPC,4))THEN
        HTG(K)=SIZCAP(ISPC,4)-H
        IF(HTG(K) .LT. 0.1) HTG(K)=0.1
      ENDIF
C
    5 CONTINUE
C----------
C     ASSIGN DBH AND COMPUTE DBH INCREMENT FOR TREES WITH DBH LESS
C     THAN 3 INCHES (COMPUTE 10-YEAR DBH INCREMENT REGARDLESS OF
C     PROJECTION PERIOD LENGTH).
C----------
      IF(D.GE.3.0) GO TO 23
      HK=H + HTG(K)
      IF(DEBUG)WRITE(JOSTND,*)'ISPC,H,HTG,HK,D= ',ISPC,H,HTG(K),HK,D
      IF(HK .LE. 4.5) THEN
        DG(K)=0.0
        DBH(K)=D+0.001*HK
      ELSE
        IF(ISPC.EQ.10 .OR. ISPC.EQ.11) THEN
C----------
C  USE INVENTORY EQUATIONS IF CALIBRATION OF THE HT-DBH FUNCTION IS TURNED
C  OFF, OR IF WYKOFF CALIBRATION DID NOT OCCUR.
C  NOTE: THIS SIMPLIFIES TO IF(IABFLB(ISPC).EQ.1) BUT IS SHOWN IN IT'S
C        ENTIRITY FOR CLARITY.
C----------
          DKK=3.102 + 0.021*H
          DK=3.102 + 0.021*HK
        IF(.NOT.LHTDRG(ISPC) .OR.
     &     (LHTDRG(ISPC) .AND. IABFLG(ISPC).EQ.1))THEN
            KSPC=22
            IF(ISPC .EQ. 11) KSPC=27
            CALL HTDBH (IFOR,KSPC,DK,HK,1)
            IF(H .LE. 4.5) THEN
              DKK=D
            ELSE
              CALL HTDBH (IFOR,KSPC,DKK,H,1)
            ENDIF
            IF(DEBUG)WRITE(JOSTND,*)'INV EQN DUB IFOR,ISPC,H,HK,DK,'
     &      ,'DKK,KSPC= ',IFOR,ISPC,H,HK,DK,DKK,KSPC
            IF(DEBUG)WRITE(JOSTND,*)'ISPC,LHTDRG,IABFLG= ',
     &      ISPC,LHTDRG(ISPC),IABFLG(ISPC)
          ENDIF
        ELSE
          DK=(-6.39362/(ALOG(HK-4.5)-4.2976))-1.0
          IF(ISPC.EQ.5) DK=(-5.09974/(ALOG(HK-4.5)-4.21391))-1.0
          IF(H .LE. 4.5) THEN
            DKK=D
          ELSE
            DKK=(-6.39362/(ALOG(H -4.5)-4.2976))-1.0
            IF(ISPC.EQ.5) DKK=(-5.09974/(ALOG(H -4.5)-4.21391))-1.0
          ENDIF
          IF(DEBUG)WRITE(JOSTND,*)'I,ISPC,H,HK,DK,DKK,XRDGRO= ',
     &    I,ISPC,H,HK,DK,DKK,XRDGRO
        ENDIF
C----------
C         IF CALLED FROM **ESTAB** ASSIGN DIAMETER
C----------
        IF(LESTB) THEN
          DBH(K)=DK
          IF(DBH(K).LT.0.1) DBH(K)=0.1
          DBH(K)=DBH(K)+0.001*HK
          DG(K)=DBH(K)
        ELSE
C----------
C         COMPUTE DIAMETER INCREMENT BY SUBTRACTION, APPLY USER
C         SUPPLIED MULTIPLIERS, AND CHECK TO SEE IF COMPUTED VALUE
C         IS WITHIN BOUNDS.
C----------
          IF(DK.LT.0.0 .OR. DKK.LT.0.0)THEN
            DG(K)=HTG(K)*0.2*BARK*XRDGRO
            DK=D+DG(K)
          ELSE
            DG(K)=(DK-DKK)*BARK*XRDGRO
          ENDIF
          IF((ISPC.EQ.10 .OR. ISPC.EQ.11) .AND. LHTDRG(ISPC)
     &        .AND. IABFLG(ISPC).EQ.0) DG(K)=0.1*HTG(K)*XRDGRO
          IF(DG(K) .LT. 0.0) DG(K)=0.0
C----------
C    SCALE DIAMETER INCREMENT TO 10-YR ESTIMATE.
C    TO BE CONSISTENT WITH GRADD, THE SCALE ADJUSTMENT IS
C    ON GROWTH IN DDS TERMS RATHER THAN INCHES OF DG
C----------
          IF(DEBUG)WRITE(JOSTND,*)'K,D,DG(K),BARK,SCALE2,YR,FNT = ',
     &    K,D,DG(K),BARK,SCALE2,YR,FNT
          DDS=(DG(K)*(2.0*BARK*D+DG(K))) * SCALE2
          DG(K)=SQRT((D*BARK)**2.0+DDS)-BARK*D
C
        ENDIF
        IF((DBH(K)+DG(K)).LT.DIAM(ISPC))THEN
          DG(K)=DIAM(ISPC)-DBH(K)
        ENDIF
      ENDIF
C----------
C  CHECK FOR TREE SIZE CAP COMPLIANCE
C----------
      CALL DGBND(ISPC,DBH(K),DG(K))
C
   23 CONTINUE
C----------
C  RETURN TO PROCESS NEXT TRIPLE IF TRIPLING.  OTHERWISE,
C  PRINT DEBUG AND RETURN TO PROCESS NEXT TREE.
C----------
      IF(LESTB .OR. .NOT.LTRIP .OR. L.GE.2) GO TO 22
      L=L+1
      K=ITRN+2*I-2+L
      GO TO 2
C----------
C  END OF GROWTH PREDICTION LOOP.  PRINT DEBUG INFO IF DESIRED.
C----------
   22 CONTINUE
      IF(DEBUG)THEN
      HTNEW=HT(I)+HTG(I)
      WRITE(JOSTND,9987) I,ISPC,HT(I),HTG(I),HTNEW,DBH(I),DG(I)
 9987 FORMAT('IN REGENT, I=',I4,',  ISPC=',I3,'  CUR HT=',F7.2,
     &       ',  HT INC=',F7.4,',  NEW HT=',F7.2,',  CUR DBH=',F10.5,
     &       ',  DBH INC=',F7.4)
      ENDIF
   25 CONTINUE
   30 CONTINUE
      GO TO 91
C----------
C  SMALL TREE HEIGHT CALIBRATION SECTION.
C----------
   40 CONTINUE
      DO 45 ISPC=1,MAXSP
      HCOR(ISPC)=0.0
      CORTEM(ISPC)=1.0
      NUMCAL(ISPC)=0
   45 CONTINUE
      IF (ITRN.LE.0) GO TO 91
      IF(IFINTH .EQ. 0)  GOTO 95
      SCALE3 = REGYR / FINTH
C----------
C  BEGIN PROCESSING TREE LIST IN SPECIES ORDER.  DO NOT CALCULATE
C  CORRECTION TERMS IF THERE ARE NO TREES FOR THIS SPECIES.
C----------
      DO 100 ISPC=1,MAXSP
      CORNEW=1.0
      I1=ISCT(ISPC,1)
      IF(I1.EQ.0 .OR. .NOT. LHTCAL(ISPC)) GO TO 100
      N=0
      SNP=0.0
      SNX=0.0
      SNY=0.0
      I2=ISCT(ISPC,2)
      IREFI=IREF(ISPC)
      RSI=SITEAR(ISPC)
      IF(RSI.LT.40.)RSI=40.
C----------
C  BEGIN TREE LOOP WITHIN SPECIES.  IF MEASURED HEIGHT INCREMENT IS
C  LESS THAN OR EQUAL TO ZERO, OR DBH IS LESS THAN 5.0, THE RECORD
C  WILL BE EXCLUDED FROM THE CALIBRATION.
C----------
      DO 60 I3=I1,I2
      I=IND1(I3)
      H=HT(I)
      TBAL=(1.0 - (PCT(I)/100.)) * BA
      IICR=((ICR(I)-1)/10)+1
      IF(IICR.GT.9) IICR=9
      RCR=FLOAT(IICR)
C----------
C  DIA GT 3 INCHES INCLUDED IN OVERALL MEAN
C----------
      IF(IHTG.LT.2) H=H-HTG(I)
      IF(DBH(I).GE.5.0.OR.H.LT.0.01) GO TO 60
      IF(ISPC.EQ.10 .OR. ISPC.EQ.11) THEN
        CRCODE  = FLOAT(ICR(I))/10.0
        IPCCF=ITRE(I)
        PTCCF = PCCF(IPCCF)
        POTHTG = RHCON(ISPC) * (10.2 - 0.03*PTCCF)
        IF(POTHTG .LT. 1.) POTHTG=1.
        SATBA=280.
        IF(D .LT. 2.0) SATBA=280.*(D/2.)
        IF(D .LT. 0.5) SATBA=70.
        HBALIM=SATBA-1.
C----------
C  COMPUTE A BASAL AREA TERM FOR THE HEIGHT GROWTH MODIFIER THAT IS
C  A WEIGHTED COMBINATION OF THE POINT BASAL AREA ND THE STAND
C  BASAL AREA.  DONNELLY 7/18/97.
C----------
        PNTWT = 0.9
        BAWT = PNTWT*BAAA(IPCCF) + (1.0-PNTWT)*BA
C
        IF(BAWT .GT. HBALIM) THEN
          HBA=HBALIM
        ELSE
          HBA = BAWT
        ENDIF
        IF(HBA .LT. 1.0) HBA=1.0
        XHMOD=(1.0-EXP(-2.0*(SATBA-HBA)/HBA))*CRCODE*0.12
        IF(XHMOD .GT. 1.0) XHMOD=1.0
        EDH = POTHTG * XHMOD
      ELSE
        EDH = -4.0166 + 0.451*RCR - 0.00611*TBAL + 0.09426*RSI
        EDH = EDH * RHCON(ISPC)
      ENDIF
      P=PROB(I)
      IF(HTG(I).LT.0.001) GO TO 60
      TERM=HTG(I) * SCALE3
      SNP=SNP+P
      SNX=SNX+EDH*P
      SNY=SNY+TERM*P
      N=N+1
C----------
C  PRINT DEBUG INFO IF DESIRED.
C----------
      IF(DEBUG)WRITE(JOSTND,9991) NPLT,I,ISPC,H,DBH(I),ICR(I),
     & PCT(I),RELDM1,RHCON(ISPC),EDH,TERM
 9991 FORMAT('NPLT=',A26,',  I=',I5,',  ISPC=',I3,',  H=',F6.1,
     & ',  DBH=',F5.1,',  ICR',I5,',  PCT=',F6.1,',  RELDEN=',
     & F6.1 / 12X,'RHCON=',F10.3,',  EDH=',F10.3,', TERM=',F10.3)
C----------
C  END OF TREE LOOP WITHIN SPECIES.
C----------
   60 CONTINUE
      IF(DEBUG) WRITE(JOSTND,9992) ISPC,SNP,SNX,SNY
 9992 FORMAT(/'SUMS FOR SPECIES ',I2,':  SNP=',F10.2,
     & ';  SNX=',F10.2,';  SNY=',F10.2)
C----------
C  COMPUTE CALIBRATION TERMS.  CALIBRATION TERMS ARE NOT COMPUTED
C  IF THERE WERE FEWER THAN NCALHT (DEFAULT=5) HEIGHT INCREMENT
C  OBSERVATIONS FOR A SPECIES.
C----------
      IF(N.LT.NCALHT) GO TO 80
C----------
C  CALCULATE MEANS FOR THE POPULATION AND FOR THE SAMPLE ON THE
C  NATURAL SCALE.
C----------
      SNX=SNX/SNP
      SNY=SNY/SNP
C----------
C  CALCULATE RATIO ESTIMATOR.
C----------
      CORNEW = SNY/SNX
      IF(CORNEW.LE.0.0) CORNEW=1.0E-4
      HCOR(ISPC)=ALOG(CORNEW)
C----------
C  TRAP CALIBRATION VALUES OUTSIDE 2.5 STANDARD DEVIATIONS FROM THE
C  MEAN. IF C IS THE CALIBRATION TERM, WITH A DEFAULT OF 1.0, THEN
C  LN(C) HAS A MEAN OF 0.  -2.5 < LN(C) < 2.5 IMPLIES
C  0.0821 < C < 12.1825
C----------
      IF(CORNEW.LT.0.0821 .OR. CORNEW.GT.12.1825) THEN
        CALL ERRGRO(.TRUE.,27)
        WRITE(JOSTND,9194)ISPC,JSP(ISPC),CORNEW
 9194   FORMAT(T28,'SMALL TREE HTG: SPECIES = ',I2,' (',A3,
     &  ') CALCULATED CALIBRATION VALUE = ',F8.2)
        CORNEW=1.0
        HCOR(ISPC)=0.0
      ENDIF
   80 CONTINUE
      CORTEM(IREFI) = CORNEW
      NUMCAL(IREFI) = N
  100 CONTINUE
C----------
C  END OF CALIBRATION LOOP.  PRINT CALIBRATION STATISTICS AND RETURN
C----------
      WRITE(JOSTND,9993) (NUMCAL(I),I=1,NUMSP)
 9993 FORMAT(/'NUMBER OF RECORDS AVAILABLE FOR SCALING'/
     >       'THE SMALL TREE HEIGHT INCREMENT MODEL',
     >        ((T48,11(I4,2X)/)))
   95 CONTINUE
      WRITE(JOSTND,9994) (CORTEM(I),I=1,NUMSP)
 9994 FORMAT(/'INITIAL SCALE FACTORS FOR THE SMALL TREE'/
     >      'HEIGHT INCREMENT MODEL',
     >       ((T48,11(F5.2,1X)/)))
C----------
C OUTPUT CALIBRATION TERMS IF CALBSTAT KEYWORD WAS PRESENT.
C----------
      IF(JOCALB .GT. 0) THEN
        KOUT=0
        DO 207 K=1,MAXSP
        IF(CORTEM(K).NE.1.0 .OR. NUMCAL(K).GE.NCALHT) THEN
          SPEC=NSP(MAXSP,1)(1:2)
          ISPEC=MAXSP
          DO 203 KK=1,MAXSP
          IF(K .NE. IREF(KK)) GO TO 203
          ISPEC=KK
          SPEC=NSP(KK,1)(1:2)
          GO TO 2031
  203     CONTINUE
 2031     WRITE(JOCALB,204)ISPEC,SPEC,NUMCAL(K),CORTEM(K)
  204     FORMAT(' CAL: SH',1X,I2,1X,A2,1X,I4,1X,F6.3)
          KOUT = KOUT + 1
        ENDIF
  207   CONTINUE
        IF(KOUT .EQ. 0)WRITE(JOCALB,209)
  209   FORMAT(' NO SH VALUES COMPUTED')
        WRITE(JOCALB,210)
  210   FORMAT(' CALBSTAT END')
      ENDIF
   91 IF(DEBUG)WRITE(JOSTND,9995)ICYC
 9995 FORMAT('LEAVING SUBROUTINE REGENT  CYCLE =',I5)
      RETURN
      ENTRY REGCON
C----------
C  ENTRY POINT FOR LOADING OF REGENERATION GROWTH MODEL
C  CONSTANTS  THAT REQUIRE ONE-TIME RESOLUTION.
C---------
      DO 90 ISPC=1,MAXSP
      RHCON(ISPC) = 1.0
      IF(LRCOR2.AND.RCOR2(ISPC).GT.0.0)
     &RHCON(ISPC)=RCOR2(ISPC)
   90 CONTINUE
      RETURN
      END
