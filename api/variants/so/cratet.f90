      SUBROUTINE CRATET
      use findage_mod, only: findag
      
      use prgprm_mod
      use arrays_mod
      use plot_mod
      use coeffs_mod
      use contrl_mod
      use outcom_mod
	  use htcal_mod
      use varcom_mod
      implicit none
!----------
!  **CRATET--SO   DATE OF LAST REVISION:  04/18/11
!----------
!  THIS SUBROUTINE IS CALLED PRIOR TO PROJECTION.  IT HAS THE
!  FOLLOWING FUNCTIONS:
!
!    1)  CALL **RCON** TO LOAD SITE DEPENDENT MODEL COEFFICIENTS.
!    2)  REGRESSION TO ESTIMATE COEFFICIENTS OF LOCAL HEIGHT-
!        DIAMETER RELATIONSHIP.
!    3)  DUB IN MISSING HEIGHTS.
!    4)  CALL **DENSE** TO COMPUTE STAND DENSITY.
!    5)  SCALE CROWN RATIOS AND CALL **CROWN** TO DUB IN ANY MISSING
!        VALUES.
!    6)  DEFINE DG BASED ON CALIBRATION CONTROL PARAMETERS AND
!        CALL **DGDRIV** TO CALIBRATE DIAMETER GROWTH EQUATIONS.
!    7)  DELETE DEAD TREES FROM INPUT TREE LIST AND REALIGN IND1
!        AND ISCT.
!    8)  PRINT A TABLE DESCRIBING CONTROL PARAMETERS AND INPUT
!        VARIABLES.
!----------
!
!----------
!  INTERNAL VARIABLES.
!
!      KNT2 -- USED TO STORE COUNTS FOR PRINTING IN CONTROL
!              SUMMARY TABLE.
!     SPCNT -- USED TO ACCUMULATE NUMBER OF TREES PER ACRE BY
!              SPECIES AND TREE CLASS FOR CALCULATION OF
!              INITIAL SPECIES-TREE CLASS COMPOSITION VECTOR.
!----------
      LOGICAL DEBUG,MISSCR,TKILL
      CHARACTER*4 UNDER
      INTEGER KNT2(MAXSP),KNT(MAXSP),NH
      INTEGER I,J,II,ISPC,IPTR,I1,I2,I3,K1,K2,K3,K4,JCR,IICR,JJ,IS,IM
      REAL AX,Q,SUMX,H,D,BX,XX,YY,XN,HS,SPCNT(MAXSP,3)
      REAL SITAGE,SITHT,AGMAX,HTMAX,HTMAX2,D1,D2
!----------
!     THE SPECIES ORDER IS VARIANT SPECIFIC, SEE BLKDATA FOR A LIST.
!----------
!  INITIALIZE INTERNAL VARIABLES:
!----------
      UNDER = '----'
!-----------
!  SEE IF WE NEED TO DO SOME DEBUG.
!-----------
      CALL DBCHK (DEBUG,'CRATET',6,ICYC)
!-------
!  IF THERE ARE TREE RECORDS, BRANCH TO PREFORM CALIBRATION.
!-------
      AX=0.
      IF (ITRN.GT.0) GOTO 1
!----------
!   CALL MAICAL TO CALCULATE MAI
!----------
      CALL MAICAL
      CALL RCON
      ONTREM(7)=0.
      CALL DENSE
      CALL DGDRIV
      CALL REGENT(.FALSE.,1)
    1 CONTINUE
      DO 5 I=1,MAXSP
      SPCNT(I,1)=0.0
      SPCNT(I,2)=0.0
      SPCNT(I,3)=0.0
      IF (ISCT(I,1).EQ.0) GOTO 5
      J=IREF(I)
      IUSED(J)=NSP(I,1)
    5 CONTINUE
      IF((ITRN.LE.0).AND.(IREC2.GE.MAXTP1))GO TO 245
!----------
!  PRINT SPECIES LABELS AND NUMBER OF OBSERVATIONS IN CONTROL
!  TABLE.  THEN, RESET COUNTERS TO ZERO.
!----------
      WRITE(JOSTND,'(//''CALIBRATION STATISTICS:''//)')
      WRITE(JOSTND,9000) (IUSED(I), I=1,NUMSP)
 9000 FORMAT ((T49,11(1X,A2,3X)/))
      IF(NUMSP .LE. 11) THEN
        WRITE(JOSTND,9001) (UNDER, I=1,NUMSP)
      ELSE
        WRITE(JOSTND,9001) (UNDER, I=1,11)
      ENDIF
 9001 FORMAT (T49,11(A4,2X))
      WRITE(JOSTND,9002) (KOUNT(I), I=1,NUMSP)
 9002 FORMAT(/,'NUMBER OF RECORDS PER SPECIES', &
              ((T49,11(I4,2X)/)))
      DO 10 I=1,MAXSP
      KNT(I)=0
      KNT2(I)=0
   10 CONTINUE
!----------
!   CALL MBACAL TO IDENTIFY SITE SPECIES
!----------
      CALL MBACAL
!----------
!   CALL MAICAL TO CALCULATE MAI
!----------
      CALL MAICAL
!----------
!  CALL **RCON** TO INITIALIZE SITE DEPENDENT MODEL COEFFICIENTS.
!----------
      CALL RCON
!----------
!  CALL **RDPSRT** AND **DENSE** TO COMPUTE INITIAL STAND DENSITY
!  STATISTICS.  ONTREM(7) IS SET TO ZERO HERE TO ASSURE THAT RELDM1
!  WILL BE ASSIGNED IN **DENSE** IN THE FIRST CYCLE.
!----------
      DO 15 I=1,ITRN
      IND(I)=IND1(I)
   15 CONTINUE
      CALL RDPSRT(ITRN,DBH,IND,.FALSE.)
      ONTREM(7)=0.0
!----------
!  PREPARE INPUT DATA FOR DIAMETER GROWTH MODEL CALIBRATION.  IF
!  IDG IS 1, CONVERT THE PAST DIAMETER MEASUREMENT CARRIED IN DG TO
!  DIAMETER GROWTH.  IF IDG IS 3, CONVERT THE CURRENT DIAMETER
!  MEASUREMENT CARRIED IN DG TO DIAMETER GROWTH.  ACTUAL DIAMETER
!  INCREMENT MEASUREMENTS WILL BE CORRECTED FOR BARK GROWTH IN
!  THE CALIBRATION ROUTINE. (THIS CODE WAS MOVED HERE SO THAT THE
!  BACKDATING ALGORITHM IN **DENSE**, INVOKED DURING CALIBRATION,
!  IS CORRECT.)
!----------
      Q=1.0
      IF(IDG.EQ.3) Q=-1.0
      DO 230 II=1,ITRN
      I=IND1(II)
      IF(I.GE.IREC2) GO TO 230
      IF(DG(I).LE.0.0) GO TO 220
      IF(IDG.EQ.0.OR.IDG.EQ.2) GO TO 230
      DG(I)=Q*(DBH(I)-DG(I))
      GO TO 230
  220 CONTINUE
      DG(I)=-1.0
  230 CONTINUE
!---------
!  SET LBKDEN TRUE IF DIAMETERS ARE TO BE BACKDATED FOR DENSITY
!  CALCULATIONS.  AFTER THIS CALL TO DENSE, INSURE LBKDEN=FALSE.
!---------
      LBKDEN= IDG.LT.2
      CALL DENSE
      LBKDEN= .FALSE.
!----------
!  DELETE NON-PROJECTABLE RECORDS, AND REALIGN POINTERS TO THE
!  SPECIES ORDERED SORT.
!----------
      IF(IREC2.EQ.MAXTP1) GO TO 60
      DO 50 I=IREC2,MAXTRE
      ISPC=ISP(I)
      IPTR=IREF(ISPC)
      IF(IMC(I).EQ.7)KNT(IPTR)=KNT(IPTR)+1
      IF (DEBUG) WRITE(JOSTND,9003) I,IMC(I),ISPC
 9003 FORMAT('IN CRATET: DEAD TREE RECORD:  I=',I4,',  IMC=',I2, &
             ',  SPECIES=',I2,' (9003 CRATET)')
      IF(ITRN.GT.0)THEN
        I1=ISCT(ISPC,1)
        I2=ISCT(ISPC,2)
        DO 30 I3=I1,I2
        IF(IND1(I3).EQ.I) GO TO 40
   30   CONTINUE
   40   IND1(I3)=IND1(I2)
        ISCT(ISPC,2)=I2-1
        IF(ISCT(ISPC,2).GE.ISCT(ISPC,1)) GO TO 50
        ISCT(ISPC,1)=0
        ISCT(ISPC,2)=0
      ENDIF
   50 CONTINUE
!----------
!  WRITE CALIBRATION TABLE ENTRY FOR NON-PROJECTABLE RECORDS AND RESET
!  KNT ARRAY TO ZERO.
!----------
      WRITE(JOSTND,9004) (KNT(I),I=1,NUMSP)
 9004 FORMAT(/,'NUMBER OF RECORDS CODED AS RECENT MORTALITY', &
              ((T49,11(I4,2X)/)))
!---------
!  RESET TREE RECORD COUNTERS AND SAVE THE NUMBER OF SPECIES.
!----------
      ITRN=IREC1
      IF((ITRN.LE.0).AND.(IREC2.GE.MAXTP1))GOTO 245
      IF(ITRN.LE.0)GOTO 60
      ISPC=NUMSP
!---------
!  MAKE SURE THAT ALL THE SPECIES ORDER SORTS AND THE IND2 ARRAY
!  ARE IN THE PROPER ORDER. FIRST, SAVE THE SPECIES REFERENCES.
!---------
      DO 51 I=1,MAXSP
      KNT(I)=IREF(I)
   51 CONTINUE
      CALL SPESRT
!---------
!  IF THE NUMBER OF SPECIES HAS CHANGED, WE MUST REWRITE THE
!  COLUMN HEADINGS.
!---------
      IF (ISPC.NE.NUMSP) THEN
         WRITE(JOSTND,52)
   52    FORMAT (/'***** NOTE:  SPECIES HAVE BEEN DROPPED.')
         DO 55 I=1,MAXSP
         IF (ISCT(I,1).EQ.0) GOTO 55
         J=IREF(I)
         IUSED(J)=NSP(I,1)
   55    CONTINUE
         WRITE(JOSTND,9000) (IUSED(I), I=1, NUMSP)
         WRITE(JOSTND,9001) (UNDER, I=1, NUMSP)
      ELSE
!
!        RESET THE REFERENCES.
!
         DO 57 I=1,MAXSP
         IREF(I)=KNT(I)
   57    CONTINUE
      ENDIF
!----------
!  SORT REMAINING TREE RECORDS IN ORDER OF DESCENDING DIAMETER.
!  STORE POINTERS TO SORTED ORDER IN IND.
!----------
      CALL RDPSRT(ITRN,DBH,IND,.TRUE.)
   60 CONTINUE
      DO 65 I=1,MAXSP
      KNT(I)=0
   65 CONTINUE
!----------
!  ENTER LOOP TO ADJUST HEIGHT-DBH MODEL FOR LOCAL CONDITIONS.  IF
!  THERE ARE 3 OR MORE TREES WITH MEASURED HEIGHTS FOR A GIVEN
!  SPECIES, ADJUST THE INTERCEPT (ASYMPTOTE) IN THE MODEL SO THAT
!  THE MEAN RESIDUAL FOR THE MEASURED TREES IS ZERO.
!  IF LHTDRG IS FALSE FOR A GIVEN SPECIES THEN ALL DUBBING IS DONE WITH
!  DEFAULT VALUES.
!----------
      DO 150 ISPC=1,MAXSP
      AA(ISPC)=0.
      BB(ISPC)=0.
      I1=ISCT(ISPC,1)
      IF(I1.LE.0) GO TO 141
      I2=ISCT(ISPC,2)
      IPTR=IREF(ISPC)
!----------
!  INITIALIZE SUMS FOR THIS SPECIES.
!----------
      K1=0
      K2=0
      K3=0
      K4=0
      SUMX=0.0
!----------
!  ENTER TREE LOOP WITHIN SPECIES.
!----------
      DO 80 I3=I1,I2
      I=IND1(I3)
      H=HT(I)
      NH=NORMHT(I)
      D=DBH(I)
      IF(ICR(I).LE.0)THEN
        JCR=5
      ELSE
        JCR=((ICR(I)-1)/10)+1
        IF(JCR.GT.9)JCR=9
        IF(JCR.LE.0)JCR=5
      ENDIF
      BX=HTT2(ISPC,JCR)
!----------
!  BYPASS SUMS FOR TREES WITH MISSING HEIGHT OR TRUNCATED TOPS.
!  FOR SPECIES USING LOGIC FROM THE WC VARIANT -
!  SEPERATE EQUATIONS ARE USED FOR TREES 5" AND LARGER VS TREES
!  LESS THAN 5" DBH. CALIBRATION OF THE HT-DBH RELATIONSHIP IS
!  ONLY DONE ON THE EQUATION USED FOR TREES 5" AND LARGER.
!----------
      SELECT CASE(ISPC)
      CASE (15,19:23,25,26,28:31,33)
        IF(H.LE.4.5 .OR. NH.LT.0 .OR. D.LT.5.0) GO TO 70
      CASE DEFAULT
        IF(H.LE.4.5 .OR. NH.LT.0 .OR. D.LT.3.0) GO TO 70
      END SELECT
      K1=K1+1
      XX = BX/(D+1.)
      YY = ALOG(H-4.5)
      SUMX=SUMX+YY-XX
      GO TO 80
!----------
!  COUNT NUMBER OF MISSING HEIGHTS AND BROKEN OR DEAD TOPS.  LOAD THE
!  ARRAY IND2 WITH SUBSCRIPTS TO THE RECORDS WITH MISSING HEIGHTS.
!----------
   70 CONTINUE
      IF(NH.LT.0) K3=K3+1
      IF(H.GT.0.0 .AND. NH.EQ.0) GO TO 80
      K2=K2+1
      IND2(K2)=I
!----------
!  END OF SUMMATION LOOP FOR THIS SPECIES.
!----------
!*** ONE LINE FIX FOR GROWTH METHOD 1 PROBLEM. DIXON 11-16-90.
      IF(HT(I) .LE. 0.1) HTG(I)=0.0
   80 CONTINUE
!----------
!  IF THERE ARE LESS THAN THREE OBSERVATIONS OR LHTDRG IS FALSE THEN
!   DUB HEIGHTS USING DEFAULT COEFFICIENTS FOR THIS SPECIES.
!----------
      KNT(IPTR)=K3
      IF(K1 .LT.3 .OR. .NOT. LHTDRG(ISPC)) GO TO 100
      XN=FLOAT(K1)
      AA(ISPC)=SUMX/XN
!----------
!  IF THE INTERCEPT IS NEGATIVE, USE DEFAULTS.
!----------
      IF(AA(ISPC).GE.0.0)THEN
        IABFLG(ISPC)=0
      ENDIF
!----------
!  DUB IN MISSING HEIGHTS.
!  A VALUE LESS THAN ZERO STORED IN 'HT' => THAT TREE WAS TOP KILLED.
!  CONSEQUENTLY, A VALUE OF 80% OF THE PREDICTED HEIGHT IS STORED AS
!  THE TRUNCATED HEIGHT.
!----------
  100 CONTINUE
      IF(K2.EQ.0) GO TO 140
      DO 130 JJ=1,K2
      II=IND2(JJ)
      IF(ICR(II).LE.0)THEN
        JCR=5
      ELSE
        JCR=((ICR(II)-1)/10)+1
        IF(JCR.GT.9)JCR=9
        IF(JCR.LE.0)JCR=5
      ENDIF
      D=DBH(II)
!
!  LOGIC FROM SORNEC 11 (HTT1,HTT2 VARIABLES)
!
      SELECT CASE (ISPC)
      CASE(1:8,10,12,13,32)
        AX=HTT1(ISPC,JCR)
        BX=HTT2(ISPC,JCR)
      CASE DEFAULT
        AX=HT1(ISPC)
        BX=HT2(ISPC)
      END SELECT
      IF (IABFLG(ISPC) .EQ. 0) AX=AA(ISPC)
      TKILL = NORMHT(II) .LT. 0.
      IF(DEBUG)WRITE(JOSTND,9005) ISPC,AX,BX,IABFLG(ISPC)
 9005 FORMAT('HEIGHT-DIAMETER COEFFICIENTS FOR SPECIES ',I2, &
            ':  INTERCEPT=',F10.6,'  SLOPE=',F10.6,'  FLAG=',I3, &
       ' (9005 CRATET)')
!
      IF(D .LE. 0.1)THEN
        H=1.01
        GO TO 115
      ENDIF
!
!  LOGIC FOR SPECIES FROM WC VARIANT (SEPARATE EQS. FOR SMALL TREES)
!
      SELECT CASE(ISPC)
      CASE (15,19:23,25,26,28:31,33)
        IF(D .LT. 5.0) THEN
          IICR=ICR(II)
          IF(IICR.LE.0)IICR=40
          H = HTT1(ISPC,1) + HTT1(ISPC,2)*D &
            + HTT1(ISPC,3)*FLOAT(IICR) + HTT1(ISPC,4)*D*D &
            + HTT1(ISPC,5)
          IF(ISPC.EQ.19.OR.ISPC.EQ.20) &
          H=EXP(H)
        ELSE
          H=EXP(AX+BX/(D+1.0))+4.5
        ENDIF
!
!  ALL OTHER SPECIES
!
      CASE DEFAULT
        H=EXP(AX+BX/(D+1.0))+4.5
      END SELECT
!
      IF (DEBUG) WRITE(JOSTND,88) AX,BX,D,H
  88  FORMAT('CRATET DUBBED HEIGHT: AX,BX,D,H=',4F8.2)
!
! SMALL PP USE A DIFFERENT LOGIC. RJ 7-26-88
!
      IF(ISPC.EQ.10 .AND. D.LT.3.0) THEN
      IF(JCR .GT. 7)JCR=7
      H = 8.31485 + 3.03659*D - 0.59200*JCR
      ENDIF
!----------
!  USE INVENTORY EQUATIONS IF CALIBRATION OF THE HT-DBH FUNCTION IS TURNED
!  OFF, OR IF WYKOFF CALIBRATION DID NOT OCCUR.
!  NOTE: THIS SIMPLIFIES TO IF(IABFLB(ISPC).EQ.1) BUT IS SHOWN IN IT'S
!        ENTIRITY FOR CLARITY.
!----------
      IF(ISPC.EQ.11.OR.ISPC.EQ.16.OR.ISPC.EQ.24)GOTO 105
      IF(.NOT.LHTDRG(ISPC) .OR. &
         (LHTDRG(ISPC) .AND. IABFLG(ISPC).EQ.1))THEN
        CALL HTDBH (IFOR,ISPC,D,H,0)
        IF(DEBUG)WRITE(JOSTND,*)'INVENTORY EQN DUBBING IFOR,ISPC,D,H= ' &
        ,IFOR,ISPC,D,H
      ENDIF
  105 CONTINUE
!
      IF(H .LT. 4.5) H=4.5
  115 CONTINUE
      IF(TKILL) GO TO 120
      HT(II)=H
      K4=K4+1
      GO TO 125
  120 CONTINUE
      NORMHT(II)=H*100.0+0.5
      IF(ITRUNC(II).EQ.0) THEN
         IF(HT(II).GT.0.0) THEN
            ITRUNC(II)=80.0*HT(II)+0.5
         ELSE
            ITRUNC(II)=80.0*H+0.5
            HT(II)=H
         ENDIF
      ELSE
         IF(HT(II).GT.0.0) THEN
            IF(HT(II).LT.(ITRUNC(II)*0.01)) HT(II)=ITRUNC(II)*0.01
         ELSE
            HT(II)=ITRUNC(II)*0.01
         ENDIF
      ENDIF
      IF(NORMHT(II)*0.01.LT.HT(II)) NORMHT(II)=HT(II)*100.0
  125 CONTINUE
  130 CONTINUE
      KNT2(IPTR)=K4
!----------
!  END OF SPECIES LOOP.  PRINT HEIGHT-DIAMETER COEFFICIENTS ON
!  DEBUG UNIT IF DESIRED.
!----------
  140 CONTINUE
      IF(DEBUG)THEN
      WRITE(JOSTND,*)'END OF HEIGHT DUBBING FOR SPECIES = ',ISPC
      ENDIF
!----------
!  LOOP THROUGH DEAD TREES AND DUB MISSING HEIGHTS FOR THIS SPECIES.
!----------
  141 CONTINUE
      IF(IREC2 .GT. MAXTRE) GO TO 150
      DO 145 II=IREC2,MAXTRE
      IF(ISP(II).NE.ISPC) GO TO 145
      IF(ICR(II).LE.0)THEN
        JCR=5
      ELSE
        JCR=((ICR(II)-1)/10)+1
        IF(JCR.GT.9)JCR=9
        IF(JCR.LE.0)JCR=5
      ENDIF
      D=DBH(II)
!
!  LOGIC FROM SORNEC 11 (HTT1,HTT2 VARIABLES)
!
      SELECT CASE(ISPC)
      CASE(1:8,10,12,13,32)
        AX=HTT1(ISPC,JCR)
        BX=HTT2(ISPC,JCR)
      CASE DEFAULT
        AX=HT1(ISPC)
        BX=HT2(ISPC)
      END SELECT
!
      IF (IABFLG(ISPC) .EQ. 0) AX=AA(ISPC)
      TKILL = NORMHT(II) .LT. 0.
      IF(HT(II).GT.0. .AND. TKILL) GO TO 142
      IF(HT(II).GT.0.) GO TO 146
!
      IF(D .LE. 0.1)THEN
        H=1.01
        GO TO 144
      ENDIF
!
!  LOGIC FOR SPECIES FROM WC VARIANT (SEPARATE EQS. FOR SMALL TREES)
!
      SELECT CASE(ISPC)
      CASE (15,19:23,25,26,28:31,33)
        IF(D .LT. 5.0) THEN
          IICR=ICR(II)
          IF(IICR.LE.0)IICR=40
          H = HTT1(ISPC,1) + HTT1(ISPC,2)*D &
            + HTT1(ISPC,3)*FLOAT(IICR) + HTT1(ISPC,4)*D*D &
            + HTT1(ISPC,5)
          IF(ISPC .LE. 15 &
           .OR. (ISPC.GE.17 .AND. ISPC.LE.20) &
           .OR. (ISPC.GE.30 .AND. ISPC.LE.33)) H = EXP(H)
        ELSE
          H=EXP(AX+BX/(D+1.0))+4.5
        ENDIF
!
!  ALL OTHER SPECIES
!
      CASE DEFAULT
        H=EXP(AX+BX/(D+1.0))+4.5
      END SELECT
!
!  SMALL PP USES A DIFFERENT LOGIC. RJ 7-26-88
!
      IF(ISPC.EQ.10 .AND. D.LT.3.0) THEN
      IF(JCR .GT. 7)JCR=7
      H = 8.31485 + 3.03659*D - 0.59200*JCR
      ENDIF
!----------
!  USE INVENTORY EQUATIONS IF CALIBRATION OF THE HT-DBH FUNCTION IS TURNED
!  OFF, OR IF WYKOFF CALIBRATION DID NOT OCCUR.
!  NOTE: THIS SIMPLIFIES TO IF(IABFLB(ISPC).EQ.1) BUT IS SHOWN IN IT'S
!        ENTIRITY FOR CLARITY.
!----------
      IF(ISPC.EQ.11 .OR. ISPC.EQ.16 .OR. ISPC.EQ.24) GO TO 106
      IF(.NOT.LHTDRG(ISPC) .OR. &
         (LHTDRG(ISPC) .AND. IABFLG(ISPC).EQ.1))THEN
        CALL HTDBH (IFOR,ISPC,D,H,0)
        IF(DEBUG)WRITE(JOSTND,*)'INVENTORY EQN DUBBING IFOR,ISPC,D,H= ' &
        ,IFOR,ISPC,D,H
      ENDIF
!
  106 CONTINUE
      IF(H .LT. 4.5) H=4.5
  144 CONTINUE
      IF(TKILL) GO TO 142
      HT(II)=H
      GO TO 146
  142 CONTINUE
      IF(HT(II) .GT. 0.) THEN
        NORMHT(II)=HT(II)*100.0+0.5
      ELSE
        NORMHT(II)=H*100.0+0.5
      ENDIF
      IF(ITRUNC(II).EQ.0) THEN
         IF(HT(II).GT.0.0) THEN
            ITRUNC(II)=80.0*HT(II)+0.5
         ELSE
            ITRUNC(II)=80.0*H+0.5
            HT(II)=H
         ENDIF
      ELSE
         IF(HT(II).GT.0.0) THEN
            IF(HT(II).LT.(ITRUNC(II)*0.01)) HT(II)=ITRUNC(II)*0.01
         ELSE
            HT(II)=ITRUNC(II)*0.01
         ENDIF
      ENDIF
      IF(NORMHT(II)*0.01.LT.HT(II)) NORMHT(II)=HT(II)*100.0
!----------
!   CALL FIRE SNAG MODEL TO ADD THE DEAD TREES TO THE
!   SNAG LIST; DEFLATE PROB(II), WHICH WAS TEMPORARILY
!   ADJUSTED TO ALLOW BACKDATING FOR CALIBRATION PURPOSES,
!   IN **NOTRE**
!----------
  146 CONTINUE
      IF (TKILL) THEN
        HS = ITRUNC(II) * 0.01
      ELSE
        HS = HT(II)
      ENDIF
      CALL FMSSEE (II,ISPC,D,HS, &
        (PROB(II)/(FINT/FINTM)),3,.FALSE.,JOSTND)
!
  145 CONTINUE
!----------
  150 CONTINUE
!----------
!  END OF HEIGHT DUBBING SEGMENT.  PRINT CONTROL TABLE ENTRIES FOR
!  USEABLE HEIGHTS AND MISSING HEIGHTS, AND REINITIALIZE COUNTERS.
!----------
      WRITE(JOSTND,9006) (KNT2(I),I=1,NUMSP)
 9006 FORMAT(/,'NUMBER OF RECORDS WITH MISSING HEIGHTS', &
             ((T49,11(I4,2X)/)))
      WRITE(JOSTND,9007) (KNT(I),I=1,NUMSP)
 9007 FORMAT(/,'NUMBER OF RECORDS WITH BROKEN OR DEAD TOPS', &
             ((T49,11(I4,2X)/)))
      DO 160 I=1,MAXSP
      KNT(I)=0
      KNT2(I)=0
  160 CONTINUE
!----------
!  CHECK FOR MISSING CROWNS ON LIVE TREES.
!  SAVE PCT IN OLDPCT TO RETAIN AN OLD PCTILE VALUE.
!----------
      MISSCR = .FALSE.
      DO 190 I=1,ITRN
      OLDPCT(I)= PCT(I)
      IF(ICR(I).LE.0)THEN
        MISSCR = .TRUE.
        ISPC=ISP(I)
        IPTR=IREF(ISPC)
        KNT2(IPTR)=KNT2(IPTR)+1
      ENDIF
      IF(ITRE(I).LE.0) ITRE(I) = 9999
  190 CONTINUE
!----------
!  CHECK FOR MISSING CROWNS ON CYCLE 0 DEAD TREES.
!----------
      IF(IREC2 .GT. MAXTRE) GO TO 192
      DO 191 I=IREC2,MAXTRE
      IF(ICR(I).LE.0)MISSCR=.TRUE.
  191 CONTINUE
  192 CONTINUE
!----------
!  CALL **CROWN** IF ANY CROWN RATIOS ARE MISSING.
!----------
      IF(MISSCR)CALL CROWN
!----------
!  PRINT CONTROL TABLE ENTRY FOR MISSING CROWN RATIOS; RESET COUNTERS.
!----------
      WRITE(JOSTND,9008) (KNT2(I),I=1,NUMSP)
 9008 FORMAT(/,'NUMBER OF RECORDS WITH MISSING CROWN RATIOS', &
             ((T49,11(I4,2X)/)))
      DO 200 I=1,MAXSP
      KNT2(I)=0
  200 CONTINUE
!----------
!   CALL AVHT40 TO CALCULATE AVERAGE HEIGHT. THIS CALL IS NEEDED
!   IN SORNEC BECAUSE DGF ROUTINE USES AVH IN CALCULATION OF DDS.
!----------
      CALL AVHT40
!----------
!  CALL DGDRIV TO CALIBRATE DIAMETER GROWTH EQUATIONS.
!----------
      IF(DEBUG)WRITE(JOSTND,*)'CALL DGDRIV FROM CRATET SECOND TIME'
      CALL DGDRIV
!----------
!  PREPARE INPUT DATA FOR HEIGHT GROWTH MODEL CALIBRATION; IT'S DONE
!  THE SAME AS THE DIAMETER GROWTH MODEL SEEN ABOVE.
!----------
      IF(IHTG.EQ.0 .OR. IHTG.EQ.2) GOTO 236
      Q = 1.
      IF(IHTG.EQ.3) Q = -1.
      DO 233 I=1,ITRN
      IF(HTG(I).LE.0.) GOTO 233
      HTG(I) = Q * (HT(I)-HTG(I))
      IF(HT(I) .LE. 0.0) HTG(I)=0.0
  233 CONTINUE
  236 CONTINUE
!----------
!  ESTIMATE MISSING TOTAL TREE AGE
!----------
      IF(DEBUG)WRITE(JOSTND,*)'IN CRATET, CALLING FINDAG'
      DO I=1,ITRN
      IF(ABIRTH(I) .LE. 0.)THEN
        SITAGE = 0.0
        SITHT = 0.0
        AGMAX = 0.0
        HTMAX = 0.0
        HTMAX2 = 0.0
        ISPC = ISP(I)
        D1 = DBH(I)
        H = HT(I)
        D2 = 0.0
        CALL FINDAG(I,ISPC,D1,D2,H,SITAGE,SITHT,AGMAX,HTMAX,HTMAX2, &
                    DEBUG)
        IF(SITAGE .GT. 0.)ABIRTH(I)=SITAGE
      ENDIF
      ENDDO
!----------
!  CALL REGENT TO CALIBRATE THE SMALL TREE HEIGHT INCREMENT MODEL.
!----------
      CALL REGENT(.FALSE.,1)
!----------
!  LOAD SPCNT WITH NUMBER OF TREES PER ACRE BY SPECIES AND TREE
!  CLASS.
!----------
      DO 240 I=1,ITRN
      IS=ISP(I)
      IM=IMC(I)
      SPCNT(IS,IM)=SPCNT(IS,IM)+PROB(I)
  240 CONTINUE
!----------
!  COMPUTE DISTRIBUTION OF TREES PER ACRE AND SPECIES-TREE CLASS
!  COMPOSITION BY TREES PER ACRE.
!----------
  245 CONTINUE
      CALL PCTILE(ITRN,IND,PROB,WK3,ONTCUR(7))
      CALL DIST(ITRN,ONTCUR,WK3)
      CALL COMP(OSPCT,IOSPCT,SPCNT)
      IF (ITRN.LE.0) GO TO 500
!----------
!  CALL **DENSE** TO CALCULATE STAND DENSITY STATISTICS FOR
!  INITIAL INVENTORY.
!----------
      IF(DEBUG) WRITE(JOSTND,9013) ICYC
 9013 FORMAT('CALLING DENSE, CYCLE=',I2)
      CALL DENSE
!----------
!  COUNT AND PRINT NUMBER OF RECORDS WITH MISTLETOE.
!----------
      CALL MISCNT(KNT)
      WRITE(JOSTND,248) (KNT(I),I=1,NUMSP)
  248 FORMAT(/,'NUMBER OF RECORDS WITH MISTLETOE',((T49,11(I4,2X)/)))
!----------
!  CALL **SDICHK** TO SEE IF INITIAL STAND SDI IS ABOVE THE SPECIFIED
!  MAXIMUM SDI.  RESET MAXIMUM SDI IF THIS IS THE CASE.
!----------
      CALL SDICHK
!
  500 CONTINUE
      IF(DEBUG)WRITE(JOSTND,510)ICYC
  510 FORMAT('LEAVING SUBROUTINE CRATET  CYCLE =',I5)
!
      RETURN
      END