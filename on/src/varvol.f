        SUBROUTINE VARVOL
      use contrl_mod
      use volstd_mod
      use plot_mod
      use arrays_mod
      use prgprm_mod
      implicit none
C----------
C  **VARVOL--ON    DATE OF LAST REVISION:   11/02/12
C----------
C
C  THIS SUBROUTINE CALLS THE APPROPRIATE VOLUME CALCULATION ROUTINE
C  FROM THE NATIONAL CRUISE SYSTEM VOLUME LIBRARY FOR METHB OR METHC
C  EQUAL TO 6.  IT ALSO CONTAINS ANY OTHER SPECIAL VOLUME CALCULATION
C  METHOD SPECIFIC TO A VARIANT (METHB OR METHC = 5, 8)
C
C  FOR ONTARIO:
C    VN = TOTAL MERCH CUBIC (PULPWOOD AND SAWLOG) (GTV)
C    VM = MERCH SAWLOG CUBIC FOOT VOLUME (GMV)
C    VC = VOLUME WITH CULL, BASED ON GMV
C    BBFV = NOT USED
C
C  THIS SUBROUTINE INCLUDES LOGIC FOR ALL ONTARIO.
C  NOTE THAT THE NATCRS PART OF THE ROUTINE IS THE SAME AS THE LS\VARVOL
C
C  IF **NATCRS** IS CALLED, THIS SUBROUTINE CALLS **R9CLARK**
C  IF **OCFVOL** IS CALLED, THEN **HONVOL**, **ZAKVOL**, **ZAKHT** ARE CALLED
C  IF **OBFVOL** IS CALLED, THEN THERE ARE NO CALLS
C
C  SCRIBNER BOARD FEET ARA CALCULATED FOR MN (CHIPPEWA AND SUPERIOR),
C  WI (CHEQUAMEGON-NICOLET), AND UPPER MI (OTTAWA AND HIAWATHA NFS),
C  AND INTERNATIONAL 1/4" IS USED EVERYWHERE ELSE
C----------
C
C----------
      REAL VOL(15),BOLTHT(21),LOGLEN(7,21),BBFV1,UPSHT1
      LOGICAL TKILL,CTKFLG,BTKFLG,DEBUG,DONE,LCONE,LTOTAL
      CHARACTER VLSP*3,VVER*7,LSSP(72)*3,CSSP(96)*3,NESP(108)*3
      CHARACTER VOLEQN*10,TEMEQN*10,CTYPE*1,EQN*10,HTTYP*1
      CHARACTER*2 FORST,PROD
      INTEGER FOREST
      INTEGER I1,I0,I02,I01,IHT1,IHT2,I,ISPC,IERR,IT,ITRNC
      REAL BRATIO,TDIBC,TDIBB,SAWBF,SAWCU,X01,TOTCU,BOLT1,BOLT2
      REAL SINDX,VM,VN,BBFV,H,D,BARK,VMAX,X02,X03
      REAL VM2, ZHT
	CHARACTER*10 EQNC,EQNB

	CHARACTER ONEQ(MAXSP)*3

C     DEFINE SPECIES DATA ARRAYS
C     DEFINE WHICH VOLUME EQUATION EACH SPECIES WILL USE.
      DATA ONEQ/
     &  'ZAK','HON','ZAK','ZAK','ZAK','ZAK','ZAK','ZAK','ZAK','ZAK',
     &  'ZAK','HON','ZAK','ZAK','ZAK','ZAK','ZAK','ZAK','ZAK','HON',
     &  'HON','HON','HON','ZAK','HON','ZAK','ZAK','HON','HON','HON',
     &  'HON','HON','HON','HON','HON','HON','HON','HON','HON','ZAK',
     &  'ZAK','ZAK','ZAK','ZAK','HON','ZAK','HON','ZAK','ZAK','ZAK',
     &  'ZAK','ZAK','HON','ZAK','HON','HON','ZAK','HON','ZAK','HON',
     &  'HON','HON','HON','ZAK','ZAK','ZAK','ZAK','HON','ZAK','ZAK',
     &  'ZAK','ZAK'/

C----------
C  DEFINE VARIABLES
C    EQUATION NUMBERS:
C        DONE    --LOGICAL OPERATOR TO SIGNIFY ALL VOLUMES HAVE
C                  BEEN COMPUTED
C----------
C  NATIONAL CRUISE SYSTEM ROUTINES (METHOD = 6)
C----------
      ENTRY NATCRS (VN,VM,BBFV,ISPC,D,H,TKILL,BARK,ITRNC,VMAX,
     1              CTKFLG,BTKFLG,IT)
C----------
C  CHECK FOR DEBUG
C----------
      CALL DBCHK(DEBUG,'VARVOL',6,ICYC)
      IF(DEBUG)WRITE(JOSTND,*)'IN VARVOL, ICYC= ',ICYC
C----------
C  INITIALIZE VOLUME ARRAY TO ZERO
C----------
      DO  10 I=1,15
      VOL(I)= 0.
   10 CONTINUE

      DONE=.FALSE.
C----------
C  REAL CONSTANT ARGUMENTS
C----------
      UPSHT1=0.
      X01=0.
      X02=2.
      X03=0.
C----------
C  INTEGER CONSTANT ARGUMENTS
C----------
        IERR=0
        I01=0
        I1=1
C----------
C  CHARACTER CONSTANT ARGUMENTS
C----------
        CTYPE='F'
        HTTYP='L'
C----------
C  TREE DBH IS LESS THAN PULPWOOD MIN DBH, OR NO PULPWOOD HT
C----------
      IF(D.LT.DBHMIN(ISPC))THEN
C
C  CASE 1: TREE DOES NOT MEET PULPWOOD SPECS
C
        TOTCU=0.
        X02=0.
        X03=0.
C----------
C  TREE DBH IS >PULPWOOD MIN DBH BUT <SAWTIMBER MIN DBH, OR NO SAW HT
C----------
      ELSEIF(D.LT.BFMIND(ISPC))THEN
C
C  CASE 2: TREE ONLY HAS PULPWOOD (PROD='02')
C
        IF(DEBUG)WRITE(JOSTND,*)' IT,CASE 2 PWVOL VEQNNC,D,H= ',
     &  IT,VEQNNC(ISPC),D,H

C
        PROD='02'
        X02=0.
        CALL R9CLARK(VEQNNC(ISPC),X01,BFTOPD(ISPC),TOPD(ISPC),D,X02,X03,
     &  H,LOGDIA,BOLTHT,LOGLEN,LOGVOL,VOL,I01,I01,I1,I01,I01,PROD,IERR,
     &  CTYPE,UPSHT1)
C
        IF(DEBUG)WRITE(JOSTND,*)' AFTER CASE 2 PWVOL IERR,VOL=',IERR,VOL
C
        TOTCU=VOL(4)
        SAWCU=0.0
        SAWBF=0.0
        DONE=.TRUE.
C----------
C  TREE DBH IS >SAWTIMBER MIN DBH, POSITIVE SAW HT AND PULP HT
C----------
      ELSE

C  CASE 3: TREE HAS SAWTIMBER AND PULPWOOD (PROD='01' AND TOPWOOD)
C
C  IF THE VOLEQNUM KEYWORD WAS USED TO SET DIFFERENT EQ. NOS. FOR CF AND
C  FOR BF THEN 2 CALLS TO R9CLARK ARE REQUIRED OTHERWISE ONE CALL IS
C  ENOUGH
C
        IF(DEBUG)WRITE(JOSTND,*)' IT,CASE 3 SBVOL VEQNNC(ISPC),H,D,',
     &  IT,VEQNNC(ISPC),H,D
C
        PROD='01'
        CALL R9CLARK(VEQNNC(ISPC),X01,BFTOPD(ISPC),TOPD(ISPC),D,X02,X03,
     &  H,LOGDIA,BOLTHT,LOGLEN,LOGVOL,VOL,I01,I1,I1,I01,I1,PROD,IERR,
     &  CTYPE,UPSHT1)
C
C  THE NEXT VERSION OF CRUISE PROCESSING WILL USE THE FOLLOWING CONDITIONAL
C  LOGIC. THE LOGIC APPEARS IN 4 PLACES (10/27/2011)
C
C        IF((KODFOR.EQ.903).OR.(KODFOR.EQ.909).OR.(KODFOR.EQ.913).OR.
C     &     (KODFOR.EQ.907).OR.(KODFOR.EQ.910))THEN
          SAWBF=VOL(2)
C        ELSE
C          SAWBF=VOL(10)
C        ENDIF

        SAWCU=VOL(4)
        TOTCU=SAWCU+VOL(7)
        IF(DEBUG)WRITE(JOSTND,*)' AFTER CASE 3(CALL 2)TCVOL IERR,VOL= '
     &   ,IERR,VOL
        IF(VEQNNB(ISPC).EQ.VEQNNC(ISPC))THEN
          DONE=.TRUE.
        ELSE
          IF(DEBUG)WRITE(JOSTND,*)' IT,CASE 3 SBVOL VEQNNB(ISPC),H,D,',
     &    'FORST= ',IT,VEQNNB(ISPC),H,D
          CALL R9CLARK(VEQNNB(ISPC),X01,BFTOPD(ISPC),TOPD(ISPC),D,X02,
     &         X03,H,LOGDIA,BOLTHT,LOGLEN,LOGVOL,VOL,I01,I1,I1,I01,
     &         I1,PROD,IERR,CTYPE,UPSHT1)
C          IF((KODFOR.EQ.903).OR.(KODFOR.EQ.909).OR.(KODFOR.EQ.913).OR.
C     &       (KODFOR.EQ.907).OR.(KODFOR.EQ.910))THEN
            SAWBF=VOL(2)
C          ELSE
C            SAWBF=VOL(10)
C          ENDIF
          DONE=.TRUE.
          IF(DEBUG)WRITE(JOSTND,*)' AFTER CASE 3 (CALL 2)TCVOL IERR,',
     &     'VOL= ',IERR,VOL
        ENDIF
        IF(DEBUG)WRITE(JOSTND,*)' IT,AFTER CASE 3 TCVOL IERR,VOL= ',
     &    IERR,VOL
      ENDIF
      IF(D.GE.BFMIND(ISPC))THEN
        IF(IT.GT.0)HT2TD(IT,1)=X02
      ELSE
        IF(IT.GT.0)HT2TD(IT,1)=0.
      ENDIF
      IF(D.GE.DBHMIN(ISPC))THEN
        IF(IT.GT.0)HT2TD(IT,2)=X03
      ELSE
        IF(IT.GT.0)HT2TD(IT,2)=0.
      ENDIF
      IF (DONE)GOTO 50
C----------
C--TREE DID NOT MEET PULPWOOD SPECS
C----------
      IF(D .LT. BFMIND(ISPC))THEN
C
C  CASE 4: TREE DOES NOT MEET SAWTIMBER OR PULPWOOD SPECS
C
        SAWCU=0.0
        SAWBF=0.0
      ELSE
C
C  CASE 5: STRANGE CASE SPECS MET AND TREE ONLY HAS SAWTIMBER(PROD='01')
C  IF THE VOLEQNUM KEYWORD WAS USED TO SET DIFFERENT EQ. NOS. FOR CF AND
C  FOR BF THEN 2 CALLS TO R9CLARK ARE REQUIRED OTHERWISE ONE CALL IS
C  ENOUGH
C
        IF(DEBUG)WRITE(JOSTND,*)' IT,CASE 5 R9VOL VEQNNC(ISPC),D= ',
     &  VEQNNC(ISPC),D
C
        PROD='01'
        CALL R9CLARK(VEQNNC(ISPC),X01,BFTOPD(ISPC),TOPD(ISPC),D,X02,
     &  X03,H,LOGDIA,BOLTHT,LOGLEN,LOGVOL,VOL,I01,I1,I1,I01,I01,
     &  PROD,IERR,CTYPE,UPSHT1)
C
        IF(DEBUG)WRITE(JOSTND,*)' AFTER CASE 5 (CALL 1)R9VOL IERR,VOL=',
     &   IERR,VOL
C
        SAWCU=VOL(4)
C        IF((KODFOR.EQ.903).OR.(KODFOR.EQ.909).OR.(KODFOR.EQ.913).OR.
C     &     (KODFOR.EQ.907).OR.(KODFOR.EQ.910))THEN
          SAWBF=VOL(2)
C        ELSE
C          SAWBF=VOL(10)
C        ENDIF
        IF(VEQNNB(ISPC).NE.VEQNNC(ISPC))THEN
          IF(DEBUG)WRITE(JOSTND,*)' IT,CASE 5 R9VOL VEQNNB(ISPC),D= ',
     &    IT,VEQNNB(ISPC),D
          CALL R9CLARK(VEQNNB(ISPC),X01,BFTOPD(ISPC),TOPD(ISPC),D,X02,
     &    X03,H,LOGDIA,BOLTHT,LOGLEN,LOGVOL,VOL,I01,I1,I1,I01,I01,PROD,
     &    IERR,CTYPE,UPSHT1)
C        IF((KODFOR.EQ.903).OR.(KODFOR.EQ.909).OR.(KODFOR.EQ.913).OR.
C     &     (KODFOR.EQ.907).OR.(KODFOR.EQ.910))THEN
          SAWBF=VOL(2)
C        ELSE
C          SAWBF=VOL(10)
C        ENDIF
        IF(DEBUG)WRITE(JOSTND,*)' AFTER CASE 5 (CALL 2)R9VOL IERR,VOL=',
     &   IERR,VOL
        ENDIF
        IF(D.GE.BFMIND(ISPC))THEN
          IF(IT.GT.0)HT2TD(IT,1)=X02
        ELSE
          IF(IT.GT.0)HT2TD(IT,1)=0.
        ENDIF
        IF(D.GE.DBHMIN(ISPC))THEN
          IF(IT.GT.0)HT2TD(IT,2)=X03
        ELSE
          IF(IT.GT.0)HT2TD(IT,2)=0.
        ENDIF
      ENDIF

C     SET RETURN VALUES.

   50 CONTINUE
      IF(DEBUG)WRITE(JOSTND,*)' TOTCU=',TOTCU,' SAWCU=',SAWCU,
     &   ' SAWBF=',SAWBF
      VN=TOTCU
      VMAX=VN
      VM=SAWCU
      BBFV=SAWBF
      CTKFLG = .TRUE.
      BTKFLG = .TRUE.
      IF(VN.LE.0.)THEN
        VN=0.
        CTKFLG = .FALSE.
      ENDIF
      IF(VM.LE.0.)THEN
        VM=0.
        BTKFLG = .FALSE.
      ENDIF
      IF(BBFV.LE.0.)THEN
        BBFV=0.
        BTKFLG = .FALSE.
      ENDIF
      IF(DEBUG)WRITE(JOSTND,*)' RETURNING VN,VMAX,VM,BBFV,IERR= ',
     &VN,VMAX,VM,BBFV,IERR
C
C  VOLUME PROCESSING ERRORS ARE PROCESSED HERE
C
      IF(IERR.EQ.12)CALL ERRGRO(.TRUE.,36)

      RETURN

C     ==========================
C     ENTER ANY OTHER CUBIC HERE

      ENTRY OCFVOL (VN,VM,ISPC,D,H,TKILL,BARK,ITRNC,VMAX,LCONE,
     1              CTKFLG,IT)

C     CHECK FOR DEBUG

      CALL DBCHK(DEBUG,'VARVOL',6,ICYC)
      IF(DEBUG)WRITE(JOSTND,*)'IN VARVOL, OCFVOL'
C
      IF(METHC(ISPC).EQ.5)THEN
C----------
C  IF METHC =5 USE GEVORKIANTZ METHOD
C----------
         CALL GVRVOL (VN,VM,BBFV1,ISPC,D,H,TKILL,BARK,ITRNC,VMAX,
     1              CTKFLG,BTKFLG,IT)
      ELSE

C     SET INITIAL VALUES.  IF DIAMETER LIMITS OR TREE VALUE CLASS NOT MET
C     THEN RETURN.
C     (Note, in original varvol, routine would return even if IMC=3.
C      This has been changed in the Ontario routine to accept IMC=3.
C           Therefore, IMC(IT) no longer in the IF statement)

        VN=0.0001
        VM=0.0001
        VMAX=0.0
C      IF (IMC(IT) .GT. 3 .OR. D .LT. DBHMIN(ISPC))GOTO 100
        IF (D .LT. DBHMIN(ISPC))GOTO 100

C     DETERMINE WHICH TYPE OF VOLUME TO CALCULATE

        IF (ONEQ(ISPC) .EQ. 'ZAK') THEN

C     CALCULATE GTV (= NET CUBIC FOOT VOLUME (VN) = PULPWOOD + SAWLOG)

            LTOTAL = .TRUE.
            CALL ZAKVOL (VN,ISPC,D,H,H,IT,BARK,LTOTAL)
            IF(DEBUG)WRITE(JOSTND,*) 'SPEC=',ISPC,' IT=',IT,' VN=',VN,
     &     ' IMC=',IMC(IT),' D=',D,' SI=',SITEAR(ISPC),' VM=',VM

C     CALCULATE GMV (= MERCH CUBIC FOOT VOLUME (VM) = SAWLOG)

            LTOTAL = .FALSE.
            CALL ZAKHT  (D,H,BARK,ISPC,IT,ZHT) ! need Ht @ minDBH
            VM = 0.0
            IF (ZHT .GT. 0.0) THEN
                CALL ZAKVOL (VM,ISPC,D,H,ZHT,IT,BARK,LTOTAL)
            ENDIF
        ELSE
            CALL HONER (VN,VM,ISPC,D,H,IT)
	  ENDIF
      ENDIF

C     SET RETURN VALUES HERE

  100 CONTINUE
      VMAX=VN
      CTKFLG = .TRUE.
      IF(VN.LE.0.)THEN
        VN=0.
        CTKFLG = .FALSE.
      ENDIF
      IF(VM.LE.0.)THEN
        VM=0.
      ENDIF

      RETURN
C
      ENTRY OBFVOL (BBFV,ISPC,D,H,TKILL,BARK,ITRNC,VMAX,LCONE,
     1              BTKFLG,IT)

C     CALCULATE BOARD FEET (IN FVS-ON THIS IS NET MERCHANTABLE VOLUME: NMV)
C     BY COMPUTING GMV. TO MINIMIZE CODE CHANGES, THIS REQUIRE RECALCULATING
C     MERCH CUBIC FEET (= GROSS MERCH VOL) ONCE AGAIN USING CODE COPIED FROM OCFVOL

C     CHECK FOR DEBUG
      CALL DBCHK(DEBUG,'VARVOL',6,ICYC)
      IF(DEBUG)THEN
        WRITE(JOSTND,*)'IN OBFVOL ISPC,SI,D,TOPD =', ISPC,SITEAR(ISPC),
     &            D,TOPD(ISPC)
      ENDIF

      IF(METHB(ISPC).EQ.5)THEN
C----------
C  GEVORKIANTZ METHOD (METHB=5)
C----------
        CALL GVRVOL(VN,VM,BBFV,ISPC,D,H,TKILL,BARK,ITRNC,VMAX,
     1              CTKFLG,BTKFLG,IT)
      ELSE

        VN=0.0001
        VM=0.0001
        VMAX=0.0
        IF (IMC(IT) .GT. 3 .OR. D .LT. DBHMIN(ISPC))GOTO 200

C     DETERMINE WHICH TYPE OF VOLUME TO CALCULATE

        IF (ONEQ(ISPC) .EQ. 'ZAK') THEN

C     CALCULATE NET CUBIC FOOT VOLUME (VN) == PULPWOOD + SAWLOG

            LTOTAL = .TRUE.
            CALL ZAKVOL (VN,ISPC,D,H,H,IT,BARK,LTOTAL)
            IF(DEBUG)WRITE(JOSTND,*) 'SPEC=',ISPC,' IT=',IT,' VN=',VN,
     &     ' IMC=',IMC(IT),' D=',D,' SI=',SITEAR(ISPC),' VM=',VM

C     CALCULATE MERCH CUBIC FOOT VOLUME (VM) == SAWLOG

            LTOTAL = .FALSE.
            CALL ZAKHT  (D,H,BARK,ISPC,IT,ZHT) ! need Ht @ minDBH
            VM = 0.0
            IF (ZHT .GT. 0.0) THEN
                CALL ZAKVOL (VM,ISPC,D,H,ZHT,IT,BARK,LTOTAL)
            ENDIF
        ELSE
            CALL HONER (VN,VM,ISPC,D,H,IT)
	  ENDIF
C
C     SET RETURN VALUES HERE
C
  200   CONTINUE

        VM2 = MAX(VM,0.0)
        CALL MOWRASKI(VM2,IT,ISPC,BBFV)
      ENDIF

      BTKFLG=.TRUE.
      IF(BBFV .LE. 0.0)THEN
        BBFV=0.0
        BTKFLG = .FALSE.
      ENDIF
      RETURN
C----------
C  ENTRY POINT FOR SENDING VOLUME EQN NUMBER TO THE FVS-TO-NATCRZ ROUTINE
C  NOTE: RETURNED EQN NUMBER IS FOR BOARD FOOT SAWTIMBER.
C        PULPWOOD PORTIONS USE 911 FOR FIRST THREE CHARACTERS;
C        CUBIC FOOT SAWTIMBER USES 912 FOR FIRST THREE CHARACTERS.
C----------

      ENTRY GETEQN(ISPC,D,H,EQNC,EQNB,TDIBC,TDIBB)
      EQNC=VEQNNC(ISPC)
      EQNB=VEQNNB(ISPC)
      TDIBC=TOPD(ISPC)*BRATIO(ISPC,D,H)
      TDIBB=BFTOPD(ISPC)*BRATIO(ISPC,D,H)
      RETURN
C
      END

