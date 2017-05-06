        SUBROUTINE FVSVOL
        use prgprm_mod
        use arrays_mod
        use coeffs_mod
        use contrl_mod
        use plot_mod
        use volstd_mod
        IMPLICIT NONE
C----------
C VOLUME $Id$
C----------
C
C  THIS SUBROUTINE CALLS THE VOLINIT ROUTINE TO ACCESS THE
C  THE NATIONAL CRUISE SYSTEM VOLUME LIBRARY FOR METHB OR METHC
C  EQUAL TO 5 (GEVORKIANTZ) OR 6. IT ALSO CONTAINS ENTRY POINTS
C  AT THE END FOR OTHER VARIANT-SPECIFIC SPECIAL VOLUME
C  CALCULATION METHODZ (METHB OR METHC = 8)
C----------
C
      COMMON/FVSVOLCOM/IREGN,FORST,VOLEQ,MTOPP,MTOPS,PROD
C
C
COMMONS
C
C----------
      INTEGER IT,ITRNC,ISPC,INTFOR,IERR,IZERO
      INTEGER I1,IREGN,IFC,IFIASP,LOGST
      REAL VMAX,BARK,H,D,BBFV,VM,VN
      REAL FC,DBTBH,TVOL1,TVOL4,TDIBB,TDIBC,BRATIO
      REAL NOLOGP,NOLOGS
      CHARACTER CFTOP*1,BFTOP*1
      CHARACTER CTYPE*1,FORST*2,HTTYPE*1,PROD*2,FIASP*4
      REAL SCALEN(20),BOLHT(21),LOGLEN(20),TVOL(15)
      INTEGER ERRFLAG,TLOGS
      LOGICAL TKILL,CTKFLG,BTKFLG,LCONE,DEBUG
      CHARACTER*10 EQNC,EQNB,VOLEQ
      INTEGER I3,I7,I15,I20,I21,I01,I02
      INTEGER CUTFLG,BFPFLG,CUPFLG,CDPFLG,SPFLG
      REAL HT1PRD,HT2PRD,X1,MTOPP,MTOPS
      CHARACTER CONSPEC*4,VVER*7,LIVEDUM*1
      REAL S3,BV,BEHRE
      REAL ALVN,VOLT,VOLM,STUMP,DMRCH,HTMRCH
      INTEGER ITD,BADUM,SIDUM,HTTDUM,IDIST
      REAL BBF,VVN,TVOL7,TVOL2
      REAL XTOPD,HTC1,HTC2
      
C----------
C  NATIONAL CRUISE SYSTEM ROUTINES (METHOD = 6)
C----------
      ENTRY NATCRS (VN,VM,BBFV,ISPC,D,H,TKILL,BARK,ITRNC,VMAX,
     1              CTKFLG,BTKFLG,IT)
C-----------
C  SEE IF WE NEED TO DO SOME DEBUG.
C-----------
      CALL DBCHK (DEBUG,'FVSVOL',6,ICYC)
      IF(DEBUG) WRITE(JOSTND,3)ICYC,IFOR
    3 FORMAT(' ENTERING SUBROUTINE FVSVOL CYCLE,IFOR =',I5,i4)
      IF(DEBUG)WRITE(JOSTND,*)' AFTER NATCRS ARGUMENTS= ',VN,VM,BBFV,
     &ISPC,D,H,TKILL,BARK,ITRNC,VMAX,CTKFLG,BTKFLG,IT
C
      CALL VARVER(VVER)
      IDIST=1
      IF(KODFOR.GT.10000)THEN
        IREGN = KODFOR/10000
        INTFOR=KODFOR/100-IREGN*100
      ELSE
        IREGN = KODFOR/100
        INTFOR = KODFOR - (KODFOR/100)*100
      ENDIF
      IF(VVER(:2).EQ.'SN')IDIST=KODFOR-(KODFOR/100)*100
      WRITE(FORST,'(I2)')INTFOR
      IF(INTFOR.LT.10)FORST(1:1)='0'
      WRITE(FIASP,('(A)'))JSP(ISPC)
      HTTYPE='F'
      IERR=0
      DBTBH = D*(1-BARK)
      IF(DEBUG)WRITE(JOSTND,*)' INTFOR, IREGN= ',INTFOR, IREGN
      DO 100 IZERO=1,15
      TVOL(IZERO)=0.
  100 CONTINUE
C
C  REGION 9 INPUTS OUTSIDE-BARK TOP DIAMETERS TO VOLINIT
C  REGION 8 DOESN'T CARE, TOP DIAMETERS ARE HARD WIRED
C  WESTERN VARIANTS INPUT INSIDE-BARK TOP DIAMETERS 
C
      IF((IREGN.EQ.8).OR.(IREGN.EQ.9))THEN
        MTOPS=TOPD(ISPC)
        MTOPP=BFTOPD(ISPC)
      ELSE
        MTOPS=TOPD(ISPC)*BARK
        MTOPP=BFTOPD(ISPC)*BARK
      ENDIF
C----------
C  CALL TO VOLUME INTERFACE - VOLINIT - CUBIC VOLUMES
C  INITIALIZE CONSTANT ARGUMENTS, AND VARIABLES
C----------
      IF(D.LT.DBHMIN(ISPC))THEN
        TVOL(1)=0.
        TVOL(4)=0.
        TVOL(7)=0.
        BBFV=0.
C
C  EASTERN VARIANTS DONT'T NEED TOTAL VOLUME SO GO TO NEXT TREE
C  FOR WESTERN VARIANTS CALCULATE TOTAL CUBIC FOR ALL TREES
C
        IF((IREGN.EQ.8).OR.(IREGN.EQ.9))GO TO 500
        VOLEQ=VEQNNC(ISPC)
        STUMP=STMP(ISPC)
        PROD='02'
      ELSEIF(D.LT.BFMIND(ISPC))THEN
        VOLEQ=VEQNNC(ISPC)
        STUMP=STMP(ISPC)
        PROD='02'
        IF(IREGN.EQ.8)THEN
          IF((TOPD(ISPC).NE.0.).AND.TOPD(ISPC).NE.4.0)THEN
            TVOL4=0.
          ELSE
C----------
C  CALL R8VOL TO CALCULATE PULPWOOD AND HEIGHT TO 4 IN TOP
C  SET EQUATION NUMBER FOR PULPWOOD TREE
C----------
            WRITE(CFTOP,'(I1)')NINT(TOPD(ISPC))
            VOLEQ(3:3)=CFTOP
C----------
C  SET CONSTANT ARGUMENTS IN CALL TO R8VOL
C
C  INTEGER CCONSTANT ARGUMENTS - ALL PULPWOOD TREE
C----------
            XTOPD=0.
            FORST='  '
            CTYPE='F'
            BFPFLG=1
            CUPFLG=1
            SPFLG=1
            IERR=0
            I01=0
            I02=0
            HTC1=0.
            HTC2=0.
C           
            CALL R8VOL(VOLEQ,D,H,HTC1,HTC2,XTOPD,PROD,TVOL,FORST,
     &                 I01,I02,CTYPE,BFPFLG,CUPFLG,SPFLG,IERR)
C           
            IF(DEBUG)WRITE(JOSTND,*)'AFTER R8VOL-VOLEQ= ',VOLEQ,'ISPC= '
     &      ,ISPC,' D=',D,' H=',H,' HTC1=',HTC1,' HTC2=',HTC2,' PROD=',
     &      PROD,' VOL4=',TVOL(4),' VOL5=',TVOL(5),' VOL7=',TVOL(7)
C
            IF(IT.GT.0)HT2TD(IT,2)=HTC2
            TVOL4=TVOL(4)
          ENDIF
          GO TO 500
        ENDIF                                  ! END OF REGION 8 PULP LOGIC
C
      ELSE
        VOLEQ=VEQNNC(ISPC)
        STUMP=BFSTMP(ISPC)
        PROD='01'
        IF(IREGN.EQ.8)THEN
          WRITE(BFTOP,'(I1)')NINT(BFTOPD(ISPC))
          VOLEQ(3:3)=BFTOP
          IF ((BFTOPD(ISPC).NE.7.0 .AND. ISPC.LE.17)
     &    .OR. ((BFTOPD(ISPC).NE.9.0) .AND. (ISPC.GT.17) .AND.
     &    (JSP(ISPC)(1:2).NE.'OS')) .OR. (BFTOPD(ISPC).NE.7.0
     &    .AND.JSP(ISPC)(1:2).EQ.'OS') .OR. (D.LT.BFMIND(ISPC)))THEN
            TVOL4=0.0
            TVOL2=0.0
            IF(IT.GT.0)HT2TD(IT,1)=0.
            IF(IT.GT.0)HT2TD(IT,2)=0.
            GOTO 500
          ENDIF
        ENDIF
      ENDIF
      I1=0
      I3=3
      I7=7
      I15=15
      I20=20
      I21=21
      X1=0.
C----------
C  CONSTANT CHARACTER ARGUMENTS
C----------
      CTYPE='F'
C----------
C  PRODUCT FLAGS
C----------
C  TOAL CUBIC
      CUTFLG=1
C  BF
      BFPFLG=1
C  MERCH CUBIC
      CUPFLG=1
C  CORDWOOD
      CDPFLG=1
C  SECONDARY PRODUCT
      SPFLG=1
C----------
C  GET FORM CLASS FOR THIS TREE.
C----------
      CALL FORMCL(ISPC,IFOR,D,FC)
      IFC=IFIX(FC)
C
C  THE NVEL USES FORMCLASS TO PASS NUMBER OF STEMS FOR
C  R3 WOODLAND SPECIES, IF DVE EQ.NO. SET IFC TO ZERO
C   
      IF((VOLEQ(4:6).EQ.'DVE').OR.(VOLEQ(4:6).EQ.'dve'))IFC=0     
      IF(DEBUG)WRITE(JOSTND,*)' ISPC,INTFOR,D,FC,IFC= ',
     &ISPC,INTFOR,D,FC,IFC
      TLOGS = 0.
      NOLOGP = 0.
      NOLOGS = 0.
      HT1PRD=0.
      HT2PRD=0.
      IERR=0
      SIDUM=0
      BADUM=0
      HTTDUM=0
      LIVEDUM=' '
      CONSPEC='    '
C
C
C
      IF(IREGN.EQ.5)MTOPP=TOPD(ISPC)*BARK
C
      DBTBH = D*(1-BARK)
      IF(DEBUG)WRITE(JOSTND,*)' CALLING VOLILNIT CF ISPC,ARGS = ',
     &  ISPC,IREGN,FORST,VEQNNC(ISPC),MTOPP,MTOPS,STMP(ISPC),D,H,
     &  IFC,DBTBH,BARK,CUPFLG,BFPFLG,PROD
C
      CALL VOLINIT(IREGN,FORST,VOLEQ,MTOPP,MTOPS,STUMP,
     & D,X1,HTTYPE,H,I1,HT1PRD,HT2PRD,X1,X1,X1,X1,I1,X1,X1,IFC,
     & DBTBH,BARK*100.,I3,I7,I15,I20,I21,TVOL,LOGVOL,LOGDIA,LOGLEN,
     & BOLHT,TLOGS,NOLOGP,NOLOGS,CUTFLG,BFPFLG,CUPFLG,CDPFLG,
     & SPFLG,CONSPEC,PROD,HTTDUM,LIVEDUM,
     & BADUM,SIDUM,CTYPE,IERR,IDIST)
C
      IF(DEBUG)WRITE(JOSTND,*)'AFTER VOLINIT CF IERR,PROD,TVOL= ',
     & IERR,PROD,TVOL
      IF(DEBUG)WRITE(JOSTND,*)' VOLEQ,HT1PRD,TLOGS,NOLOGP= ',
     & VOLEQ,HT1PRD,TLOGS,NOLOGP      
      IF(DEBUG)WRITE(JOSTND,*)' AFTER VOLINIT CF TVOL= ',TVOL
      IF(DEBUG)WRITE(JOSTND,*)' AFTER VOLINIT CF PROD,LOGVOL= ',
     &                          PROD,LOGVOL
      IF(DEBUG)WRITE(JOSTND,*)' AFTER VOLINIT CF LOGDIA= ',
     &                         LOGDIA
      IF(DEBUG)WRITE(JOSTND,*)' AFTER VOLINIT CF LOGLEN= ',
     &                           LOGLEN
C----------
C  STORE CALCULATED MERCH HEIGHTS
C----------
      IF(D.GE.DBHMIN(ISPC))THEN
        IF(IT.GT.0)HT2TD(IT,2)=HT1PRD
      ELSE
        IF(IT.GT.0)HT2TD(IT,2)=HT2PRD
      ENDIF        
      IF(D.GE.BFMIND(ISPC))THEN
        IF(IT.GT.0)HT2TD(IT,1)=HT1PRD
      ELSE
        IF(IT.GT.0)HT2TD(IT,1)=HT2PRD
      ENDIF
      IF((IREGN.EQ.8).AND.(HT1PRD.LT.10.))THEN
        TVOL(4)=0.
        TVOL(2)=0.
      ENDIF  
C----------
C  END OF CF SECTION
C  IF THE BF VOLUME EQ. IS DIFFERENT THAN THE CF VOLUME EQ.
C  THEN STORE CUBIC VOLUMES AND CALL PROFILE AGAIN TO CALCULATE
C  BF VOLUMES ONLY
C----------
      IF((VEQNNB(ISPC).NE.VEQNNC(ISPC)).OR.(IREGN.EQ.5))THEN
        TVOL1=TVOL(1)
        TVOL4=TVOL(4)
        TVOL7=TVOL(7)
        VOLEQ=VEQNNB(ISPC)
        IF((IREGN.EQ.8)
     &                 .AND.(VOLEQ(4:6).EQ.'CLK'))THEN
          WRITE(BFTOP,'(I1)')NINT(BFTOPD(ISPC))
          VOLEQ(3:3)=BFTOP
        ENDIF
        DO IZERO=1,15
        TVOL(IZERO)=0.
        ENDDO
C
C
C
C
        I1=0
        I3=3
        I7=7
        I15=15
        I20=20
        I21=21
        X1=0.
C----------
C  CONSTANT CHARACTER ARGUMENTS
C----------
        CTYPE='F'
C----------
C  PRODUCT FLAGS
C----------
C  TOAL CUBIC
        CUTFLG=0
C  BF
        BFPFLG=1
C  MERCH CUBIC
        CUPFLG=0
C  CORDWOOD
        CDPFLG=0
C  SECONDARY PRODUCT
        SPFLG=0
C  CONSTANTS
        TLOGS = 0.
        NOLOGP = 0.
        NOLOGS = 0.
        IF((IREGN.NE.8)
     &     .AND.(VOLEQ(4:6).NE.'DVE'))THEN
          HT1PRD=0.
          HT2PRD=0.
        ENDIF
        IF((IREGN.EQ.8).AND.(VOLEQ(4:6).EQ.'CLK'))THEN
          HT1PRD=0.
          HT2PRD=0.
        ENDIF
        IERR=0
        SIDUM=0
        BADUM=0
        HTTDUM=0
        LIVEDUM=' '
        CONSPEC='   '
C
C
C
C
        IF(IREGN.EQ.5)MTOPP=BFTOPD(ISPC)*BARK
C
      DBTBH = D*(1-BARK)
      IF(DEBUG)WRITE(JOSTND,*)' CALLING VOLILNIT BF ISPC,ARGS = ',
     &  ISPC,IREGN,FORST,VEQNNB(ISPC),MTOPP,MTOPS,BFSTMP(ISPC),D,H,
     &  HT1PRD,IFC,DBTBH,BARK,CUTFLG,BFPFLG,CUPFLG,CDPFLG,
     &  SPFLG,CONSPEC,PROD
C
        CALL VOLINIT(IREGN,FORST,VOLEQ,MTOPP,MTOPS,BFSTMP(ISPC),
     &  D,X1,HTTYPE,H,I1,HT1PRD,HT2PRD,X1,X1,X1,X1,I1,X1,X1,IFC,
     &  DBTBH,BARK*100.,I3,I7,I15,I20,I21,TVOL,LOGVOL,LOGDIA,LOGLEN,
     &  BOLHT,TLOGS,NOLOGP,NOLOGS,CUTFLG,BFPFLG,CUPFLG,CDPFLG,
     &  SPFLG,CONSPEC,PROD,HTTDUM,LIVEDUM,
     &  BADUM,SIDUM,CTYPE,IERR,IDIST)
C
        IF(DEBUG)WRITE(JOSTND,*)' VOLEQ,HT1PRD,TLOGS,NOLOGP= ',
     &   VOLEQ,HT1PRD,TLOGS,NOLOGP      
        IF(DEBUG)WRITE(JOSTND,*)' AFTER VOLINIT BF TVOL= ',TVOL
        IF(DEBUG)WRITE(JOSTND,*)' AFTER VOLINIT BF PROD,LOGVOL= ',
     &                            PROD,LOGVOL
        IF(DEBUG)WRITE(JOSTND,*)' AFTER VOLINIT BF LOGDIA= ',
     &                            LOGDIA
        IF(DEBUG)WRITE(JOSTND,*)' AFTER VOLINIT BF LOGLEN= ',
     &                            LOGLEN
C
        IF(D.GE.BFMIND(ISPC))THEN
          IF(IT.GT.0)HT2TD(IT,1)=HT1PRD
        ELSE
          IF(IT.GT.0)HT2TD(IT,1)=HT2PRD
        ENDIF
C
C  WHEN THERE IS NO CF VOL. EQ. NO. (E.G. CF METHC=2 OR 3, AND CFVOL
C  OR OCFVOL IS CALLED FROM VOLS, MUST SET VMAX SO TRUNCATED VOLUMES
C  ARE CALCULATED
C
        TVOL(1)=TVOL1
        TVOL(4)=TVOL4
        TVOL(7)=TVOL7
        IF((IREGN.EQ.8).AND.(HT1PRD.LT.10.))THEN
          TVOL(4)=0.
          TVOL(2)=0.
        ENDIF  
      ENDIF                   ! END OF BF SECTION
  500 CONTINUE
C----------
C  SET RETURN VALUES.
C----------
      IF((IREGN.EQ.8).OR.(IREGN.EQ.9))THEN
        IF(D.LT.BFMIND(ISPC))THEN
          VN=TVOL(4)
          VM=0.
        ELSE
          VN=TVOL(4)+TVOL(7)
          VM=TVOL(4)
        ENDIF
        IF(VN.LT.0.)VN=0.
        VMAX=VN
      ELSE                  ! ALL OTHER REGIONS
        VN=TVOL(1)
        IF(VN.LT.0.)VN=0.
        VMAX=VN
        IF(D .LT. DBHMIN(ISPC))THEN
          VM = 0.
        ELSE
          VM=TVOL(4)
          IF(VM.LT.0.)VM=0.
        ENDIF
      ENDIF
C
      IF(D.LT.BFMIND(ISPC))THEN
        BBFV=0.
      ELSE
        IF(METHB(ISPC).EQ.9) THEN
          BBFV=TVOL(10)
        ELSE
          BBFV=TVOL(2)
        ENDIF
        IF(BBFV.LT.0.)BBFV=0.
      ENDIF
      CTKFLG = .TRUE.
      BTKFLG = .TRUE.
      RETURN
C
C
C----------
C  ENTER ANY OTHER CUBIC HERE
C----------
      ENTRY OCFVOL (VN,VM,ISPC,D,H,TKILL,BARK,ITRNC,VMAX,LCONE,
     1              CTKFLG,IT)
      VVN=0.0
      BBF=0.0
      CALL VARVER(VVER)
      IF(VVER(:2).EQ.'AK')THEN
        CALL FVSBRUCEDEMARS(VN,VM,VMAX,D,H,ISPC,BARK,LCONE,CTKFLG)
      ELSEIF((VVER(:2).EQ.'SM').OR.(VVER(:2).EQ.'SP').OR.
     &       (VVER(:2).EQ.'BP').OR.(VVER(:2).EQ.'SF').OR.
     &       (VVER(:2).EQ.'LP'))THEN
        CALL FVSHANNBARE(VN,VM,VMAX,ISPC,D,H,CTKFLG)
      ELSEIF(VVER(:2).EQ.'NC')THEN
        CALL FVSSIERRALOG(VN,VM,VMAX,ISPC,D,H,BARK,LCONE,CTKFLG)
      ELSEIF((VVER(:2).EQ.'CS').OR.(VVER(:2).EQ.'LS').OR.
     &        (VVER(:2).EQ.'NE'))THEN
        VN=0.
        VM=0.
        IF (IMC(IT) .GE. 3 .OR. D .LT. DBHMIN(ISPC))GOTO 600
        CALL TWIGCF(ISPC,H,D,VN,VM,IT)
  600 CONTINUE
      ELSE
        VN=0.
        VMAX=0.
        VM=0.
        CTKFLG = .FALSE.
      ENDIF
C----------
C  SET RETURN VALUES HERE
C----------
      CTKFLG = .TRUE.
      IF(VN.LE.0.)THEN
        VN=0.
        CTKFLG = .FALSE.
      ENDIF
      IF(VM.LE.0.)THEN
        VM=0.
      ENDIF
      VMAX=VN
      RETURN
C
C
C----------
C  ENTER ANY OTHER BOARD HERE.
C----------
      ENTRY OBFVOL (BBFV,ISPC,D,H,TKILL,BARK,ITRNC,VMAX,LCONE,
     1              BTKFLG,IT)
      CALL VARVER(VVER)
      IF(VVER(:2).EQ.'AK')THEN      
        VVN=0.0
        BBF=0.0
        IF(D .GE. 9.0 .AND. H .GT. 40.0)
     &  CALL FVSOLDGRO(ISPC,VVN,D,H,BBF)
        BBFV=BBF
      ELSEIF((VVER(:2).EQ.'SM').OR.(VVER(:2).EQ.'SP').OR.
     &       (VVER(:2).EQ.'BP').OR.(VVER(:2).EQ.'SF').OR.
     &       (VVER(:2).EQ.'LP'))THEN
        CALL HANNBAREBF(BBFV,ISPC,D,H,VMAX,BTKFLG)
      ELSEIF(VVER(:2).EQ.'NC')THEN
        ITD=BFTOPD(ISPC)+0.5
        IF(ITD.GT.100) ITD = 100
        CALL LOGS(D,H,ITD,ITD,DBHMIN(ISPC),BFMIND(ISPC),ISPC,
     &            BFSTMP(ISPC),BV,JOSTND)
        BBFV=BV
      ELSEIF((VVER(:2).EQ.'CS').OR.(VVER(:2).EQ.'LS').OR.
     &        (VVER(:2).EQ.'NE'))THEN
        BBFV=0.
        IF(D .LT. BFMIND(ISPC) .OR. IMC(IT) .GT. 1)GOTO 700
        CALL TWIGBF(ISPC,H,D,VMAX,BBFV)
  700   CONTINUE
      ELSE
        BBFV=0.
      ENDIF
      BTKFLG = .TRUE.
      IF(BBFV .LE. 0.) THEN
        BBFV=0.
        BTKFLG=.FALSE.
      ENDIF    
      RETURN
C
C
C----------
C  ENTRY POINT FOR SENDING VOLUME EQN NUMBER TO THE FVS-TO-NATCRZ ROUTINE
C----------
      ENTRY GETEQN(ISPC,D,H,EQNC,EQNB,TDIBC,TDIBB)
      EQNC=VEQNNC(ISPC)
      EQNB=VEQNNB(ISPC)
      TDIBC=TOPD(ISPC)*BRATIO(ISPC,D,H)
      TDIBB=BFTOPD(ISPC)*BRATIO(ISPC,D,H)
      RETURN
C
      END
