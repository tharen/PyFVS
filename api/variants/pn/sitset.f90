      SUBROUTINE SITSET
      use siteht_mod, only: get_siteht

      use prgprm_mod
      use contrl_mod
      use plot_mod
      use varcom_mod
      use volstd_mod

      IMPLICIT NONE
!----------
!  **SITSET--PN   DATE OF LAST REVISION:  05/11/11
!----------
!
! THIS SUBROUTINE LOADS THE SITEAR ARRAY WITH A SITE INDEX FOR EACH
! SPECIES, GIVEN A SITE INDEX AND SITE SPECIES, AND LOADS THE SDIDEF
! ARRAY WITH SDI MAXIMUMS FOR SPECIES WHICH WERE NOT ASSIGNED A VALUE
! USING THE SDIMAX KEYWORD.
!----------
      LOGICAL DEBUG
      CHARACTER FORST*2,FORDUM*2,DIST*2,PROD*2,VAR*2,VOLEQ*10
      CHARACTER*4 ASPEC
      INTEGER IFIASP, ERRFLAG
      INTEGER NSISET,NSDSET,I,JSISP,INDEX,KNTECO,NTOHI,ISEQ,NUM
      INTEGER ISFLAG,ITOHI,ISPC,J,JJ,K,INTFOR,IREGN,IRDUM
      REAL SIAGE(MAXSP)
      REAL FORMAX,RSI,RSDI,AG,SINDX,SI
!
      DATA FORMAX/950./
!-----------
!  SEE IF WE NEED TO DO SOME DEBUG.
!-----------
      CALL DBCHK (DEBUG,'SITSET',6,ICYC)
!----------
!  DETERMINE HOW MANY SITE VALUES AND SDI VALUES WERE SET VIA KEYWORD.
!----------
      NSISET=0
      NSDSET=0
      DO 5 I=1,MAXSP
      IF(SITEAR(I) .GT. 0.) NSISET=NSISET+1
      IF(SDIDEF(I) .GT. 0.) NSDSET=NSDSET+1
    5 CONTINUE
      IF(DEBUG)WRITE(JOSTND,*)'ENTERING SITSET, SITE VALUES SET = ', &
      NSISET,'  SDI VALUES SET = ',NSDSET
!----------
!  SET SITE SPECIES AND SITE INDEX IF NOT SET BY KEYWORD.
!  FOR REGION 6 FORESTS SET SDI DEFAULTS HERE ALSO.
!----------
!     REGION 6 FOREST --- CALL **ECOCLS** WITH THE ECOCLASS CODE,
!     AND GET BACK THE DEFAULT SITE SPECIES, ALL SITE INDICIES,
!     AND DEFAULT SDI MAXIMUMS ASSOCIATED WITH THE ECOCLASS
!----------
      JSISP=0
      INDEX=0
      KNTECO=0
      NTOHI=0
   10 CALL ECOCLS (PCOM,ASPEC,RSDI,RSI,ISFLAG,NUM,INDEX,ISEQ)
      KNTECO=KNTECO+1
      IF(DEBUG)WRITE(JOSTND,*)'AFTER ECOCLS,PCOM,ASPEC,RSDI,RSI,', &
      'ISFLAG,NUM,INDEX,ISEQ = ',PCOM,ASPEC,RSDI,RSI,ISFLAG, &
      NUM,INDEX,ISEQ
!----------
! IF DEFAULT SDI IS OUT OF BOUNDS, RESET IT.
!----------
        ITOHI=0
        IF(RSDI .GT. FORMAX)THEN
          RSDI=FORMAX
          ITOHI=1
        ENDIF
!
      IF(ISEQ .EQ. 0) GO TO 25
      IF(JSISP.EQ.0 .AND. ISFLAG.EQ.1) JSISP=ISEQ
      IF(ISISP.LE.0 .AND. ISFLAG.EQ.1) ISISP=ISEQ
      IF(SITEAR(ISEQ).LE.0. .AND. NSISET.EQ.0) SITEAR(ISEQ)=RSI
      IF(SDIDEF(ISEQ) .LE. 0.) THEN
        SDIDEF(ISEQ)=RSDI
        IF(ITOHI .GT. 0) NTOHI=NTOHI+1
      ENDIF
      IF(ISISP.GT.0 .AND. ISFLAG.EQ.1) THEN
        IF(SDIDEF(ISISP) .LE. 0.) THEN
          SDIDEF(ISISP)=RSDI
          IF(ITOHI .GT. 0) NTOHI=NTOHI+1
        ENDIF
      ENDIF
   25 CONTINUE
      IF(NUM.GT.1 .AND. KNTECO.LT.NUM) THEN
        INDEX=INDEX+1
        GO TO 10
      ENDIF
!---------
!  ON THE CHANCE THAT A SITE SPECIES WAS NOT ENCOUNTERED IN **ECOCLS**
!  PROVIDE A REGION 6 GLOBAL DEFAULT.
!----------
      IF(ISISP .LE. 0) ISISP = 16
      IF(SITEAR(ISISP) .LE. 0.0) SITEAR(ISISP) = 100.
!----------
! TRANSLATE SITE INDEX TO A REFERENCE AGE FOR EACH SPECIES.
!----------
      CALL SICHG(ISISP,SITEAR(ISISP),SIAGE)
!----------
! IF SITEAR HAS NOT BEEN SET WITH SITECODE KEYWORD,
! LOAD IT WITH SITE VALUES CALCULATED HERE.
!----------
      DO 30 ISPC=1,MAXSP
      IF(DEBUG)WRITE(JOSTND,*)'IN SITSET ISISP,ISPC,SITEAR =', &
      ISISP,ISPC,SITEAR(ISISP)
      IF(SITEAR(ISPC) .GT. 0.0) GO TO 30
      AG=SIAGE(ISPC)
      SINDX=SITEAR(ISISP)
      IF(DEBUG)WRITE(JOSTND,*)'CALLING HTCALC SINDX,ISISP,AG,SI=', &
      SINDX,ISISP,AG,SI
      SI = 0.
      CALL HTCALC(SINDX,ISISP,AG,SI,JOSTND,DEBUG)
!      call get_siteht(sindx,isisp,ag,si)

      IF(DEBUG)WRITE(JOSTND,*)'RETURN FROM HTCALC,SINDX,ISISP,AG,SI=', &
      SINDX,ISISP,AG,SI
      IF(ISPC.EQ.20 .AND. ISISP.NE.20)THEN
        SI=SI/3.281
        IF(SI.GT.28.)SI=28.
      ENDIF
!----------
! MISC. HARDWOODS USE CURTIS SI CURVE CREATED FOR DF. THE
! FOLLOWING EQUATIONS ARE USED TO REDUCE THEIR GROWTH UNTIL
! SI CURVES ARE FOUND FOR THESE SPECIES. 4/26/01 EES.
!----------
      IF(ISPC.EQ.21 .AND. ISISP.NE.21)SI=SI*.75
      IF(ISPC.EQ.23 .AND. ISISP.NE.23)SI=SI*.65
      IF(ISPC.EQ.24 .AND. ISISP.NE.24)SI=SI*1.5
      IF(ISPC.EQ.25 .AND. ISISP.NE.25)SI=SI*.70
      IF(ISPC.EQ.26 .AND. ISISP.NE.26)SI=SI*.75
      IF(ISPC.EQ.27 .AND. ISISP.NE.27)SI=SI*.85
      IF(ISPC.EQ.29 .AND. ISISP.NE.29)SI=SI*.23
      IF(ISPC.EQ.31 .AND. ISISP.NE.31)SI=SI*.70
      IF(ISPC.EQ.33 .AND. ISISP.NE.33)SI=SI*.25
      IF(ISPC.EQ.34 .AND. ISISP.NE.34)SI=SI*.60
      IF(ISPC.EQ.35 .AND. ISISP.NE.35)SI=SI*.25
      IF(ISPC.EQ.36 .AND. ISISP.NE.36)SI=SI*.50
      IF(ISPC.EQ.37 .AND. ISISP.NE.37)SI=SI*.50
!----------
! FOR WHITE OAK USE METHOD DERIVED BY GOULD TO GET ESTIMATE OF MAXIMUM
! HEIGHT AT AGE 100) FROM DF, FROM KING CURVE (BASE AGE 50).
!----------
      IF(ISPC.EQ.28 .AND. ISISP.NE.28)SI=114.2*(1-EXP(-0.0266*SI))**2.26
!
      SITEAR(ISPC) = SI
   30 CONTINUE
!----------
! LOAD THE SDIDEF ARRAY
!----------
      DO 80 I=1,MAXSP
        IF(SDIDEF(I) .GT. 0.0) GO TO 80
        SDIDEF(I) = SDIDEF(ISISP)
   80 CONTINUE
      IF(BAMAX.LE.0) BAMAX=SDIDEF(ISISP)*(PMSDIU/100.)*0.54542
!
      DO 92 I=1,15
      J=(I-1)*10 + 1
      JJ=J+9
      IF(JJ.GT.MAXSP)JJ=MAXSP
      WRITE(JOSTND,90)(NSP(K,1)(1:2),K=J,JJ)
   90 FORMAT(/'SPECIES ',5X,10(A2,6X))
      WRITE(JOSTND,91)(SDIDEF(K),K=J,JJ )
   91 FORMAT('SDI MAX ',   10F8.0)
      IF(JJ .EQ. MAXSP)GO TO 93
   92 CONTINUE
   93 CONTINUE
      IF(NTOHI .GT. 0)WRITE(JOSTND,102)FORMAX
  102 FORMAT(/'*NOTE -- AT LEAST ONE DEFAULT MAXIMUM SDI EXCEEDED THE FO &
      REST DEFAULT MAXIMUM. FOREST DEFAULT MAXIMUM OF ',F5.0,' USED.',/ &
      ,'          YOU MAY NEED TO SPECIFICALLY RESET VALUES FOR THESE SP &
      ECIES USING THE SDIMAX KEYWORD.')
!----------
!  LOAD VOLUME DEFAULT MERCH. SPECS.
!----------
      SELECT CASE(IFOR)
      CASE(4,5,6)
        DO I=1,MAXSP
        IF(DBHMIN(I) .LE. 0.) DBHMIN(I) = 7.0
        IF(TOPD(I) .LE. 0.) TOPD(I) = 5.0
        IF(BFTOPD(I) .LE. 0.0) BFTOPD(I) = 5.0
        IF(BFMIND(I) .LE. 0.0) BFMIND(I) = 7.0
        END DO
      CASE DEFAULT
        DO I=1,MAXSP
        IF((DBHMIN(I) .LE. 0.).AND.(I.EQ.11)) DBHMIN(I) = 6.0
        IF((BFMIND(I) .LE. 0.).AND.(I.EQ.11)) BFMIND(I) = 6.0
        IF(DBHMIN(I) .LE. 0.) DBHMIN(I) = 7.0
        IF(TOPD(I) .LE. 0.) TOPD(I) = 4.5
        IF(BFTOPD(I) .LE. 0.) BFTOPD(I) = 4.5
        IF(BFMIND(I) .LE. 0.) BFMIND(I) = 7.0
        END DO
      END SELECT
!----------
!  LOAD VOLUME EQUATION ARRAYS FOR ALL SPECIES
!----------
      INTFOR = KODFOR - (KODFOR/100)*100
      WRITE(FORST,'(I2)')INTFOR
      IF(INTFOR.LT.10)FORST(1:1)='0'
      IREGN = KODFOR/100
      DIST='  '
      PROD='  '
      VAR='PN'
!
!  USE SPECIAL FORESTS FOR BIA LANDS
!  QUINAULT IR (FC=800 USES OLYMIC 609), ALSO IN INITRE VOLEQNUM PROCESS
!
      IF(IREGN.EQ.8)THEN
        IRDUM=6
        FORDUM='09'
      ELSE
        IRDUM=IREGN
        FORDUM=FORST
      ENDIF
!
      DO ISPC=1,MAXSP
      READ(FIAJSP(ISPC),'(I4)')IFIASP
      IF(((METHC(ISPC).EQ.6).OR.(METHC(ISPC).EQ.9)).AND. &
           (VEQNNC(ISPC).EQ.'          '))THEN
        CALL VOLEQDEF(VAR,IRDUM,FORDUM,DIST,IFIASP,PROD,VOLEQ,ERRFLAG)
        VEQNNC(ISPC)=VOLEQ
      ENDIF
      IF(((METHB(ISPC).EQ.6).OR.(METHB(ISPC).EQ.9)).AND. &
           (VEQNNB(ISPC).EQ.'          '))THEN
        CALL VOLEQDEF(VAR,IRDUM,FORDUM,DIST,IFIASP,PROD,VOLEQ,ERRFLAG)
        VEQNNB(ISPC)=VOLEQ
      ENDIF
      ENDDO
!----------
!  IF FIA CODES WERE IN INPUT DATA, WRITE TRANSLATION TABLE
!---------
      IF(LFIA) THEN
        CALL FIAHEAD(JOSTND)
        WRITE(JOSTND,211) (NSP(I,1)(1:2),FIAJSP(I),I=1,MAXSP)
 211    FORMAT ((T12,8(A3,'=',A6,:,'; '),A,'=',A6))
      ENDIF
!----------
!  WRITE VOLUME EQUATION NUMBER TABLE
!----------
      CALL VOLEQHEAD(JOSTND)
      WRITE(JOSTND,230)(NSP(J,1)(1:2),VEQNNC(J),VEQNNB(J),J=1,MAXSP)
 230  FORMAT(4(2X,A2,4X,A10,1X,A10,1X))
!
      RETURN
      END
