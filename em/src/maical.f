      SUBROUTINE MAICAL
      use htcal_mod
      use plot_mod
      use arrays_mod
      use contrl_mod
      use coeffs_mod
      use outcom_mod
      use prgprm_mod
      implicit none
C----------
C  **MAICAL--EM   DATE OF LAST REVISION:  03/26/09
C----------
C  THIS SUBROUTINE CALCULATES THE MAI FOR THE STAND. IT IS CALLED
C  FROM CRATET.
C----------
C
C----------
      LOGICAL DEBUG
      INTEGER ISPNUM(MAXSP),ISICD,IERR
      REAL SSSI,ADJMAI
C----------
C  SPECIES ORDER:
C   1=WB,  2=WL,  3=DF,  4=LM,  5=LL,  6=RM,  7=LP,  8=ES,
C   9=AF, 10=PP, 11=GA, 12=AS, 13=CW, 14=BA, 15=PW, 16=NC,
C  17=PB, 18=OS, 19=OH
C
C  SPECIES EXPANSION
C  LM USES IE LM (ORIGINALLY FROM TT VARIANT)
C  LL USES IE AF (ORIGINALLY FROM NI VARIANT)
C  RM USES IE JU (ORIGINALLY FROM UT VARIANT)
C  AS,PB USE IE AS (ORIGINALLY FROM UT VARIANT)
C  GA,CW,BA,PW,NC,OH USE IE CO (ORIGINALLY FROM CR VARIANT)
C----------
C  INITIALIZE INTERNAL VARIABLES:
C  (ISPNUM CONTAINS THE FIA CODE FOR THE PROXY EQUATION FOR THE CALL TO
C   SUBROUTINE **ADJMAI**)
C----------
      DATA ISPNUM/ 119, 117, 202, 101, 019, 101, 108, 093, 021, 122,
     &             101, 101, 101, 101, 101, 101, 101, 122, 101/
C-----------
C  CHECK FOR DEBUG.
C-----------
      CALL DBCHK (DEBUG,'MAICAL',6,ICYC)
C
      IF(DEBUG) WRITE(JOSTND,3)ICYC
    3 FORMAT(' ENTERING SUBROUTINE MAICAL  CYCLE =',I5)
C----------
C   RMAI IS FUNCTION TO CALCULATE ADJUSTED MAI.
C----------
      IF (ISISP .EQ. 0) ISISP=3
      SSSI=SITEAR(ISISP)
      IF (SSSI .EQ. 0.) SSSI=140.0
      ISICD=ISPNUM(ISISP)
      RMAI=ADJMAI(ISICD,SSSI,10.0,IERR)
      IF(RMAI .GT. 128.0)RMAI=128.0
      IF(DEBUG) WRITE(JOSTND,10)ICYC,RMAI
   10 FORMAT(' LEAVING SUBROUTINE MAICAL  CYCLE =',I5,5X,'RMAI =',F10.3)
      RETURN
      END
