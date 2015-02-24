      SUBROUTINE FMCROW
      use contrl_mod
      use fmcom_mod
      use arrays_mod
      use fmparm_mod
      use prgprm_mod
      implicit none
C----------
C  **FMCROW  FIRE-LS DATE OF LAST REVISION:  01/10/12
C----------
C     CALLED FROM: FMSDIT, FMPRUN
C     CALLS:
C
C  PURPOSE:
C     THIS SUBROUTINE CALCULATES CROWNW(TREE,SIZE), THE WEIGHT OF
C     VARIOUS SIZES OF CROWN MATERIAL THAT IS ASSOCIATED WITH EACH TREE
C     RECORD IN THE CURRENT STAND.
C----------
C  LOCAL VARIABLE DEFINITIONS:
C     D:        DBH
C     H:        HEIGHT
C     IC:       LENGTH OF LIVE CROWN
C     SP:       SPECIES
C----------
COMMONS
C----------
C  VARIABLE DECLARATIONS
C----------
      LOGICAL DEBUG
      INTEGER I,J,IC,ISPMAP(MAXSP),SPI
      REAL    D,H,SG,XV(0:5)
C----------
C  INDEX TO THE CROWN EQUATIONS USED.  THE SPECIES NUMBERS ARE THOSE
C  LISTED IN FMCROWE.F.  THE FIRST 68 ARE THE 68 SPECIES IN THE LS
C  VARIANT.
C----------
      DATA ISPMAP /1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,
     >            21,22,23,24,25,26,27,28,29,30,31,32,33,34,35,36,37,
     >            38,39,40,41,42,43,44,45,46,47,48,49,50,51,52,53,54,
     >            55,56,57,58,59,60,61,62,63,64,65,66,67,68/
C
C----------
C  CHECK FOR DEBUG
C----------
      CALL DBCHK (DEBUG,'FMCROW',6,ICYC)
      IF (DEBUG) WRITE(JOSTND,7) ICYC,ITRN
    7 FORMAT(' ENTERING FMCROW CYCLE = ',I2,' ITRN=',I5)
C
      IF (ITRN.EQ.0) RETURN
C
      DO 999 I = 1,ITRN
C----------
C  INCREMENT GROW TO KEEP TRACK OF WHETHER THIS CROWN IS FREE
C  TO GROW AFTER BEING BURNED IN A FIRE.  SKIP THE REST OF THE LOOP
C  IF GROW IS STILL LESS THAN 1 AFTER THE INCREMENT.
C----------
        IF (GROW(I) .LT. 1) GROW(I) = GROW(I) + 1
        IF (GROW(I) .LT. 1) GOTO 999
C----------
C  ARGUMENTS TO PASS
C----------
	      SPI = ISPMAP(ISP(I))
        D   = DBH(I)
        H   = HT(I)
        IC  = ICR(I)
	      SG  = V2T(ISP(I))
C----------
C  INITIALIZE ALL THE CANOPY COMPONENTS TO ZERO, AND SKIP THE REST
C  OF THIS LOOP IF THE TREE HAS NO DIAMETER, HEIGHT, OR LIVE CROWN.
C----------
        DO J = 0,5
	        XV(J) = 0.0
        ENDDO
C
        CALL FMCROWE(SPI,ISP(I),D,H,IC,SG,XV)
C----------
C  COPY TEMPORARY VALUES TO FFE ARRAY
C----------
        DO J = 0,5
          CROWNW(I,J) = XV(J)
          IF (DEBUG) WRITE(JOSTND,*) 'I=',I,' size=',J,
     &    ' CROWNW=',CROWNW(I,J)
        ENDDO
C
  999 CONTINUE
C
      RETURN
      END
