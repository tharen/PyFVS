      SUBROUTINE FMCROW
      use contrl_mod
      use fmcom_mod
      use arrays_mod
      use fmparm_mod
      use prgprm_mod
      implicit none
C----------
C  **FMCROW  FIRE-CS DATE OF LAST REVISION:  01/10/12
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
C  LISTED IN FMCROWE.F.  MAPPING IS DONE WHERE NECESSARY.  NOTE: MANY
C  SPECIES ARE MAPPED TO COMMERCIAL HARDWOODS SO THAT THE "MIXED HARDWOOD"
C  EQUATIONS IN JENKINS ET. AL. ARE USED.
C----------
      DATA ISPMAP /14,14, 5, 5, 5, 5, 5,46,45,59,
     &             59,59,59,39,39,39,39,38,39,39,
     &             37,39,39,28,15,16,16,17,19,50,
     &             18,20,21,55,55,21,21,21,22,23,
     &             44,25,26,16,29,16,30,34,34,35,
     &             34,34,33,31,32,30,30,30,30,36,
     &             34,34,30,30,34,30,34,44,67,44,
     &             44,44,44,42,40,41,48,60,60,44,
     &             24,44,64,65,56,53,44,56,57,44,
     &             44,44,44,44,47,44/
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
        IF (DEBUG) WRITE(JOSTND,8) ISP(I),SPI
    8   FORMAT(' FMCROW ISP(I) = ',I2,' SPI=',I5)
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
      IF (DEBUG) WRITE(JOSTND,9) D,CROWNW(I,0),CROWNW(I,1),
     >  CROWNW(I,2),CROWNW(I,3),CROWNW(I,4),CROWNW(I,5)
    9 FORMAT(' FMCROW DBH = ',F5.1,' CROWNW=',6F10.4)
      IF (DEBUG) WRITE(JOSTND,10) (CROWNW(I,0)+ CROWNW(I,1) +
     >  CROWNW(I,2) + CROWNW(I,3) + CROWNW(I,4) + CROWNW(I,5))
   10 FORMAT(' FMCROW SUM OF CROWNW=',F10.4)
C
  999 CONTINUE
C
      RETURN
      END
