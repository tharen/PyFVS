      SUBROUTINE AVHT40
      use contrl_mod
      use plot_mod
      use arrays_mod
      use prgprm_mod
      implicit none
C----------
C  **AVHT40--SO   DATE OF LAST REVISION:  03/02/92
C----------
C  THIS SUBROUTINE IS USED TO CALCULATE THE AVERAGE HEIGHT
C  OF THE 40 TPA OF LARGEST DIAMETER.
C
      INTEGER  I,II
      REAL     SSUMN,P

      AVH=0.
      IF (ITRN.LE.0) GOTO 70
      SSUMN=0.
      DO 60 I=1,ITRN
      II=IND(I)
      P=PROB(II)
      IF(SSUMN+P.GT.40.0) P=40.0-SSUMN
      SSUMN=SSUMN+P
      AVH=AVH+HT(II)*P
      IF(SSUMN.GE.40.0) GO TO 65
   60 CONTINUE
   65 CONTINUE
      IF (SSUMN .GT. 0.) AVH = AVH/SSUMN
   70 CONTINUE
      RETURN
      END
