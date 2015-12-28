      SUBROUTINE MBACAL
      use htcal_mod
      use pden_mod
      use arrays_mod
      use contrl_mod
      use coeffs_mod
      use eshap_mod
      use plot_mod
      use prgprm_mod
      implicit none
C----------
C  $Id$
C----------
C   THIS SUBROUTINE IDENTIFIES THE SITE SPECIES (IE SPECIES OF
C   MAXIMUM BASAL AREA IN THE STAND. THIS ROUTINE IS CALLED FROM
C   CRATET AND DENSE.
C----------
C
      INTEGER ISPC,II,I,MDX
      REAL P,XMAX
C
C----------
C     FIND SITE SPECIES. IF SITE SPECIES HAS BEEN ENTERED ON KEYWORD
C     THEN BYPASS THE CALCULATION BASED ON THE TREELIST.
C---------
      IF(LSITE) RETURN
      IF (ITRN .LE. 0 .OR. ISISP .GT. 0) RETURN
      DO 20 ISPC=1,MAXSP
      IF (ISCT(ISPC,1).EQ.0) GOTO 20
      DO 10 II=ISCT(ISPC,1),ISCT(ISPC,2)
      I=IND1(II)
C----------
C     SKIP DEAD TREES.
C---------
      IF (I.GE.IREC2) GOTO 10
      P=PROB(I)
      BARANK(ISPC)=.005454154*P*DBH(I)*DBH(I)+BARANK(ISPC)
   10 CONTINUE
   20 CONTINUE
      MDX=0
      XMAX=0.
      DO 30 I=1,MAXSP
      IF(XMAX .GT. BARANK(I)) GOTO 30
      MDX=I
      XMAX=BARANK(I)
   30 CONTINUE
      ISISP=MDX
      RETURN
      END
