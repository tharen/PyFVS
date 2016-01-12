      SUBROUTINE TRNSLO
      use plot_mod
      use prgprm_mod
      implicit none
C----------
C  $Id$
C----------
C  TRNSLO DECODES THE INPUT SLOPE VALUE TO A RATIO.
C----------
COMMONS
C     CONVERT SLOPE TO A RATIO.
C
      SLOPE=SLOPE/100.
      IF (SLOPE.LT.0.0) SLOPE=0.0
      RETURN
      END
