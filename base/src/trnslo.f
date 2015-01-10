      SUBROUTINE TRNSLO
      IMPLICIT NONE
C----------
C  $Id: trnslo.f 767 2013-04-10 22:29:22Z rhavis@msn.com $
C----------
C  TRNSLO DECODES THE INPUT SLOPE VALUE TO A RATIO.
C----------
COMMONS
C
C
      INCLUDE 'PRGPRM.F77'
C
C
      INCLUDE 'PLOT.F77'
C
C
COMMONS
C
C     CONVERT SLOPE TO A RATIO.
C
      SLOPE=SLOPE/100.
      IF (SLOPE.LT.0.0) SLOPE=0.0
      RETURN
      END
