      SUBROUTINE TRNASP
      use plot_mod
      use prgprm_mod
      implicit none
C----------
C  $Id$
C----------
C  TRNASP DECODES THE INPUT AZIMUTH VALUE GIVEN IN DEGREES AND CONVERTS
C  THE VALUE TO RADIANS. THE VALUE IS THEN ASSIGNED TO THE VARIABLE
C  ASPECT.
C----------
COMMONS
C----------
C   CONVERT ASPECT TO RADIANS.
C----------
      ASPECT=ASPECT*0.0174533
      RETURN
      END
