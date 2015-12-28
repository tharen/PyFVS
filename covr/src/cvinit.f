      SUBROUTINE CVINIT
      use contrl_mod
      use arrays_mod
      use prgprm_mod
      implicit none
C----------
C  **CVINIT DATE OF LAST REVISION:  07/11/08
C----------
C  INITIALIZES COVER VARIABLES FOR THE CURRENT STAND.
C  CALLED FROM **INITRE**.
C----------
COMMONS
      INCLUDE 'CVCOM.F77'
C
      INTEGER I,J
      ICVBGN = 0
      JOSHRB = JOSTND
C
C     VARIABLES ADDED TO CONTROL PRINTING NOHEADING OUTPUT OF COVER.
C
      JCVNOH = 24
      LCVNOH = .FALSE.
C
      LBROW = .FALSE.
      LCOV = .FALSE.
      LCNOP = .FALSE.
      LCOVER = .TRUE.
      LSHOW = .FALSE.
      LSHRUB = .TRUE.
      LCVSUM = .TRUE.
      LCAL1 = .FALSE.
      LCAL2 = .FALSE.
      LCALIB = .FALSE.
      DO 10 I = 1,3
      AVGBHT(I) = -99999.0
      AVGBPC(I) = -99999.0
   10 CONTINUE
      DO 15 I = 1,31
      SHRBHT(I) = -99999.0
      SHRBPC(I) = -99999.0
      BHTCF(I) = 1.0
      BPCCF(I) = 1.0
   15 CONTINUE
      DO 20 J=1,MAXCY1
      LTHIND(J) = .FALSE.
   20 CONTINUE
      RETURN
      END
