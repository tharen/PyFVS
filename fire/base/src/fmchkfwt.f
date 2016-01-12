      LOGICAL FUNCTION FMCHKFWT(I2)
      use contrl_mod
      use fmparm_mod
      use prgprm_mod
      implicit none
C----------
C  $Id$
C----------
C     CALLED FROM: FMDYN, FMCFMD (WS,NC,IC)
C
C  PURPOSE:
C
C     PREVENT PLACING MORE THAN MXFMOD ELEMENTS IN THE
C     FMOD/FWT ARRAY
C


      INTEGER I2

      FMCHKFWT = .FALSE.
      IF (I2 .LE. MXFMOD) THEN
        FMCHKFWT = .TRUE.
      ELSE
        WRITE (JOSTND,"(/1X,'*** FFE MODEL WARNING: TOO ',
     &    'MANY FIRE MODELS SPECIFIED; MAXIMUM= ',I2,
     &    ' CYCLE= ',I4)") MXFMOD, ICYC
        CALL RCDSET (2,.TRUE.)
      ENDIF

      RETURN
      END
