      SUBROUTINE RCDSET (IC,LRETRN)
      use prgprm_mod
      implicit none
C----------
C  $Id$
C----------
C
C     SETS THE RETURN CODE FOR THE PROGNOSIS MODEL.
C
      INCLUDE 'CONTRL.F77'
C
      INTEGER IC
      LOGICAL LRETRN
C
      IF (IC.GT.ICCODE) ICCODE=IC
      IF (.NOT.LRETRN) CALL fvsSetRtnCode (1)
      RETURN
      END
