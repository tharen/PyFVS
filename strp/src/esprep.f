      SUBROUTINE ESPREP (ISER,PNONE,PMECH,PBURN)
      use esparm_mod
      use escomn_mod
      use plot_mod
      use prgprm_mod
      implicit none
C----------
C  **ESPREP DATE OF LAST REVISION:  09/17/08
C----------
C     PREDICT DEFAULT SITE PREP PROBABILITIES.
C
      REAL PBURN,PMECH,PNONE
      INTEGER ISER
C
C     PROB(NO SITE PREP)
C
      PNONE = 0.75
C
C     PROB(MECH SITE PREP)
C
      PMECH = 0.20
C
C     PROB(BURN SITE PREP)
C
      PBURN = 0.05
      RETURN
      END
