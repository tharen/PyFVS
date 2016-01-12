      SUBROUTINE FMTDEL (IVAC,IREC)
      use fmcom_mod
      use arrays_mod
      use fmparm_mod
      use prgprm_mod
      implicit none
C----------
C  $Id$
C----------
C
C     THIS SUBROUTINE IS USED TO DELETE TREE RECORDS BY MOVING
C     RECORD IREC TO POSITION IVAC.
C
C     THE FIRE/SNAG MODEL.
C
C  CALLED BY :
C     TREDEL
C
      INTEGER IREC,IVAC,JJ

      IF (.NOT. LFMON) RETURN

C     fmprob and fmicr MAY not need to be compressed, tre-del'ed, etc???

      FMPROB(IVAC) = FMPROB(IREC)
      FMICR(IVAC)  = FMICR(IREC)

      OLDHT(IVAC)  = OLDHT(IREC)
      OLDCRL(IVAC) = OLDCRL(IREC)
      GROW(IVAC)   = GROW(IREC)

      DO JJ = 0, 5
         OLDCRW(IVAC, JJ) = OLDCRW(IREC, JJ)
         CROWNW(IVAC, JJ) = CROWNW(IREC, JJ)
      ENDDO

      RETURN
      END



