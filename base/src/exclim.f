      SUBROUTINE EXCLIM
      use prgprm_mod
      implicit none
C----------
C  $Id$
C----------
C
C     EXTRA EXTERNAL REFERENCES FOR CLIMATE EXTENSION CALLS
C
      LOGICAL DEBUG,LKECHO,L
      INTEGER KEY,IRECNT,IPNT,ILIMIT
      CHARACTER*8 KEYWRD,NOCLIM
      REAL SDIDEF(MAXSP),XMAX,TREEMULT(MAXTRE),WK3(MAXTRE)
C
      DATA NOCLIM/'*NO CLIM'/
      ENTRY CLINIT
      RETURN
      ENTRY CLACTV (L)
      L=.FALSE.
      RETURN
      ENTRY CLSETACTV (L)
      RETURN
      ENTRY CLPUT (WK3,IPNT,ILIMIT)
      RETURN
      ENTRY CLGET (WK3,IPNT,ILIMIT)
      RETURN
      ENTRY CLIN  (DEBUG,LKECHO)
      CALL ERRGRO (.TRUE.,11)
      RETURN
      ENTRY CLKEY(KEY,KEYWRD)
      KEYWRD=NOCLIM
      RETURN
      ENTRY CLGMULT(TREEMULT)
      TREEMULT=1.
      RETURN
      ENTRY CLMORTS
      RETURN
      ENTRY CLMAXDEN (SDIDEF,XMAX)
      RETURN
      ENTRY CLAUESTB
      RETURN
      END
