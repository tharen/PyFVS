      SUBROUTINE ORGTRIP(I,ITFN)
      use prgprm_mod
      use organon_mod
      implicit none
C----------
C  **ORGTRIP  ORGANON--DATE OF LAST REVISION:  04/05/2015
C----------
C
C     TRIPLES TREE VARIABLES NEEDED FOR ORGANON PROCESSING.
C
C----------
C
      INTEGER I,ITFN
C
      IORG(ITFN)=IORG(I)
C
      RETURN
      END
