      SUBROUTINE LNKCHN (ITREE)
      use contrl_mod
      use plot_mod
      use arrays_mod
      use prgprm_mod
      implicit none
C----------
C  $Id$
C----------
C
      INTEGER ITREE,II,KPTRII,IR
C
      II = ISP(ITREE)
      IF (IBEGIN(II) .NE. 0) GO TO 30
      IBEGIN(II) = ITREE
      NUMSP=NUMSP + 1
      IUSED(NUMSP)=NSP(II,1)
      IREF(II)= NUMSP
      GO TO 40
   30 CONTINUE
      KPTRII=KPTR(II)
      IND2(KPTRII) = ITREE
   40 CONTINUE
      KPTR(II) = ITREE
      IR = IREF(II)
C
C     COUNT NUMBER OF TREE RECORDS PER SPECIES
C
      KOUNT(IR)=KOUNT(IR) + 1
C
      RETURN
      END
