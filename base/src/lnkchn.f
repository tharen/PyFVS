      SUBROUTINE LNKCHN (ITREE)
      use prgprm_mod
      implicit none
C----------
C  $Id$
C----------
C
      INCLUDE 'ARRAYS.F77'
C
      INCLUDE 'CONTRL.F77'
C
      INCLUDE 'PLOT.F77'
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
