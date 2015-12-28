      SUBROUTINE LNKINT
      use contrl_mod
      use prgprm_mod
      implicit none
C----------
C  $Id$
C----------
C
      INTEGER I
C
      NUMSP=0
      DO 20 I=1,MAXSP
      ISCT(I,1)=0
      ISCT(I,2)=0
      IBEGIN(I)=0
      IREF(I)=0
      KOUNT(I)=0
   20 CONTINUE
      RETURN
      END
