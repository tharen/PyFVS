      SUBROUTINE SVCMP1
      use contrl_mod
      use svdata_mod
      use workcm_mod
      use prgprm_mod
      implicit none
C----------
C  $Id$
C----------
C
C     STAND VISUALIZATION GENERATION
C     N.L.CROOKSTON -- RMRS MOSCOW -- NOVEMBER 1998
C
C     INITIALIZE THE TREE STATUS CODES SO THEY CAN HOLD RE-
C     REFERENCING DATA.
C
C     CALLED FROM COMPRS AND TREDEL.
C
      INTEGER I
C
      IF (JSVOUT.EQ.0) RETURN
      IF (NSVOBJ.EQ.0) RETURN
      IF (ITRN.EQ.0) RETURN
      DO I=1,ITRN
         IWKCM1(I)=0
      ENDDO
      RETURN
      END
