      SUBROUTINE SVCMP3
      use contrl_mod
      use svdata_mod
      use arrays_mod
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
C     CALLED FROM COMPRS AND TREDEL.
C
      INTEGER ISVOBJ,IPT
C
      IF (JSVOUT.EQ.0) RETURN
      IF (NSVOBJ.EQ.0) RETURN
C
C     IF THERE ARE NO LIVE TREES LEFT, THEN REMOVE REFERENCES
C     TO LIVE TREES.
C
      IF (ITRN.EQ.0) THEN
         DO ISVOBJ=1,NSVOBJ
            IF (IOBJTP(ISVOBJ).EQ.1) IOBJTP(ISVOBJ)=0
         ENDDO
         RETURN
      ENDIF
C
C     IPT POINTS TO TREE RECORDS.  IF ZERO, THEN THE POINTER
C     POINTS TO A RECORD THAT WILL BE REMOVED.
C
      DO ISVOBJ=1,NSVOBJ
         IF (IOBJTP(ISVOBJ).EQ.1) THEN
            IPT=IS2F(ISVOBJ)
            IF (IPT.GT.0) THEN
               IPT=IWKCM1(IPT)
               IF (IPT.GT.0) IS2F(ISVOBJ)=IPT
            ENDIF
         ENDIF
      ENDDO
      RETURN
      END
