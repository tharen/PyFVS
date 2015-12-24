      SUBROUTINE SPESRT
      use contrl_mod
      use arrays_mod
      use prgprm_mod
      implicit none
C----------
C  $Id$
C----------
C
C     REALIGNS THE SPECIES-ORDER SORT AFTER THE TREE LIST HAS
C     BEEN COMPRESSED AND/OR TREES HAVE BEEN ADDED VIA THE
C     REGENERATION ESTABLISHMENT SUBMODEL.
C
      INTEGER I
C
C     INITIALIZE VARIABLES
C
      NUMSP=0
      DO I=1,MAXSP
         ISCT(I,1)=0
         ISCT(I,2)=0
         IBEGIN(I)=0
         IREF(I)=0
         KOUNT(I)=0
      ENDDO
      IF (IREC1.EQ.0) THEN
         ITRN=0
      ELSE
C
C        SET UP INDEX POINTERS FOR CHAIN SORTING.
C
C        ESTABLISH A LINK IN THE CHAIN SORT FOR ALL LIVE TREES.
C
         DO I=1,IREC1
            CALL LNKCHN (I)
         ENDDO
C
C        SET UP THE POINTERS TO THE SPECIES-ORDER SORT; IND1 AND ISCT.
C
         CALL SETUP
      ENDIF
C
      RETURN
      END
