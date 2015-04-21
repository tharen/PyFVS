      SUBROUTINE BASDAM(ITREE,ICODES)
      IMPLICIT NONE
C----------
C  $Id$
C----------
C
C  SUBROUTINE TO PROCESS THE SPECIAL TREE STATUS CODES, AND
C  PERCENT DEFECT CODES.
C----------
C
C     ITREE  = TREE POINTER FOR LIVE TREES.
C     ICODES = DAMAGE CODES ARRAY.
C              25 = PERCENT DEFECT CODE FOR BOTH MERCH CF AND BF
C              26 = PERCENT DEFECT CODE FOR MERCH CUBIC FOOT VOLUME
C              27 = PERCENT DEFECT CODE FOR MERCH BOARD FOOT VOLUME
C              28 = SITE TREE CODE; PROCESSED IN INTREE
C              55 = SPECIAL STATUS CODE
C
C    DEFECT() VALUES ARE CODED 11223344, WHERE
C       11 = INPUT MC DEFECT PERCENT
C       22 = INPUT BF DEFECT PERCENT
C       33 = MC DEFECT % USED FOR A GIVEN CYCLE
C       44 = BF DEFECT % USED FOR A GIVEN CYCLE
C
C  COMMONS
C
      INCLUDE 'PRGPRM.F77'
C
C
      INCLUDE 'ARRAYS.F77'
C
C
C  COMMONS
C----------
      INTEGER ICODES(6),ITREE,J,ITEMP
C----------
C  INITIALIZE BASE MODEL DEFECT AND SPECIAL STATUS VECTORS.
C----------
      ISPECL(ITREE)=0
      DEFECT(ITREE)=0
C----------
C  PROCESS CODES.
C----------
      DO 100 J=1,5,2
         IF(ICODES(J).EQ.25 .OR. ICODES(J).EQ.26) THEN
            ITEMP=ICODES(J+1)
            IF(ITEMP .GT. 99) ITEMP = 99
            IF(ITEMP .LT.  0) ITEMP = 0
            DEFECT(ITREE) = DEFECT(ITREE) + ITEMP*1000000
         ENDIF
         IF(ICODES(J).EQ.25 .OR. ICODES(J).EQ.27) THEN
            ITEMP=ICODES(J+1)
            IF(ITEMP .GT. 99) ITEMP = 99
            IF(ITEMP .LT.  0) ITEMP = 0
            DEFECT(ITREE) = DEFECT(ITREE) + ITEMP*10000
         ENDIF
         IF(ICODES(J).EQ.55) THEN
            ISPECL(ITREE)=ICODES(J+1)
            IF(ISPECL(ITREE).GT.99) ISPECL(ITREE)=99
            IF(ISPECL(ITREE).LT.0) ISPECL(ITREE)=0
         ENDIF
  100 CONTINUE
      RETURN
      END