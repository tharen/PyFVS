      SUBROUTINE LOGS(DBH,HT,IBCD,BDMIN,ISP,STMP,BV)
      IMPLICIT NONE
C----------
C  **LOGS--EM   DATE OF LAST REVISION:  03/26/09
C----------
C  REGION 5 BOARD FOOT VOLUME MODELS.
C  BY K.STUMPF, ADAPTED BY B.KRUMLAND, P.J.DAUGHERTY
C  NOT FOR USE IN REGION 1, SO JUST SET BV TO ZERO AND RETURN
C----------
C
      REAL DBH,HT,BDMIN,STMP,BV
      INTEGER IBCD,ISP
C
      BV=0.
      RETURN
      END
