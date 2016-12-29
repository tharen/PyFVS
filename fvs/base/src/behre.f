      FUNCTION BEHRE(L1,L2)
      IMPLICIT NONE
C----------
C  $Id$
C----------
C
C  THIS FUNCTION CALCULATES THE VOLUME OF A SOLID OF REVOLUTION
C  DESCRIBED BY A BEHRE TAPER CURVE USING PARAMETERS 'AHAT' AND 'BHAT'.
C  THE LIMITS OF INTEGRATION ARE 'L1' AND 'L2'.
C
C  IT SHOULD BE NOTED THAT THE VOLUME CALCULATED IS OFF BY A FACTOR
C  OF PI/(A CUBED) - THIS VALUE WOULD CANCEL OUT IN THE RATIOS OF
C  BEHRE VOLUMES USED IN CALLING ROUTINE 'VOLS'.
C
COMMONS
C
C
      INCLUDE 'PRGPRM.F77'
C
C
      INCLUDE 'COEFFS.F77'
C
C
COMMONS
C
      REAL L1,L2,BEHRE,ALB1,ALB2
C
      ALB1 = AHAT*L1 + BHAT
      ALB2 = AHAT*L2 + BHAT
      BEHRE = ALB2 - ALB1 - 2.0*BHAT*(ALOG(ALB2) - ALOG(ALB1))
     &        - BHAT*BHAT/ALB2 + BHAT*BHAT/ALB1
      RETURN
      END
