      FUNCTION BRATIO(IS,D,H)
      IMPLICIT NONE
C----------
C  **BRATIO--NI DATE OF LAST REVISION:  04/09/08
C----------
C
C FUNCTION TO COMPUTE BARK RATIOS. THIS ROUTINE IS VARIANT SPECIFIC
C AND EACH VARIANT USES ONE OR MORE OF THE ARGUMENTS PASSED TO IT.
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
      INTEGER IS
      REAL H,D,BRATIO
C
      BRATIO=BKRAT(IS)
      RETURN
      END
