      BLOCK DATA CUBRDS
      IMPLICIT NONE
C----------
C  **CUBRDS--WC    DATE OF LAST REVISION:  05/19/08
C----------
C  DEFAULT PARAMETERS FOR THE CUBIC AND BOARD FOOT VOLUME EQUATIONS.
C----------
COMMONS
C
C
      INCLUDE 'PRGPRM.F77'
C
C
      INCLUDE 'VOLSTD.F77'
C
C
COMMONS
C----------
C  COEFFICIENTS FOR CUBIC FOOT VOLUME FOR TREES THAT ARE SMALLER THAN
C  THE TRANSITION SIZE
C----------
      DATA CFVEQS/ 273*0.0 /
C----------
C  COEFFICIENTS FOR CUBIC FOOT VOLUME FOR TREES THAT ARE LARGER THAN
C  THE TRANSITION SIZE
C----------
      DATA CFVEQL/ 273*0.0 /
C----------
C  FLAG IDENTIFYING THE SIZE TRANSITION VARIABLE; 0=D, 1=D2H
C----------
      DATA ICTRAN/39*0/
C----------
C  TRANSITION SIZE.  TREES OF LARGER SIZE (D OR D2H) WILL COEFFICIENTS
C  FOR LARGER SIZE TREES.
C----------
      DATA CTRAN/ 39*0.0 /
C----------
C  COEFFICIENTS FOR BOARD FOOT VOLUME FOR TREES THAT ARE SMALLER THAN
C  THE TRANSITION SIZE
C----------
      DATA BFVEQS/ 273*0.0 /
C----------
C  COEFFICIENTS FOR BOARD FOOT VOLUME FOR TREES THAT ARE LARGER THAN
C  THE TRANSITION SIZE
C----------
      DATA BFVEQL/ 273*0.0/
C----------
C  FLAG IDENTIFYING THE SIZE TRANSITION VARIABLE; 0=D, 1=D2H
C----------
      DATA IBTRAN/ 39*0 /
C----------
C  TRANSITION SIZE.  TREES OF LARGER SIZE (D OR D2H) WILL USE COEFFICIENTS
C  FOR LARGER SIZE TREES.
C----------
      DATA BTRAN/ 39*0.0 /
      END
