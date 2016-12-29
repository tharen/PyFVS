      BLOCK DATA CUBRDS
      IMPLICIT NONE
C----------
C  **CUBRDS--TT   DATE OF LAST REVISION:  03/09/10
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
C SPECIES ORDER FOR TETONS VARIANT:
C
C  1=WB,  2=LM,  3=DF,  4=PM,  5=BS,  6=AS,  7=LP,  8=ES,  9=AF, 10=PP,
C 11=UJ, 12=RM, 13=BI, 14=MM, 15=NC, 16=MC, 17=OS, 18=OH
C
C VARIANT EXPANSION:
C BS USES ES EQUATIONS FROM TT
C PM USES PI (COMMON PINYON) EQUATIONS FROM UT
C PP USES PP EQUATIONS FROM CI
C UJ AND RM USE WJ (WESTERN JUNIPER) EQUATIONS FROM UT
C BI USES BM (BIGLEAF MAPLE) EQUATIONS FROM SO
C MM USES MM EQUATIONS FROM IE
C NC AND OH USE NC (NARROWLEAF COTTONWOOD) EQUATIONS FROM CR
C MC USES MC (CURL-LEAF MTN-MAHOGANY) EQUATIONS FROM SO
C OS USES OT (OTHER SP.) EQUATIONS FROM TT
C
C----------
C  COEFFICIENTS FOR CUBIC FOOT VOLUME FOR TREES THAT ARE SMALLER THAN
C  THE TRANSITION SIZE
C----------
      DATA CFVEQS/
     &      0.0,      0.0,     0.0,     0.0,     0.0,     0.0,    0.0,
     &      0.0,      0.0,     0.0,     0.0,     0.0,     0.0,    0.0,
     &      0.0,      0.0,     0.0,     0.0,     0.0,     0.0,    0.0,
     &      0.0,      0.0,     0.0,     0.0,     0.0,     0.0,    0.0,
     &      0.0,      0.0,     0.0,     0.0,     0.0,     0.0,    0.0,
     &   -0.343,      0.0,     0.0, 0.00224,     0.0,     0.0,    0.0,
     &      0.0,      0.0,     0.0,     0.0,     0.0,     0.0,    0.0,
     &      0.0,      0.0,     0.0,     0.0,     0.0,     0.0,    0.0,
     &      0.0,      0.0,     0.0,     0.0,     0.0,     0.0,    0.0,
     & 0.030288,      0.0,     0.0,0.002213,     0.0,     0.0,    0.0,
     &      0.0,      0.0,     0.0,     0.0,     0.0,     0.0,    0.0,
     &      0.0,      0.0,     0.0,     0.0,     0.0,     0.0,    0.0,
     &      0.0,      0.0,     0.0,     0.0,     0.0,     0.0,    0.0,
     &      0.0,      0.0,     0.0,     0.0,     0.0,     0.0,    0.0,
     &      0.0,      0.0,     0.0,     0.0,     0.0,     0.0,    0.0,
     &      0.0,      0.0,     0.0,     0.0,     0.0,     0.0,    0.0,
     &      0.0,      0.0,     0.0,     0.0,     0.0,     0.0,    0.0,
     &      0.0,      0.0,     0.0,     0.0,     0.0,     0.0,    0.0/
C----------
C  COEFFICIENTS FOR CUBIC FOOT VOLUME FOR TREES THAT ARE LARGER THAN
C  THE TRANSITION SIZE
C----------
      DATA CFVEQL/
     &      0.0,      0.0,     0.0,     0.0,0.002782,  1.9041, 1.0488,
     &      0.0,      0.0,     0.0,     0.0,0.002782,  1.9041, 1.0488,
     &      0.0,      0.0,0.003865,0.001714,     0.0,     0.0,    0.0,
     &      0.0,      0.0,     0.0,     0.0,0.002782,  1.9041, 1.0488,
     &      0.0,      0.0,0.003865,0.001714,     0.0,     0.0,    0.0,
     &    1.071,      0.0,     0.0, 0.00217,     0.0,     0.0,    0.0,
     &      0.0,      0.0,     0.0,     0.0,0.002782,  1.9041, 1.0488,
     &      0.0,      0.0,0.003865,0.001714,     0.0,     0.0,    0.0,
     &      0.0,      0.0,0.003865,0.001714,     0.0,     0.0,    0.0,
     &-1.557103,      0.0,     0.0,0.002474,     0.0,     0.0,    0.0,
     &      0.0,      0.0,     0.0,     0.0,0.002782,  1.9041, 1.0488,
     &      0.0,      0.0,     0.0,     0.0,0.002782,  1.9041, 1.0488,
     &      0.0,      0.0,     0.0, 0.00219,     0.0,     0.0,    0.0,
     &      0.0,      0.0,     0.0, 0.00219,     0.0,     0.0,    0.0,
     &      0.0,      0.0,     0.0,     0.0,     0.0,     0.0,    0.0,
     &      0.0,      0.0,     0.0, 0.00219,     0.0,     0.0,    0.0,
     &      0.0,      0.0,     0.0,     0.0,0.002782,  1.9041, 1.0488,
     &      0.0,      0.0,     0.0,     0.0,     0.0,     0.0,    0.0/
C----------
C  FLAG IDENTIFYING THE SIZE TRANSITION VARIABLE; 0=D, 1=D2H
C----------
      DATA ICTRAN/
     & 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 
     & 0, 0, 0, 0, 0, 0, 0, 0/
C----------
C  TRANSITION SIZE.  TREES OF LARGER SIZE (D OR D2H) WILL COEFFICIENTS 
C  FOR LARGER SIZE TREES.
C---------- 
      DATA CTRAN/
     &      0.0,      0.0,     0.0,     0.0,     0.0,
     &     20.0,      0.0,     0.0,     0.0,  6000.0,
     &      0.0,      0.0,     0.0,     0.0,     0.0, 
     &      0.0,      0.0,     0.0/
C----------
C  COEFFICIENTS FOR BOARD FOOT VOLUME FOR TREES THAT ARE SMALLER THAN 
C  THE TRANSITION SIZE
C----------
      DATA BFVEQS/
     &   -8.085,      0.0,     0.0, 0.01208,     0.0,     0.0,    0.0,
     &   -8.085,      0.0,     0.0, 0.01208,     0.0,     0.0,    0.0,
     &  -25.332,      0.0,     0.0, 0.01003,     0.0,     0.0,    0.0,
     &  -11.403,      0.0,     0.0, 0.01011,     0.0,     0.0,    0.0,
     &  -11.851,      0.0,     0.0, 0.01149,     0.0,     0.0,    0.0,
     &  -18.544,      0.0,     0.0, 0.01197,     0.0,     0.0,    0.0,
     &   -6.600,      0.0,     0.0, 0.01161,     0.0,     0.0,    0.0,
     &  -11.851,      0.0,     0.0, 0.01149,     0.0,     0.0,    0.0,
     &  -11.403,      0.0,     0.0, 0.01011,     0.0,     0.0,    0.0,
     &  -50.340,      0.0,     0.0, 0.01201,     0.0,     0.0,    0.0,
     &  -11.403,      0.0,     0.0, 0.01011,     0.0,     0.0,    0.0,
     &  -11.403,      0.0,     0.0, 0.01011,     0.0,     0.0,    0.0,
     &  -37.314,      0.0,     0.0, 0.01203,     0.0,     0.0,    0.0,
     &  -37.314,      0.0,     0.0, 0.01203,     0.0,     0.0,    0.0,
     &      0.0,      0.0,     0.0,     0.0,     0.0,     0.0,    0.0,
     &  -37.314,      0.0,     0.0, 0.01203,     0.0,     0.0,    0.0,
     &  -11.403,      0.0,     0.0, 0.01011,     0.0,     0.0,    0.0,
     &      0.0,      0.0,     0.0,     0.0,     0.0,     0.0,    0.0/
C----------
C  COEFFICIENTS FOR BOARD FOOT VOLUME FOR TREES THAT ARE LARGER THAN 
C  THE TRANSITION SIZE
C----------
      DATA BFVEQL/
     &   14.111,      0.0,     0.0, 0.01103,     0.0,     0.0,    0.0,
     &   14.111,      0.0,     0.0, 0.01103,     0.0,     0.0,    0.0,
     &   -9.522,      0.0,     0.0, 0.01011,     0.0,     0.0,    0.0,
     &  124.425,      0.0,     0.0, 0.00694,     0.0,     0.0,    0.0,
     &    1.620,      0.0,     0.0, 0.01158,     0.0,     0.0,    0.0,
     &  -21.309,      0.0,     0.0, 0.01216,     0.0,     0.0,    0.0,
     &   -6.600,      0.0,     0.0, 0.01161,     0.0,     0.0,    0.0,
     &    1.620,      0.0,     0.0, 0.01158,     0.0,     0.0,    0.0,
     &  124.425,      0.0,     0.0, 0.00694,     0.0,     0.0,    0.0,
     & -298.784,      0.0,     0.0, 0.01595,     0.0,     0.0,    0.0,
     &  124.425,      0.0,     0.0, 0.00694,     0.0,     0.0,    0.0,
     &  124.425,      0.0,     0.0, 0.00694,     0.0,     0.0,    0.0,
     &  -50.680,      0.0,     0.0, 0.01306,     0.0,     0.0,    0.0,
     &  -50.680,      0.0,     0.0, 0.01306,     0.0,     0.0,    0.0,
     &      0.0,      0.0,     0.0,     0.0,     0.0,     0.0,    0.0,
     &  -50.680,      0.0,     0.0, 0.01306,     0.0,     0.0,    0.0,
     &  124.425,      0.0,     0.0, 0.00694,     0.0,     0.0,    0.0,
     &      0.0,      0.0,     0.0,     0.0,     0.0,     0.0,    0.0/
C----------
C  FLAG IDENTIFYING THE SIZE TRANSITION VARIABLE; 0=D, 1=D2H
C----------
      DATA IBTRAN/
     & 1, 1, 0, 1, 0, 1, 0, 0, 1, 0,
     & 1, 1, 0, 0, 0, 0, 1, 0/
C----------
C  TRANSITION SIZE.  TREES OF LARGER SIZE (D OR D2H) WILL USE COEFFICIENTS 
C  FOR LARGER SIZE TREES.
C---------- 
      DATA BTRAN/
     &  21100.0,  21100.0,     21.0, 42800.0,    21.0,
     &  14600.0,      0.0,     21.0, 42800.0,    20.5,
     &  42800.0,  42800.0,     20.5,    20.5,     0.0, 
     &     20.5,  42800.0,      0.0/
      END
