      SUBROUTINE SMDGF(ISPC,H,CR,RD,SDIAM)
      IMPLICIT NONE
C----------
C TT $Id$
C----------
C  THIS SUBROUTINE COMPUTES THE DIAMETER FOR SMALL TREES.  PREDICTIONS
C  ARE BASED ON SPECIES, HEIGHT, CROWN RATIO, AND POINT CCF;
C  CALLED FROM REGENT.
C----------
C
COMMONS
C
C
      INCLUDE 'PRGPRM.F77'
C
C
COMMONS
C----------
      INTEGER ISPC
      REAL SDHTCR(MAXSP),SDHPCF(MAXSP),SDCR(MAXSP),SDHL4(MAXSP)
      REAL SDIAM,RD,CR,H,HLESS4,DLESS3
c----------
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
C----------
C  SDHTCR COEFFICIENTS WERE SCALED TO MAINTAIN CONSISTENCY WITH 
C  **SMHTGF**;  CR IS SCALED 0-100 (WRW--3/26/91).
C----------
      DATA SDHTCR /
     &  0.000231,  0.000231,  -0.28654,        0.,   0.04125,
     &  -0.41227,  -0.41227,   0.04125,  -0.15906,        0.,
     &        0.,        0.,        0.,        0.,        0.,
     &        0.,  0.000231,        0./
      DATA SDHPCF /
     &  -0.00005,  -0.00005,   0.13469,        0.,   0.17486,
     &   0.16944,   0.16944,   0.17486,   0.15323,        0.,
     &        0.,        0.,        0.,        0.,        0.,
     &        0.,  -0.00005,        0./
C----------
C  CROWN RATIO COEFFICIENTS WERE SCALED TO MAINTAIN CONSISTENCY
C  WITH **SMHTGF**; CR IS SCALED 0-100 (WRW--3/26/91).
C----------
      DATA SDCR /
     &  0.001711,  0.001711,  0.002736,        0., -0.002371,
     &  0.003191,  0.003191, -0.002371,       0.0,        0.,
     &        0.,        0.,        0.,        0.,        0.,
     &        0.,  0.001711,        0./
      DATA SDHL4 /
     &   0.17023,   0.17023,   0.00036,        0.,  -0.00070,
     &  -0.00220,  -0.00220,  -0.00070,       0.0,        0.,
     &        0.,        0.,        0.,        0.,        0.,
     &        0.,   0.17023,        0./
C
      SELECT CASE (ISPC)
C
      CASE(3,5:9)
C----------
C FOR DF,BS,AS,LP,ES,AF USE ALTERNATE DIAMETER MODEL.
C SDHTCR USED FOR CONSTANT COEFFICIENT
C SDHPCF USED FOR HEIGHT COEFFICIENT
C SDCR USED FOR CROWN RATIO COEFFICIENT
C SDHL4 USED FOR RELATIVE DENSITY COEFFICIENT (I.E. PCCF)
C----------
        SDIAM = SDHTCR(ISPC) + SDHPCF(ISPC)*H + SDCR(ISPC)*CR
     &  + SDHL4(ISPC)*RD
C----------
C ALL OTHER ORIGINAL TT VARIANT SPECIES (WB, LM, OS)
C----------
      CASE DEFAULT
        HLESS4 = H - 4.5
        DLESS3 = SDHTCR(ISPC)*HLESS4*CR
     &   + SDHPCF(ISPC) * HLESS4 * RD
     &   + SDCR(ISPC) * CR
     &   + SDHL4(ISPC) * HLESS4
        SDIAM = DLESS3 + 0.3
C
      END SELECT
C
      RETURN
      END
