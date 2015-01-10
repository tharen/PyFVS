module coeffs_mod
    use prgprm_mod, only : maxsp
    implicit none
    !CODE SEGMENT COEFFS
    !----------
    !  **COEFFS DATE OF LAST REVISION:   05/18/04
    !----------
          INTEGER IORDER(MAXSP)
          REAL    COR(MAXSP),COR2(MAXSP),CRCON(MAXSP),DGCON(MAXSP), &
                  DGCCF(MAXSP),DGDSQ(MAXSP),HDGCOF,H2COF,HTCON(MAXSP), &
                  AHAT,BHAT,RHCON(MAXSP),BKRAT(MAXSP),SIGMA(MAXSP), &
                  FU(MAXSP),FL(MAXSP),FM(MAXSP),WCI(MAXSP),BFLA0(MAXSP), &
                  BFLA1(MAXSP),CFLA0(MAXSP),CFLA1(MAXSP),HT1(MAXSP), &
                  HT2(MAXSP),SIGMAR(MAXSP),DIFH(MAXSP),BFDEFT(9,MAXSP), &
                  CFDEFT(9,MAXSP),ATTEN(MAXSP),BJTHET,BJPHI,BJRHO(40), &
                  VARDG(MAXSP),SMCON(MAXSP)
          COMMON /COEFFS/ COR,COR2,CRCON,DGCON,DGCCF,DGDSQ,HDGCOF,H2COF, &
                          HTCON,AHAT,BHAT,RHCON,IORDER,BKRAT,SIGMA,FU,FL, &
                          FM,WCI,BFLA0,BFLA1,CFLA0,CFLA1,HT1,HT2,SIGMAR, &
                          DIFH,BFDEFT,CFDEFT,ATTEN,BJTHET,BJPHI,BJRHO, &
                          VARDG,SMCON
    !----------
    !  DEFINITIONS OF VARIABLES IN 'COEFFS' COMMON BLOCK:
    !  (THIS COMMON IS FOR MODEL COEFFICIENTS WHICH ARE HABITAT
    !  AND SITE DEPENDENT.  RDH AND RDSC ARE LOADED IN **BLOCK DATA**;
    !  COR, COR2, AND CORTEM ARE LOADED BY INPUT AND CALIBRATION;
    !  IORDER IS LOADED IN **INITRE**; OTHER VARIABLES ARE LOADED IN
    !  **RCON**.  ARRAYS ARE SUBSCRIPTED BY SPECIES.)
    !----------
    !
    !      AHAT -- AN ESTIMATE OF THE BEHRE TAPER CURVE PARAMETER 'A'.
    !     ATTEN -- NUMBER OF OBSERVATIONS ON WHICH THE DIAMETER
    !              INCREMENT MODEL FOR EACH SPECIES IS BASED.  THIS
    !              IS USED TO WEIGHT THE CALCULATED CORRECTION TERM.
    !     BFLA0 -- CONSTANT TERMS FOR BOARD FOOT FORM-DEFECT CORRECTION
    !              EQUATIONS.
    !     BFLA1 -- COEFFICIENTS FOR LINEAR TERMS IN BOARD FOOT FORM-
    !              DEFECT CORRECTION EQUATIONS.
    !      BHAT -- AN ESTIMATE OF THE BEHRE TAPER CURVE PARAMETER 'B'.
    !    BJTHET -- AUTO-REGRESSIVE PARM IN BOX-JENKINS ARMA(1,1) MODEL.
    !    BJPHI  -- MOVING-AVERAGE PARAMETER IN BOX-JENKINS ARMA MODEL.
    !    BJRHO  -- SERIAL CORRELATION AT LAG=J YEARS OF ARMA MODEL WITH
    !              PARAMETERS BJTHET AND BJPHI.
    !     CFDA0 -- CONSTANT TERMS FOR CUBIC FOOT FORM-DEFECT CORRECTION
    !              EQUATIONS.
    !     CFLA0 -- COEFFICIENTS FOR LINEAR TERMS IN CUBIC FOOT FORM-
    !              DEFECT CORRECTION EQUATIONS.
    !     CFLA1 -- COEFFICIENTS FOR QUADRATIC TERMS IN CUBIC FOOT FORM-
    !              DEFECT CORRECTION EQUATIONS.
    !       COR -- DIAMETER GROWTH MODEL CORRECTION TERMS.  COMPUTED IN
    !              **DGDRIV**.   STORED IN ADDITIVE FORM.
    !      COR2 -- USED TO STORE VALUES OF CORRECTION TERMS ON INPUT.
    !              STORED IN MULTIPLICATIVE FORM.  READ BY KEYWORD
    !              CONTROL.
    !     CRCON -- CONSTANT TERMS FOR CROWN RATIO MODELS.  COMBINE
    !              HABITAT TYPE, SLOPE, ASPECT, AND ELEVATION EFFECTS.
    !     DIFH  -- ATTUNATION VECTOR FOR REGENT HEIGHT GROWTH MODEL.
    !     DGCON -- CONSTANT TERMS FOR DIAMETER GROWTH MODELS. COMBINE
    !              HABITAT TYPE, SLOPE, ASPECT, AND ELEVATION EFFECTS.
    !    HDGCOF -- COEFFICIENTS FOR DIAMETER GROWTH TERMS IN HEIGHT
    !              INCREMENT MODELS.  DEPENDENT ON HABITAT TYPE.
    !     H2COF -- COEFFICIENT FOR HEIGHT SQUARED TERM IN HEIGHT
    !              INCREMENT MODEL.  DEPENDENT ON HABITAT TYPE.
    !     HTCON -- CONSTANT TERMS IN HEIGHT GROWTH MODEL.  DEPENDENT
    !              ON HABITAT TYPE.
    !       RDH -- COEFFICIENTS FOR HEIGHT TERMS IN SMALL TREE HEIGHT-
    !              DIAMETER EQUATIONS.
    !      RDSC -- CONSTANT TERMS IN SMALL TREE HEIGHT-DIAMETER EQUATIONS.
    !     RHCON -- CONSTANT TERMS FOR SMALL TREE HEIGHT GROWTH EQUATIONS.
    !              COMBINE HABITAT TYPE, SLOPE, ASPECT, AND ELEVATION
    !              EFFECTS.
    !    IORDER -- THINNING SELECTION PRIORITIES.  DEFAULT VALUES ARE
    !              HABITAT DEPENDENT, BUT CAN BE REPLACED BY KEYWORD
    !              CONTROL.  DEFINED IN **INITRE**
    !     BKRAT -- RATIO OF DBH INSIDE BARK TO BAH OUTSIDE BARK BY
    !              SPECIES. INITIALIZED IN BLKDATA.
    !     SIGMA -- POOLED VARIANCE ESTIMATE FOR THE DIAMETER GROWTH
    !              MODEL.  COMPUTED IN DGDRIV DURING CALIBRATION.
    !        FU -- MULTIPLIER FOR ACCELERATED DIAMETER GROWTH ESTIMATE
    !              WHEN RECORD TRIPLING.  COMPUTED BY SPECIES IN DGDRIV
    !              DURING CALIBRATION.
    !        FL -- MULTIPLIER FOR DECELERATED DIAMETER GROWTH ESTIMATE
    !              WHEN RECORD TRIPLING.  COMPUTED BY SPECIES IN DGDRIV
    !              DURING RECORD CALIBRATION.
    !        FM -- MULTIPLIER MOR MIDDLE DIAMETER GROWTH ESTIMATE WHEN
    !              RECORD TRIPLING.  COMPUTED BY SPECIES IN DGDRIV DURING
    !              CALIBRATION.
    !       WCI -- USED INITIALLY TO STORE BAYESIAN GROWTH SAMPLE TREE
    !              WEIGHTS FOR DIAMETER GROWTH MODEL CALIBRATION.  THERE-
    !              AFTER STORED ASYMPTOTE FOR CORRECTION TERM ATTENTUATION.
    !              COMPUTED BY SPECIES IN FIRST CYCLE IN DGDRIV.
    !
    !     CCF COEFFICIENTS ARE LOADED IN RCON.
    !     CCF COEFFICIENTS FOR TREES THAT ARE GREATER THAN 10.0 IN. DBH:
    !
    !      RD1 -- CONSTANT TERM IN CROWN COMPETITION FACTOR EQUATION,
    !             SUBSCRIPTED BY SPECIES
    !
    !      RD2 -- COEFFICIENT FOR SUM OF DIAMETERS TERM IN CROWN
    !             COMPETITION FACTOR EQUATION,SUBSCRIPTED BY
    !             SPECIES
    !
    !      RD3 -- COEFFICIENT FOR SUM OF DIAMETER SQUARED TERM IN
    !             CROWN COMPETITION EQUATION, SUBSCRIPTED BY SPECIES
    !
    !     CCF COEFFICIENTS FOR TREES THAT ARE LESS THAN 10.0 IN. DBH:
    !
    !      RDA -- MULTIPLIER.
    !
    !      RDB -- EXPONENT.  CCF(I) = EXP( RDA*DBH**RDB )
    !
    !
    !       HT1 -- DEFAULT COEFICIENTS FOR HEIGHT DIAMETER RELATIONSHIP
    !       HT2    SUBSCRIPTED BY SPECIES. THESE COEFFICIENTS ARE USED IN
    !              CRATET FOR CALIBRATING HEIGHT-DIAMETER EQUATIONS.
    !    SIGMAR -- REGRESSION ESTIMATES OF THE STANDARD ERRORS FOR THE
    !              IMBEDDED MODELS. LOADED IN DGCONS AND USED IN DGDRIV.
    !    CFDEFT -- ARRAY HOLDING THE PERCENTAGE OF CUBIC VOLUME DEFECT FOR
    !              SPECIFIED DIAMETER CLASSES. DEFAULTS CAN BE KEYWORD
    !              MODIFIED.
    !    BFDEFT -- ARRAY HOLDING THE PERCENTAGE OF BOARD FOOT VOLUME DEFECT
    !              FOR SPECIFIED DIAMETER CLASSES. KEYWORD MODIFIED.
    !     SMCON -- CONSTANT TERMS FOR DIAMETER GROWTH MODELS FOR TREES < 10.
    !              COMBINES HABITAT TYPE, SLOPE, ASPECT, AND ELEVATION EFFECTS.
    !-----END SEGMENT
end module coeffs_mod
