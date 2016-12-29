      SUBROUTINE MPOTPR (PROTBK)
      IMPLICIT NONE
C----------
C  **MPOTPR         DATE OF LAST REVISION:  06/14/13
C----------
C
C     PROTBK = THE PROBABILITY OF A MPB OUTBREAK.
C
C     ***** WARNING:  DO NOT CALL THIS ROUTINE UNLESS THERE
C                     IS LP IN THE TREE LIST (SEE MPBGO).
C
C Revision History
C   11/07/00 Lance David (FHTET)
C     Changed array reference RELDSP(7) to RELDSP(IDXLP) for relative
C     density of Lodgepole Pine.
C   11/09/00 Lance David (FHTET)
C     Added variable descriptions and code comments.
C     Condition requiring stand age to be at least 40 was commented out 
C     sometime in the past and I deleted it.
C   07/02/10 Lance R. David (FMSC)
C     Added IMPLICIT NONE.
C----------
C-----------
COMMONS
C
C
      INCLUDE 'PRGPRM.F77'
C
C
      INCLUDE 'CONTRL.F77'
C
C
      INCLUDE 'PLOT.F77'
C
C
      INCLUDE 'MPBCOM.F77'
C
C
COMMONS
C
C     VARIABLE DESCRIPTIONS:
C     ISTDT  - START DATE SPECIFIED IN RANSTART KEYWORD (DEFAULT = 1).
C     ICYC   - FVS CYCLE NUMBER
C     IY     - CALENDAY YEAR
C     A45DBH - AVERAGE DBH OF ALL LODGEPOLE 4.5 INCHES AND LARGER.
C     IDXLP  - ARRAY INDEX NUMBER FOR LODGEPOLE PINE.
C     RELDSP - FVS RELATIVE DENSITY OF SPECIES INDEXED BY SPECIES NUMBER.
C     RELDEN - FVS RELATIVE DENSITY OF STAND, ALL SPECIES INCLUDED.
C     CNTLP  - SUM OF LODGEPOLE PINE TPA (PROB(I)) FOR STAND
C     PBALPP - PROPORTION OF BASAL AREA THAT IS LODGEPOLE PINE.
C
C     CALCULATE THE PROBABILITY OF AN OUTBREAK OCCURING
C
C     PROBABILITY IS EQUAL TO ZERO IF ANY OF THE MINIMUM CONDITIONS
C     ARE NOT MET.
C

      REAL PROTBK, X
      
      PROTBK = 0.0

C.... AT OR BEYOND MPB START CYCLE OR DATE
      IF ((ISTDT .LE. 40 .AND. ICYC .LT. ISTDT) .OR.
     &    (ISTDT .GT. 40 .AND. IY(ICYC) .LT. ISTDT)) GOTO 10

C.... AVERAGE DBH OF 4.5+ LP MUST BE AT LEAST 6.0 INCHES, INDICATING
C.... THAT THERE ARE ADEQUATE LARGER LP IN THE LP POPULATION. 
      IF ( A45DBH  .LT.  6.0 ) GO TO 10

C.... 25+% OF THE STAND BASAL AREA IS LP
      IF ( PBALPP .LT. 0.25 ) GO TO 10

C.... 20+% OF THE TREES IN THE STAND ARE LP
      IF ( RELDSP(IDXLP) / RELDEN .LT. 0.20 ) GO TO 10

C.... POPULATION OF LP IS AT LEAST 40 TREES PER ACRE.
      IF ( CNTLP .LT. 40.0 ) GO TO 10

C.... END OF MINIMUM CONDITIONS

C.... PROPORTION OF BASAL AREA IN LODGEPOLE IS LIMITED TO 0.8 
C.... FOR THE PROBABILITY FUNCTION.
      X = PBALPP
      IF ( X .GT. 0.8 ) X = 0.8
C
      PROTBK = 1. / ( 1. + EXP( 9.583
     >       - 0.08967 * ( X *RELDEN)
     >            ))
C
C
   10 CONTINUE
      IF ( .NOT. DEBUIN ) GO TO 50
      WRITE (JOMPB,40) CNTLP,TLP45,A45DBH,BA,BALPP,PBALPP,
     >                  RELDSP(IDXLP),RELDEN,PROTBK
   40 FORMAT (/,'ALL LPP/ACRE= ',E13.3,'; >4.5 LPP/ACRE= ',E13.3,
     >        '; AVE >4.5 DBH= ',E13.3,'; TOT BA= ',E13.3,//,
     >        'BALPP= ',E13.3,'; PBALPP= ',E13.3,'; LPP-CCF= ',E13.3,
     >        '; TOT CCF= ',E13.3,'; PROB OUTBREAK= ',E13.3)
   50 CONTINUE
      RETURN
      END
