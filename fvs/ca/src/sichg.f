      SUBROUTINE SICHG(ISISP,SSITE,SIAGE)
      IMPLICIT NONE
C----------
C  **SICHG--CA   DATE OF LAST REVISION:  02/22/08
C----------
COMMONS
C
C
      INCLUDE 'PRGPRM.F77'
C
C
COMMONS
C
C THIS ROUTINE TAKES THE SITE INDEX OF THE SITE SPECIES AND CALCULATES
C AN AGE FOR THAT CURVE, GIVEN THE SITE, WHERE A NEW HT (OR PROXY AGE)
C MAY BE LOOKED UP.
C----------
C   SITE CURVES ARE LOADED (BLKDAT BB_ COEFFS) IN THE FOLLOWING ORDER:
C   (ALL CURVES ARE BASE AGE 50)
C       FIRST   POSITION ARE KING DOUGLAS-FIR; WEYERH FOR PAP 8
C       SECOND  POSITION ARE DOLPH WHITE FIR PSW 185;
C       THIRD   POSITION ARE HANN-SCRIVANI DOUGLAS-FIR
C       FOURTH  POSITION ARE HANN-SCRIVANI PONDEROSA PINE
C       FIFTH   POSITION ARE DOLPH RED FIR PSW 206
C       SIXTH   POSITION ARE DAHMS LODGEPOLE RP-PNW-8
C       SEVENTH POSITION ARE POWERS BLACK OAK RES NOTE PSW-262
C       EIGHTH  POSITION ARE PORTER & WIANT TANOAK JOF 4/95 286-287
C       NINTH   POSITION ARE PORTER & WIANT MADRONE  (DITTO)
C       TENTH   POSITION ARE PORTER & WIANT RED ALDER  (DITTO)
C
C   INDEX = ARRAY HOLDING THE INDEX FOR ACCESSING THE APPROPRIATE SITE
C           CURVE COEFFICIENTS IN THE BB_ ARRAYS.
C----------
      INTEGER DIFF,REFAGE(10)
      CHARACTER*1 ISILOC,REFLOC(10)
      REAL SIAGE(MAXSP)
      INTEGER INDEX(MAXSP)
      REAL SSITE,AGE2BH
      INTEGER ISISP,INDX,I,JNDX
C----------
C SPECIES ORDER IN CA VARIANT:
C  1=PC  2=IC  3=RC  4=WF  5=RF  6=SH  7=DF  8=WH  9=MH 10=WB
C 11=KP 12=LP 13=CP 14=LM 15=JP 16=SP 17=WP 18=PP 19=MP 20=GP
C 21=JU 22=BR 23=GS 24=PY 25=OS 26=LO 27=CY 28=BL 29=EO 30=WO
C 31=BO 32=VO 33=IO 34=BM 35=BU 36=RA 37=MA 38=GC 39=DG 40=FL
C 41=WN 42=TO 43=SY 44=AS 45=CW 46=WI 47=CN 48=CL 49=OH
C----------
      DATA INDEX/ 3,  3,  3,  3,  5,  5,  3,  3,  5,  6,
     &            6,  6,  6,  6,  4,  3,  4,  4,  4,  4,
     &            6,  3,  3,  7,  3,  9,  9,  9,  9,  7,
     &            7,  7,  9, 10,  7, 10,  9,  9,  7,  7,
     &           10,  8, 10, 10, 10, 10, 10, 10, 10/
C
      DATA REFLOC/'B','B','B','B','B','T','B','T','T','T'/
      DATA REFAGE/ 50, 50, 50, 50, 50, 50, 50, 50, 50, 50/
C----------
C DETERMINE AGE TO BREAST HEIGHT
C----------
      INDX=INDEX(ISISP)
      GO TO (10,20,30,40,50,60,70,80,90,100),INDX
C----------
C KING DF
C----------
   10 CONTINUE
      IF(SSITE .GT. 135.) THEN
        AGE2BH = 6.
      ELSEIF (SSITE.GT.115 .AND. SSITE.LE.135.) THEN
        AGE2BH = 7.
      ELSEIF (SSITE.GT.95 .AND. SSITE.LE.115) THEN
        AGE2BH = 8.
      ELSEIF (SSITE.GT.75 .AND. SSITE.LE.95) THEN
        AGE2BH = 9.
      ELSE
        AGE2BH = 10.
      ENDIF
      GO TO 110
C----------
C DOLPH WHITE FIR
C----------
   20 CONTINUE
      IF(SSITE .GT. 100.) THEN
        AGE2BH = 5.
      ELSEIF (SSITE.GT.60 .AND. SSITE.LE.100.) THEN
        AGE2BH = 7.
      ELSE
        AGE2BH = 10.
      ENDIF
      GO TO 110
C----------
C HANN-SCRIVANI DOUGLAS-FIR
C----------
   30 CONTINUE
      AGE2BH = 16.0 - 0.1*SSITE
      IF(AGE2BH .LT. 1.) AGE2BH = 1.
      GO TO 110
C----------
C HANN-SCRIVANI PONDEROSA PINE
C----------
   40 CONTINUE
      AGE2BH = 10.4 - 0.06*SSITE
      IF(AGE2BH .LT. 1.) AGE2BH = 1.
      GO TO 110
C----------
C DOLPH RED FIR
C----------
   50 CONTINUE
      AGE2BH = 18.
      GO TO 110
C----------
C DAHMS LODGEPOLE PINE
C----------
   60 CONTINUE
      AGE2BH = 10. 
      GO TO 110
C----------
C POWERS BLACK OAK
C----------
   70 CONTINUE
      AGE2BH = 2.
      GO TO 110
C----------
C PORTER & WIANT TANOAK
C----------
   80 CONTINUE
      AGE2BH = 10.820513 - 0.102564*SSITE
      IF(AGE2BH .LT. 1.) AGE2BH = 1.
      IF(AGE2BH .GT. 7.) AGE2BH = 7.
      GO TO 110
C----------
C PORTER & WIANT MADRONE
C----------
   90 CONTINUE
      AGE2BH = 12.309524 - 0.119048*SSITE
      IF(AGE2BH .LT. 1.) AGE2BH = 1.
      IF(AGE2BH .GT. 7.) AGE2BH = 7.
      GO TO 110
C----------
C PORTER & WIANT RED ALDER
C----------
  100 CONTINUE
      AGE2BH = 4.0 - 0.026316*SSITE
      IF(AGE2BH .LT. 1.) AGE2BH = 1.
      IF(AGE2BH .GT. 3.) AGE2BH = 3.
C
  110 CONTINUE
C----------
C ISILOC IS THE PLACE THE AGE FOR THE SITE FROM STDINFO IS TAKEN
C----------
      ISILOC = REFLOC(INDX)
C----------
C  SET UP THE ARRAY TO TELL WHETHER YOU NEED TO SLIDE UP OR DOWN THE SIT
C LINE TO ADJUST FOR TOTAL AGE OR BREAST HIGH AGE
C----------
      DO 120 I=1,MAXSP
      JNDX = INDEX(I)
      IF(ISILOC .EQ. 'T' .AND. REFLOC(JNDX) .EQ. 'B')DIFF=-1
      IF(ISILOC .EQ. REFLOC(JNDX))DIFF=0
      IF(ISILOC .EQ. 'B' .AND. REFLOC(JNDX) .EQ. 'T')DIFF=1
      SIAGE(I) = REFAGE(JNDX) + AGE2BH*DIFF
  120 CONTINUE
      RETURN
      END
