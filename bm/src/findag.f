      SUBROUTINE FINDAG(I,ISPC,D1,D2,H,SITAGE,SITHT,AGMAX1,HTMAX1,
     &                  HTMAX2,DEBUG)
      use contrl_mod
      use plot_mod
      use arrays_mod
      use prgprm_mod
      implicit none
C----------
C  **FINDAG--BM  DATE OF LAST REVISION:  01/13/11
C----------
C  THIS ROUTINE FINDS EFFECTIVE TREE AGE BASED ON INPUT VARIABLE(S)
C  CALLED FROM ***COMCUP
C  CALLED FROM ***CRATET
C  CALLED FROM ***HTGF
C  CALLED FROM ***REGENT
C
C  CALLS ***HTCALC
C----------
C  COMMONS
C
C----------
C  DECLARATIONS
C----------
      LOGICAL DEBUG
      INTEGER I,ISPC
      REAL AGMAX(MAXSP),AHMAX(MAXSP),AG,BHMAX(MAXSP)
      REAL D1,D2,H,HTMAX1,HTMAX2,AGMAX1,SITAGE,SITHT
      REAL DIFF,HGUESS,SINDX,TOLER
C----------
C  DATA STATEMENTS
C----------
      DATA AGMAX/
     & 200., 110., 180., 130., 250., 900., 140., 150., 150., 200.,
     & 400., 400., 200., 200., 100., 100., 200., 100./
      DATA AHMAX/
     &        2.3,     12.86,   -2.86,     21.29,     52.27,       80.,
     &        2.3,       20.,   45.27,     -5.00,       85.,       85.,
     &        50.,      100.,     75.,      125.,     -5.00,      100./
      DATA BHMAX/
     &       2.39,      1.32,    1.54,      1.24,      1.14,        0.,
     &       1.75,       1.1,    1.24,      1.30,        0.,        0.,
     &         0.,        0.,      0.,        0.,      1.30,        0./
C----------
C  INITIALIZATIONS
C----------
      TOLER=2.0
      AGMAX1 = AGMAX(ISPC)
      SINDX = SITEAR(ISPC)
      HTMAX1 = AHMAX(ISPC) + BHMAX(ISPC)*SINDX
      IF(ISPC.EQ.5)HTMAX1 = HTMAX1 * 3.281
C----------
C  CRATET CALLS FINDAG AT THE BEGINING OF THE SIMULATION TO
C  CALCULATE THE AGE OF INCOMMING TREES.  AT THIS POINT ABIRTH(I)=0.
C  THE AGE OF INCOMMING TREES HAVING H>=HMAX IS CALCULATED BY
C  ASSUMEING A GROWTH RATE OF 0.10FT/YEAR FOR THE INTERVAL H-HMAX.
C  TREES REACHING HMAX DURING THE SIMULATION ARE IDENTIFIED IN HTGF.
C----------
      IF(H .GE. HTMAX1) THEN
        SITAGE = AGMAX1 + (H - HTMAX1)/0.10
        SITHT = H
        GO TO 30
      ENDIF
C----------
C  DEAL WITH SPECIES THAT DON'T NEED ITEATION HERE:
C
C  COMPUTE HT GROWTH AND AGE FOR ASPEN. EQN FROM WAYNE SHEPPARD RMRS.
C----------
      IF(ISPC.EQ.15)THEN
        SITAGE = (H*2.54*12.0/26.9825)**(1.0/1.1752)
        SITHT = H
        GO TO 30
C----------
C  WESTERN JUNIPER
C  WHITEBARK PINE
C  LIMBER PINE
C----------
      ELSEIF(ISPC.EQ.6 .OR. ISPC.EQ.11 .OR. ISPC.EQ.12) THEN
        SITAGE = 0.
        SITHT = H
        GO TO 30
      ENDIF
C
      AG=2.0
C----------
C THE FOLLOWING 5 LINES ARE AN RJ FIX 7-28-88
C----------
      IF(ISPC.EQ.10 .OR. ISPC.EQ.17) AG=(98.38*EXP(SINDX*(-0.0422)))+1.0
      IF(AG .LT. 2.0)AG = 2.0
      IF(ISPC.EQ.3)AG=18.0
C
   75 CONTINUE
C
C----------
C  ORIGINAL BM SPECIES
C  SPECIES FROM THE WC VARIANT (PY, YC, CW, OH)
C
C  CALL HTCALC TO CALCULATE POTENTIAL HT GROWTH
C----------
      CALL HTCALC(SINDX,ISPC,AG,HGUESS,JOSTND,DEBUG)
C
      IF(DEBUG)WRITE(JOSTND,91200)I,ISPC,AG,HGUESS,H
91200 FORMAT(' IN GUESS AN AGE--I,ISPC,AGE,HGUESS,H ',2I5,3F10.2)
C
      IF(HGUESS .LE. 0.0) THEN
        AG = AG + 2.0
        GO TO 75
      ENDIF
      DIFF=ABS(HGUESS-H)
      IF(DIFF .LE. TOLER .OR. H .LT. HGUESS)THEN
        SITAGE = AG
        SITHT = HGUESS
        GO TO 30
      END IF
      AG = AG + 2.
C
      IF(AG .GT. AGMAX1) THEN
C----------
C  H IS TOO GREAT AND MAX AGE IS EXCEEDED
C----------
        SITAGE = AGMAX1
        SITHT = H
        GO TO 30
      ELSE
        GO TO 75
      ENDIF
C
   30 CONTINUE
C
      IF(DEBUG)WRITE(JOSTND,50)I,SITAGE,SITHT
   50 FORMAT(' LEAVING SUBROUTINE FINDAG  I,SITAGE,SITHT =',
     &I5,2F10.3)
C
      RETURN
      END
