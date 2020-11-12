      SUBROUTINE ESSUBH (I,HHT,EMSQR,DILATE,DELAY,ELEV,IHTSER,GENTIM,
     &  TRAGE)
      IMPLICIT NONE
C----------
C STRP $Id$
C----------
C
COMMONS
C
C
      INCLUDE 'PRGPRM.F77'
C
C
      INCLUDE 'ESPARM.F77'
C
C
      INCLUDE 'ESCOMN.F77'
C
C
COMMONS
C
C     ASSIGNS HEIGHTS TO SUBSEQUENT AND PLANTED TREE RECORDS
C     CREATED BY THE ESTABLISHMENT MODEL.
C
C
      INTEGER IHTSER,I,N,ITIME
      REAL TRAGE,GENTIM,ELEV,DELAY,DILATE,EMSQR,HHT,AGE
C
C
C
C     COMING INTO ESSUBH, TRAGE IS THE AGE OF THE TREE AS SPECIFIED ON 
C     THE PLANT OR NATURAL KEYWORD.  LEAVING ESSUBH, TRAGE IS THE NUMBER 
C     BETWEEN PLANTING (OR NATURAL REGENERATION) AND THE END OF THE 
C     CYCLE.  AGE IS TREE AGE UP TO THE TIME REGENT WILL BEGIN GROWING 
C     THE TREE.
C
      N=DELAY+0.5
      IF(N.LT.-3) N=-3
      DELAY=FLOAT(N)
      ITIME=TIME+0.5
      IF(N.GT.ITIME) DELAY=TIME
      AGE=TIME-DELAY-GENTIM+TRAGE
      IF(AGE.LT.1.0) AGE=1.0
      TRAGE=TIME-DELAY
      GO TO (10,20,30,40,50,60,70,80,90,100,110),I
C
C     HEIGHT OF TALLEST SUBSEQUENT SPECIES 1
C
   10 CONTINUE
      HHT = 1.0
      GO TO 120
C
C     HEIGHT OF TALLEST SUBS. SPECIES 2
C
   20 CONTINUE
      HHT = 1.0
      GO TO 120
C
C     HT OF TALLEST SUBS. SPECIES 3
C
   30 CONTINUE
      HHT = 1.0
      GO TO 120
C
C     HT OF TALLEST SUBS. SPECIES 4
C
   40 CONTINUE
      HHT = 1.0
      GO TO 120
C
C     HT OF TALLEST SUBS. SPECIES 5
C
   50 CONTINUE
      HHT = 1.0
      GO TO 120
C
C     HT OF TALLEST SUBS. SPECIES 6
C
   60 CONTINUE
      HHT = 1.0
      GO TO 120
C
C     HT OF TALLEST SUBS. SPECIES 7
C
   70 CONTINUE
      HHT = 1.0
      GO TO 120
C
C     HT OF TALLEST SUBS. SPECIES 8
C
   80 CONTINUE
      HHT = 1.0
      GO TO 120
C
C     HT OF TALLEST SUBS. SPECIES 9
C
   90 CONTINUE
      HHT = 1.0
      GO TO 120
C
C     HEIGHT OF TALLEST SUBS. SPECIES 10
C
  100 CONTINUE
      HHT = 1.0
      GO TO 120
  110 CONTINUE
C
C     HT OF TALLEST SUBS. SPECIES 11
C
      HHT = 1.0
  120 CONTINUE
      RETURN
      END
