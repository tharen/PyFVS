      SUBROUTINE CVNOHD
      use outcom_mod
      use contrl_mod
      use plot_mod
      use arrays_mod
      use prgprm_mod
      implicit none
C----------
C  **CVNOHD--COVR   DATE OF LAST REVISION:  06/28/13
C----------
C     WRITES THE COVER STATISTICS TO A FLAT FILE (NOHEADINGS).
C---------
COMMONS
      INCLUDE 'CVCOM.F77'
C
      INTEGER I,ITHN,ISUM1,ISUM2,ISUM31,ISUM32,ISUM33,ISUM3,ISUM4,ISUM7
      INTEGER ISUM8,ISUM9,ISUM10,ISUM11,ISUM12,ISUM13,ISUM14,ISUM15
      CHARACTER*11 CISN
C
C     RETURN IF NO COVER STATISTICS WERE COMPUTED.
C
      IF (.NOT.LCOV) RETURN
C
C     RETURN IF THE NOHEADING VERSION OF THE COVER STATS IS NOT WANTED.
C
      IF (.NOT.LCVNOH) RETURN
C
C     CALL THE PPE AND GET THE INTERNAL STAND NUMBER (RETURNED AS A
C     STRING OF ZEROS IF THE PPE IS NOT PRESENT IN THE SYSTEM).
C
      CALL PPISN (CISN)
C
C     LOOP OVER ALL CYCLES PLUS ONE.
C
      DO 100 I=1,NCYC+1
C
C     IF THE COUNTER IS LESS THAT THE COVER BEGINNING CYCLE, SKIP
C     THE OUTPUT.
C
      IF (I.LT.ICVBGN) GO TO 100
C
C     LOOP OVER THIN AND NON-THIN.
C
      DO 90  ITHN=1,2
C
C     IF ON THE THIN LOOP AND NO THINNING OCCURRED, SKIP THE OUTPUT.
C
      IF (ITHN.EQ.2 .AND. .NOT.LTHIND(I)) GOTO 90
C
C     PREPARE STATISTICS FOR OUTPUT (CODE IS TAKEN FROM CVOUT).
C
      ISUM1  = IFIX (.5+TIMESD(I,ITHN))
      ISUM2  = IFIX (.5+PGT0(I,ITHN)*100.)
      ISUM31 = IFIX (.5+CLOW(I,ITHN))
      ISUM32 = IFIX (.5+CMED(I,ITHN))
      ISUM33 = IFIX (.5+CTALL(I,ITHN))
      ISUM3  = IFIX (.5+TOTLCV(I,ITHN))
      ISUM4  = IFIX (.5+SBMASS(I,ITHN))
      ISUM7  = ICVAGE(I)
      ISUM8  = IFIX (.5+STDHT(I,ITHN))
      ISUM9  = IFIX (.5+TPCTCV(I,ITHN))
      ISUM10 = IFIX (.5+TOTBMS(I,ITHN))
      ISUM11 = IFIX (.5+SDIAM(I,ITHN))
      ISUM12 = IFIX (.5+TRETOT(I,ITHN))
      ISUM13 = IFIX (.5+PROXHT(I,ITHN,1))
      ISUM14 = IFIX (.5+PROXHT(I,ITHN,2))
      ISUM15 = IFIX (.5+TPROAR(I,ITHN))
C
C     WHEN THE DATE OF DISTURBANCE IS GREATER THAN 40 YEARS, DO NOT
C     WRITE THE SHRUB DATA, ONLY THE TREE STATISTICS
C
      IF (TIMESD(I,ITHN) .LE. 40.) THEN
         WRITE (JCVNOH,10) NPLT,MGMID,IY(I),ITHN,ISUM1,
     >         ISUM2,ISUM31,ISUM32,ISUM33,ISUM3,ASHT(I,ITHN),
     >         ISUM4,TWIGS(I,ITHN),ISTAGE(I,ITHN),ISUM7,ISUM8,
     >         ISUM9,ISUM10,ISUM11,ISUM12,ISUM13,ISUM14,ISUM15,CISN
   10    FORMAT (A26,1X,A4,I5,I2,I3,
     >          5I3,F5.1,I6,F5.1,I3,
     >          3I5,I6,I5,I6,3I6,1X,A11)
      ELSE
         WRITE (JCVNOH,20) NPLT,MGMID,IY(I),ITHN,ISUM1,ISUM7,ISUM8,
     >          ISUM9,ISUM10,ISUM11,ISUM12,ISUM13,ISUM14,ISUM15,CISN
   20    FORMAT (A26,1X,A4,I5,I2,I3,
     >          34X,3I5,I6,I5,I6,3I6,1X,A11)
      ENDIF
   90 CONTINUE
  100 CONTINUE
      RETURN
      END
