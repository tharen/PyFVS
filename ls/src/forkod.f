      SUBROUTINE FORKOD
      use contrl_mod
      use plot_mod
      use prgprm_mod
      implicit none
C----------
C  **FORKOD DATE OF LAST REVISION:  07/11/08
C----------
C
C     TRANSLATES FOREST CODE INTO A SUBSCRIPT, IFOR, AND IF
C     KODFOR IS ZERO, THE ROUTINE RETURNS THE DEFAULT CODE.
C
C----------
C  NATIONAL FORESTS:
C  902 = CHEQUAMEGON
C  903 = CHIPPEWA
C  904 = HURON-MANISTEE
C  906 = NICOLET
C  907 = OTTAWA
C  909 = SUPERIOR
C  910 = HIAWATHA
C  913 = CHEQUAMEGON-NICOLET
C  924 = MANISTEE (MAP TO HURON-MANISTEE)
C----------
      INTEGER JFOR(9),KFOR(9),NUMFOR,I
      DATA JFOR/902,903,904,906,907,909,910,913,924/
      DATA NUMFOR/9/
      DATA KFOR/9*1/
C
      IF (KODFOR .EQ. 0) GOTO 30
      DO 10 I=1,NUMFOR
      IF (KODFOR .EQ. JFOR(I)) GOTO 20
   10 CONTINUE
      CALL ERRGRO (.TRUE.,3)
      WRITE(JOSTND,11) JFOR(IFOR)
   11 FORMAT(T12,'FOREST CODE USED FOR THIS PROJECTION IS',I4)
      GOTO 30
   20 CONTINUE
      IF(I .EQ. 9)THEN
        WRITE(JOSTND,21)
   21   FORMAT(T12,'MANISTEE NF (924) BEING MAPPED TO HURON-',
     &  'MANISTEE (904) FOR FURTHER PROCESSING.')
        I=3
      ENDIF
      IFOR=I
      IGL=KFOR(I)
   30 CONTINUE
      KODFOR=JFOR(IFOR)
C----------
C  SET DEFAULT TLAT, TLONG, AND ELEVATION VALUES, BY FOREST
C----------
      SELECT CASE(KODFOR)
      CASE(902,906,913)
        IF(TLAT.EQ.0) TLAT=45.93
        IF(TLONG.EQ.0)TLONG=90.44
        IF(ELEV.EQ.0) ELEV=15.
      CASE(903)
        IF(TLAT.EQ.0) TLAT=47.38
        IF(TLONG.EQ.0)TLONG=94.60
        IF(ELEV.EQ.0) ELEV=13.
      CASE(904)
        IF(TLAT.EQ.0) TLAT=44.25
        IF(TLONG.EQ.0)TLONG=85.40
        IF(ELEV.EQ.0) ELEV=9.
      CASE(907)
        IF(TLAT.EQ.0) TLAT=46.45
        IF(TLONG.EQ.0)TLONG=90.17
        IF(ELEV.EQ.0) ELEV=14.
      CASE(909)
        IF(TLAT.EQ.0) TLAT=46.78
        IF(TLONG.EQ.0)TLONG=92.11
        IF(ELEV.EQ.0) ELEV=16.
      CASE(910)
        IF(TLAT.EQ.0) TLAT=45.75
        IF(TLONG.EQ.0)TLONG=87.06
        IF(ELEV.EQ.0) ELEV=8.
      END SELECT
      RETURN
      END
