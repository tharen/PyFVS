      SUBROUTINE FORKOD
      use contrl_mod
      use plot_mod
      use prgprm_mod
      implicit none
C----------
C  **FORKOD--NC   DATE OF LAST REVISION:  01/20/15
C----------
C
C     TRANSLATES FOREST CODE INTO A SUBSCRIPT, IFOR, AND IF
C     KODFOR IS ZERO, THE ROUTINE RETURNS THE DEFAULT CODE.
C
C----------
C  NATIONAL FORESTS:
C  505 = KLAMATH
C  510 = SIX RIVERS
C  514 = SHASTA-TRINITY
C  611 = SISKIYOU
C  705 = HOOPA INDIAN RESERVATION
C  800 = SIMPSON TIMBER
C  712 = BLM COOS BAY
C  518 = TRINITY (MAPPED TO SHASTA-TRINITY)
C  507 = LOS PADRES (MAPPED TO SIX RIVERS)
C  508 = MENDOCINO (MAPPED TO SIX RIVERS)
C----------
      INTEGER JFOR(10),KFOR(10),NUMFOR,I
      DATA JFOR/505,510,514,611,705,800,712,518,507,508/, NUMFOR /10/
      DATA KFOR/10*1/
C
      IF (KODFOR .EQ. 0) GOTO 30
      DO 10 I=1,NUMFOR
      IF (KODFOR .EQ. JFOR(I)) GOTO 20
   10 CONTINUE
      CALL ERRGRO (.TRUE.,3)
      WRITE(JOSTND,11) JFOR(IFOR)
   11 FORMAT(T12,'FOREST CODE USED IN THIS PROJECTION IS ',I4)
      GOTO 30
   20 CONTINUE
      IF(I .EQ. 8)THEN
        WRITE(JOSTND,21)
   21   FORMAT(T12,'TRINITY NF (518) BEING MAPPED TO SHASTA-TRINITY ',
     &  '(514) FOR FURTHER PROCESSING.')
        I=3
      ELSEIF(I .EQ. 9)THEN
        WRITE(JOSTND,22)
   22   FORMAT(T12,'LOS PADRES NF (507) BEING MAPPED TO SIX RIVERS ',
     &  '(510) FOR FURTHER PROCESSING.')
        I=2
      ELSEIF(I .EQ. 10)THEN
        WRITE(JOSTND,23)
   23   FORMAT(T12,'MENDOCINO NF (508) BEING MAPPED TO SIX RIVERS ',
     &  '(510) FOR FURTHER PROCESSING.')
        I=2
      ENDIF
      IFOR=I
      IGL=KFOR(I)
   30 CONTINUE
      KODFOR=JFOR(IFOR)
      RETURN
      END
