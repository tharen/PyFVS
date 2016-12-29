      SUBROUTINE HABTYP (KARD2,ARRAY2)
      IMPLICIT NONE
C----------
C  **HABTYP--CI   DATE OF LAST REVISION:  06/20/11
C----------
C
C     TRANSLATES HABITAT TYPE  CODE INTO SUBSCRIPTS:
C        ITYPE = INDEX TO NI ORIGINAL 30 HABITAT TYPES
C        ICINDX = INDEX TO CI TYPE
C     IF KODTYP IS ZERO, THE ROUTINE RETURNS THE DEFAULT CODE.
C----------
COMMONS
C
C
      INCLUDE 'PRGPRM.F77'
C
C
      INCLUDE 'PLOT.F77'
C
C
      INCLUDE 'CONTRL.F77'
C
C
      INCLUDE 'CICOM.F77'
COMMONS
C----------
      LOGICAL LPVCOD,LPVREF,LPVXXX
      CHARACTER*10 KARD2
      INTEGER NIHMAP(130),I
      REAL ARRAY2
C-----------
C  DATA STATEMENTS
C----------
      DATA NIHMAP/ 11*1, 3*2, 3*3, 2*2, 3, 4*4, 5, 6, 3*7, 4*8,
     & 24*9, 10, 2*11, 5*12, 13, 12, 2*13, 6*17, 2*18, 7*19, 7*20,
     & 4*21, 3*22, 3*23, 2*24, 2*25, 2*26, 11*27, 4*28, 7*29, 30/
C----------
C  INITIALIZATIONS
C----------
      LPVREF=.FALSE.
      LPVCOD=.FALSE.
      LPVXXX=.FALSE.
C----------
C  IF REFERENCE CODE IS NON-ZERO THEN MAP PV CODE/REF. CODE TO
C  FVS HABITAT TYPE/ECOCLASS CODE IN THE KODTYP VARIABLE.
C  THEN PROCESS KODTYP CODE (KODTYP IS PASSED BACK TO HABTYP
C  THROUGH COMMON) 
C----------
      IF(CPVREF.NE.'          ') THEN
        ICL5=0
        CALL PVREF4(KARD2,LPVCOD,LPVREF)
        IF((LPVCOD.AND.LPVREF).AND.
     &  (KODTYP.LT.10.OR.KODTYP.GT.999))THEN
          CALL ERRGRO(.TRUE.,34)
          LPVXXX=.TRUE.
        ELSEIF((.NOT.LPVCOD).AND.(.NOT.LPVREF))THEN
          CALL ERRGRO(.TRUE.,33)
          CALL ERRGRO(.TRUE.,32)
          LPVXXX=.TRUE.
        ELSEIF((.NOT.LPVREF).AND.LPVCOD)THEN
          CALL ERRGRO(.TRUE.,32)
          LPVXXX=.TRUE.
        ELSEIF((.NOT.LPVCOD).AND.LPVREF)THEN
          CALL ERRGRO(.TRUE.,33)
          LPVXXX=.TRUE.
        ENDIF
      ENDIF
C
      IF(((KODTYP .LT. 10).OR.(KODTYP .GT. 999)).AND.(.NOT.LPVXXX))THEN
        CALL ERRGRO (.TRUE.,14)
      ELSEIF(.NOT.LPVXXX)THEN
        DO 20 I=1,130
        IF(KODTYP .LT. ICITYP(I)) GO TO 40
   20   CONTINUE
        I=131
   40   CONTINUE
        IF(I .EQ. 1) THEN
          ICINDX=1
          ITYPE=1
        ELSE
          ICINDX=I-1
          ITYPE=NIHMAP(I-1)
        ENDIF
      ENDIF
      KODTYP=ICITYP(ICINDX)
      IF(LSTART)WRITE(JOSTND,31) KODTYP
   31 FORMAT(/,T12,'HABITAT TYPE MAPPED TO',I4,' FOR THIS PROJECTION.')
C
      RETURN
      END
