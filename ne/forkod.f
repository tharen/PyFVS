      SUBROUTINE FORKOD
      IMPLICIT NONE
C----------
C NE $Id$
C----------
C
C     TRANSLATES FOREST CODE INTO A SUBSCRIPT, IFOR, AND IF
C     KODFOR IS ZERO, THE ROUTINE RETURNS THE DEFAULT CODE.
C
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
COMMONS
C
C  ------------------------
C  NATIONAL FORESTS:
C  914 = WAYNE
C  922 = WHITE MOUNTAIN
C  919 = ALLEGHENY
C  920 = GREEN MTN - FINGER LAKES
C  921 = MONONGAHELA
C  911 = OLD WAYNE-HOOSIER (MAP TO WAYNE)
C  930 = FINGER LAKES (MAP TO GREEN MTN - FINGER LAKES)
C  ------------------------
C  RESERVATION PSUEDO CODES:
C  8200 = PASSAMAQUODDY RESERVATION    (MAPPED TO 922 WHITE MOUNTAIN)    
C  8201 = PENOBSCOT OFF-RESERVATION TL (MAPPED TO 922 WHITE MOUNTAIN)
C  8202 = HOULTON MALISEET RESERVATION (MAPPED TO 922 WHITE MOUNTAIN)
C  8203 = MASHANTUCKET PEQUOT RES      (MAPPED TO 922 WHITE MOUNTAIN)
C  8204 = PAUCATUCK EASTERN PEQUOT RES (MAPPED TO 922 WHITE MOUNTAIN)
C  8206 = NARRAGANSETT RESERVATION     (MAPPED TO 922 WHITE MOUNTAIN)
C  8208 = WAMPANOAG-AQUINNAH TL        (MAPPED TO 922 WHITE MOUNTAIN)
C  8209 = AROOSTOOK BAND OF MICMAC TL  (MAPPED TO 922 WHITE MOUNTAIN)
C  8211 = MOHEGAN RESERVATION          (MAPPED TO 922 WHITE MOUNTAIN)
C  8214 = CAYUGA NATION TDSA           (MAPPED TO 930 FINGER LAKES)
C  8215 = ONONDAGA NATION RESERVATION  (MAPPED TO 930 FINGER LAKES)
C  8216 = TONAWANDA RESERVATION        (MAPPED TO 930 FINGER LAKES)
C  8217 = TUSCARORA NATION RESERVATION (MAPPED TO 930 FINGER LAKES)
C  8218 = ONEIDA NATION RESERVATION    (MAPPED TO 930 FINGER LAKES)
C  ------------------------

      INTEGER JFOR(7),KFOR(7),NUMFOR,I
      LOGICAL USEIGL, FORFOUND
      DATA JFOR/914,922,919,920,921,911,930/
      DATA NUMFOR/7/
      DATA KFOR/7*1/

      USEIGL = .TRUE.
      FORFOUND = .FALSE.


      SELECT CASE (KODFOR)

C       CROSSWALK FOR RESERVATION PSUEDO CODES & LOCATION CODE
        CASE (8200)
          WRITE(JOSTND,60)
   60     FORMAT(/,'********',T12,'PASSAMAQUODDY RESERVATION (8200) ',
     &    'BEING MAPPED TO WHITE MOUNTAIN NF (922) FOR FURTHER ',
     &    'PROCESSING.')
          IFOR = 2
        CASE (8201)
          WRITE(JOSTND,61)
   61     FORMAT(/,'********',T12,'PENOBSCOT OFF-RESERVATION TL (8201)',
     &    ' BEING MAPPED TO WHITE MOUNTAIN NF (922) FOR FURTHER ',
     &    'PROCESSING.')
          IFOR = 2
        CASE (8202)
          WRITE(JOSTND,62)
   62     FORMAT(/,'********',T12,'HOULTON MALISEET RESERVATION (8202)',
     &    ' BEING MAPPED TO WHITE MOUNTAIN NF (922) FOR FURTHER ',
     &    'PROCESSING.')
          IFOR = 2
        CASE (8203)
          WRITE(JOSTND,63)
   63     FORMAT(/,'********',T12,'MASHANTUCKET PEQUOT RES (8203) ',
     &    'BEING MAPPED TO WHITE MOUNTAIN NF (922) FOR FURTHER ',
     &    'PROCESSING.')
          IFOR = 2
        CASE (8204)
          WRITE(JOSTND,64)
   64     FORMAT(/,'********',T12,'PAUCATUCK EASTERN PEQUOT RES (8204)',
     &    ' BEING MAPPED TO WHITE MOUNTAIN NF (922) FOR FURTHER ',
     &    'PROCESSING.')
          IFOR = 2
        CASE (8206)
          WRITE(JOSTND,65)
   65     FORMAT(/,'********',T12,'NARRAGANSETT RESERVATION (8206) ',
     &    'BEING MAPPED TO WHITE MOUNTAIN NF (922) FOR FURTHER ',
     &    'PROCESSING.')
          IFOR = 2
        CASE (8208)
          WRITE(JOSTND,66)
   66     FORMAT(/,'********',T12,'WAMPANOAG-AQUINNAH TL (8208) BEING ',
     &    'MAPPED TO WHITE MOUNTAIN NF (922) FOR FURTHER PROCESSING.')
          IFOR = 2
        CASE (8209)
          WRITE(JOSTND,67)
   67     FORMAT(/,'********',T12,'AROOSTOOK BAND OF MICMAC TL (8209) ',
     &    'BEING MAPPED TO WHITE MOUNTAIN NF (922) FOR FURTHER ',
     &    'PROCESSING.')
          IFOR = 2
        CASE (8211)
          WRITE(JOSTND,68)
   68     FORMAT(/,'********',T12,'MOHEGAN RESERVATION (8211) BEING ',
     &    'MAPPED TO WHITE MOUNTAIN NF (922) FOR FURTHER PROCESSING.')
          IFOR = 2
        CASE (8214)
          WRITE(JOSTND,69)
   69     FORMAT(/,'********',T12,'CAYUGA NATION TDSA (8214) BEING ',
     &    'MAPPED TO FINGER LAKES NF (930) FOR FURTHER PROCESSING.')
          IFOR = 7
        CASE (8215)
          WRITE(JOSTND,70)
   70     FORMAT(/,'********',T12,'ONONDAGA NATION RESERVATION (8215) ',
     &    'BEING MAPPED TO FINGER LAKES NF (930) FOR FURTHER ',
     &    'PROCESSING.')
          IFOR = 7
        CASE (8216)
          WRITE(JOSTND,71)
   71     FORMAT(/,'********',T12,'TONAWANDA RESERVATION (8216) BEING ',
     &    'MAPPED TO FINGER LAKES NF (930) FOR FURTHER PROCESSING.')
          IFOR = 7
        CASE (8217)
          WRITE(JOSTND,72)
   72     FORMAT(/,'********',T12,'TUSCARORA NATION RESERVATION (8217)',
     &    ' BEING MAPPED TO FINGER LAKES NF (930) FOR FURTHER ',
     &    'PROCESSING.')
          IFOR = 7
        CASE (8218)
          WRITE(JOSTND,73)
   73     FORMAT(/,'********',T12,'ONEIDA NATION RESERVATION (8218) ',
     &    'BEING MAPPED TO FINGER LAKES NF (930) FOR FURTHER ',
     &    'PROCESSING.')
          IFOR = 7
C       END CROSSWALK FOR RESERVATION PSUEDO CODES & LOCATION CODE

        CASE DEFAULT
        
C         CONFIRMS THAT KODFOR IS AN ACCEPTED FVS LOCATION CODE
C         FOR THIS VARIANT FOUND IN DATA ARRAY JFOR
          DO 10 I=1,NUMFOR
            IF (KODFOR .EQ. JFOR(I)) THEN
              IFOR = I
              FORFOUND = .TRUE.
              EXIT
            ENDIF
   10     CONTINUE        
          
C         LOCATION CODE ERROR TRAP       
          IF (.NOT. FORFOUND) THEN
            CALL ERRGRO (.TRUE.,3)
            WRITE(JOSTND,11) JFOR(IFOR)
   11       FORMAT(/,'********',T12,'FOREST CODE USED IN THIS ',
     &      'PROJECTION IS',I4)
            USEIGL = .FALSE.
          ENDIF

      END SELECT


C     FOREST MAPPING CORRECTION
      SELECT CASE (IFOR)
        CASE (6)
          WRITE(JOSTND,21)
   21     FORMAT(/,'********',T12,'WAYNE-HOOSIER NF (911) BEING ',
     &    'MAPPED TO WAYNE (914) FOR FURTHER PROCESSING.')
          IFOR = 1
        CASE (7)
          WRITE(JOSTND,22)
   22     FORMAT(/,'********',T12,'FINGER LAKES NF (930) BEING MAPPED ',
     &    'TO GREEN MTN-FINGER LAKES (920) FOR FURTHER PROCESSING.')
          IFOR = 4
      END SELECT

C----------
C  SET DEFAULT TLAT, TLONG, AND ELEVATION VALUES, BY FOREST
C----------
      SELECT CASE(IFOR)
        CASE(1)
          IF(TLAT.EQ.0) TLAT=39.33
          IF(TLONG.EQ.0)TLONG=82.10
          IF(ELEV.EQ.0) ELEV=9.
        CASE(3)
          IF(TLAT.EQ.0) TLAT=41.84
          IF(TLONG.EQ.0)TLONG=79.15
          IF(ELEV.EQ.0) ELEV=17.
        CASE(4)
          IF(TLAT.EQ.0) TLAT=43.61
          IF(TLONG.EQ.0)TLONG=72.97
          IF(ELEV.EQ.0) ELEV=19.
        CASE(5)
          IF(TLAT.EQ.0) TLAT=38.93
          IF(TLONG.EQ.0)TLONG=79.85
          IF(ELEV.EQ.0) ELEV=30.
        CASE(2)
          IF(TLAT.EQ.0) TLAT=43.53
          IF(TLONG.EQ.0)TLONG=71.47
          IF(ELEV.EQ.0) ELEV=20.
      END SELECT 
C     SET THE IGL VARIABLE ONLY IF DEFAULT FOREST IS USED
C     GEOGRAPHIC LOCATION CODE: 1=NORTH, 2=CENTRAL, 3=SOUTH
C     USED TO SET SOME EQUATIONS IN REGENERATION AND PERHAPS
C     HEIGHT-DIAMETER IN DIFFERENT VARIANTS.
      IF (USEIGL) IGL = KFOR(IFOR)

      KODFOR=JFOR(IFOR)
      RETURN
      END