      SUBROUTINE OPLIST (LFIRST,NPLT,MGMID,ITITLE)
      use contrl_mod
      use keycom_mod
      use metric_mod
      use prgprm_mod
      implicit none
C----------
C  $Id$
C----------
C
C     OPTION PROCESSING ROUTINE - NL CROOKSTON - JAN 1981 - MOSCOW
C
C     OPLIST WRITES A SUMMARY OF THE ACTIVITIES.
C     IF LFIRST IS TRUE, THE INITIAL ACTIVITY SCHEDULE IS WRITTEN;
C     AND IF LFIRST IS FALSE, THE FINAL SCHEDULE, INCLUDING THE
C     DISPOSITION OF ACTIVITIES, IS WRITTEN.
C
C     ENTRY POINT PPACTS:     (ADDED OCT. 26 1987 BY NL CROOKSTON).
C
C     WRITES THE ACTIVITIES THAT WERE DONE FOR THE PPE TO DATA SET
C     JOUT.  CISN IS THE PPE INTERNAL STAND NUMBER.
C
      INCLUDE 'OPCOM.F77'
C
      INTEGER NTRSLT,I1,I,ICY,I2,II,IACTK,IDT,KEY,LOC,J1,J2,J,ID,K
      INTEGER JOUT,NCNT,IARY,IKEY,IKEY2
      CHARACTER*11 CISN
      CHARACTER*26 NPLT
      CHARACTER*4  MGMID,CLBWID
      CHARACTER*72 ITITLE
      CHARACTER*20 IDISPO(2)
      LOGICAL      LINE,LFIRST,LPPEON
      CHARACTER*8  KEYWRD,UNKNOW,TITL,SCHED,SUMMR
      CHARACTER*4  TAB2(17)
      PARAMETER    (NTRSLT=151)
      INTEGER      ITRSL1(NTRSLT),ITRSL2(NTRSLT)
      REAL         PRMMLT(N_METRIC_PRMS)

      DATA ITRSL1/
     >       33,   80,   81,   82,   90,   91,   92,   93,   94,   95,
     >       96,   97,   98,   99,  100,  101,  102,  110,  111,  120,
     >      198,  199,  200,  201,  202,  203,  204,  205,  215,  216,
     >      217,  218,  222,  223,  224,  225,  226,  227,  228,  229,
     >      230,  231,  232,  233,  234,  235,  236,  248,  249,  250,
     >      260,  427,  428,  429,  430,  431,  432,  440,  442,  443,
     >      450,  490,  491,  492,  493,  555,  810,  811,  900, 1001,
     >     1002, 1003, 1004, 1005, 1006, 1007, 1008, 1009, 2001, 2002,
     >     2003, 2004, 2005, 2006, 2007, 2008, 2009, 2150, 2151, 2152,
     >     2153, 2154, 2155, 2156, 2157, 2158, 2159, 2160, 2208, 2209,
     >     2210, 2320, 2401, 2402, 2403, 2414, 2415, 2416, 2417, 2430,
     >     2431, 2432, 2433, 2501, 2503, 2504, 2505, 2506, 2507, 2508,
     >     2512, 2515, 2520, 2521, 2522, 2523, 2525, 2527, 2528, 2529,
     >     2530, 2538, 2539, 2544, 2545, 2547, 2548, 2549, 2550, 2551,
     >     2552, 2553, 2605, 2606, 2607, 2608, 2609, 2701, 2702, 2703,
     >     2704/
C
      DATA ITRSL2/
     >       33,   17,   96,  102,    3,   58,   62,   70,   59,  207,
     >       45,   88,  110,  111,  118,  616,  603,   38,   37,  138,
     >      135,   92,   48,   49,   71,  103,  107,  120,   41,   42,
     >       43,   44,   23,   24,   25,   26,   27,   28,   29,   30,
     >      112,  115,   35,  122,  124,  129,  136,  128,  108,   78,
     >       34,  216,  211,  212,  202,  203,  229,  213,  215,   93,
     >      226,  203,  204,  230,  205,  406,  306,  336,  512,  901,
     >      902,  903,  904,  905,  906,  910,  911,  916, 1001, 1004,
     >     1005, 1030, 1007, 1010, 1011, 1023, 1024, 1114, 1108, 1114,
     >     1109, 1116, 1114, 1114, 1124, 1114, 1124, 1114, 1208, 1209,
     >     1210, 1320, 1401, 1402, 1403, 1414, 1415, 1416, 1417, 1430,
     >     1431, 1432, 1433, 1501, 1543, 1504, 1505, 1506, 1507, 1508,
     >     1512, 1515, 1520, 1521, 1522, 1523, 1525, 1527, 1528, 1533,
     >     1534, 1538, 1539, 1544, 1545, 1547, 1548, 1549, 1550, 1551,
     >     1552, 1553, 1601, 1602, 1603, 1604, 1605, 1301, 1302, 1303,
     >     1304/
C
      DATA TAB2/'CMPU','BASE','ESTB','DFTM','MPB', 'COVR',
     >          'DBS ','    ','    ','RUST','MIST','WSBE','DFB',
     >          'WPBM','RDIS','FIRE','ECON'/
      DATA UNKNOW/'*UNKNOWN'/,SCHED/'SCHEDULE'/,SUMMR/'SUMMARY '/
      DATA IDISPO/'DELETED OR CANCELED ',
     >            'NOT DONE            '/
C
C     ITRSL1= TRANSLATION TABLE, PART ONE. CONTAINS AN ORDERED LIST
C             OF VALID ACTIVITY CODES.
C     ITRSL2= TRANSLATION TABLE, PART TWO. CONTAINS A SUBSCRIPT TO
C             THE POSITION IN THE KEYWORD TABLES THAT CONTAINS THE
C             KEYWORD CORRESPONDING TO THE ACTIVITY.
C             IF THE SUBSCRIPT IS UNDER 200, THEN THE KEYWORD IS
C             PART OF THE BASE AND FOUND IN 'TABLE'.  IF IT IS OVER
C             199, THEN THE KEYWORD IS FROM AS EXTENSION AND NUMBERED
C             AS FOLLOWS:
C                     200-299  ESTAB
C                     300-399  DFTM
C                     400-499  MPB
C                     500-599  COVER
C                     600-699  DBS  (DATABASE KEYWORDS)
C                     700-799  ** free spot **
C                     800-899  ** free spot **
C                     900-999  BRUST
C                    1000-1099 MIST
C                    1100-1199 WSBWE (GenDefol)
C                    1200-1299 DFB
C                    1300-1399 WPBM--STAND LEVEL ACTIVITIES.
C                    1400-1499 WRD VER. 3.0
C                    1500-1599 FIRE
C                    1600-1699 ECON
C
C     NTRSLT= THE LENGTH OF THE TRANSLATION TABLES.
C
C     IF THERE ARE NO ACTIVITIES, AND THIS IS THE SECOND CALL,
C     DO NOT PRINT THE ACTIVITY SCHEDULE.
C
      IF (.NOT.LFIRST  .AND. IMGL.EQ.1) RETURN
C
C     FIND OUT IF THE PPE IS ACTIVE.
C
      CALL PPEATV (LPPEON)
C
C     WRITE HEADING
C
      IF (.NOT.LFIRST) WRITE (JOSTND,15)
      IF (.NOT.LFIRST) WRITE (JOSTND,15)
      TITL=SUMMR
      IF (LFIRST) TITL=SCHED
      WRITE (JOSTND,10) TITL,NPLT,MGMID,ITITLE
   10 FORMAT (//T55,'ACTIVITY ',A8//' STAND ID= ',A26,
     >        '    MGMT ID= ',A4,4X,A72//1X,130('-'))
      IF (LFIRST) WRITE (JOSTND,11)
   11 FORMAT(/' CYCLE  DATE  EXTENSION  KEYWORD   DATE  PARAMETERS:'/
     >        ' -----  ----  ---------  --------  ----  ',90('-'))
      IF (.NOT.LFIRST) WRITE (JOSTND,12)
   12 FORMAT(/' CYCLE  DATE  EXTENSION  KEYWORD   DATE  ',
     >        'ACTIVITY DISPOSITION  PARAMETERS:'/
     >        ' -----  ----  ---------  --------  ----  ',20('-'),
     >      2X,68('-'))
C
C     IF THIS THE SECOND CALL (LFIRST=FALSE) MAKE SURE THAT THE DATE
C     AN ACTIVITY WAS ACCOMPLISHED IS USED TO ASSIGN THE ACTIVITY
C     TO THE CYCLE RATHER THAN THE DATE THE ACTIVITY WAS SCHEDULED.
C
C     (1) SAVE THE DATE ARRAY (IDATE) IN IOPCYC AND LOAD IDATE WITH
C         THE DATE THE ACTIVITY WAS ACCOMPLISHED.
C
      IF (LFIRST) GOTO 16
      I1=IMGL-1
      DO 13 I=1,I1
      IOPCYC(I)=IDATE(I)
      IF(IACT(I,4).GT.0) IDATE(I)=IACT(I,4)
   13 CONTINUE
C
C     (2) REESTABLISH THE ASSIGNMENT OF ACTIVITIES TO CYCLES.
C
      CALL OPSORT(I1,IDATE,ISEQ,IOPSRT,.TRUE.)
      CALL OPCYCL(NCYC,IY)
C
C     CREATE AND POPULATE COMPUTE DATABASE TABLE IF REQUIRED
C
      CALL DBSCMPU
C
C     (3) RETURN THE VALUES OF IDATE
C
      DO 14 I=1,I1
      IDATE(I)=IOPCYC(I)
   14 CONTINUE
   16 CONTINUE
C
C     DO FOR ALL CYCLES
C
      LINE=.TRUE.
      DO 160 ICY=1,NCYC
      I1=IMGPTS(ICY,1)
      IF (I1.GT.0) GOTO 30
      IF (LINE) WRITE (JOSTND,15)
   15 FORMAT (' ')
      LINE=.FALSE.
      WRITE (JOSTND,20) ICY,IY(ICY)
   20 FORMAT (1X,I4,I7)
      GOTO 160
   30 CONTINUE
      LINE=.TRUE.
      WRITE (JOSTND,35) ICY,IY(ICY)
   35 FORMAT (/1X,I4,I7)
C
C     IF THERE ARE NO ACTIVITIES DURING THE CYCLE;
C     THEN: BRANCH TO END CYCLE.
C
      I2=IMGPTS(ICY,2)
C
C     DO FOR ALL ACTIVITIES WITHIN THE CYCLE.
C
      DO 150 II=I1,I2
      I=IOPSRT(II)
      IACTK=IACT(I,1)
      IDT=IDATE(I)
C
C     INITIALIZE THE PARAMETER MULTIPLIERS FOR METRIC CONVERSION.
C     AT MOST 12 PARAMETERS ARE IN THE ARGUMENT LIST (AT PRESENT)
C
      DO IARY = 1,N_METRIC_PRMS
        PRMMLT(IARY) = 1.0
      ENDDO
C
C     FIND THE ACTIVITY KODE (IACTK) IN THE TRANSLTAION TABLE.
C
      CALL OPBISR (NTRSLT,ITRSL1,IACTK,KEY)
C
C     IF THE ACTIVITY KODE IS NOT IN THE TRANSLATION TABLE;
C     THEN: LOAD THE KEYWRD WITH '*UNKNOWN' AND BRANCH TO WRITE.
C
      IF (KEY.LE.0) GOTO 36
      CALL MCNVRT(IACTK,2,PRMMLT)
      KEY=ITRSL2(KEY)
      GOTO 40
   36 CONTINUE
      KEYWRD=UNKNOW
      LOC=2
      GO TO 61
   40 CONTINUE
C
C     ELSE: LOAD KEYWRD WITH THE ACTUAL KEYWORD AND LOC WITH LOCATION
C     POINTER.
C
      LOC=(KEY/100)+1
      IF (KEY .LT. 200) THEN
        KEY=MOD(KEY,200)
      ELSE
        KEY=MOD(KEY,100)
      ENDIF
      GOTO (42,42,43,44,45,46,47,48,49,52,53,54,55,56,57,58,59),LOC
   42 CONTINUE
C
C     IF THE ACTIVITY CODE IS FOR A COMPUTE, THEN SET THE KEYWORD UP
C     TO BE THE VARIABLE NAME.
C
      IF (IACTK.EQ.33) THEN
         LOC=IFIX(PARMS(IACT(I,2)+1))
         IF (LOC.GT.500) LOC=LOC-500
         KEYWRD=CTSTV5(LOC)
         LOC=1
      ELSE
         KEYWRD=TABLE(KEY)
         LOC=2
      ENDIF
      GO TO 61
   43 CONTINUE
      CALL ESKEY(KEY,KEYWRD)
      GO TO 61
   44 CONTINUE
      CALL TMKEY(KEY,KEYWRD)
      GO TO 61
   45 CONTINUE
      CALL MPKEY(KEY,KEYWRD)
      GO TO 61
   46 CONTINUE
      CALL CVKEY(KEY,KEYWRD)
      GO TO 61
   47 CONTINUE
      CALL DBSKEY(KEY,KEYWRD)
      GO TO 61
   48 CONTINUE
C  ** free spot **
      GO TO 61
   49 CONTINUE
C  ** free spot **
      GO TO 61
   52 CONTINUE
      CALL BRKEY (KEY,KEYWRD)
      GO TO 61
   53 CONTINUE
      CALL MISKEY (KEY,KEYWRD)
      GO TO 61
   54 CONTINUE
      CALL BWEKEY (KEY,KEYWRD)
      GO TO 61
   55 CONTINUE
      CALL DFBKEY (KEY,KEYWRD)
      GO TO 61
   56 CONTINUE
C
C  ENTRY MOVED FROM BMPPIN TO BMIN (AJM 9/05)
C
      CALL BMKEY (KEY,KEYWRD)
      GO TO 61
   57 CONTINUE
      CALL RDKEY (KEY,KEYWRD)
      GO TO 61
   58 CONTINUE
      CALL FMKEY (KEY,KEYWRD)
      GO TO 61
   59 CONTINUE
      CALL ECKEY (KEY,KEYWRD)
   61 CONTINUE
      J1=IACT(I,2)
C
C     IF THIS IS THE SECOND CALL (LFIRST=FALSE), BRANCH ALTERNATIVE
C     LOGIC TO WRITE ACTIVITY DISPOSITION.
C
      IF (.NOT.LFIRST) GOTO 100
C
C     J1 AND J2 POINT TO THE PARAMETERS FOR THE ACTIVITY.
C     IF THERE ARE PARAMETERS;
C     THEN: WRITE THE PARAMETERS ALONG WITH THE KEYWORD.
C
      IF (J1.LE.0) GOTO 90
      J2=IACT(I,3)
C
C     IF WRITTING THE COMPUTE'S, THEN ONLY WRITE 1 PARM.
C
      IF (LOC.EQ.1) J2=J1
      WRITE (JOSTND,60) TAB2(LOC),KEYWRD,IDT,
     >                  (PARMS(J)*PRMMLT(J-J1+1),J=J1,J2)
   60 FORMAT (T17,A4,T26,A8,I6,((T42,8F10.2)))
      GOTO 150
   90 CONTINUE
C
C     ELSE: WRITE WITHOUT THE PARAMETERS.
C
      WRITE (JOSTND,60) TAB2(LOC),KEYWRD,IDT
      GOTO 150
  100 CONTINUE
C
C     WRITE DISPOSITION ALONG WITH ACTIVITY.
C
      ID=IACT(I,4)
      K=0
      IF(ID.LE.0) K=ID+2
      IF(J1.LE.0) GOTO 95
      J2=IACT(I,3)
      IF (LOC.EQ.1) J2=J1
      IF(K.EQ.0) WRITE(JOSTND,91) TAB2(LOC),KEYWRD,IDT,ID,
     >                            (PARMS(J)*PRMMLT(J-J1+1),J=J1,J2)
      IF(K.GT.0) WRITE(JOSTND,92) TAB2(LOC),KEYWRD,IDT,IDISPO(K),
     >                            (PARMS(J)*PRMMLT(J-J1+1),J=J1,J2)
   91 FORMAT (T17,A4,T26,A8,I6,'  DONE IN',I5,((T64,6F10.2)))
   92 FORMAT (T17,A4,T26,A8,I6,2X,A20,((T64,6F10.2)))
      GOTO 140
   95 CONTINUE
      IF(K.EQ.0) WRITE(JOSTND,91) TAB2(LOC),KEYWRD,IDT,ID
      IF(K.GT.0) WRITE(JOSTND,92) TAB2(LOC),KEYWRD,IDT,
     >           IDISPO(K)
  140 CONTINUE
C
C     IF THE PPE IS NOT RUNNING, THEN WRITE OUTPUT FOR CHEAPO
C
      IF (.NOT.LPPEON) CALL ECOPLS (KEYWRD,IACT(I,4),PARMS,J1,J2)
  150 CONTINUE
  160 CONTINUE
C
C     WRITE END-OF-TABLE
C
      WRITE (JOSTND,180)
  180 FORMAT (1X,130('-'))
      RETURN
C
      ENTRY PPACTS (JOUT,CLBWID,CISN)
C
C     WRITE THE "ACCOMPLISHED" ACTIVITIES FOR THE PPE.
C
      IF (IMGL.LE.1 .OR. JOUT.LE.0) RETURN
      DO 400 II=1,(IMGL-1)
      I=IOPSRT(II)
      IF (IACT(I,4).LE.0) GOTO 400
      CALL OPBISR (NTRSLT,ITRSL1,IACT(I,1),KEY)
      IF (KEY.LE.0) GOTO 400
      CALL MCNVRT(IACT(I,1),2,PRMMLT)
      KEY=ITRSL2(KEY)
C
C     FIND THE KEYWORD.
C
      LOC=(KEY/100)+1
      IF (KEY .LT. 200) THEN
        KEY=MOD(KEY,200)
      ELSE
        KEY=MOD(KEY,100)
      ENDIF
      GOTO (242,242,243,244,245,246,247,248,249,252,253,254,255,
     >      256,257,258,259),LOC
  242 CONTINUE
      IF (KEY.EQ.33) THEN
         LOC=IFIX(PARMS(IACT(I,2)+1))
         IF (LOC.GT.500) LOC=LOC-500
         KEYWRD=CTSTV5(LOC)
         LOC=1
      ELSE
         KEYWRD=TABLE(KEY)
         LOC=2
      ENDIF
      GOTO 261
  243 CONTINUE
      CALL ESKEY(KEY,KEYWRD)
      GOTO 261
  244 CONTINUE
      CALL TMKEY(KEY,KEYWRD)
      GOTO 261
  245 CONTINUE
      CALL MPKEY(KEY,KEYWRD)
      GOTO 261
  246 CONTINUE
      CALL CVKEY(KEY,KEYWRD)
      GOTO 261
  247 CONTINUE
      CALL DBSKEY(KEY,KEYWRD)
      GOTO 261
  248 CONTINUE
C  ** FREE SPOT **
      GO TO 261
  249 CONTINUE
C  ** FREE SPOT **
      GO TO 261
  252 CONTINUE
      CALL BRKEY (KEY,KEYWRD)
      GOTO 261
  253 CONTINUE
      CALL MISKEY (KEY,KEYWRD)
      GOTO 261
  254 CONTINUE
      CALL BWEKEY (KEY,KEYWRD)
      GOTO 261
  255 CONTINUE
      CALL DFBKEY (KEY,KEYWRD)
      GOTO 261
  256 CONTINUE
      CALL BMKEY (KEY,KEYWRD)
      GOTO 261
  257 CONTINUE
      CALL RDKEY (KEY,KEYWRD)
      GOTO 261
  258 CONTINUE
      CALL FMKEY (KEY,KEYWRD)
      GOTO 261
  259 CONTINUE
      CALL ECKEY (KEY,KEYWRD)
  261 CONTINUE
C
C     WRITE THE KEYWORD AND THE PARAMETERS.
C
      J1=IACT(I,2)
      IF (J1.GT.0) THEN
         J2=IACT(I,3)
         IF (LOC.EQ.1) J2=J1
         DO 280 NCNT=1,(((J2-J1)/4)+1)
         IF (J2.GT.(J1+4)) J2=J1+3
         WRITE (JOUT,270) CLBWID,CISN,NCNT,TAB2(LOC),KEYWRD,IDATE(I),
     >                    (PARMS(J)*PRMMLT(J-J1+1),J=J1,J2)
  270    FORMAT (A4,T7,A11,T19,'2',I2,T22,A4,T27,A8,T36,I4,:,T41,4F10.2)
         J1=J2+1
         J2=IACT(I,3)
  280    CONTINUE
      ELSE
         WRITE (JOUT,270) CLBWID,CISN, 1,TAB2(LOC),KEYWRD,IDATE(I)
      ENDIF
  400 CONTINUE
      RETURN

C     CALL FROM MCNVRT TO FIND ACTIVITY CODES IN OPLIST ARRAYS

      ENTRY OPKEY(IKEY)
      CALL OPBISR (NTRSLT,ITRSL1,IKEY,IKEY2)
      IF (IKEY2.LE.0) RETURN
      IKEY=ITRSL2(IKEY2)
      RETURN
      END
