      SUBROUTINE RPTWTR (NCYC2,IONER)
      IMPLICIT NONE
C----------
C METRIC-PPBASE $Id$
C----------
C
C     PRINTS THE STAND COMPOSITION, AND SAMPLE TREE-STAND ATTRIBUTE
C     TABLES.
C
C     NOTE THAT THIS IS A REWRITE OF DISPLY, AND A POOR ONE AT THAT.
C
C     PART OF THE PARALLEL PROCESSING EXTENSION OF PROGNOSIS SYSTEM.
C     N.L. CROOKSTON--FORESTRY SCIENCES LAB, MOSCOW, ID--FEB 1982.
C
C     NCYC2 = CYCLE NUMBER.
C     IONER = A LIST OF POINTERS TO THE ELEMENTS THAT MAKE UP THE
C             OUTPUT FOR THE REPORT.
C
COMMONS
C
C
      INCLUDE 'PRGPRM.F77'
C
C
      INCLUDE 'PPEPRM.F77'
C
C
      INCLUDE 'CONTRL.F77'
C
C
      INCLUDE 'OUTCOM.F77'
C
C
      INCLUDE 'PLOT.F77'
C
C
      INCLUDE 'ARRAYS.F77'
C
C
      INCLUDE 'PPCNTL.F77'
C
C
      INCLUDE 'METRIC.F77'
C
C
COMMONS
C
C     STORAGE ALLOCATION:
C
      INTEGER NCYC2,IC2,IPLACE,IOAGE,JYR,IRT,I,JSDI
      INTEGER IONER(*)
C
      REAL         ALL(15000),
      INTEGER      ISNUMS(3000),ICYCS(3000),
      INTEGER      LOCS(3000),IELTS(3000),IPTS(3000)
      EQUIVALENCE (ALL(1),BFV(1)),(ALL(1),ISNUMS(1)),
     >            (ALL(3001 ),ICYCS(1)),
     >            (ALL(6001 ),LOCS (1)),
     >            (ALL(9001 ),IELTS(1)),
     >            (ALL(12001),IPTS (1))
C
C----------
C  DIMENSIONS FOR INTERNAL VARIABLES (USED AS OUTPUT LABELS):
C----------
      CHARACTER*9 AT1,AT2,AT3,AT4
      CHARACTER*10 STD(3)
C----------
C  DATA STATEMENTS:
C----------
      DATA AT1/'REMOVAL  '/,AT2/'VOLUME:  '/,AT3/
     & 'RESIDUAL '/,AT4/'ACCRETION'/
C----------
C
C     IF OUTPUT IS CURRENTLY BEING PRINTED IN THE KEYWORD TABLE; THEN:
C     CLOSE THE KEYWRD TABLE IF OTHER MAJOR TABLES WILL BE PRINTED;
C     OTHERWISE LEAVE IT OPEN FOR MORE KEYWORDS.
C     NOTE: LFLAG=TRUE IF A NEW KEYWORD-TABLE HEADING IS NEEDED, AND
C     FALSE IF THE CURRENT OUTPUT IS PART OF THE KEYWORD TABLE.
C
      IF (LFLAG) GOTO 20
      IF (.NOT.(LSMPT.OR.LSNDCP)) GOTO 20
C
C     CLOSE KEYWORD TABLE AND INDECATE A NEW ONE WILL BE NEEDED.
C
      WRITE (JOPPRT,10)
   10 FORMAT (1X,130('-'))
      LFLAG=.TRUE.
   20 CONTINUE
C
C     CREATE THE REQUESTED REPORTS...LOOP THRU ALL CYCLES, INCLUDING
C     THE 'BEGINNING' AND 'ENDING' OUTPUTS.
C
      DO 200 IC2=1,NCYC2
      IPLACE=IONER(IC2)
      IPTELS=IELTS(IPLACE)
      IPLACE=LOCS(IPLACE)
C
      IF (PDEBUG) WRITE (JOPPRT,25) IPLACE,IPTELS
   25 FORMAT (/' RPTWTR CALLING GETPRT, IPLACE=',I5,'; IPTELS=',I5)
      CALL GETPRT(IPLACE,JYR,IOAGE)
C
C     GETPRT SETS THE NECESSARY VARIABLES WHICH CONTROL WHICH WRITE
C     STMTS WILL BE USED BY THIS ROUTINE.  THE USER CONTROLS THE
C     VALUES OF LSNDCP, LSMPT, AND LCHEAPO, USING KEYWORDS.
C
      IF (PDEBUG) WRITE (JOPPRT,26) LFLAG,LSTART,LSNDCP,LSMPT,ICL6
   26 FORMAT (/' IN RPTWTR: LFLAG=',L2,'; LSTART=',L2,'; LSNDCP=',L2,
     >        '; LSMPT=',L2,'; ICL6=',I5)
C
C     IF WE ARE AT THE 'BEGINNING' OUTPUT AND THE STAND COMPOSTION
C     TABLE HAS BEEN REQUESTED, THEN WRITE A HEADING.
C
      IF (LSTART.AND.LSNDCP)
     >    CALL GHEADS (NPLT,MGMID,JOPPRT,0,ITITLE)
C
C     IF EXAMPLE TREE/STAND ATTRIBUTE TABLE IS REQUESTED; THEN: WRITE
C     HEADER RECORD TO THE SCRATCH FILE.
C
      IF (.NOT.LSTART .OR. .NOT.LSMPT) GOTO 30
      IRT = 0
      WRITE (JOTREE) IRT,NPLT,MGMID
   30 CONTINUE
C----------
C   IF USER HAS SPECIFIED KEYWORDS THAT ALTER VOLUME EQUATION PARAMETERS
C   (IE VOLUMME BFVOLUME,ETC) THEN PRINT LABEL THAT INDICATES USER
C   SPECIFIED STANDARDS.
C----------
      IF (LCVOLS) THEN
         STD(1)='USER TOTAL'
         STD(2)='USER MERCH'
      ELSE
         STD(1)='TOTAL'
         STD(2)='MERCH'
      ENDIF
C
      IF (LBVOLS) THEN
         STD(3)='USER MERCH'
      ELSE
         STD(3)='MERCH'
      ENDIF
C
      IF(ICL6.LT.0) GOTO 70
C
C     WRITE REMOVAL STATISTICS.  BYPASS IF LSTART IS TRUE OR NUMBER
C     OF TREES REMOVED WAS LESS THAN OR EQUAL TO ZERO.
C
      IF (.NOT.LSNDCP) GOTO 60
      IF(LSTART) GOTO 50
      IF(ONTREM(7).LE.0.0) GOTO 40
      WRITE (JOPPRT,9003)
 9003 FORMAT(/)
      WRITE(JOPPRT,9004) AT1,(ONTREM(I)*INtoCM,I=1,6),
     >  ONTREM(7)/ACRtoHA,(OSPTT(I),IOSPTT(I),I=1,4)
 9004 FORMAT(7X,A9,3X,5F7.1,F8.1,F9.0,' TREES   ',
     >       3(F5.0,'% ',A3,','),F5.0,'% ',A3)
      WRITE(JOSTND,9005) AT2,STD(1),(OCVREM(I)*INtoCM,I=1,6),
     >  OCVREM(7)*FT3pACRtoM3pHA,(OSPTV(I),IOSPTV(I),I=1,4),
     >  STD(2),(OMCREM(I)*INtoCM,I=1,6),OMCREM(7)*FT3pACRtoM3pHA
C
 9005 FORMAT(7X,A9/9X,A10,5F7.1,F8.1,F9.0,' CU M    ',
     >  3(F5.0,'% ',A3,','),F5.0,'% ',A3/
     >  9X,A10,5F7.1,F8.1,F9.0,' CU M    ',
     >  3(F5.0,'% ',A3,','),F5.0,'% ',A3)
      WRITE(JOSTND,9003)
      WRITE(JOSTND,9004) AT3,(ONTRES(I)*INtoCM,I=1,6),
     &  ONTRES(7)/ACRtoHA,(OSPRT(I),IOSPRT(I),I=1,4)
C
C     WRITE ACCRETION AND MORTALITY STATISTICS.  BYPASSED IF LSTART
C     IS TRUE.
C
   40 CONTINUE
      WRITE (JOPPRT,9003)
      WRITE (JOPPRT,9006) AT4,(OACC(I)*INtoCM,I=1,6),
     &  OACC(7)*FT3pACRtoM3pHA,(OSPAC(I),IOSPAC(I),I=1,4)
      IF (LMORT) THEN
        WRITE(JOSTND,9006) 'USER MORT',(OMORT(I)*INtoCM,I=1,6),
     &    OMORT(7)*FT3pACRtoM3pHA,(OSPMO(I),IOSPMO(I),I=1,4)
      ELSE
        WRITE(JOSTND,9006) 'MORTALITY',(OMORT(I)*INtoCM,I=1,6),
     &    OMORT(7)*FT3pACRtoM3pHA,(OSPMO(I),IOSPMO(I),I=1,4)
      ENDIF
 9006 FORMAT(7X,A9,3X,5F7.1,F8.1,F9.0,' CU M/YR ',
     >       3(F5.0,'% ',A3,','),F5.0,'% ',A3)

C
C     WRITE CURRENT STAND STATISTICS.
C
   50 CONTINUE
      WRITE (JOPPRT,9003)
      WRITE (JOPPRT,9007) JYR,(ONTCUR(I)*INtoCM,I=1,6),
     &  ONTCUR(7)/ACRtoHA,(OSPCT(I),IOSPCT(I),I=1,4)
 9007 FORMAT(/,1X,I4,'  TREES',T20,5F7.1,F8.1,F9.0,' TREES   ',
     >       3(F5.0,'% ',A3,','),F5.0,'% ',A3)
      WRITE(JOSTND,9005) AT2,STD(1),(OCVCUR(I),I=1,6),
     &  OCVCUR(7)*FT3pACRtoM3pHA,(OSPCV(I),IOSPCV(I),I=1,4),
     &  STD(2),(OMCCUR(I),I=1,6),OMCCUR(7)*FT3pACRtoM3pHA,
     &  (OSPMC(I),IOSPMC(I),I=1,4)
   60 CONTINUE
C
C     WRITE OUTPUT FOR SAMPLE TREES.  FIRST, OUTPUT STAND CONDITIONS
C     FOR PREVIOUS CYCLE.  BYPASS IF LSTART IS TRUE.
C
      IF(LSTART) GOTO 100
   70 CONTINUE
C
C     BYPASS WRITTING OF SAMPLE TREE DATA IF THE TABLE HAS BEEN
C     SURPRESSED BY THE USER.
C
      IF (.NOT.LSMPT) GOTO 90
C
C     GOTO STATEMENT 80 AND WRITE BOTH PRE AND POST THINNING STATISTICS
C     IF THINNING OCCURRED IN PREVIOUS CYCLE.
C
      JSDI=SDIBC + 0.5
      IF(ONTREM(7).GT.0.0) GOTO 80
      IRT=1
      WRITE(JOTREE) IRT,IOAGE,ORMSQD*INtoCM,OLDTPA/ACRtoHA,
     &  OLDBA*FT2pACRtoM2pHA,OLDAVH*FTtoM,RELDM1,JSDI
      GOTO 90
   80 CONTINUE
      IRT=2
      WRITE(JOTREE) IRT,IOAGE,ORMSQD*INtoCM,OLDTPA/ACRtoHA,
     &  OLDBA*FT2pACRtoM2pHA,OLDAVH*FTtoM,RELDM1,JSDI
      IRT=3
      JSDI=SDIAC + 0.5
      WRITE(JOTREE) IRT,ATAVD*INtoCM,ATTPA/ACRtoHA,
     &  ATBA*FT2pACRtoM2pHA,ATAVH*FTtoM,ATCCF,JSDI
   90 CONTINUE
C
C     IF THIS IS THE LAST OF THE OUTPUT, SKIP TO ECON DATA.
C
      IF(ICL6.LT.0) GOTO 120
C
C     WRITE YEAR AND PERIOD LENGTH.
C
  100 CONTINUE
      IF (.NOT.LSMPT) GOTO 120

      I=JYR
      IF (IFST.NE.0) I=-I
      IRT=4
      WRITE(JOTREE) IRT,I,IFINT
C
C     WRITE EXAMPLE TREE DATA.
C
      IRT=5
      WRITE(JOTREE) IRT,IONSP,DBHIO*INtoCM,HTIO*FTtoM,IOICR,
     &  DGIO*INtoCM,PCTIO,PRBIO
  120 CONTINUE
  130 CONTINUE
  200 CONTINUE
      IF (ICL6 .GT. 0) GO TO 150
C
C     PRINT THE EXAMPLE TREE RECORD AND STAND ATTRIBUTE TABLE.
C
      IF (LSMPT) CALL PRTEXM (JOTREE,JOSTND,ITITLE)
  150 CONTINUE
      RETURN
      END
