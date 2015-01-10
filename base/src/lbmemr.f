      LOGICAL FUNCTION LBMEMR (LENMEM,MEM,LENSET,SET)
      IMPLICIT NONE
C----------
C  $Id: lbmemr.f 767 2013-04-10 22:29:22Z rhavis@msn.com $
C----------
C
C     TRUE IF MEM IS A MEMBER OF SET, FALSE OTHERWISE.
C
C     PART OF THE LABEL PROCESSING COMPONENT OF THE PROGNOSIS SYSTEM
C     N.L. CROOKSTON -- INTERMOUNTAIN RESEARCH STATION -- JAN 1987
C
C     LENMEM= LENGTH OF MEMBER STRING
C     MEM   = C*250 STRING CONTAINING THE MEMBER
C     LENSET= LENGTH OF THE DEFINED PART STRING SET
C     SET   = C*250 STRING CONTAINING THE SET
C
      CHARACTER*250 SET,MEM,WRK
      INTEGER LENSET,LENMEM,IP,LENWRK
C
      LBMEMR=.FALSE.
C
C     LOOP THROUGH THE MEMBERS OF SET
C
      IP=1
   10 CONTINUE
C
C     IF IP IS GREATER THAN THE LENGTH OF THE SET, END THE LOOP
C
      IF (IP.GT.LENSET) GOTO 20
C
C     SET WRK BE THE FIRST MEMBER OF SET BETWEEN LOCATION IP AND
C     LENSET.
C
      CALL LB1MEM (IP,LENSET,SET,LENWRK,WRK)
C
C     COMPARE MEM WITH WRK...IF EQUAL, THEN MEMBERSHIP HAS BEEN
C     ESTABLISHED.  IF NOT, CONTINUE THE SEARCH.
C
      IF (LENMEM .EQ. LENWRK) THEN
         IF (MEM(1:LENMEM).EQ.WRK(1:LENWRK)) THEN
            LBMEMR=.TRUE.
            GOTO 20
         ENDIF
      ENDIF
C
C     INCREMENT IP TO POINT TO THE NEXT PART OF SET AND BRANCH BACK.
C
      IP=IP+LENWRK+2
      GOTO 10
   20 CONTINUE
      RETURN
      END

