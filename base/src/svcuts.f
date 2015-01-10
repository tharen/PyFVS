      SUBROUTINE SVCUTS (IVAC,SSNG,DSNG,CTCRWN) 
      IMPLICIT NONE
C----------
C  $Id: svcuts.f 767 2013-04-10 22:29:22Z rhavis@msn.com $
C----------
C
C     STAND VISUALIZATION GENERATION
C     N.L.CROOKSTON -- RMRS MOSCOW -- NOVEMBER 1998
C     J.J.MARCHINEK -- RMRS MOSCOW -- JANUARY 1999
C
C     UPDATES THE VISUALIZATION FOR HARVEST, WRITES THE POST
C     CUTTING REPORT, AND CLEANS UP THE LIST OF REFERENCES
C     TO "REMOVED" TREE RECORDS.
C
C     CALLED FROM CUTS. 
C     INPUT:
C     IVAC=THE NUMBER OF COMPLETELY CUT TREES
C
COMMONS
C
C
      INCLUDE 'PRGPRM.F77'
C
C
      INCLUDE 'ARRAYS.F77'
C
C
      INCLUDE 'CONTRL.F77'
C
C
      INCLUDE 'SVDATA.F77'
C
C
COMMONS
C
C     SSNG  = SNAGS/ACRE CREATED BY CUTTING, BUT LEFT STANDING
C     DSNG  = SNAGS/ACRE CREATED BY CUTTING, DOWN LEFT IN STAND
C     CTCRWN= TREES/ACRE REMOVED BUT WHOSE CROWNS ARE STILL IN THE
C             STAND.
C
      INTEGER IVAC,I,II,IPUT,ISVOBJ
      REAL SSNG(*),DSNG(*),CTCRWN(*)
      INTEGER ICUTFG(MAXTRE)
      LOGICAL LDROP
C
C     REMOVE THE HARVESTED TREES FROM THE STANDING LIVE TREE LIST
C
      CALL SVRMOV (WK3,4,SSNG,DSNG,CTCRWN,IY(ICYC))
C     
C     PRINT THE VISUALIZATION FOR AFTER CUTS (IF REMOVALS).
C
      CALL SVOUT(IY(ICYC),2,'Post cutting')
C
C     NOW, REMOVE ALL THE REFERENCES IN THE OBJECT LIST TO RECORDS THAT
C     HAVE BEEN COMPLETELY REMOVED. THIS IS DONE BY WRITTING OVER 
C     DELETED RECORDS
C
      IF (IVAC.EQ.0) RETURN
C
C     LOAD ICUTFG SO THAT IT INDICATES THAT A RECORD IS 
C     COMPLETELY CUT.  A 1 MEANS THAT IS IS COMPLETELY CUT.
C
      DO I=1,ITRN
         II=IND2(I)
         IF (II.LT.0) THEN
            ICUTFG(-II)=II
         ELSE
            ICUTFG(II)=0
         ENDIF
      ENDDO
C
C     COMPRESS THE OBJECT POINTERS. THIS IS DONE TO MAKE SURE THAT 
C     NO MEMBERS OF IS2F POINT TO A TREE DATA RECORD THAT IS TO BE
C     DELETED.
C
      IPUT=0
      DO ISVOBJ=1,NSVOBJ
        LDROP = .FALSE.
        IF (IOBJTP(ISVOBJ).NE.4) THEN
          IF (IOBJTP(ISVOBJ).EQ.0 .OR.
     >       (IOBJTP(ISVOBJ).EQ.1 .AND. ICUTFG(IS2F(ISVOBJ)).LT.0)) THEN
             LDROP = .TRUE.
          ENDIF
        ENDIF
        IF (LDROP) THEN
          IF (IPUT.EQ.0) IPUT=ISVOBJ
        ELSE
          IF (IPUT.GT.0 .AND. IPUT.LT.ISVOBJ) THEN
            IS2F(IPUT)=IS2F(ISVOBJ)
            XSLOC(IPUT)=XSLOC(ISVOBJ)
            YSLOC(IPUT)=YSLOC(ISVOBJ)
            IOBJTP(IPUT)=IOBJTP(ISVOBJ)
            IPUT=IPUT+1
          ENDIF
        ENDIF
      ENDDO
      IF (IPUT.GT.0) NSVOBJ=IPUT-1
C
      RETURN
      END

