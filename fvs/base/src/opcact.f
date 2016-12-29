      SUBROUTINE OPCACT (KODE,CSTR)
      IMPLICIT NONE
C----------
C  $Id$
C----------
C     OPTION PROCESSING ROUTINE - NL CROOKSTON - JAN 2003 - MOSCOW
C
C     OPCACT IS USED TO STORE A STRING ASSOCIATED WITH THE MORE RECENTLY
C     STORED ACTIVITIY.  THIS ROUTINE MUST BE CALLED AFTER THE CALL THAT
C     ADDS AN ACTIVITY TO THE ACTIVITY LIST.  THE STRING IS ASSOCITED WITH
C     THE MOST RECENTLY ADDED ACTIVITY. ONLY CALL THIS ROUTINE IF THE
C     MOST RECENTLY ADDED ACTIVITY WAS SUCCESSFULLY ADDED.
C
C     KODE = THE RETURN CODE WHERE:
C            0   ALL WENT OK,
C            1   STRING COULD NOT BE ADDED BECAUSE STORAGE IS FULL.
C                A WARNING MESSAGE IS ISSUED VIA ERRGRO.
C     CSTR = THE STRING THAT IS BEING ADDED.
C
COMMONS
C
C
      INCLUDE 'PRGPRM.F77'
C
C
      INCLUDE 'OPCOM.F77'
C
C
      INCLUDE 'CONTRL.F77'
C
C
COMMONS
C
      LOGICAL LDEB
      CHARACTER*(*) CSTR
      INTEGER KODE,NCHAR,I
C
C     SEE IF WE NEED WRITE DEBUG.
C
      CALL DBCHK (LDEB,'OPCACT',6,ICYC)
      IF (LDEB) WRITE (JOSTND,10) ICACT,MXCACT,LOPEVN,IMPL,IEPT,
     >   TRIM(CSTR)
   10 FORMAT (' IN OPCACT: ICACT=',I4,' MXCACT=',I6,' LOPEVN=',L2,
     >        ' IMPL=',I4,' IEPT=',I5,' CSTR=',A)
C
C     MAKE SURE THERE IS ROOM IN THE ACTIVITY STORAGE AREAS
C     TO STORE THE ACTIVITY AND ITS PARAMETERS.
C
      NCHAR=LEN_TRIM(CSTR)
      IF (NCHAR+ICACT.GT.MXCACT) THEN
         KODE=1
         CALL ERRGRO (.TRUE.,10)
         RETURN
      ELSE
         KODE=1
C
C        SET THE POINTER TO THE MOST RECENT ACTIVITY AND LOAD THE
C        STARTING POSITION.
C
         IF (LOPEVN) THEN
            I=IEPT+1
         ELSE
            I=IMGL-1
         ENDIF
         IACT(I,5)=ICACT
C
C        STORE THE CHARACTER STRING.
C
         DO I=1,NCHAR
            CACT(ICACT)=CSTR(I:I)
            ICACT=ICACT+1
         ENDDO
         CACT(ICACT)=CHAR(0)
         ICACT=ICACT+1
      ENDIF
      RETURN
      END
