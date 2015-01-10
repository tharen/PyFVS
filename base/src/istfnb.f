      FUNCTION ISTFNB (STRING)
      IMPLICIT NONE
C----------
C  $Id: istfnb.f 767 2013-04-10 22:29:22Z rhavis@msn.com $
C----------
C
C     FIND THE LOCATION OF THE FIRST NON-BLANK CHAR IN A STRING.
C     RETURN A ZERO IF THE ENTIRE STRING IS BLANK.
C
      INTEGER ISTFNB,I
      CHARACTER*(*) STRING
      IF (STRING.EQ.' ') THEN
         ISTFNB=0
      ELSE
         DO 10 I=1,LEN(STRING)
         IF (STRING(I:I).NE.' ') THEN
            ISTFNB=I
            RETURN
         ENDIF
   10    CONTINUE
      ENDIF
      RETURN
      END
