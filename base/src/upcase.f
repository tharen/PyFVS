      SUBROUTINE UPCASE (C)
      IMPLICIT NONE
C----------
C  $Id: upcase.f 767 2013-04-10 22:29:22Z rhavis@msn.com $
C----------
      INTEGER IP
      CHARACTER C
      CHARACTER*26 UPPER,LOWER
      DATA UPPER /'ABCDEFGHIJKLMNOPQRSTUVWXYZ'/
      DATA LOWER /'abcdefghijklmnopqrstuvwxyz'/
      IP=INDEX(LOWER,C)
      IF (IP.GT.0) C=UPPER(IP:IP)
      RETURN
      END
