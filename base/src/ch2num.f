      SUBROUTINE CH2NUM (C2,ICYC)
      IMPLICIT NONE
C----------
C  $Id: ch2num.f 767 2013-04-10 22:29:22Z rhavis@msn.com $
C----------
C
      INTEGER ICYC
      CHARACTER*2 C2
C
C     WRITES A TWO DIGIT INTEGER INTO A CHARACTER*2 STRING.
C
      WRITE (C2,20) ICYC
   20 FORMAT (I2)
      RETURN
      END
