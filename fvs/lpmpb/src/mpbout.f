      SUBROUTINE MPBOUT
      IMPLICIT NONE
C----------
C  **MPBOUT        DATE OF LAST REVISION:  06/14/13
C----------
C
C     PART OF THE MOUNTAIN PINE BEETLE EXTENSION OF PROGNOSIS SYSTEM.
C
C     WRITES FINAL MPB SUMMARY OUTPUT.
C
C Revision History
C   02/08/88 Last noted revision date.
C   07/02/10 Lance R. David (FMSC)
C     Added IMPLICIT NONE.
C----------
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
      INCLUDE 'MPBCOM.F77'
C
C
COMMONS
C
      CHARACTER*133 DSTRNG
      CHARACTER*3 YES
      CHARACTER*2 NO
      
      INTEGER I
      
      DATA YES/'YES'/,NO/'NO'/
 
      IF (.NOT.LMPB1) RETURN
      CALL MPBHED
      WRITE (JOMPB,9015)
 9015 FORMAT (8('-'),' MOUNTAIN PINE BEETLE ACTIVITY SUMMARY ',
     >         8('-'),//,'----- CYCLE -----    ',
     >        'PROBABILITY  WAS THERE AN OUTBREAK',/,
     >        'NUMBER      YEARS    OF OUTBREAK',
     >        '      IN THIS STAND',/,55('-'),/)
 
      DO 120 I = 1,NCYC
      IF(WORKIN(I))WRITE (JOMPB,9016) I,IY(I),IY(I+1),PRBMPB(I),YES
      IF(.NOT.WORKIN(I))WRITE(JOMPB,9016)I,IY(I),IY(I+1),PRBMPB(I),NO
 9016 FORMAT (I3,I7,' -',I5,F15.5,13X,A3)
  120 CONTINUE
      WRITE(JOMPB,*)

      ENDFILE JOMPB

C
C     THE REST OF THE SUBROUTINE COPIES THE MOUNTAIN PINE BEETLE
C     OUTPUT TO THE MAIN PROGNOSIS PRINT FILE (JOSTND).
C
      IF (JOMPB.EQ.JOSTND) RETURN
      REWIND JOMPB
  125 CONTINUE
      READ (JOMPB,130,END=9999) DSTRNG
  130 FORMAT(A133)
      WRITE (JOSTND,130) DSTRNG
      GOTO 125
 9999 CONTINUE
      REWIND JOMPB
      RETURN
      END
