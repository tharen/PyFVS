      SUBROUTINE MPBCUP
      use contrl_mod
      use plot_mod
      use prgprm_mod
      implicit none
C----------
C  **MPBCUP        DATE OF LAST REVISION:  07/02/10
C----------
C
C     INTERFACING PROGRAM TO CALL EITHER MPBDRV OR COLDRV.
C
      INCLUDE 'MPBCOM.F77'
C
C Revision History
C   02/08/88 Last noted revision date.
C   07/02/10 Lance R. David (FMSC)
C----------
C
C************************* EXECUTION BEGINS ***************************
C
      IF (LPOPDY) THEN
         CALL MPBDRV
      ELSE
         CALL COLDRV
      ENDIF
      RETURN
      END
