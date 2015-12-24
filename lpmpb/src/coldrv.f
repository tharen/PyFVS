      SUBROUTINE COLDRV
      use contrl_mod
      use plot_mod
      use prgprm_mod
      implicit none
C----------
C  **COLDRV        DATE OF LAST REVISION:  07/02/10
C----------
C
C     PART OF THE MOUNTAIN PINE BEETLE EXTENSION OF PROGNOSIS.
C
C     THIS MODULE DRIVES ROUTINES THAT KILL LODGEPOLE PINE
C     USING COLE'S MODEL (1983).
C
C Revision History
C   02/08/88 Last noted revision date.
C   07/02/10 Lance R. David (FMSC)
C----------
C
      INCLUDE 'COLCOM.F77'
C
      INCLUDE 'MPBCOM.F77'
C
      CALL COLDBH
      CALL COLMOD (IFINT,ICYC,IY(ICYC),NPLT)
      CALL COLMRT
      RETURN
      END
