      SUBROUTINE RDSADD(I,TP)
      use contrl_mod
      use arrays_mod
      use prgprm_mod
      implicit none
C----------
C  **RDSADD      LAST REVISION:  09/02/14
C----------
C
C  ADD TO THE STUMP LIST.
C
C  CALLED BY :
C     RDSTR   [ROOT DISEASE]
C
C  CALLS     :
C     RDSSIZ  (SUBROUTINE)  [ROOT DISEASE]
C     RDSTP   (SUBROUTINE)  [ROOT DISEASE]
C     RDDBUG  (SUBROUTINE)  [ROOT DISEASE]
C
C  PARAMETERS :
C     I      - (I ) Tree record number.
C     TP     - (I ) Proportion of trees in current tree record not
C                   cut by (Prognosis) subroutine CUTS.
C
C  COMMON BLOCK VARIABLES :
C
C  LOCAL VARIABLES :
C     DEN
C     JJ
C
C  Revision History :
C   04/07/93 - Last revision date.
C   09/02/14 Lance R. David (FMSC)
C
C----------------------------------------------------------------------
C
C.... PARAMETER INCLUDE FILES
C
      INCLUDE 'RDPARM.F77'
C
C.... COMMON INCLUDE FILES
C
      INCLUDE 'RDCOM.F77'
      INCLUDE 'RDARRY.F77'
      INCLUDE 'RDADD.F77'
C
C.... Local variables
C
      INTEGER I, ISL, JJ
      REAL TP, DEN

      DEN = PROBIT(I) * (1.0 - TP)
      JJ = ISP(I)

      CALL RDSSIZ (JJ,DBH(I),STCUT,ISL,ISPS,IRTSPC)
      CALL RDSTP  (ISL,JJ,DEN,DBH(I),ROOTL(I))

      RETURN
      END
