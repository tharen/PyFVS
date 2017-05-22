module findage_mod
    use prgprm_mod
    use contrl_mod
    use arrays_mod
    use plot_mod
    implicit none

    real :: age_steps(8),max_age(maxsp)

    ! FIXME: max_age could be a program level parameter
    data max_age / maxsp*200. /
    
    ! Predefined binary search age increments
    data age_steps /0.5,0.25,0.125,0.0625,0.0313,0.0156,0.00781,0.00391/
    
    contains

    SUBROUTINE FINDAG(I,ISPC,D1,D2,H,SITAGE,SITHT,AGMAX,HTMAX1,HTMAX2,DEBUG)

!----------
!  **FINDAG--CA  DATE OF LAST REVISION:  01/12/11
!----------
!  THIS ROUTINE FINDS EFFECTIVE TREE AGE BASED ON INPUT VARIABLE(S)
!  CALLED FROM ***COMCUP
!  CALLED FROM ***CRATET
!  CALLED FROM ***HTGF
!  CALLS ***HTCALC
!----------
!  COMMONS
!
!  DECLARATIONS
!----------
      INTEGER I,ISPC
      LOGICAL DEBUG
      REAL AGEMAX(MAXSP),AGMAX,AG,DIFF,H,HGUESS,SINDX,TOLER
      REAL SITAGE,SITHT,D1,D2,HTMAX1,HTMAX2
!----------
!  DATA STATEMENTS
!----------
      DATA AGEMAX/ MAXSP*200. /
!----------
!  INITIALIZATIONS
!----------
      TOLER=2.0
      SINDX = SITEAR(ISPC)
      AGMAX=AGEMAX(ISPC)
      IF(IFOR .LE. 5) AGMAX=400.
      AG = 2.0
      
      if (fast_age_search) then
        call guess_age(sindx,ispc,h,sitht,sitage)
      else

!----------
! R5 USE DUNNING/LEVITAN SITE CURVE.
! R6 USE **HTCALC** SITE CURVES.
! SPECIES DIFFERENCES ARE ARE ACCOUNTED FOR BY THE SPECIES
! SPECIFIC SITE INDEX VALUES WHICH ARE SET AFTER KEYWORD PROCESSING.
!----------
   75 CONTINUE
!
          HGUESS = 0.
          CALL HTCALC(IFOR,SINDX,ISPC,AG,HGUESS,JOSTND,DEBUG)
    !
      IF(DEBUG)WRITE(JOSTND,91200)I,IFOR,AG,HGUESS,H
91200 FORMAT(' IN GUESS AN AGE--I,IFOR,AGE,HGUESS,H ',2I5,3F10.2)
    !
          DIFF=ABS(HGUESS-H)
          IF(DIFF .LE. TOLER .OR. H .LT. HGUESS)THEN
            SITAGE = AG
            SITHT = HGUESS
        GO TO 30
          END IF
          AG = AG + 2.
    !
          IF(AG .GT. AGMAX) THEN
    !----------
    !  H IS TOO GREAT AND MAX AGE IS EXCEEDED
    !----------
            SITAGE = AGMAX
            SITHT = H
        GO TO 30
          ELSE
        GO TO 75
          ENDIF
!
   30 CONTINUE
      end if

      IF(DEBUG)WRITE(JOSTND,50)I,SITAGE,SITHT
   50 FORMAT(' LEAVING SUBROUTINE FINDAG  I,SITAGE,SITHT =', &
      I5,2F10.3)
!
      RETURN

      END SUBROUTINE FINDAG

end module findage_mod