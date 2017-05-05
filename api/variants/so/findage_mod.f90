module findage_mod
    use prgprm_mod
    use contrl_mod, only : jostnd,fast_age_search
    use arrays_mod
    use plot_mod
    
    implicit none
    
    real :: age_steps(8),max_age(maxsp)

    ! FIXME: max_age could be a program level parameter
    data max_age / &
            500., 400., 900., 450., 650., 650., 350., 400., 500., 900., &
            900., 350., 350., 400., 400., 400., 400., 550., 550., 350., &
            350., 100., 100., 100., 100.,  50., 250.,  50.,  75.,  50., &
            50., 400., 100./
            
    ! Predefined binary search age increments
    data age_steps /0.5,0.25,0.125,0.0625,0.0313,0.0156,0.00781,0.00391/

    contains
    
      SUBROUTINE FINDAG(I,ISPC,D1,D2,H,SITAGE,SITHT,AGMAX1,HTMAX1,HTMAX2,DEBUG)
    
!----------
!  **FINDAG--SO  DATE OF LAST REVISION:  08/19/15
!----------
!  THIS ROUTINE FINDS EFFECTIVE TREE AGE BASED ON INPUT VARIABLE(S)
!  CALLED FROM **COMCUP
!  CALLED FROM **CRATET
!  CALLED FROM **HTGF
!----------
!----------
!  DECLARATIONS
!----------
      LOGICAL DEBUG
      INTEGER I,ISPC
      REAL AGMAX(MAXSP),HTMAX(MAXSP)
      REAL AG,DIFF,H,HGUESS,SINDX,TOLER
      REAL SITAGE,SITHT,AGMAX1,HTMAX1,HTMAX2,D1,D2
      REAL AG2,HITE1,HITE2
      REAL HTGR
      character(len=100) :: fmt
!----------
!  DATA STATEMENTS
!----------
      AGMAX = (/ &
       500., 400., 900., 450., 650., 650., 350., 400., 500., 900., &
       900., 350., 350., 400., 400., 400., 400., 550., 550., 350., &
       350., 100., 100., 100., 100.,  50., 250.,  50.,  75.,  50., &
        50., 400., 100./)
!
      HTMAX = (/ &
       165., 160., 180., 180., 150., 150., 130., 165., 180., 175., &
        80., 165., 120., 165., 165.,  85., 175., 165., 165.,  50., &
        50., 100., 100.,  75., 125.,  30.,  75.,  30.,  30.,  20., &
        25., 165., 100./)
!----------
!  INITIALIZATIONS
!----------
      TOLER=2.0
      SINDX = SITEAR(ISPC)
      AGMAX1 = AGMAX(ISPC)
      IF(IFOR.GT.3 .AND. IFOR.LT.10) AGMAX1=400.
      HTMAX1 = HTMAX(ISPC)
      AG = 2.0

!----------
! THE FOLLOWING LINES ARE AN RJ FIX 7-28-88
!----------
      IF(ISPC.EQ.2 .OR. ISPC.EQ.10) AG=(98.38*EXP(SINDX*(-0.0422)))+1.0
      IF(AG .LT. 2.0)AG = 2.0
      IF(ISPC .EQ. 3)AG=18.0
!----------
!  CRATET CALLS FINDAG AT THE BEGINING OF THE SIMULATION TO
!  CALCULATE THE AGE OF INCOMING TREES.  AT THIS POINT ABIRTH(I)=0.
!  THE AGE OF INCOMING TREES HAVING H>=HTMAX1 IS CALCULATED BY
!  ASSUMING A GROWTH RATE OF 0.10FT/YEAR FOR THE INTERVAL H-HTMAX1.
!  TREES REACHING HTMAX1 DURING THE SIMULATION ARE IDENTIFIED IN HTGF.
!----------
      IF(H .GE. HTMAX1) THEN
        SITAGE = AGMAX1 + (H - HTMAX1)/0.10
        SITHT = H
        IF(DEBUG)WRITE(JOSTND,*)' ISPC,AGMAX1,H,HTMAX1= ',ISPC,AGMAX1,H,HTMAX1
        
      ENDIF
!----------
!  DEAL WITH SPECIES THAT DON'T NEED ITEATION HERE:
!
!  COMPUTE HT GROWTH AND AGE FOR ASPEN. EQN FROM WAYNE SHEPPARD RMRS.
!----------
      IF(ISPC.EQ.24)THEN
        SITAGE = (H*2.54*12.0/26.9825)**(1.0/1.1752)
        SITHT = H
        GO TO 30
!----------
!  WESTERN JUNIPER
!  WHITEBARK PINE
!----------
      ELSEIF(ISPC.EQ.11 .OR. ISPC.EQ.16) THEN
        SITAGE = 0.
        SITHT = H
        GO TO 30
      ENDIF

      ! If fast age flag is set call the binary search routine
      if (fast_age_search) then
          call guess_age(sindx,ispc,h,sitht,sitage)
          
      else
        do
! NOTE: This is the original linear search routine
!
          HGUESS = 0.
          IF(DEBUG)WRITE(JOSTND,*)' IN FINDAG, CALLING HTCALC'
          CALL HTCALC(IFOR,SINDX,ISPC,AG,HGUESS,JOSTND,DEBUG)
!
      IF(DEBUG)WRITE(JOSTND,91200)I,IFOR,ISPC,AG,HGUESS,H
91200   FORMAT(' FINDAG I,IFOR,ISPC,AG,HGUESS,H ',3I5,3F10.2)
!
          DIFF = ABS(HGUESS-H)
          IF(DIFF .LE. TOLER .OR. H .LT. HGUESS)THEN
            SITAGE = AG
            SITHT = HGUESS
            exit
          END IF
          AG = AG + 2.

          IF(AG .GT. AGMAX1) THEN
    !----------
    !  H IS TOO GREAT AND MAX AGE IS EXCEEDED
    !----------
            SITAGE = AGMAX1
            SITHT = H
            exit
          ELSE
            cycle
          END IF
!
        end do
   end if
   
      IF(DEBUG)WRITE(JOSTND,50)I,SITAGE,SITHT,AGMAX1,HTMAX1
   50 FORMAT(' LEAVING SUBROUTINE FINDAG  I,SITAGE,SITHT,AGMAX1,', &
      'HTMAX1 = ',I5,4F10.3)
!
      RETURN
      END SUBROUTINE FINDAG

end module findage_mod
