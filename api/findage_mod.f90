module findage_mod
    use prgprm_mod
    use contrl_mod
    use arrays_mod
    use plot_mod

    use siteht_mod, only: get_siteht

    implicit none

    real :: age_steps(6),max_age(maxsp)

    ! FIXME: max_age could be a program level parameter
    data max_age / maxsp*200. /
    ! Predefined binary search age increments
    ! Assumes max_age is always 200
    !data age_steps /50.0,25.0,12.5,6.125,3.0625,1.53125/ !,0.765625/
    data age_steps /50.0,26.0,14.0,8.0,4.0,2.0/

    contains

    SUBROUTINE FINDAG(I,ISPC,D,D2,H,SITAGE,SITHT,AGMAX1,HTMAX,HTMAX2,DEBUG)
        ! Estimate the age of a tree using species site curves
        ! Replaces findag.f

        implicit none
        !----------
        !  **FINDAG--WC  DATE OF LAST REVISION:  01/12/11
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
        INTEGER I,ISPC,MAPPHD,MAPHD(MAXSP)
        REAL AG,DIFF,H,HGUESS,SINDX,TOLER
        REAL HDRAT1(8),HDRAT2(8)
        REAL SITAGE,SITHT,AGMAX1,HTMAX,HTMAX2,D,D2
        !----------
        !  DATA STATEMENTS
        !----------

        DATA MAPHD  /3*1,2*2,6,2,2*3,2,4,4*5,2*6,3,2*7,8*8,5*3,4*8,2*6/
        DATA HDRAT1 /4.3396271,4.3149844,3.2412923,2.3475244, &
                5.5324838,6.3657425,4.0156013,3.9033821/
        DATA HDRAT2 /43.9957174,39.6317079,62.7139427,65.7622908, &
                18.6043842,16.2223589,51.9732476,59.3370816/
        !----------
        !  INITIALIZATIONS
        !----------
        SINDX = SITEAR(ISPC)
        AGMAX1 = max_age(ISPC)
        MAPPHD = MAPHD(ISPC)
        HTMAX = HDRAT1(MAPPHD)*D+HDRAT2(MAPPHD)
        HTMAX2 = HDRAT1(MAPPHD)*D2+HDRAT2(MAPPHD)
        !----------
        !  CRATET CALLS FINDAG AT THE BEGINING OF THE SIMULATION TO
        !  CALCULATE THE AGE OF INCOMMING TREES.  AT THIS POINT ABIRTH(I)=0.
        !  THE AGE OF INCOMMING TREES HAVING H>=HMAX IS CALCULATED BY
        !  ASSUMEING A GROWTH RATE OF 0.10FT/YEAR FOR THE INTERVAL H-HMAX.
        !  TREES REACHING HMAX DURING THE SIMULATION ARE IDENTIFIED IN HTGF.
        !----------
        IF(H .GE. HTMAX) THEN
            SITAGE = AGMAX1 + (H - HTMAX)/0.10
            SITHT = H
            if (DEBUG) then
                WRITE(JOSTND,*)' H,HTMAX,AGMAX1,SITAGE,SITHT= ', &
                        H,HTMAX,AGMAX1,SITAGE,SITHT
            end if
            return
        ENDIF

!
!   75 CONTINUE
!C----------
!C  CALL HTCALC TO CALCULATE POTENTIAL HT GROWTH
!C----------
!      HGUESS = 0.0
!!      CALL HTCALC(SINDX,ISPC,AG,HGUESS,JOSTND,DEBUG)
!      call get_siteht(sindx,ispc,ag,hguess)
!C
!      IF(DEBUG)WRITE(JOSTND,91200)AG,HGUESS,H
!91200 FORMAT(' IN GUESS AN AGE--AGE,HGUESS,H ',3F10.2)
!C
!      DIFF=ABS(HGUESS-H)
!      IF(DIFF .LE. TOLER .OR. H .LT. HGUESS)THEN
!        SITAGE = AG
!        SITHT = HGUESS
!        GO TO 30
!      END IF
!      AG = AG + 2.
!C
!      IF(AG .GT. AGMAX1) THEN
!C----------
!C  H IS TOO GREAT AND MAX AGE IS EXCEEDED
!C----------
!        SITAGE = AGMAX1
!        SITHT = H
!        GO TO 30
!      ELSE
!        GO TO 75
!      ENDIF
!C

        call guess_age(sindx,ispc,h,sitht,sitage)

   30   continue
        if (debug) then
            WRITE(JOSTND,50)I,SITAGE,SITHT
   50       FORMAT(' LEAVING SUBROUTINE FINDAG  I,SITAGE,SITHT =', &
                    I5,2F10.3)
        end if

        return
    end subroutine findag

    subroutine guess_age(site_idx,spp_idx,height,site_height,site_age)
        ! Use a binary search to find the equivalent site age
        ! Adapted from http://rosettacode.org/wiki/Binary_search#Fortran
        ! Use an array pointer to split the search range at each step.
        ! TODO: Implement a leftmost search

        use siteht_mod, only: get_siteht
        implicit none

        !f2py intent(in) :: site_idx,spp_idx,height
        !f2py intent(out) :: site_height,site_age

        ! Args
        real :: site_idx,height,site_height,site_age
        integer :: spp_idx

        ! Locals
        integer :: i, check_age

        site_height = 0.0
        site_age = 0.0

        ! Check the lower age bound
        check_age = 2.0
!        call get_siteht(site_idx,spp_idx,age_ptr(1),site_height)
        call htcalc(site_idx,spp_idx,real(check_age),site_height,0,0)
        if (height<=site_height) then
!            write (*,*) 'Ht below range: ', 'age:',check_age,'si:',site_idx &
!                    ,'spp:',spp_idx,'hgt:',height,'hgt^:',site_height
            site_age = check_age
            ! FIXME: is this the original intent
            site_height = height
            return
        end if

        ! Check the upper age bound
        check_age = max_age(spp_idx)
!        call get_siteht(site_idx,spp_idx,check_age,site_height)
        call htcalc(site_idx,spp_idx,real(check_age),site_height,0,0)
        if (height>=site_height) then
!            write (*,*) 'Ht above range: ', 'age:',check_age &
!                    ,'si:',site_idx,'spp:',spp_idx,'hgt:',height,'hgt^:',site_height
            site_age = max_age(spp_idx)
            site_height = height
            return
        end if

        ! Loop through the age increments to narrow in on the current height
        check_age = max_age(spp_idx) / 2
        do i=1,size(age_steps)
!            call get_siteht(site_idx,spp_idx,check_age,site_height)
            call htcalc(site_idx,spp_idx,real(check_age),site_height,0,0)
            if (site_height > height) then
                check_age = check_age - age_steps(i)
            elseif (site_height < height) then
                check_age = check_age + age_steps(i)
            else
                exit
            end if
        end do

        site_age = check_age

    end subroutine guess_age

end module findage_mod
