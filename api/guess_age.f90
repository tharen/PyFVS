subroutine guess_age(site_idx,spp_idx,height,site_height,site_age)
    ! Find the site_age that results in a site_height equivalent to
    ! the given tree height.
    ! Implemented as a binary search of fixed length.  Height equivalence
    ! is assumed for the final age step in the module variable age_steps.
    
    use findage_mod, only : max_age, age_steps
    
    implicit none

    !f2py intent(in) :: site_idx,spp_idx,height
    !f2py intent(out) :: site_height,site_age

    ! Args
    real :: site_idx,height,site_height,site_age
    integer :: spp_idx

    ! Locals
    integer :: i
    real :: check_age

    site_height = 0.0
    site_age = 0.0

    ! Check the lower age bound
    check_age = 2.0
!        call get_siteht(site_idx,spp_idx,check_age,site_height)
    call htcalc(site_idx,spp_idx,check_age,site_height,0,0)
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
    call htcalc(site_idx,spp_idx,check_age,site_height,0,0)
    if (height>=site_height) then
!            write (*,*) 'Ht above range: ', 'age:',check_age &
!                    ,'si:',site_idx,'spp:',spp_idx,'hgt:',height,'hgt^:',site_height
        site_age = check_age
        site_height = height
        return
    end if

    ! Fixed length binary search to find the age of equivalent height
    check_age = max_age(spp_idx) - age_steps(1)
    do i=2,size(age_steps)
!            call get_siteht(site_idx,spp_idx,check_age,site_height)
        call htcalc(site_idx,spp_idx,check_age,site_height,0,0)
        if (site_height > height) then
            check_age = check_age - age_steps(i)
        else
            check_age = check_age + age_steps(i)
        end if

    end do

    site_age = check_age

end subroutine guess_age
