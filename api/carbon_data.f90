module carbon_data
    ! Capture carbon estimates at each cycle boundary
    use prgprm_mod, only: maxcy1
    implicit none

    real, dimension(17,maxcy1) :: carbon_summary

    contains

    subroutine copy_forest_carbon(carb)
        ! Copy stand carbon estimates for the current cycle
        use contrl_mod, only: icyc

        real :: carb(*)
        integer :: x

        do x= 1,11
            carbon_summary(x,icyc) = carb(x)
        end do

    end subroutine copy_forest_carbon

    subroutine copy_harvest_carbon(carb)
        ! Copy harvested carbon estimates for the current cycle
        use contrl_mod, only: icyc

        real :: carb(*)
        integer :: x

        do x= 1,6
            carbon_summary(11+x,icyc) = carb(x)
        end do

    end subroutine copy_harvest_carbon

end module carbon_data
