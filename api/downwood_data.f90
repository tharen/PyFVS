module downwood_data
    ! Capture carbon estimates at each cycle boundary
    use prgprm_mod, only: maxcy1
    implicit none

    real, dimension(16,maxcy1) :: downwood_summary

    contains

    subroutine copy_downwood(ddw)
        ! Copy down wood estimates for the current cycle
        use contrl_mod, only: icyc

        real :: ddw(*)
        integer :: x

        do x= 1,16
            downwood_summary(x,icyc) = ddw(x)
        end do

    end subroutine copy_downwood

end module downwood_data
