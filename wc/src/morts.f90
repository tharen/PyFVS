subroutine morts
! Replaces the base morts routine to switch between the original FVS
! mortality routines and the ported ORGANON routines.

    use contrl_mod, only: use_fvs_morts
    implicit none

    if (use_fvs_morts) then
        call morts_fvs
    else
        call morts_organon
    end if
    
end subroutine morts

subroutine morcon
    use contrl_mod, only: use_fvs_morts
    implicit none

    if (use_fvs_morts) then
        call morcon_fvs
    else
        call morcon_organon
    end if
end subroutine morcon