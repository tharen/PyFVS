subroutine morts
    use contrl_mod, only: use_fvs_morts
    implicit none

    if use_fvs_morts==.true. then
        call morts_fvs
    else
        call morts_organon
    end if

end subroutine morts
