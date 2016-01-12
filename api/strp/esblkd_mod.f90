module esblkd_mod
    contains
    subroutine ESBLKD()
        use eshap_mod
        use prgprm_mod
        use esrncm_mod
        implicit none
        !----------
        !   **ESBLKD--STRP   DATE OF LAST REVISION:   09/17/08
        !----------
        JOREGT = 17
        ESS0 = 55329D0
        ESSS = 55329.0
    end subroutine ESBLKD
end module esblkd_mod
