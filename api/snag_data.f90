module snag_data
    !Collect snag records for each cycle

    use prgprm_mod, only: maxcy1
    use fmparm_mod, only: mxsnag

    implicit none

    !TODO: Can the snag and downwood models be executed without the full FFE routine

    !TODO: Impliment fmparm_mod.f90 as done for tree_data
    !   mxsnag=2000 is in FMPARM, and maxcyc=40 is in PRGPRM

    real, dimension(maxcy1,mxsnag) :: snag_dense_hard, snag_dense_soft, snag_dbh
    integer, dimension(maxcy1,mxsnag) :: snag_spp

    contains

    subroutine copy_snag_data()
        use contrl_mod, only: icyc
        use fmcom_mod, only: sps,dbhs,denih,denis

        implicit none

        snag_spp(icyc+1,:) = sps(:)
        snag_dbh(icyc+1,:) = dbhs(:)
        snag_dense_hard(icyc+1,:) = denih(:)
        snag_dense_soft(icyc+1,:) = denis(:)

    end subroutine copy_snag_data

end module snag_data
