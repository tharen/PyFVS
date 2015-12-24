module snag_data
    ! Arrays and routines to capture snag records for each cycle.

    !TODO: Can the snag and downwood models be executed without the full FFE routine

    use prgprm_mod, only: maxcy1
    use fmparm_mod, only: mxsnag

    implicit none

    real, dimension(maxcy1,mxsnag) :: hard_density, soft_density &
            , dead_dbh, hard_ht, soft_ht, hard_vol, soft_vol
    integer, dimension(maxcy1,mxsnag) :: spp_seq, year_dead
    integer, dimension(maxcy1) :: num_recs

    contains

    subroutine copy_snag_data()
        ! Copy snag data to the module data arrays for the current cycle
        ! Called from fvs_step.tree_grow prior to updating the growth and
        ! mortality increments to capture the prior end of cycle snag list.

        !NOTE: The use of `grospc` to scale the snag density is in conflict
        !   with the canned snag reports.  This may be a bug in the FVS reports.

        use contrl_mod, only: icyc
        use plot_mod, only: grospc
        use fmcom_mod, only: sps,dbhs,denih,denis,htis,htih,nsnag,hard,yrdead

        implicit none

        integer :: x
        real :: tempv

        num_recs(icyc) = nsnag
        spp_seq(icyc,:nsnag) = sps(:nsnag)
        dead_dbh(icyc,:nsnag) = dbhs(:nsnag)
        year_dead(icyc,:nsnag) = yrdead(:nsnag)

        do x = 1,nsnag
            if (denis(x) > 0.0) then
                ! soft snags are always soft
                soft_density(icyc,x) = denis(x) / grospc
                soft_ht(icyc,x) = htis(x)
                call fmsvol (x, htis(x),tempv,.false.,0)
                soft_vol(icyc,x) = tempv * denis(x) / grospc

            elseif (hard(x)) then
                ! hard snags depend on the initial hard ratio
                hard_density(icyc,x) = denih(x) / grospc
                hard_ht(icyc,x) = htih(x)
                call fmsvol (x, htih(x),tempv,.false.,0)
                hard_vol(icyc,x) = tempv * denih(x) / grospc

            else
                soft_density(icyc,x) = denih(x) / grospc
                soft_ht(icyc,x) = htih(x)
                call fmsvol (x, htih(x),tempv,.false.,0)
                soft_vol(icyc,x) = tempv * denih(x) / grospc
            end if
        end do

    end subroutine copy_snag_data

end module snag_data
