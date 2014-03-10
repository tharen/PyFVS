module tree_data
    use prgprm_mod
    use contrl_mod, only: itrn,icyc
    implicit none

    !!TODO: Perhaps the api tree data could be represented by an array
    !       of tree objects type(tree), dimension(maxcy1,maxtre) :: sim_trees
    integer, dimension(maxcy1,maxtre) :: tree_seq,tree_id,plot_seq,age,spp_seq
    integer, dimension(maxcy1) :: num_recs
    real, dimension(maxcy1,maxtre) :: live_tpa,cut_tpa,mort_tpa
    real, dimension(maxcy1,maxtre) :: live_dbh,dbh_incr,ba_pctl,pnt_bal
    real, dimension(maxcy1,maxtre) :: ht_total,ht_merch_cuft,ht_merch_bdft,ht_incr
    real, dimension(maxcy1,maxtre) :: cr_width,cr_ratio
    real, dimension(maxcy1,maxtre) :: cuft_total,cuft_net,bdft_net &
            ,defect_cuft,defect_bdft

    save

    contains

    subroutine copy_tree_data()
        !use contrl_mod, only: itrn
        use arrays_mod, only: idtree, itre, isp, prob, wk2, wk4, dbh, dg, ht &
                , ht2td, htg, cfv, bfv, wk1, defect, crwdth, icr, pct
        use plot_mod, only: grospc
        use varcom_mod, only: ptbalt
        use workcm_mod, only : work1
        integer :: i,x

        !TODO: Is it faster to copy only up to itrn
        !           eg. live_dbh(i,:) = dbh(:)

        !offset tree data so that cycle 0 data is stored in the first slot
        i = icyc+1

        !write(*,*) 'GROWING SPACE FACTOR:',grospc

        !copy tree data to the tree_data module
        num_recs(i) = itrn
        tree_seq(i,:itrn) = (/(x, x=1,itrn, 1)/)
        tree_id(i,:itrn) = idtree(:itrn)
        plot_seq(i,:itrn) = itre(:itrn)
        spp_seq(i,:itrn) = isp(:itrn)
        live_tpa(i,:itrn) = prob(:itrn)/grospc

        ! Mortality records are tripled before the grow routine returns
        if (icyc>0) mort_tpa(i,:itrn) = wk2(:itrn)/grospc

        !cut tpa is copied in grincr.f.
!        if (icyc>0) cut_tpa(i,:itrn) = wk4(:itrn)/grospc

        live_dbh(i,:itrn) = dbh(:itrn)
        ba_pctl(i,:itrn) = pct(:itrn)
        pnt_bal(i,:itrn) = ptbalt(:itrn)
        dbh_incr(i,:itrn) = dg(:itrn)
        if (icyc==0) dbh_incr(i,:itrn) = work1(:itrn)
        ht_total(i,:itrn) = ht(:itrn)
        ht_merch_cuft(i,:itrn) = ht2td(:itrn, 1)
        ht_merch_bdft(i,:itrn) = ht2td(:itrn, 2)
        ht_incr(i,:itrn) = htg(:itrn)

        cuft_total(i,:itrn) = cfv(:itrn)
        cuft_net(i,:itrn) = wk1(:itrn)
        bdft_net(i,:itrn) = bfv(:itrn)

        defect_cuft(i,:itrn) = (defect - mod(defect,100)) / 10000.0
        defect_bdft(i,:itrn) = mod(defect,100) / 100.0

        cr_width(i,:itrn) = crwdth(:itrn)
        cr_ratio(i,:itrn) = icr(:itrn)

    end subroutine copy_tree_data

    subroutine copy_mort_data(cycle,mort)
        implicit none

        integer :: cycle
        real, dimension(maxtre) :: mort

        ! Mortality estimates copied after the call to `morts` in grincr.f
        !  tripling occurs right after call morts, so maybe "untripling"
        !  would be OK to bring this back out of the FVS guts.
        mort_tpa(cycle+1,:) = mort(:) !wk2(:)/grospc

    end subroutine copy_mort_data

    subroutine copy_cuts_data(cycle,cut)
        implicit none

        integer :: cycle
        real, dimension(maxtre) :: cut

        !!NOTE: cut trees are still in wk4 after the second evmon call in grincr.f
        !!      wk3 is modified by then
        cut_tpa(cycle,:) = cut(:)

    end subroutine copy_cuts_data

end module tree_data
