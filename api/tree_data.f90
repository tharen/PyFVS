module tree_data
    use prgprm_mod, only: maxcy1,maxtre
    use contrl_mod, only: itrn,icyc
    use plot_mod, only: grospc
    implicit none

    !!TODO: Perhaps the api tree data could be represented by an array
    !       of tree objects type(tree), dimension(maxcy1,maxtre) :: sim_trees
    !FIXME: The array dimensions are backwards and need to be column major
    integer, dimension(maxcy1) :: num_recs
    integer, dimension(maxcy1,maxtre) :: tree_seq,tree_id,plot_seq,age,spp_seq
    real, dimension(maxcy1,maxtre) :: &
            live_tpa,cut_tpa,mort_tpa,live_dbh,dbh_incr,ba_pctl,pnt_bal &
            ,ht_total,ht_merch_cuft,ht_merch_bdft,ht_incr,cr_width,cr_ratio &
            ,cuft_total,cuft_net,bdft_net,defect_cuft,defect_bdft

    ! Flag used to indicate that tree details are to be collected at run time
    logical :: save_tree_data=.true.

    save
    contains

    subroutine init_tree_data()
        num_recs(:) = 0
        tree_seq(:,:) = 0
        tree_id(:,:) = 0
        plot_seq(:,:) = 0
        spp_seq(:,:) = 0
        age(:,:) = 0
        live_tpa(:,:) = 0.0
        mort_tpa(:,:) = 0.0
        cut_tpa(:,:) = 0.0
        live_dbh(:,:) = 0.0
        ba_pctl(:,:) = 0.0
        pnt_bal(:,:) = 0.0
        dbh_incr(:,:) = 0.0
        ht_total(:,:) = 0.0
        ht_merch_cuft(:,:) = 0.0
        ht_merch_bdft(:,:) = 0.0
        ht_incr(:,:) = 0.0
        cuft_total(:,:) = 0.0
        cuft_net(:,:) = 0.0
        bdft_net(:,:) = 0.0
        defect_cuft(:,:) = 0.0
        defect_bdft(:,:) = 0.0
        cr_width(:,:) = 0.0
        cr_ratio(:,:) = 0.0
    end subroutine init_tree_data

    subroutine copy_tree_data()
        use arrays_mod, only: idtree, itre, isp, prob, wk2, wk4, dbh, dg, ht &
                , ht2td, htg, cfv, bfv, wk1, defect, crwdth, icr, pct, abirth
        use varcom_mod, only: ptbalt
        use workcm_mod, only : work1

        integer :: i,x

        ! Increment the array position so that period zero is in the first slot
        i = icyc+1

!        write(*,*) 'Save tree list for cycle ',i,' TPA: ', sum(prob(:itrn)/grospc)

        !copy tree data to the tree_data module
        num_recs(i) = itrn
        tree_seq(i,:itrn) = (/(x, x=1,itrn, 1)/)
        tree_id(i,:itrn) = idtree(:itrn)
        plot_seq(i,:itrn) = itre(:itrn)
        spp_seq(i,:itrn) = isp(:itrn)
        age(i,:itrn) = abirth(:itrn)
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
        mort_tpa(cycle+1,:itrn) = mort(:itrn)/grospc

    end subroutine copy_mort_data

    subroutine copy_cuts_data()
        use arrays_mod, only: wk3
        ! Copy the cut trees
        ! This is triggered by the cuts.f routine before preening the treelist.
        implicit none
        cut_tpa(icyc,:itrn) = wk3(:itrn)/grospc
    end subroutine copy_cuts_data

end module tree_data
