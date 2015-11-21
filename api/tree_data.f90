module tree_data
    ! Provides multidimensional arrays and subroutines for collecting
    ! tree level attributes for an entire simulation

    use prgprm_mod, only: maxcy1,maxtre
    use contrl_mod, only: itrn,icyc
    use plot_mod, only: grospc
    implicit none

    !!TODO: Perhaps the api tree data could be represented by an array
    !       of tree objects type(tree), dimension(maxcy1,maxtre) :: sim_trees
    !FIXME: The array dimensions are backwards and need to be column major
    integer, dimension(maxcy1) :: num_recs
    integer, dimension(maxtre,maxcy1) :: tree_seq,tree_id,plot_seq,age,spp_seq
    real, dimension(maxtre,maxcy1) :: &
            live_tpa,cut_tpa,mort_tpa,live_dbh,dbh_incr,ba_pctl,pnt_bal &
            ,ht_total,ht_merch_cuft,ht_merch_bdft,ht_incr,cr_width,cr_ratio &
            ,cuft_total,cuft_net,bdft_net,defect_cuft,defect_bdft

    ! Flag used to indicate that tree details are to be collected at run time
    logical :: save_tree_data=.true.

    save
    contains

    subroutine init_tree_data()
        ! Initialize the tree attribute arrays to zero
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
        ! Populate the live tree attributes for the beginning of this cycle
        use arrays_mod, only: idtree, itre, isp, prob, dbh, dg, ht &
                , ht2td, htg, cfv, bfv, wk1, defect, crwdth, icr, pct, abirth
        use varcom_mod, only: ptbalt
        use workcm_mod, only : work1

        integer :: i,x

        ! Increment the array position so that period zero is in the first slot
        i = icyc

!        write(*,*) 'Save tree list for cycle ',i,' TPA: ', sum(prob(:itrn)/grospc)

        !copy tree data to the tree_data module
        num_recs(i) = itrn
        tree_seq(:itrn,i) = (/(x, x=1,itrn, 1)/)
        tree_id(:itrn,i) = idtree(:itrn)
        plot_seq(:itrn,i) = itre(:itrn)
        spp_seq(:itrn,i) = isp(:itrn)
        age(:itrn,i) = int(abirth(:itrn))
        live_tpa(:itrn,i) = prob(:itrn)/grospc

!        ! Mortality records are tripled before the grow routine returns
!        if (icyc>0) mort_tpa(:itrn,i) = wk2(:itrn)/grospc

        !cut tpa is copied in grincr.f.
!        if (icyc>0) cut_tpa(:itrn,i) = wk4(:itrn)/grospc

        live_dbh(:itrn,i) = dbh(:itrn)
        ba_pctl(:itrn,i) = pct(:itrn)
        pnt_bal(:itrn,i) = ptbalt(:itrn)
        dbh_incr(:itrn,i) = dg(:itrn)
        if (icyc==0) dbh_incr(:itrn,i) = work1(:itrn)
        ht_total(:itrn,i) = ht(:itrn)
        ht_merch_cuft(:itrn,i) = ht2td(:itrn, 1)
        ht_merch_bdft(:itrn,i) = ht2td(:itrn, 2)
        ht_incr(:itrn,i) = htg(:itrn)

        cuft_total(:itrn,i) = cfv(:itrn)
        cuft_net(:itrn,i) = wk1(:itrn)
        bdft_net(:itrn,i) = bfv(:itrn)

        defect_cuft(:itrn,i) = (defect(:itrn) - mod(defect(:itrn),100)) / 10000.0
        defect_bdft(:itrn,i) = mod(defect(:itrn),100) / 100.0

        cr_width(:itrn,i) = crwdth(:itrn)
        cr_ratio(:itrn,i) = icr(:itrn)

    end subroutine copy_tree_data

    subroutine copy_mort_data()
        use arrays_mod, only: wk2
        implicit none

        ! Mortality estimates copied after the call to `morts` in grincr.f
        mort_tpa(:itrn,icyc+1) = wk2(:itrn)/grospc

    end subroutine copy_mort_data

    subroutine copy_cuts_data()
        ! Copy the cut trees
        ! Call from GRINCR right after the call to CUTS
        use arrays_mod, only: wk3
        implicit none

        cut_tpa(:itrn,icyc+1) = wk3(:itrn)/grospc

    end subroutine copy_cuts_data

end module tree_data
