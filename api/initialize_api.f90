module initialize_api
    contains
    subroutine init
        use api_options_mod, only: use_fvs_morts
        implicit none
        !f2py
        use_fvs_morts = .false.
    end subroutine init
end module initialize_api
