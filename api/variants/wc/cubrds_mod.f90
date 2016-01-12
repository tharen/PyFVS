module cubrds_mod
    contains
    subroutine CUBRDS()
        use volstd_mod
        !      use prgprm_mod
        implicit none
        !----------
        !  **CUBRDS--WC    DATE OF LAST REVISION:  05/19/08
        !----------
        !  DEFAULT PARAMETERS FOR THE CUBIC AND BOARD FOOT VOLUME EQUATIONS.
        !
        integer :: k

        !  COEFFICIENTS FOR CUBIC FOOT VOLUME FOR TREES THAT ARE SMALLER THAN
        !  THE TRANSITION SIZE
        !----------
        CFVEQS = 0.0
        !----------
        !  COEFFICIENTS FOR CUBIC FOOT VOLUME FOR TREES THAT ARE LARGER THAN
        !  THE TRANSITION SIZE
        !----------
        CFVEQL = 0.0
        !----------
        !  FLAG IDENTIFYING THE SIZE TRANSITION VARIABLE; 0=D, 1=D2H
        !----------
        ICTRAN = 0
        !----------
        !  TRANSITION SIZE.  TREES OF LARGER SIZE (D OR D2H) WILL COEFFICIENTS
        !  FOR LARGER SIZE TREES.
        !----------
        CTRAN = 0.0
        !----------
        !  COEFFICIENTS FOR BOARD FOOT VOLUME FOR TREES THAT ARE SMALLER THAN
        !  THE TRANSITION SIZE
        !----------
        BFVEQS = 0.0
        !----------
        !  COEFFICIENTS FOR BOARD FOOT VOLUME FOR TREES THAT ARE LARGER THAN
        !  THE TRANSITION SIZE
        !----------
        BFVEQL = 0.0
        !----------
        !  FLAG IDENTIFYING THE SIZE TRANSITION VARIABLE; 0=D, 1=D2H
        !----------
        IBTRAN = 0
        !----------
        !  TRANSITION SIZE.  TREES OF LARGER SIZE (D OR D2H) WILL USE COEFFICIENTS
        !  FOR LARGER SIZE TREES.
        !----------
        BTRAN = 0.0
    end subroutine cubrds
end module cubrds_mod
