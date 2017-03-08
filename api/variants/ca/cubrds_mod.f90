module cubrds_mod
    contains
    subroutine CUBRDS()
        use volstd_mod
        use prgprm_mod
        implicit none
        
        integer :: k
        
        !----------
        !  **CUBRDS--CA    DATE OF LAST REVISION:  02/22/08
        !----------
        !  DEFAULT PARAMETERS FOR THE CUBIC AND BOARD FOOT VOLUME EQUATIONS.
        !----------
        !OMMONS
        !----------
        !  COEFFICIENTS FOR CUBIC FOOT VOLUME FOR TREES THAT ARE SMALLER THAN
        !  THE TRANSITION SIZE
        !----------
        CFVEQS(:,:) = reshape((/(0.0,k=1,343)/),(/7,maxsp/))
        !----------
        !  COEFFICIENTS FOR CUBIC FOOT VOLUME FOR TREES THAT ARE LARGER THAN
        !  THE TRANSITION SIZE
        !----------
        CFVEQL(:,:) = reshape((/(0.0001,k=1,343)/),(/7,maxsp/))
        !----------
        !  FLAG IDENTIFYING THE SIZE TRANSITION VARIABLE; 0=D, 1=D2H
        !----------
        ICTRAN(:) = (/(0,k=1,49)/)
        !----------
        !  TRANSITION SIZE.  TREES OF LARGER SIZE (D OR D2H) WILL COEFFICIENTS
        !  FOR LARGER SIZE TREES.
        !----------
        CTRAN(:) = (/(0.0,k=1,49)/)
        !----------
        !  COEFFICIENTS FOR BOARD FOOT VOLUME FOR TREES THAT ARE SMALLER THAN
        !  THE TRANSITION SIZE
        !----------
        BFVEQS(:,:) = reshape((/(1.0,k=1,343)/),(/7,maxsp/))
        !----------
        !  COEFFICIENTS FOR BOARD FOOT VOLUME FOR TREES THAT ARE LARGER THAN
        !  THE TRANSITION SIZE
        !----------
        BFVEQL(:,:) = reshape((/(1.0,k=1,343)/),(/7,maxsp/))
        !----------
        !  FLAG IDENTIFYING THE SIZE TRANSITION VARIABLE; 0=D, 1=D2H
        !----------
        IBTRAN(:) = (/(0,k=1,49)/)
        !----------
        !  TRANSITION SIZE.  TREES OF LARGER SIZE (D OR D2H) WILL USE COEFFICIENTS
        !  FOR LARGER SIZE TREES.
        !----------
        BTRAN(:) = (/(20.5,k=1,49)/)

    end subroutine cubrds
end module cubrds_mod
