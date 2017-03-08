module estcor_mod
    use prgprm_mod
    implicit none
!ODE SEGMENT ESTCOR
!----------
!  **ESTCOR DATE OF LAST REVISION:  06/14/00
!----------
      REAL HCOR(MAXSP)
      COMMON /ESTCOR/ HCOR
!-----END SEGMENT
end module estcor_mod