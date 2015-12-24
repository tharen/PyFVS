module fmprop_mod
    use prgprm_mod, only : maxsp
    implicit none
    save
    !----------
    !  $Id: FMPROP.F77 767 2013-04-10 22:29:22Z rhavis@msn.com $
    !----------
    !  **FMPROP  FIRE
    !-----------------------------------------------------------------------
    !  PURPOSE:
    !     COMMON BLOCK VARIABLES RELATED TO THE FATE OF C IN HARVEST PRODUCTS
    !     INITIALIZED THROUGH DATA STATEMENT IN VARIANT **FMCBLK**
    !-----------------------------------------------------------------------
    !

    !     FAPROP(I,J,K,L):
    !       I: 1:2 Region Code (most variants are 1; some have 2,
    !              depending on habitat)
    !       J: 1:101 - Yr 0-100
    !       K: 1:3   - Data; 1=INUSE,2=LANDFILL,3=ENERGY (4=EMITTED defunct)
    !       M: 1:2   - 1=PULP,2=SAWLOG
    !       N: 1:2   - 1=SOFTWOOD, 2=HARDWOOD
    !     BIOGRP(MAXSP): SPECIES GROUPING FOR THE JENKINS EQUATIONS

      REAL     FAPROP(2,101,3,2,2)
      INTEGER  BIOGRP(MAXSP)

end module fmprop_mod
