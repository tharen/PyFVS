module pden_mod
    use prgprm_mod, only : maxplt, maxsp
    !CODE SEGMENT PDEN
    !----------
    !  **PDEN   DATE OF LAST REVISION:  11/30/05
    !----------
          REAL          OVER(MAXSP,MAXPLT),BAAA(MAXPLT), &
                        OLDFNT,REGNBK,PCCF(MAXPLT),PTPA(MAXPLT)
    !
    !       PCCF = POINT CCF, LOADED IN **DENSE**.
    !       PTPA = POINT TPA, LOADED IN **DENSE**.
    !     REGNBK = BREAK DBH BETWEEN UNDERSTORY AND OVERSTORY TREES.
    !              SET IN BLKDAT, USED DENSE, ESTAB, ESFLTR.
    !-----END SEGMENT
end module pden_mod
