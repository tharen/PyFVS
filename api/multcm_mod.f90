module multcm_mod
    use prgprm_mod, only : maxsp
    !CODE SEGMENT MULTCM
    !----------
    !  **MULTCM DATE OF LAST REVISION:  06/29/00
    !----------
          REAL   XDMULT(MAXSP),XHMULT(MAXSP),XRHMLT(MAXSP),XMMULT(MAXSP), &
                 XRDMLT(MAXSP),XMDIA1(MAXSP),XMDIA2(MAXSP)
    !
          COMMON /MULTCM/ XDMULT,XHMULT,XRHMLT,XMMULT,XRDMLT,XMDIA1,XMDIA2
    !
    !-----END SEGMENT
end module multcm_mod
