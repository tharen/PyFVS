subroutine step_tregro()
    ! Replaces TREGRO for the step API
    use snag_data, only: copy_snag_data

    LOGICAL LTMGO,LMPBGO,LDFBGO,LBWEGO,LCVATV,LBGCGO,DEBUG
    INTEGER :: ISTOPRES,IRTNCD,ISTOPDONE
    LTMGO=.FALSE.
    LMPBGO=.FALSE.
    LDFBGO=.FALSE.
    LBWEGO=.FALSE.
    LCVATV=.FALSE.
    LBGCGO=.FALSE.

    DEBUG=.FALSE.

    !CALL GRINCR TO COMPUTE INCREMENTS AND SEE IF BUG MODELS ARE ACTIVE.
    CALL GRINCR (DEBUG,1,LTMGO,LMPBGO,LDFBGO,LBWEGO,LCVATV,LBGCGO)
    !call step_grincr(debug,1,ltmgo,lmpbgo,ldfbgo,lbwego,lcvatv,lbgcgo)

    ! Copy snag records prior to mortality estimation for this cycle
    ! This captures mortality from the previous cycle, which influenced the
    ! current growth increment
    call copy_snag_data()

    !CALL GRADD TO COMPUTE BUGS AND ADD THE INCREMENTS.
    CALL GRADD (DEBUG,1,LTMGO,LMPBGO,LDFBGO,LBWEGO,LCVATV,LBGCGO)

end subroutine step_tregro
