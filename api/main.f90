      PROGRAM MAIN
      use siteht_mod
      use plot_mod, only: sitear
      IMPLICIT NONE

!----------
!  $Id: main.f 829 2013-05-08 20:55:27Z rhavis@msn.com $
!----------
      INTEGER rtnCode,lenCL,i
!
!     PROCSS THE COMMAND LINE. Passing an empty string signals that the
!     real command line arguments will be fetched.
!
      do i = 1,1

      lenCl = 0
      CALL fvsSetCmdLine(' ',lenCL,rtnCode)
      IF (rtnCode.NE.0) GOTO 10

!     RUN ALL THE CYCLES and STANDS--unless there is a stop point!

      DO
        CALL FVS(rtnCode)
        IF (rtnCode .NE. 0) exit
      ENDDO

      ![6, 16, 18, 19, 22]
      write(*,*) 'lookup: ',lu_tally(6),lu_tally(16) &
            ,lu_tally(18),lu_tally(19),lu_tally(22)
      write(*,*) 'failover: ',fail_over(6),fail_over(16) &
            ,fail_over(18),fail_over(19),fail_over(22)

      write(*,*) 'site: ', sitear(6),sitear(16),sitear(18) &
            ,sitear(19),sitear(22)

      lu_tally(:) = 0
      fail_over(:) = 0

      enddo

   10 CONTINUE 

      call fvsGetICCode(i)

      IF (i .EQ. 0) STOP	

      GO TO (11,12,13,14,15), i 
   11 CONTINUE
      STOP 10
   12 CONTINUE
      STOP 20
   13 CONTINUE
      STOP 30
   14 CONTINUE
      STOP 40
   15 CONTINUE
      STOP 50
      END
      
