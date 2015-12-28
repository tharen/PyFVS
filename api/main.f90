      PROGRAM MAIN
      use blkdat_mod, only: blkdat
      use esblkd_mod, only: esblkd
      use cubrds_mod, only: cubrds
      use siteht_mod
      use plot_mod, only: sitear
      IMPLICIT NONE

!----------
!  $Id: main.f 829 2013-05-08 20:55:27Z rhavis@msn.com $
!----------
      INTEGER rtnCode,lenCL,i

#ifdef PROFILING
      ! In profiling mode multiple iterations of the same keyword
      ! file can be run to increase the sample size for gprof
      integer :: iters,j
      character(len=32) :: arg

      print '(a)', 'Do profiling iterations'
      iters = 1
      do j=0, command_argument_count()
        call get_command_argument(j,arg)
        if (len_trim(arg)==0) exit
        if ((arg=='--iters') .or. (arg=='-i')) then
            call get_command_argument(j+1,arg)
            read(arg, '(i10)'), iters
            print '(a i4)', 'Number of iterations:',iters
        endif
      enddo

      do j=1, iters
#endif

      ! Initialize the variant parameters and arrays
      call blkdat()
      call esblkd()
      call cubrds()

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

#ifdef PROFILING
      print '(a i4)',"End of run: ",j
      if (i > 0) exit
      enddo
#endif

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
      
