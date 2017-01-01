module prgprm_mod
    use iso_c_binding
    implicit none
!
!     PARAMETERS FOR THE PROGNOSIS MODEL ARE:
!
      integer :: MAXTRE,MAXTP1,MAXPLT,MAXSP
      integer :: MAXCYC,MAXCY1,MAXSTR,MXFRCDS

      PARAMETER (MAXTRE=3000)
      PARAMETER (MAXTP1=MAXTRE+1)
      PARAMETER (MAXPLT=500)
      PARAMETER (MAXSP =39)
      PARAMETER (MAXCYC=40)
      PARAMETER (MAXCY1=MAXCYC+1)
      PARAMETER (MAXSTR=20)
      PARAMETER (MXFRCDS=20)

      integer(kind=c_int), protected, bind(c,name='maxtre') :: maxtre_c
      integer(kind=c_int), protected, bind(c,name='maxtp1') :: maxtp1_c
      integer(kind=c_int), protected, bind(c,name='maxplt') :: maxplt_c

      data maxtre_c/maxtre/
      data maxtp1_c/maxtp1/
      data maxplt_c/maxplt/

      contains

      function get_maxtre() result(i) bind(c)
          integer(c_int) :: i
          i = maxtre
      end function
      function get_maxplt() result(i) bind(c)
          integer(c_int) :: i
          i = maxplt
      end function
      function get_maxsp() result(i) bind(c)
          integer(c_int) :: i
          i = maxsp
      end function
      function get_maxcyc() result(i) bind(c)
          integer(c_int) :: i
          i = maxcyc
      end function



!
!     *** PARAMETERS OF OPTION PROCESSING ARE IN COMMON OPCOM ***
!
!     MAXTRE= THE MAX NUMBER OF TREE RECORDS THAT PROGNOSIS CAN PROCESS.
!             NOTE: A REALISTIC MIN VALUE FOR THIS PARAMETER IS ABOUT
!                   400.  IF THE ESTABLISHMENT EXTENSION IS NOT BEING
!                   USED, A SOMEWHAT SMALLER VALUED MAY BE USED.  ALSO
!                   NOTE THAT CHANGING THIS PARAMETER MAY CHANGE THE
!                   MODELS BEHAVIOR.  THE NORMAL SETTING IS 1350.
!     MAXTP1= THE MAX NUMBER OF TREE RECORDS PLUS 1.
!     MAXPLT= THE MAX NUMBER OF INDIVIDUAL PLOTS THAT PROGNOSIS CAN
!             PROCESS.
!     MAXSP = THE MAX NUMBER OF SPECIES REPRESENTED IN THE MODEL.
!     MAXCYC= THE MAX NUMBER OF CYCLES ALLOWED IN THE MODEL.
!     MAXCY1= THE MAX NUMBER OF CYCLES PLUS 1.
!     MAXSTR= MAXIMUM NUMBER OF SITE TREES.
!     MXFRCDS=MAXIMUM FOREST CODES (ESCOMN).
!

end module prgprm_mod
