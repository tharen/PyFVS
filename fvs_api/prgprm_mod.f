      module prgprm_mod
      use iso_c_binding
      implicit none
!     
      include "PRGPRM.F77"

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
      
      end module prgprm_mod
