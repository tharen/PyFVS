module fvs_wrap
    use iso_c_binding, only : c_char
    use cstr, only : f2c_string, c2f_string

    implicit none

    contains

    subroutine version(ver) bind(c, name='version')
        character(kind=c_char, len=1) :: ver(7)

        ver = f2c_string('0.0.0a0')

    end subroutine version

    subroutine run_fvs(kwd, rtn) bind(c, name='run_fvs')
        use iso_c_binding, only : c_int
        character(kind=c_char), intent(in) :: kwd(*)
        integer(c_int), intent(out) :: rtn
        character(len=255) :: kwd_f
        integer :: l
        
        kwd_f = c2f_string(kwd)
        l = len_trim(kwd_f)
        print *, kwd_f(1:l+14)
        call fvssetcmdline('--keywordfile='//trim(kwd_f),l+14,rtn)
        call fvs(rtn)
    end subroutine run_fvs

end module fvs_wrap


