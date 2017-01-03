module fvs_wrap
    use iso_c_binding, only : c_char,c_int
    use cstr, only : f2c_string, c2f_string

    implicit none

    contains

    subroutine api_version(ver) bind(c, name='api_version')
        character(kind=c_char, len=1), intent(out) :: ver(8)

        ver = f2c_string('0.1.0a')

    end subroutine api_version

    subroutine run_fvs(kwd, rtn) bind(c, name='run_fvs')
        character(kind=c_char, len=1), intent(in) :: kwd(*)
        integer(kind=c_int), intent(out) :: rtn
        
        character(len=255) :: f_kwd
        integer :: l
       
        rtn = 0 
        f_kwd = c2f_string(kwd)
        l = len_trim(f_kwd)
        call fvssetcmdline('--keywordfile='//trim(f_kwd), l+14, rtn)
        call fvs(rtn)

    end subroutine run_fvs

end module fvs_wrap


