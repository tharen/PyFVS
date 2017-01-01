module cstr
    implicit none
    
    contains

    pure function f2c_string(f_str) result(c_str)
        ! Convert from a fortran string to a C string
        ! From http://fortranwiki.org/fortran/show/Generating+C+Interfaces
        use iso_c_binding, only : c_char, c_null_char
        implicit none

        character(len=*), intent(in) :: f_str
        ! c_char strings must have length 1
        character(kind=c_char, len=1) :: c_str(len_trim(f_str)+1)
        integer i,n

        n = len_trim(f_str)
        do i=1,n
            c_str(i:i) = f_str(i:i)
        end do
        c_str(n+1:n+1) = c_null_char

    end function f2c_string

    function c2f_string(c_str) result(f_str)
        use iso_c_binding, only : c_char, c_null_char
        implicit none

        character(kind=c_char, len=1), intent(in) :: c_str(*)
        character(len=:), allocatable :: f_str
        integer i,j

        i = 1
        do
            if (c_str(i+1)==c_null_char) exit
            i = i + 1
        end do
    
    allocate(character(len=i) :: f_str)
        do j=1,i
            f_str(j:j) = c_str(j)
        end do
    
    end function c2f_string

end module cstr
